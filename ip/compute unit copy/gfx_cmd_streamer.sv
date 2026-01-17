module gfx_cmd_streamer (
    input  logic        clk,
    input  logic        rst_n,

    // Configuration
    input  logic        enable,
    input  logic        cfg_valid,
    input  logic [31:0] ring_base,
    input  logic [31:0] ring_size_bytes,
    input  logic [31:0] ring_cons_ptr_bytes,
    input  logic [31:0] completion_base,

    // Status
    output logic        busy,
    output logic [31:0] cons_ptr_bytes,
    output logic [31:0] fence_counter,
    output logic        fault,
    output logic [7:0]  fault_opcode,

    // External 32-bit memory port (single outstanding read assumed)
    output logic        mem_req_valid,
    output logic        mem_req_is_load,
    output logic [31:0] mem_req_addr,
    output logic [31:0] mem_req_wdata,
    output logic [3:0]  mem_req_wstrb,
    input  logic        mem_req_ready,
    input  logic        mem_resp_valid,
    input  logic [31:0] mem_resp_data,

    // Graphics pipeline issue port
    output logic                  issue_valid,
    output isa_pkg::decode_ctrl_t issue_ctrl,
    output logic [31:0]           issue_op_a,
    output logic [31:0]           issue_op_b,
    output logic [127:0]          issue_vec_a,
    output logic [127:0]          issue_vec_b,
    input  logic                  issue_ready,
    input  logic                  queue_full,
    input  logic                  gpu_busy
);
    import isa_pkg::*;

    // Command header word0 fields
    logic [7:0]  cmd_opcode;
    logic [7:0]  cmd_flags;
    logic [7:0]  cmd_rdst;
    logic [7:0]  cmd_len;
    logic [31:0] cmd_arg0;
    logic [31:0] cmd_arg1;
    logic [31:0] cmd_arg2;

    localparam logic [7:0] FLAG_IRQ     = 8'h01;
    localparam logic [7:0] FLAG_FENCE_E = 8'h02;
    localparam logic [7:0] FLAG_FENCE_W = 8'h04;
    localparam logic [7:0] FLAG_PAYLOAD = 8'h08;

    localparam logic [7:0] OP_WRITE_MEM   = 8'h01;
    localparam logic [7:0] OP_READ_MEM    = 8'h02;
    localparam logic [7:0] OP_BARRIER     = 8'h22;
    localparam logic [7:0] OP_COPY_BUFFER = 8'h23;

    localparam logic [7:0] OP_LOAD_GSTATE = 8'h40;
    localparam logic [7:0] OP_LOAD_GPARAM = 8'h41;
    localparam logic [7:0] OP_GDRAW       = 8'h42;
    localparam logic [7:0] OP_LOAD_RSTATE = 8'h43;
    localparam logic [7:0] OP_RDRAW       = 8'h44;
    localparam logic [7:0] OP_RRECT       = 8'h45;

    // gfx_pipeline funct3 mapping
    localparam logic [2:0] F3_RSTATE = 3'b000;
    localparam logic [2:0] F3_RSETUP = 3'b001;
    localparam logic [2:0] F3_RDRAW  = 3'b010;
    localparam logic [2:0] F3_RRECT  = 3'b011;
    localparam logic [2:0] F3_GSTATE = 3'b100;
    localparam logic [2:0] F3_GPARAM = 3'b101;
    localparam logic [2:0] F3_GDRAW  = 3'b110;

    function automatic decode_ctrl_t make_gfx_ctrl(input logic [2:0] funct3);
        decode_ctrl_t c;
        c = '0;
        c.is_valid = 1'b1;
        c.is_gfx   = 1'b1;
        c.funct3   = funct3;
        return c;
    endfunction

    typedef enum logic [4:0] {
        ST_IDLE,

        ST_HDR_REQ,
        ST_HDR_WAIT,
        ST_DECODE,

        ST_FENCE_WAIT_REQ,
        ST_FENCE_WAIT_WAIT,
        ST_FENCE_WAIT_CHECK,

        ST_EXEC_ISSUE,
        ST_EXEC_BARRIER,

        ST_WRITE_PAYLOAD_RD_REQ,
        ST_WRITE_PAYLOAD_RD_WAIT,
        ST_WRITE_PAYLOAD_WR_REQ,
        ST_WRITE_PAYLOAD_WR_WAIT,

        ST_EMIT_WAIT_GPU,
        ST_EMIT_WR_REQ,
        ST_EMIT_WR_WAIT,

        ST_ADVANCE
    } state_t;

    state_t state;

    logic [31:0] cons_off;
    logic [31:0] ring_base_r;
    logic [31:0] ring_size_r;
    logic [31:0] completion_base_r;

    logic [31:0] hdr_words [0:3];
    logic [1:0]  hdr_idx;

    logic [31:0] payload_words;
    logic [31:0] payload_idx;
    logic [31:0] payload_data;

    // Helpers for ring wrap
    function automatic logic [31:0] ring_add(input logic [31:0] off, input logic [31:0] inc);
        logic [31:0] tmp;
        begin
            tmp = off + inc;
            if (ring_size_r != 32'd0) begin
                if (tmp >= ring_size_r) tmp = tmp - ring_size_r;
            end
            return tmp;
        end
    endfunction

    function automatic logic [31:0] ring_addr(input logic [31:0] off);
        return ring_base_r + off;
    endfunction

    function automatic logic [31:0] completion_addr(input logic [7:0] slot);
        return completion_base_r + {22'd0, slot, 2'b00};
    endfunction

    always_comb begin
        // Defaults
        mem_req_valid   = 1'b0;
        mem_req_is_load = 1'b1;
        mem_req_addr    = 32'd0;
        mem_req_wdata   = 32'd0;
        mem_req_wstrb   = 4'b0000;

        issue_valid = 1'b0;
        issue_ctrl  = '0;
        issue_op_a  = 32'd0;
        issue_op_b  = 32'd0;
        issue_vec_a = 128'd0;
        issue_vec_b = 128'd0;

        busy = (state != ST_IDLE);

        unique case (state)
            ST_HDR_REQ: begin
                mem_req_valid   = 1'b1;
                mem_req_is_load = 1'b1;
                mem_req_addr    = ring_addr(ring_add(cons_off, {30'd0, hdr_idx, 2'b00}));
            end

            ST_FENCE_WAIT_REQ: begin
                mem_req_valid   = 1'b1;
                mem_req_is_load = 1'b1;
                mem_req_addr    = completion_addr(cmd_rdst);
            end

            ST_WRITE_PAYLOAD_RD_REQ: begin
                mem_req_valid   = 1'b1;
                mem_req_is_load = 1'b1;
                mem_req_addr    = ring_addr(ring_add(cons_off, 32'd16 + (payload_idx << 2)));
            end

            ST_WRITE_PAYLOAD_WR_REQ: begin
                mem_req_valid   = 1'b1;
                mem_req_is_load = 1'b0;
                mem_req_addr    = cmd_arg0 + (payload_idx << 2);
                mem_req_wdata   = payload_data;
                mem_req_wstrb   = 4'hF;
            end

            ST_EMIT_WR_REQ: begin
                mem_req_valid   = 1'b1;
                mem_req_is_load = 1'b0;
                mem_req_addr    = completion_addr(cmd_rdst);
                mem_req_wdata   = fence_counter + 32'd1;
                mem_req_wstrb   = 4'hF;
            end

            ST_EXEC_ISSUE: begin
                unique case (cmd_opcode)
                    OP_LOAD_RSTATE: begin
                        issue_valid = 1'b1;
                        issue_ctrl  = make_gfx_ctrl(F3_RSTATE);
                        issue_op_a  = cmd_arg1;
                    end
                    OP_LOAD_GSTATE: begin
                        issue_valid = 1'b1;
                        issue_ctrl  = make_gfx_ctrl(F3_GSTATE);
                        issue_op_a  = cmd_arg1;
                    end
                    OP_LOAD_GPARAM: begin
                        issue_valid = 1'b1;
                        issue_ctrl  = make_gfx_ctrl(F3_GPARAM);
                        issue_op_a  = cmd_arg1;
                    end
                    OP_GDRAW: begin
                        issue_valid = 1'b1;
                        issue_ctrl  = make_gfx_ctrl(F3_GDRAW);
                        issue_op_a  = cmd_arg0;
                    end
                    OP_RDRAW: begin
                        issue_valid = 1'b1;
                        issue_ctrl  = make_gfx_ctrl(F3_RDRAW);
                        issue_op_a  = cmd_arg0;
                    end
                    OP_RRECT: begin
                        issue_valid = 1'b1;
                        issue_ctrl  = make_gfx_ctrl(F3_RRECT);
                        issue_op_a  = cmd_arg0;
                    end
                    default: begin
                        // no issue
                    end
                endcase
            end

            default: begin end
        endcase
    end

    // Sequential FSM
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state             <= ST_IDLE;
            ring_base_r        <= 32'd0;
            ring_size_r        <= 32'd0;
            completion_base_r  <= 32'd0;
            cons_off           <= 32'd0;
            cons_ptr_bytes     <= 32'd0;
            hdr_idx            <= 2'd0;
            cmd_opcode         <= 8'd0;
            cmd_flags          <= 8'd0;
            cmd_rdst           <= 8'd0;
            cmd_len            <= 8'd0;
            cmd_arg0           <= 32'd0;
            cmd_arg1           <= 32'd0;
            cmd_arg2           <= 32'd0;
            payload_words      <= 32'd0;
            payload_idx        <= 32'd0;
            payload_data       <= 32'd0;
            fence_counter      <= 32'd0;
            fault              <= 1'b0;
            fault_opcode       <= 8'd0;
        end else begin
            // Configuration load
            if (cfg_valid) begin
                ring_base_r       <= ring_base;
                ring_size_r       <= ring_size_bytes;
                completion_base_r <= completion_base;
                if ((ring_size_bytes != 32'd0) && ((ring_size_bytes & (ring_size_bytes - 32'd1)) == 32'd0)) begin
                    cons_off <= ring_cons_ptr_bytes & (ring_size_bytes - 32'd1);
                end else begin
                    cons_off <= ring_cons_ptr_bytes;
                end
                cons_ptr_bytes    <= ring_cons_ptr_bytes;
                fault             <= 1'b0;
                fault_opcode      <= 8'd0;
            end

            unique case (state)
                ST_IDLE: begin
                    hdr_idx <= 2'd0;
                    if (enable && (ring_size_r >= 32'd16) && ((ring_size_r & 32'd3) == 32'd0)) begin
                        state <= ST_HDR_REQ;
                    end
                end

                ST_HDR_REQ: begin
                    if (mem_req_ready) begin
                        state <= ST_HDR_WAIT;
                    end
                end

                ST_HDR_WAIT: begin
                    if (mem_resp_valid) begin
                        hdr_words[hdr_idx] <= mem_resp_data;
                        if (hdr_idx == 2'd3) begin
                            state   <= ST_DECODE;
                        end else begin
                            hdr_idx <= hdr_idx + 2'd1;
                            state   <= ST_HDR_REQ;
                        end
                    end
                end

                ST_DECODE: begin
                    cmd_opcode <= hdr_words[0][7:0];
                    cmd_flags  <= hdr_words[0][15:8];
                    cmd_rdst   <= hdr_words[0][23:16];
                    cmd_len    <= hdr_words[0][31:24];
                    cmd_arg0   <= hdr_words[1];
                    cmd_arg1   <= hdr_words[2];
                    cmd_arg2   <= hdr_words[3];

                    payload_words <= ((hdr_words[0][15:8] & FLAG_PAYLOAD) != 8'd0) ? {24'd0, hdr_words[0][31:24]} : 32'd0;
                    payload_idx   <= 32'd0;

                    if ((hdr_words[0][15:8] & FLAG_FENCE_W) != 8'd0) begin
                        state <= ST_FENCE_WAIT_REQ;
                    end else begin
                        state <= ST_EXEC_ISSUE;
                    end
                end

                // Fence wait: poll completion slot until it reaches current fence_counter
                ST_FENCE_WAIT_REQ: begin
                    if (mem_req_ready) begin
                        state <= ST_FENCE_WAIT_WAIT;
                    end
                end

                ST_FENCE_WAIT_WAIT: begin
                    if (mem_resp_valid) begin
                        payload_data <= mem_resp_data; // reuse as scratch
                        state        <= ST_FENCE_WAIT_CHECK;
                    end
                end

                ST_FENCE_WAIT_CHECK: begin
                    if (payload_data >= fence_counter) begin
                        state <= ST_EXEC_ISSUE;
                    end else begin
                        state <= ST_FENCE_WAIT_REQ;
                    end
                end

                // Execute/dispatch
                ST_EXEC_ISSUE: begin
                    // Special case: barrier is a pure drain point
                    if (cmd_opcode == OP_BARRIER) begin
                        state <= ST_EXEC_BARRIER;
                    end else if (cmd_opcode == OP_WRITE_MEM) begin
                        // Copy payload to VRAM at arg0
                        if (payload_words != 32'd0) begin
                            state <= ST_WRITE_PAYLOAD_RD_REQ;
                        end else begin
                            state <= ((cmd_flags & FLAG_FENCE_E) != 8'd0) ? ST_EMIT_WAIT_GPU : ST_ADVANCE;
                        end
                    end else if ((cmd_opcode == OP_LOAD_GSTATE) || (cmd_opcode == OP_LOAD_GPARAM) || (cmd_opcode == OP_LOAD_RSTATE)
                          || (cmd_opcode == OP_GDRAW) || (cmd_opcode == OP_RDRAW) || (cmd_opcode == OP_RRECT)) begin
                        // Issue to graphics pipeline when there is space
                        if (!queue_full && issue_ready) begin
                            if ((cmd_flags & FLAG_FENCE_E) != 8'd0) begin
                                state <= ST_EMIT_WAIT_GPU;
                            end else begin
                                state <= ST_ADVANCE;
                            end
                        end
                    end else begin
                        // Unsupported op: skip it, but remember the opcode
                        fault        <= 1'b1;
                        fault_opcode <= cmd_opcode;
                        state        <= ((cmd_flags & FLAG_FENCE_E) != 8'd0) ? ST_EMIT_WAIT_GPU : ST_ADVANCE;
                    end
                end

                ST_EXEC_BARRIER: begin
                    if (!gpu_busy) begin
                        state <= ((cmd_flags & FLAG_FENCE_E) != 8'd0) ? ST_EMIT_WR_REQ : ST_ADVANCE;
                    end
                end

                // WRITE_MEM payload copy: ring payload -> VRAM
                ST_WRITE_PAYLOAD_RD_REQ: begin
                    if (mem_req_ready) begin
                        state <= ST_WRITE_PAYLOAD_RD_WAIT;
                    end
                end

                ST_WRITE_PAYLOAD_RD_WAIT: begin
                    if (mem_resp_valid) begin
                        payload_data <= mem_resp_data;
                        state        <= ST_WRITE_PAYLOAD_WR_REQ;
                    end
                end

                ST_WRITE_PAYLOAD_WR_REQ: begin
                    if (mem_req_ready) begin
                        state <= ST_WRITE_PAYLOAD_WR_WAIT;
                    end
                end

                ST_WRITE_PAYLOAD_WR_WAIT: begin
                    if (payload_idx + 32'd1 >= payload_words) begin
                        state <= ((cmd_flags & FLAG_FENCE_E) != 8'd0) ? ST_EMIT_WAIT_GPU : ST_ADVANCE;
                    end else begin
                        payload_idx <= payload_idx + 32'd1;
                        state       <= ST_WRITE_PAYLOAD_RD_REQ;
                    end
                end

                // Fence emit: wait for GPU drain, then write fence value
                ST_EMIT_WAIT_GPU: begin
                    if (!gpu_busy) begin
                        state <= ST_EMIT_WR_REQ;
                    end
                end

                ST_EMIT_WR_REQ: begin
                    if (mem_req_ready) begin
                        fence_counter <= fence_counter + 32'd1;
                        state         <= ST_EMIT_WR_WAIT;
                    end
                end

                ST_EMIT_WR_WAIT: begin
                    state <= ST_ADVANCE;
                end

                // Advance consumer pointer past header + payload
                ST_ADVANCE: begin
                    // total bytes = 16 + payload_words*4
                    cons_off       <= ring_add(cons_off, 32'd16 + (payload_words << 2));
                    cons_ptr_bytes <= ring_add(cons_off, 32'd16 + (payload_words << 2));
                    hdr_idx        <= 2'd0;
                    state          <= ST_HDR_REQ;
                end

                default: state <= ST_IDLE;
            endcase
        end
    end

endmodule
