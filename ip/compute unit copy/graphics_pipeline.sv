module graphics_pipeline (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        flush_all,

    // Issue Interface (dual-enqueue into internal FIFO)
    input  logic        issue0_valid,
    input  isa_pkg::decode_ctrl_t issue0_ctrl,
    input  logic [31:0] issue0_op_a,
    input  logic [31:0] issue0_op_b,
    input  logic [127:0] issue0_vec_a,
    input  logic [127:0] issue0_vec_b,

    input  logic        issue1_valid,
    input  isa_pkg::decode_ctrl_t issue1_ctrl,
    input  logic [31:0] issue1_op_a,
    input  logic [31:0] issue1_op_b,
    input  logic [127:0] issue1_vec_a,
    input  logic [127:0] issue1_vec_b,

    output logic        queue_full,
    output logic        queue_afull,
    output logic [3:0]  queue_count, // For debug/status

    // High-level activity indicator (queue or in-flight work).
    output logic        busy,

    // Writeback Interface
    output logic        wb_valid, // For texture results
    output logic [4:0]  wb_rd,
    output logic [127:0] wb_data,
    output logic        wb_is_scalar, // Reserved

    // Texture Cache Interface (sampling)
    output logic        tex_req_valid,
    output logic [31:0] tex_req_addr,
    output logic [4:0]  tex_req_rd,
    input  logic        tex_req_ready,
    input  logic        tex_resp_valid,
    input  logic [31:0] tex_resp_data,
    input  logic [4:0]  tex_resp_rd,

    // GFX Descriptor Cache Interface (descriptor + geometry fetch)
    output logic        gfxd_req_valid,
    output logic [31:0] gfxd_req_addr,
    output logic [4:0]  gfxd_req_rd,
    input  logic        gfxd_req_ready,
    input  logic        gfxd_resp_valid,
    input  logic [31:0] gfxd_resp_data,
    input  logic [4:0]  gfxd_resp_rd,

    // Graphics/ROP store interface (to LSU WMB)
    output logic        gfx_st_valid,
    output logic [31:0] gfx_st_addr,
    output logic [31:0] gfx_st_wdata,
    output logic [3:0]  gfx_st_wstrb,
    input  logic        gfx_st_ready
);
    import isa_pkg::*;

    parameter logic [31:0] SAMPLER_TABLE_BASE = 32'h0;
    parameter int GQ_DEPTH = 8;
    parameter int TEX_REQ_Q_DEPTH = 2;
    parameter int ROP_QUAD_Q_DEPTH = 2;
    parameter int ROP_STQ_DEPTH  = 2;
    parameter int PRIM_Q_DEPTH = 4;

    localparam int unsigned GQ_IDX_W = (GQ_DEPTH <= 1) ? 1 : $clog2(GQ_DEPTH);
    localparam int unsigned GQ_CNT_W = (GQ_DEPTH <= 1) ? 1 : $clog2(GQ_DEPTH + 1);
    localparam logic [GQ_IDX_W-1:0] GQ_LAST = GQ_IDX_W'(GQ_DEPTH - 1);

    initial begin
        if (GQ_DEPTH < 1) $fatal(1, "graphics_pipeline: GQ_DEPTH must be >= 1");
        if (TEX_REQ_Q_DEPTH < 1) $fatal(1, "graphics_pipeline: TEX_REQ_Q_DEPTH must be >= 1");
        if (ROP_QUAD_Q_DEPTH < 1) $fatal(1, "graphics_pipeline: ROP_QUAD_Q_DEPTH must be >= 1");
        if (ROP_STQ_DEPTH  < 1) $fatal(1, "graphics_pipeline: ROP_STQ_DEPTH must be >= 1");
        if (PRIM_Q_DEPTH  < 1) $fatal(1, "graphics_pipeline: PRIM_Q_DEPTH must be >= 1");
    end

    // Graphics/Texture Issue Queue
    typedef struct packed {
        decode_ctrl_t ctrl;
        logic [31:0]  op_a;
        logic [31:0]  op_b;
        logic [127:0] vec_a;
        logic [127:0] vec_b;
    } gfx_issue_t;

    gfx_issue_t gq [GQ_DEPTH];
    logic [GQ_DEPTH-1:0] gq_valid;
    logic [GQ_IDX_W-1:0] gq_head;
    logic [GQ_IDX_W-1:0] gq_tail;
    logic [GQ_CNT_W-1:0] gq_count_internal;
    
    generate
        if (GQ_DEPTH <= 16) begin : gen_gfx_qcount_small
            assign queue_count = gq_count_internal[3:0];
        end else begin : gen_gfx_qcount_sat
            assign queue_count = (gq_count_internal > 4'd15) ? 4'd15 : gq_count_internal[3:0];
        end
    endgenerate
    assign queue_full = (gq_count_internal == GQ_CNT_W'(GQ_DEPTH));
    generate
        if (GQ_DEPTH <= 1) begin : gen_qafull_depth1
            assign queue_afull = queue_full;
        end else begin : gen_qafull
            assign queue_afull = (gq_count_internal >= GQ_CNT_W'(GQ_DEPTH - 1));
        end
    endgenerate

    // Per-entry dispatch/complete tracking (enables concurrent TEX+GFX in-flight while
    // retiring the FIFO strictly in-order)
    logic [GQ_DEPTH-1:0] gq_issued;
    logic [GQ_DEPTH-1:0] gq_done;

    // In-flight entry indices for engines
    logic [GQ_IDX_W-1:0] tex_idx;
    logic [GQ_IDX_W-1:0] gfx_idx;

    // Scheduler decisions (combinational, sampled on clk edge)
    logic tex_issue_fire;
    logic gfx_issue_fire;
    logic [GQ_IDX_W-1:0] tex_issue_idx;
    logic [GQ_IDX_W-1:0] gfx_issue_idx;

    // Completion pulses (combinational from engine state, sampled on clk edge)
    logic tex_complete_pulse;
    logic gfx_complete_pulse;

    // Decoupling FIFOs (defined later): expose minimal handshake signals for FSMs
    logic texq_can_push;
    logic tex_mem_enq_fire;
    logic gfx_mem_enq_fire;

    // Queue Control Logic
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            gq_head <= '0;
            gq_tail <= '0;
            gq_count_internal <= '0;
            gq_valid <= '0;
            gq_issued <= '0;
            gq_done <= '0;
        end else if (flush_all) begin
            gq_head <= '0;
            gq_tail <= '0;
            gq_count_internal <= '0;
            gq_valid <= '0;
            gq_issued <= '0;
            gq_done <= '0;
        end else begin
            // Dual-issue push/pop accounting (retire frees space for same-cycle pushes)
            logic [GQ_CNT_W-1:0] count_after_pop;
            logic               push0_fire;
            logic               push1_fire;
            logic [GQ_IDX_W-1:0] tail0;
            logic [GQ_IDX_W-1:0] tail_next1;
            logic [GQ_IDX_W-1:0] tail_next2;
            logic [GQ_IDX_W-1:0] tail_push1;
            logic [GQ_IDX_W-1:0] head1;
            logic [GQ_IDX_W-1:0] head2;
            logic [1:0]          pop_count;

            head1 = (gq_head == GQ_LAST) ? '0 : (gq_head + 1'b1);
            head2 = (head1  == GQ_LAST) ? '0 : (head1  + 1'b1);

            // Retire strictly in-order, but allow up to two completions per cycle.
            // pop1 is only allowed when pop0 is true.
            pop_count[0] = gq_valid[gq_head] && gq_done[gq_head];
            pop_count[1] = pop_count[0] && gq_valid[head1] && gq_done[head1];

            count_after_pop = gq_count_internal
                            - (pop_count[0] ? GQ_CNT_W'(1) : GQ_CNT_W'(0))
                            - (pop_count[1] ? GQ_CNT_W'(1) : GQ_CNT_W'(0));
            // IMPORTANT: issue1_valid may be asserted without issue0_valid (slot0=scalar/vec, slot1=gfx/tex).
            // Treat pushes independently so the queue doesn't develop a "hole" at gq_tail.
            push0_fire = issue0_valid && (count_after_pop < GQ_CNT_W'(GQ_DEPTH));
            push1_fire = issue1_valid
                      && (count_after_pop + (push0_fire ? GQ_CNT_W'(1) : GQ_CNT_W'(0)) < GQ_CNT_W'(GQ_DEPTH));

`ifndef SYNTHESIS
            // Sentinel trace at the queue input boundary: prove whether the RRECT op is
            // actually presenting on issue0/issue1 and whether it is being dropped due
            // to lack of queue space.
            if ((dbg_sentinel_q_events < 32)
                && issue0_valid
                && issue0_ctrl.is_gfx
                && (issue0_ctrl.funct3 == 3'b011)
                && (issue0_op_a[15:0] == 16'h8000)) begin
                $display("%0t GFX_SENT IN0 ptr=%08h push0=%0b cnt_after_pop=%0d head=%0d tail=%0d", $time,
                         issue0_op_a, push0_fire, count_after_pop, gq_head, gq_tail);
                dbg_sentinel_q_events <= dbg_sentinel_q_events + 1;
            end
            if ((dbg_sentinel_q_events < 32)
                && issue1_valid
                && issue1_ctrl.is_gfx
                && (issue1_ctrl.funct3 == 3'b011)
                && (issue1_op_a[15:0] == 16'h8000)) begin
                $display("%0t GFX_SENT IN1 ptr=%08h push1=%0b cnt_after_pop=%0d head=%0d tail=%0d", $time,
                         issue1_op_a, push1_fire, count_after_pop, gq_head, gq_tail);
                dbg_sentinel_q_events <= dbg_sentinel_q_events + 1;
            end
`endif

            tail0      = gq_tail;
            tail_next1 = (tail0      == GQ_LAST) ? '0 : (tail0      + 1'b1);
            tail_next2 = (tail_next1 == GQ_LAST) ? '0 : (tail_next1 + 1'b1);
            tail_push1 = push0_fire ? tail_next1 : tail0;

            if (push0_fire) begin
                gq[tail0].ctrl  <= issue0_ctrl;
                gq[tail0].op_a  <= issue0_op_a;
                gq[tail0].op_b  <= issue0_op_b;
                gq[tail0].vec_a <= issue0_vec_a;
                gq[tail0].vec_b <= issue0_vec_b;
                gq_valid[tail0]  <= 1'b1;
                gq_issued[tail0] <= 1'b0;
                gq_done[tail0]   <= 1'b0;
`ifndef SYNTHESIS
                if ((dbg_sentinel_q_events < 32)
                    && issue0_ctrl.is_gfx
                    && (issue0_ctrl.funct3 == 3'b011)
                    && (issue0_op_a[15:0] == 16'h8000)) begin
                    $display("%0t GFX_SENT ENQ0 idx=%0d ptr=%08h head=%0d tail=%0d cnt_after_pop=%0d", $time,
                             tail0, issue0_op_a, gq_head, gq_tail, count_after_pop);
                    dbg_sentinel_q_events <= dbg_sentinel_q_events + 1;
                end
`endif
            end

            if (push1_fire) begin
                gq[tail_push1].ctrl  <= issue1_ctrl;
                gq[tail_push1].op_a  <= issue1_op_a;
                gq[tail_push1].op_b  <= issue1_op_b;
                gq[tail_push1].vec_a <= issue1_vec_a;
                gq[tail_push1].vec_b <= issue1_vec_b;
                gq_valid[tail_push1]  <= 1'b1;
                gq_issued[tail_push1] <= 1'b0;
                gq_done[tail_push1]   <= 1'b0;
`ifndef SYNTHESIS
                if ((dbg_sentinel_q_events < 32)
                    && issue1_ctrl.is_gfx
                    && (issue1_ctrl.funct3 == 3'b011)
                    && (issue1_op_a[15:0] == 16'h8000)) begin
                    $display("%0t GFX_SENT ENQ1 idx=%0d ptr=%08h head=%0d tail=%0d cnt_after_pop=%0d", $time,
                             tail_push1, issue1_op_a, gq_head, gq_tail, count_after_pop);
                    dbg_sentinel_q_events <= dbg_sentinel_q_events + 1;
                end
`endif
            end

            if (pop_count[0]) begin
                gq_valid[gq_head]  <= 1'b0;
                gq_issued[gq_head] <= 1'b0;
                gq_done[gq_head]   <= 1'b0;
`ifndef SYNTHESIS
                if ((dbg_sentinel_q_events < 32)
                    && gq[gq_head].ctrl.is_gfx
                    && (gq[gq_head].ctrl.funct3 == 3'b011)
                    && (gq[gq_head].op_a[15:0] == 16'h8000)) begin
                    $display("%0t GFX_SENT POP0 idx=%0d ptr=%08h issued=%0b done=%0b", $time,
                             gq_head, gq[gq_head].op_a, gq_issued[gq_head], gq_done[gq_head]);
                    dbg_sentinel_q_events <= dbg_sentinel_q_events + 1;
                end
`endif
            end
            if (pop_count[1]) begin
                gq_valid[head1]  <= 1'b0;
                gq_issued[head1] <= 1'b0;
                gq_done[head1]   <= 1'b0;
`ifndef SYNTHESIS
                if ((dbg_sentinel_q_events < 32)
                    && gq[head1].ctrl.is_gfx
                    && (gq[head1].ctrl.funct3 == 3'b011)
                    && (gq[head1].op_a[15:0] == 16'h8000)) begin
                    $display("%0t GFX_SENT POP1 idx=%0d ptr=%08h issued=%0b done=%0b", $time,
                             head1, gq[head1].op_a, gq_issued[head1], gq_done[head1]);
                    dbg_sentinel_q_events <= dbg_sentinel_q_events + 1;
                end
`endif
            end

            unique case (pop_count)
                2'b00: gq_head <= gq_head;
                2'b01: gq_head <= head1;
                2'b11: gq_head <= head2;
                default: gq_head <= gq_head;
            endcase

            // Update tail/count
            unique case ({push1_fire, push0_fire})
                2'b00: gq_tail <= gq_tail;
                2'b01: gq_tail <= tail_next1;
                2'b10: gq_tail <= tail_next1;
                2'b11: gq_tail <= tail_next2;
            endcase

            gq_count_internal <= count_after_pop
                              + (push0_fire ? GQ_CNT_W'(1) : GQ_CNT_W'(0))
                              + (push1_fire ? GQ_CNT_W'(1) : GQ_CNT_W'(0));

            // Latch completion pulses into the done bitmap.
            if (tex_complete_pulse) gq_done[tex_idx] <= 1'b1;
            if (gfx_complete_pulse) gq_done[gfx_idx] <= 1'b1;

            // Mark dispatched entries.
            if (tex_issue_fire) gq_issued[tex_issue_idx] <= 1'b1;
            if (gfx_issue_fire) gq_issued[gfx_issue_idx] <= 1'b1;
        end
    end

    wire gq_head_is_tex = gq_valid[gq_head] && gq[gq_head].ctrl.is_tex;
    wire gq_head_is_gfx = gq_valid[gq_head] && gq[gq_head].ctrl.is_gfx;

    // ------------------------------------------------------------------------
    // Texture Pipeline
    // ------------------------------------------------------------------------

    // Helper Functions
    function automatic logic [7:0] srgb_lin8(input logic [7:0] c);
        // Simple gamma approx x^2.2 or x^2
        // c is 0..255. result is 0..255 linear.
        // x^2 approximation: (c*c)/255.
        // (c*c + 127) >> 8 is a fast way
        return (c * c + 8'd127) >> 8; 
    endfunction

    function automatic logic [31:0] tex_pack_rgba8(input logic [31:0] rgba, input logic srgb);
        logic [7:0] r, g, b, a;
        r = rgba[7:0]; g = rgba[15:8]; b = rgba[23:16]; a = rgba[31:24];
        if (srgb) begin
            r = srgb_lin8(r);
            g = srgb_lin8(g);
            b = srgb_lin8(b);
        end
        return {a, b, g, r};
    endfunction

    function automatic logic [127:0] tex_expand_unorm16(input logic [31:0] rgba, input logic srgb);
        logic [7:0] r, g, b, a;
        r = rgba[7:0]; g = rgba[15:8]; b = rgba[23:16]; a = rgba[31:24];
        if (srgb) begin
            r = srgb_lin8(r);
            g = srgb_lin8(g);
            b = srgb_lin8(b);
        end
        // Expand 0..255 to 0..65535 by replicating high byte: 0xAB -> 0xABAB
        return { 64'h0, {a, a}, {b, b}, {g, g}, {r, r} };
    endfunction

    function automatic logic [31:0] tex_avg4(input logic [31:0] a, input logic [31:0] b);
        // Average two packed RGBA8 colors
        return { ((a[31:24] + b[31:24]) >> 1),
                 ((a[23:16] + b[23:16]) >> 1),
                 ((a[15:8]  + b[15:8])  >> 1),
                 ((a[7:0]   + b[7:0])   >> 1)};
    endfunction

    // ------------------------------------------------------------------------
    // Texture Pipeline
    // ------------------------------------------------------------------------

    // FSM States
    typedef enum logic [4:0] {
        TEX_IDLE,
        // Descriptor words per ISA: 0x00 base, 0x04 stride, 0x08 width, 0x0C height,
        // 0x10 format, 0x14 wrap, 0x18 filter, 0x1C misc.
        TEX_DESC_BASE_REQ,   TEX_DESC_BASE_WAIT,
        TEX_DESC_STRIDE_REQ, TEX_DESC_STRIDE_WAIT,
        TEX_DESC_WIDTH_REQ,  TEX_DESC_WIDTH_WAIT,
        TEX_DESC_HEIGHT_REQ, TEX_DESC_HEIGHT_WAIT,
        TEX_DESC_FMT_REQ,    TEX_DESC_FMT_WAIT,
        TEX_DESC_WRAP_REQ,   TEX_DESC_WRAP_WAIT,
        TEX_DESC_FILT_REQ,   TEX_DESC_FILT_WAIT,
        TEX_DESC_MISC_REQ,   TEX_DESC_MISC_WAIT,
        TEX_CALC_ADDR,                   // Address calculation
        TEX_SAMPLE00_REQ, TEX_SAMPLE00_WAIT,
        TEX_SAMPLE10_REQ, TEX_SAMPLE10_WAIT,
        TEX_SAMPLE01_REQ, TEX_SAMPLE01_WAIT,
        TEX_SAMPLE11_REQ, TEX_SAMPLE11_WAIT,
        TEX_DONE
    } tex_state_e;

    tex_state_e tex_state;
    
    // Descriptor fields
    logic [31:0] tex_desc_base;
    logic [31:0] tex_desc_stride;
    logic [31:0] tex_desc_width;
    logic [31:0] tex_desc_height;
    logic [31:0] tex_desc_format;
    logic [31:0] tex_desc_wrap;
    logic [31:0] tex_desc_filter;
    logic [31:0] tex_desc_misc;

    // Decoded fields
    logic        is_bilinear;
    logic        is_srgb;
    logic        is_unorm_expand;
    logic        wrap_u; // 0=Clamp, 1=Repeat
    logic        wrap_v;
    logic [15:0] tex_w;
    logic [15:0] tex_h;
    logic [1:0]  fmt_code;
    logic [1:0]  bpp_shift;

    // Working vars
    logic [31:0] cur_u, cur_v;
    logic [4:0]  saved_rd;
    logic [31:0] tex_sampler_ptr;
    
    // Sample Storage
    logic [31:0] samp_00, samp_10, samp_01, samp_11;
    
    // Address Calc vars
    logic [31:0] addr_00, addr_10, addr_01, addr_11;

    // Initial U/V from instruction
    logic [31:0] raw_u, raw_v;

    // Output buffering
    logic [127:0] tex_res_vec;

    // Scheduler completion pulse
    assign tex_complete_pulse = (tex_state == TEX_DONE);
    
    // Address wrapping logic
    function automatic logic [31:0] apply_wrap(
        input logic [31:0] coord,
        input logic [15:0] max_dim,
        input logic        is_wrap
    );
        logic signed [31:0] scoord;
        logic signed [31:0] dim;
        logic signed [31:0] r;
        scoord = signed'(coord);
        dim    = signed'({16'h0, max_dim});

        if (max_dim == 16'h0) return 32'h0;

        if (!is_wrap) begin
            if (scoord < 0) return 32'h0;
            if (scoord >= dim) return logic'(dim - 1);
            return coord;
        end

        // Repeat: modulo with sign fixup
        r = scoord % dim;
        if (r < 0) r = r + dim;
        return logic'(r);
    endfunction

    function automatic logic [31:0] unpack_texel_to_argb8888(
        input logic [31:0] raw,
        input logic [1:0]  format_code
    );
        logic [15:0] rgb565;
        logic [7:0] r8, g8, b8;
        case (format_code)
            2'd0: unpack_texel_to_argb8888 = raw; // ARGB8888
            2'd1: begin
                rgb565 = raw[15:0];
                r8 = {rgb565[15:11], rgb565[15:13]};
                g8 = {rgb565[10:5],  rgb565[10:9]};
                b8 = {rgb565[4:0],   rgb565[4:2]};
                unpack_texel_to_argb8888 = {8'hFF, b8, g8, r8};
            end
            default: unpack_texel_to_argb8888 = raw;
        endcase
    endfunction

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tex_state <= TEX_IDLE;
            tex_desc_base   <= 0;
            tex_desc_stride <= 0;
            tex_desc_width  <= 0;
            tex_desc_height <= 0;
            tex_desc_format <= 0;
            tex_desc_wrap   <= 0;
            tex_desc_filter <= 0;
            tex_desc_misc   <= 0;
            is_bilinear <= 1'b0;
            is_srgb <= 1'b0;
            is_unorm_expand <= 1'b0;
            wrap_u <= 1'b0;
            wrap_v <= 1'b0;
            tex_w <= 16'h0;
            tex_h <= 16'h0;
            fmt_code <= 2'h0;
            bpp_shift <= 2'd2;
            tex_sampler_ptr <= 32'h0;
            tex_idx <= '0;
            wb_valid <= 0;
            wb_rd <= 0;
            wb_data <= 0;
            wb_is_scalar <= 0;
        end else if (flush_all) begin
            tex_state <= TEX_IDLE;
            tex_sampler_ptr <= 32'h0;
            tex_idx <= '0;
            wb_valid <= 0; // Cancel writeback on flush
            is_bilinear <= 1'b0;
            is_srgb <= 1'b0;
            is_unorm_expand <= 1'b0;
        end else begin
            wb_valid <= 0;

            case (tex_state)
                TEX_IDLE: begin
                    // Start TEX from the scheduler-selected FIFO entry.
                    if (tex_issue_fire) begin
                        tex_idx <= tex_issue_idx;
                        saved_rd <= gq[tex_issue_idx].ctrl.rd;
                        raw_u    <= gq[tex_issue_idx].vec_a[31:0];
                        raw_v    <= gq[tex_issue_idx].vec_a[63:32];
                        tex_sampler_ptr <= (gq[tex_issue_idx].op_b < 32)
                                       ? (SAMPLER_TABLE_BASE + (gq[tex_issue_idx].op_b << 5))
                                       : gq[tex_issue_idx].op_b;
                        tex_state <= TEX_DESC_BASE_REQ;
                    end
                end

                // --- Descriptor Fetch Sequence (32B) ---
                TEX_DESC_BASE_REQ:   if (tex_mem_enq_fire) tex_state <= TEX_DESC_BASE_WAIT;
                TEX_DESC_BASE_WAIT:  if (tex_resp_valid) begin tex_desc_base <= tex_resp_data; tex_state <= TEX_DESC_STRIDE_REQ; end

                TEX_DESC_STRIDE_REQ: if (tex_mem_enq_fire) tex_state <= TEX_DESC_STRIDE_WAIT;
                TEX_DESC_STRIDE_WAIT: if (tex_resp_valid) begin tex_desc_stride <= tex_resp_data; tex_state <= TEX_DESC_WIDTH_REQ; end

                TEX_DESC_WIDTH_REQ:  if (tex_mem_enq_fire) tex_state <= TEX_DESC_WIDTH_WAIT;
                TEX_DESC_WIDTH_WAIT: if (tex_resp_valid) begin tex_desc_width <= tex_resp_data; tex_w <= tex_resp_data[15:0]; tex_state <= TEX_DESC_HEIGHT_REQ; end

                TEX_DESC_HEIGHT_REQ: if (tex_mem_enq_fire) tex_state <= TEX_DESC_HEIGHT_WAIT;
                TEX_DESC_HEIGHT_WAIT: if (tex_resp_valid) begin tex_desc_height <= tex_resp_data; tex_h <= tex_resp_data[15:0]; tex_state <= TEX_DESC_FMT_REQ; end

                TEX_DESC_FMT_REQ:    if (tex_mem_enq_fire) tex_state <= TEX_DESC_FMT_WAIT;
                TEX_DESC_FMT_WAIT:   if (tex_resp_valid) begin
                    tex_desc_format <= tex_resp_data;
                    fmt_code <= tex_resp_data[1:0];
                    // bytes-per-pixel shift for address calc (ARGB8888=4B, RGB565=2B)
                    bpp_shift <= (tex_resp_data[1:0] == 2'd1) ? 2'd1 : 2'd2;
                    tex_state <= TEX_DESC_WRAP_REQ;
                end

                TEX_DESC_WRAP_REQ:   if (tex_mem_enq_fire) tex_state <= TEX_DESC_WRAP_WAIT;
                TEX_DESC_WRAP_WAIT:  if (tex_resp_valid) begin
                    tex_desc_wrap <= tex_resp_data;
                    wrap_u <= tex_resp_data[0];
                    wrap_v <= tex_resp_data[1];
                    tex_state <= TEX_DESC_FILT_REQ;
                end

                TEX_DESC_FILT_REQ:   if (tex_mem_enq_fire) tex_state <= TEX_DESC_FILT_WAIT;
                TEX_DESC_FILT_WAIT:  if (tex_resp_valid) begin
                    tex_desc_filter <= tex_resp_data;
                    is_bilinear <= tex_resp_data[0];
                    tex_state <= TEX_DESC_MISC_REQ;
                end

                TEX_DESC_MISC_REQ:   if (tex_mem_enq_fire) tex_state <= TEX_DESC_MISC_WAIT;
                TEX_DESC_MISC_WAIT:  if (tex_resp_valid) begin
                    tex_desc_misc <= tex_resp_data;
                    is_srgb <= tex_resp_data[0];
                    is_unorm_expand <= tex_resp_data[1];
                    tex_state <= TEX_CALC_ADDR;
                end

                // --- Address Calculation ---
                TEX_CALC_ADDR: begin
                    // Coordinates are treated as integer texel coordinates for now.
                    // 2x2 taps respect width/height bounds via clamp/repeat.
                    automatic logic [31:0] u0 = apply_wrap(raw_u,       tex_w, wrap_u);
                    automatic logic [31:0] u1 = apply_wrap(raw_u + 1,   tex_w, wrap_u);
                    automatic logic [31:0] v0 = apply_wrap(raw_v,       tex_h, wrap_v);
                    automatic logic [31:0] v1 = apply_wrap(raw_v + 1,   tex_h, wrap_v);

                    addr_00 <= tex_desc_base + (v0 * tex_desc_stride) + (u0 << bpp_shift);
                    addr_10 <= tex_desc_base + (v0 * tex_desc_stride) + (u1 << bpp_shift);
                    addr_01 <= tex_desc_base + (v1 * tex_desc_stride) + (u0 << bpp_shift);
                    addr_11 <= tex_desc_base + (v1 * tex_desc_stride) + (u1 << bpp_shift);

                    tex_state <= TEX_SAMPLE00_REQ;
                end

                // --- Sampling ---
                TEX_SAMPLE00_REQ: if (tex_mem_enq_fire) tex_state <= TEX_SAMPLE00_WAIT;
                TEX_SAMPLE00_WAIT: if (tex_resp_valid) begin
                    samp_00 <= unpack_texel_to_argb8888(tex_resp_data, fmt_code);
                    if (!is_bilinear) begin
                        // Nearest neighbor only needs 00
                        tex_res_vec = is_unorm_expand ?
                                      tex_expand_unorm16(unpack_texel_to_argb8888(tex_resp_data, fmt_code), is_srgb) :
                                      {96'h0, tex_pack_rgba8(unpack_texel_to_argb8888(tex_resp_data, fmt_code), is_srgb)};
                        tex_state <= TEX_DONE;
                    end else begin
                        tex_state <= TEX_SAMPLE10_REQ;
                    end
                end

                TEX_SAMPLE10_REQ: if (tex_mem_enq_fire) tex_state <= TEX_SAMPLE10_WAIT;
                TEX_SAMPLE10_WAIT: if (tex_resp_valid) begin
                    samp_10 <= unpack_texel_to_argb8888(tex_resp_data, fmt_code);
                    tex_state <= TEX_SAMPLE01_REQ;
                end


                TEX_SAMPLE01_REQ: if (tex_mem_enq_fire) tex_state <= TEX_SAMPLE01_WAIT;
                TEX_SAMPLE01_WAIT: if (tex_resp_valid) begin
                    samp_01 <= unpack_texel_to_argb8888(tex_resp_data, fmt_code);
                    tex_state <= TEX_SAMPLE11_REQ;
                end

                TEX_SAMPLE11_REQ: if (tex_mem_enq_fire) tex_state <= TEX_SAMPLE11_WAIT;
                TEX_SAMPLE11_WAIT: if (tex_resp_valid) begin
                    // Bilinear Average
                    // Simple average of 4 samples for now. True bilinear uses fractional parts.
                    // Given we assumed integer inputs, this really just filters the 2x2 block at that pixel.
                    logic [31:0] samp11;
                    logic [31:0] top_row;
                    logic [31:0] bot_row;
                    logic [31:0] final_color;

                    samp11 = unpack_texel_to_argb8888(tex_resp_data, fmt_code);
                    samp_11 <= samp11;
                    top_row = tex_avg4(samp_00, samp_10);
                    bot_row = tex_avg4(samp_01, samp11);
                    final_color = tex_avg4(top_row, bot_row);

                    tex_res_vec = is_unorm_expand ?
                                  tex_expand_unorm16(final_color, is_srgb) :
                                  {96'h0, tex_pack_rgba8(final_color, is_srgb)};

                    tex_state <= TEX_DONE;
                end

                TEX_DONE: begin
                    wb_valid <= 1'b1;
                    wb_rd <= saved_rd;
                    wb_data <= tex_res_vec;
                    wb_is_scalar <= 1'b0;
                    tex_state <= TEX_IDLE;
                end
            endcase
        end
    end

    // ------------------------------------------------------------------------
    // Raster Macro-ops (descriptor-based per ISA)
    // ------------------------------------------------------------------------
    // Implement RSTATE/RSETUP/RDRAW minimally:
    // - RSTATE(rs1=descriptor ptr): fetch FB base/stride/format/wh + scissor + const color (subset)
    // - RSETUP(rs1=descriptor ptr): fetch x0,y0,x1,y1,x2,y2 (subset of 96B vertex block)
    // - RDRAW: kick raster; retire only after raster+ROP are idle

    function automatic logic [31:0] vbo_addr(
        input logic [31:0] base,
        input logic [31:0] stride_bytes,
        input logic [31:0] vtx_idx,
        input logic [31:0] field_off
    );
        vbo_addr = base + (vtx_idx * stride_bytes) + field_off;
    endfunction

    typedef enum logic [7:0] {
        GFX_IDLE,
        GFX_RSTATE_FB_BASE_REQ,   GFX_RSTATE_FB_BASE_WAIT,
        GFX_RSTATE_FB_STRIDE_REQ, GFX_RSTATE_FB_STRIDE_WAIT,
        GFX_RSTATE_FB_FMT_REQ,    GFX_RSTATE_FB_FMT_WAIT,
        GFX_RSTATE_FB_WH_REQ,     GFX_RSTATE_FB_WH_WAIT,
        GFX_RSTATE_CONST_COLOR_REQ, GFX_RSTATE_CONST_COLOR_WAIT,
        GFX_RSTATE_SAMPLER_HANDLE_REQ, GFX_RSTATE_SAMPLER_HANDLE_WAIT,
        GFX_RSTATE_SCISSOR_XY_REQ,  GFX_RSTATE_SCISSOR_XY_WAIT,
        GFX_RSTATE_SCISSOR_WH_REQ,  GFX_RSTATE_SCISSOR_WH_WAIT,
        GFX_RSTATE_FLAGS0_REQ,      GFX_RSTATE_FLAGS0_WAIT,

        GFX_RSTATE_SAMP_BASE_REQ,   GFX_RSTATE_SAMP_BASE_WAIT,
        GFX_RSTATE_SAMP_STRIDE_REQ, GFX_RSTATE_SAMP_STRIDE_WAIT,
        GFX_RSTATE_SAMP_WIDTH_REQ,  GFX_RSTATE_SAMP_WIDTH_WAIT,
        GFX_RSTATE_SAMP_HEIGHT_REQ, GFX_RSTATE_SAMP_HEIGHT_WAIT,
        GFX_RSTATE_SAMP_FMT_REQ,    GFX_RSTATE_SAMP_FMT_WAIT,

        GFX_GSTATE_VBO_BASE_REQ,   GFX_GSTATE_VBO_BASE_WAIT,
        GFX_GSTATE_VBO_STRIDE_REQ, GFX_GSTATE_VBO_STRIDE_WAIT,
        GFX_GSTATE_IBO_BASE_REQ,   GFX_GSTATE_IBO_BASE_WAIT,
        GFX_GSTATE_IBO_FMT_REQ,    GFX_GSTATE_IBO_FMT_WAIT,
        GFX_GSTATE_CULL_REQ,       GFX_GSTATE_CULL_WAIT,

        GFX_GPARAM_BASE_VERTEX_REQ, GFX_GPARAM_BASE_VERTEX_WAIT,
        GFX_GPARAM_MAT_COLOR_REQ,   GFX_GPARAM_MAT_COLOR_WAIT,

        GFX_GDRAW_FIRST_REQ,  GFX_GDRAW_FIRST_WAIT,
        GFX_GDRAW_COUNT_REQ,  GFX_GDRAW_COUNT_WAIT,
        GFX_GDRAW_TOPO_REQ,   GFX_GDRAW_TOPO_WAIT,
        GFX_GDRAW_FLAGS_REQ,  GFX_GDRAW_FLAGS_WAIT,
        GFX_GDRAW_RSTATE_PTR_REQ, GFX_GDRAW_RSTATE_PTR_WAIT,
        GFX_GDRAW_BEGIN,
        GFX_GDRAW_TRI_SETUP,

        GFX_GDRAW_V0_X_REQ, GFX_GDRAW_V0_X_WAIT,
        GFX_GDRAW_V0_Y_REQ, GFX_GDRAW_V0_Y_WAIT,
        GFX_GDRAW_V0_U_REQ, GFX_GDRAW_V0_U_WAIT,
        GFX_GDRAW_V0_V_REQ, GFX_GDRAW_V0_V_WAIT,
        GFX_GDRAW_V1_X_REQ, GFX_GDRAW_V1_X_WAIT,
        GFX_GDRAW_V1_Y_REQ, GFX_GDRAW_V1_Y_WAIT,
        GFX_GDRAW_V1_U_REQ, GFX_GDRAW_V1_U_WAIT,
        GFX_GDRAW_V1_V_REQ, GFX_GDRAW_V1_V_WAIT,
        GFX_GDRAW_V2_X_REQ, GFX_GDRAW_V2_X_WAIT,
        GFX_GDRAW_V2_Y_REQ, GFX_GDRAW_V2_Y_WAIT,
        GFX_GDRAW_V2_U_REQ, GFX_GDRAW_V2_U_WAIT,
        GFX_GDRAW_V2_V_REQ, GFX_GDRAW_V2_V_WAIT,

        GFX_GDRAW_ENQ,

        GFX_GDRAW_SEND_V0,
        GFX_GDRAW_SEND_V1,
        GFX_GDRAW_SEND_V2,
        GFX_GDRAW_SEND_DRAW,
        GFX_GDRAW_WAIT,

        GFX_RRECT_X0_REQ,    GFX_RRECT_X0_WAIT,
        GFX_RRECT_Y0_REQ,    GFX_RRECT_Y0_WAIT,
        GFX_RRECT_X1_REQ,    GFX_RRECT_X1_WAIT,
        GFX_RRECT_Y1_REQ,    GFX_RRECT_Y1_WAIT,
        GFX_RRECT_COLOR_REQ, GFX_RRECT_COLOR_WAIT,
        GFX_RRECT_SEND1_V0,
        GFX_RRECT_SEND1_V1,
        GFX_RRECT_SEND1_V2,
        GFX_RRECT_DRAW1,
        GFX_RRECT_WAIT1,
        GFX_RRECT_SEND2_V0,
        GFX_RRECT_SEND2_V1,
        GFX_RRECT_SEND2_V2,
        GFX_RRECT_DRAW2,
        GFX_RRECT_WAIT2,

        GFX_RRECT_ENQ1,
        GFX_RRECT_ENQ2,

        GFX_FETCH_X0_REQ, GFX_FETCH_X0_WAIT,
        GFX_FETCH_Y0_REQ, GFX_FETCH_Y0_WAIT,
        GFX_FETCH_U0_REQ, GFX_FETCH_U0_WAIT,
        GFX_FETCH_V0_REQ, GFX_FETCH_V0_WAIT,
        GFX_FETCH_C0_REQ, GFX_FETCH_C0_WAIT,
        GFX_FETCH_X1_REQ, GFX_FETCH_X1_WAIT,
        GFX_FETCH_Y1_REQ, GFX_FETCH_Y1_WAIT,
        GFX_FETCH_U1_REQ, GFX_FETCH_U1_WAIT,
        GFX_FETCH_V1_REQ, GFX_FETCH_V1_WAIT,
        GFX_FETCH_X2_REQ, GFX_FETCH_X2_WAIT,
        GFX_FETCH_Y2_REQ, GFX_FETCH_Y2_WAIT,
        GFX_FETCH_U2_REQ, GFX_FETCH_U2_WAIT,
        GFX_FETCH_V2_REQ, GFX_FETCH_V2_WAIT,
        GFX_RDRAW_ENQ,
        GFX_SEND_V0,
        GFX_SEND_V1,
        GFX_SEND_V2,
        GFX_SEND_DRAW,
        GFX_WAIT_RASTER_DONE,
        GFX_DONE
    } gfx_state_e;

    gfx_state_e gfx_state;
    logic [31:0] v0x, v0y, v1x, v1y, v2x, v2y;
    logic [31:0] v0u, v0v, v1u, v1v, v2u, v2v;
    logic [31:0] rsetup_color;
    logic [31:0] rsetup_ptr;

    // Scheduler completion pulse
    assign gfx_complete_pulse = (gfx_state == GFX_DONE);

    // ------------------------------------------------------------------------
    // Dual-dispatch scheduler: allow TEX+GFX to run concurrently.
    // - Dispatches the oldest not-yet-issued entry.
    // - If head is already issued (or will be issued this cycle), may also dispatch head+1
    //   to the other engine (when idle).
    // - FIFO retirement remains strictly in-order (handled by gq_done + head pop logic).
    // ------------------------------------------------------------------------
    wire [GQ_IDX_W-1:0] qh1 = (gq_head == GQ_LAST) ? '0 : (gq_head + 1'b1);
    wire [GQ_IDX_W-1:0] qh2 = (qh1    == GQ_LAST) ? '0 : (qh1    + 1'b1);
    wire retire0_w = gq_valid[gq_head] && gq_done[gq_head];
    wire retire1_w = retire0_w && gq_valid[qh1] && gq_done[qh1];
    wire [1:0] retire_cnt_w = {retire1_w, retire0_w};

    wire [GQ_IDX_W-1:0] sched_head  = (retire_cnt_w == 2'b00) ? gq_head : (retire_cnt_w == 2'b01) ? qh1 : qh2;
    wire [GQ_IDX_W-1:0] sched_head1 = (sched_head == GQ_LAST) ? '0 : (sched_head + 1'b1);

    wire tex_can_start = (tex_state == TEX_IDLE) && texq_can_push;
    wire gfx_can_start = (gfx_state == GFX_IDLE);

    wire head0_can_issue = gq_valid[sched_head] && !gq_issued[sched_head];
    wire head1_can_issue = gq_valid[sched_head1] && !gq_issued[sched_head1];

    wire head0_is_tex = head0_can_issue && gq[sched_head].ctrl.is_tex;
    wire head0_is_gfx = head0_can_issue && gq[sched_head].ctrl.is_gfx;
    wire head1_is_tex = head1_can_issue && gq[sched_head1].ctrl.is_tex;
    wire head1_is_gfx = head1_can_issue && gq[sched_head1].ctrl.is_gfx;

    wire head0_will_issue_tex = tex_can_start && head0_is_tex;
    wire head0_will_issue_gfx = gfx_can_start && head0_is_gfx;
    wire head0_dispatched = gq_valid[sched_head]
                         && (gq_issued[sched_head] || gq_done[sched_head] || head0_will_issue_tex || head0_will_issue_gfx);

    always_comb begin
        tex_issue_fire = 1'b0;
        gfx_issue_fire = 1'b0;
        tex_issue_idx  = '0;
        gfx_issue_idx  = '0;

        if (tex_can_start) begin
            if (head0_is_tex) begin
                tex_issue_fire = 1'b1;
                tex_issue_idx  = sched_head;
            end else if (head0_dispatched && head1_is_tex) begin
                tex_issue_fire = 1'b1;
                tex_issue_idx  = sched_head1;
            end
        end

        if (gfx_can_start) begin
            if (head0_is_gfx) begin
                gfx_issue_fire = 1'b1;
                gfx_issue_idx  = sched_head;
            end else if (head0_dispatched && head1_is_gfx) begin
                gfx_issue_fire = 1'b1;
                gfx_issue_idx  = sched_head1;
            end
        end
    end

    // ------------------------------------------------------------------------
    // Primitive queue + raster command feeder
    // ------------------------------------------------------------------------
    localparam int unsigned PRIMQ_IDX_W = (PRIM_Q_DEPTH <= 1) ? 1 : $clog2(PRIM_Q_DEPTH);
    localparam int unsigned PRIMQ_CNT_W = (PRIM_Q_DEPTH <= 1) ? 1 : $clog2(PRIM_Q_DEPTH + 1);
    localparam logic [PRIMQ_IDX_W-1:0] PRIMQ_LAST = PRIMQ_IDX_W'(PRIM_Q_DEPTH - 1);

    typedef struct packed {
        logic [31:0] v0x, v0y;
        logic [31:0] v1x, v1y;
        logic [31:0] v2x, v2y;
        logic [31:0] v0u, v0v;
        logic [31:0] v1u, v1v;
        logic [31:0] v2u, v2v;

        logic [31:0] const_color_argb;
        logic        tex_enable;
        logic [31:0] tex_base;
        logic [31:0] tex_stride_bytes;
        logic [15:0] tex_width;
        logic [15:0] tex_height;
        logic [1:0]  tex_format;
    } prim_pkt_t;

    prim_pkt_t primq      [PRIM_Q_DEPTH];
    logic [PRIMQ_IDX_W-1:0] primq_head;
    logic [PRIMQ_IDX_W-1:0] primq_tail;
    logic [PRIMQ_CNT_W-1:0] primq_count;

    wire primq_out_valid = (primq_count != '0);
    wire primq_full = (primq_count == PRIMQ_CNT_W'(PRIM_Q_DEPTH));

    // Feeder state machine drives raster_unit command interface
    typedef enum logic [2:0] {
        FEED_IDLE,
        FEED_V0,
        FEED_V1,
        FEED_V2,
        FEED_DRAW
    } feed_state_e;

    feed_state_e feed_state;
    prim_pkt_t   hold_prim;

    logic        raster_cmd_valid;
    logic [2:0]  raster_cmd_type;
    logic [31:0] raster_cmd_data_x;
    logic [31:0] raster_cmd_data_y;
    logic        raster_cmd_ready;

    wire feed_fire = raster_cmd_valid && raster_cmd_ready;
    wire primq_pop = (feed_state == FEED_DRAW) && feed_fire;
    wire primq_can_push = (!primq_full) || primq_pop;

    // Push strobes from gfx FSM
    logic primq_push;
    prim_pkt_t primq_push_pkt;

    always_comb begin
        raster_cmd_valid  = 1'b0;
        raster_cmd_type   = 3'b000;
        raster_cmd_data_x = 32'h0;
        raster_cmd_data_y = 32'h0;

        unique case (feed_state)
            FEED_V0: begin
                raster_cmd_valid  = 1'b1;
                raster_cmd_type   = 3'b001;
                raster_cmd_data_x = hold_prim.v0x;
                raster_cmd_data_y = hold_prim.v0y;
            end
            FEED_V1: begin
                raster_cmd_valid  = 1'b1;
                raster_cmd_type   = 3'b010;
                raster_cmd_data_x = hold_prim.v1x;
                raster_cmd_data_y = hold_prim.v1y;
            end
            FEED_V2: begin
                raster_cmd_valid  = 1'b1;
                raster_cmd_type   = 3'b011;
                raster_cmd_data_x = hold_prim.v2x;
                raster_cmd_data_y = hold_prim.v2y;
            end
            FEED_DRAW: begin
                raster_cmd_valid  = 1'b1;
                raster_cmd_type   = 3'b100;
            end
            default: ;
        endcase
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            primq_head  <= '0;
            primq_tail  <= '0;
            primq_count <= '0;
            feed_state  <= FEED_IDLE;
            hold_prim   <= '0;
        end else if (flush_all) begin
            primq_head  <= '0;
            primq_tail  <= '0;
            primq_count <= '0;
            feed_state  <= FEED_IDLE;
            hold_prim   <= '0;
        end else begin
            // FIFO push
            if (primq_push && primq_can_push) begin
                primq[primq_tail] <= primq_push_pkt;
                primq_tail <= (primq_tail == PRIMQ_LAST) ? '0 : (primq_tail + 1'b1);
            end

            // FIFO pop
            if (primq_pop) begin
                primq_head <= (primq_head == PRIMQ_LAST) ? '0 : (primq_head + 1'b1);
            end

            unique case ({(primq_push && primq_can_push), primq_pop})
                2'b10: primq_count <= primq_count + 1'b1;
                2'b01: primq_count <= primq_count - 1'b1;
                default: ;
            endcase

            // Feeder
            unique case (feed_state)
                FEED_IDLE: begin
                    if (primq_out_valid) begin
                        hold_prim <= primq[primq_head];
                        feed_state <= FEED_V0;
                    end
                end
                FEED_V0: if (feed_fire) feed_state <= FEED_V1;
                FEED_V1: if (feed_fire) feed_state <= FEED_V2;
                FEED_V2: if (feed_fire) feed_state <= FEED_DRAW;
                FEED_DRAW: if (feed_fire) feed_state <= FEED_IDLE;
                default: feed_state <= FEED_IDLE;
            endcase
        end
    end

    // RSTATE regs (subset used by initial triangle fill)
    logic [31:0] r_fb_base;
    logic [31:0] r_fb_stride;
    logic [1:0]  r_fb_fmt;
    logic [15:0] r_fb_w;
    logic [15:0] r_fb_h;
    logic [31:0] r_const_color;
    logic [31:0] r_sampler_handle;
    logic [31:0] rstate_flags0;
    logic        r_scissor_en;
    logic [15:0] r_scissor_x0;
    logic [15:0] r_scissor_y0;
    logic [15:0] r_scissor_w;
    logic [15:0] r_scissor_h;

    logic [31:0] rstate_ptr;

    // RSTATE sampler descriptor (minimal subset for ROP sampling)
    logic [31:0] rstate_sampler_ptr;
    logic [31:0] r_tex_base;
    logic [31:0] r_tex_stride;
    logic [15:0] r_tex_w;
    logic [15:0] r_tex_h;
    logic [1:0]  r_tex_fmt;

    // Geometry state
    logic [31:0] gstate_ptr;
    logic [31:0] g_vbo_base;
    logic [31:0] g_vbo_stride;
    logic [31:0] g_ibo_base;
    logic [31:0] g_ibo_format;
    logic [31:0] g_cull_clip_flags;

    // Per-draw params
    logic [31:0] gparam_ptr;
    logic [31:0] g_base_vertex;
    logic [31:0] g_material_color;

    // GDRAW command
    logic [31:0] gdraw_ptr;
    logic [31:0] gd_first;
    logic [31:0] gd_count;
    logic [31:0] gd_topology;
    logic [31:0] gd_flags;
    logic [31:0] gd_rstate_ptr;

    logic [31:0] gd_remaining;
    logic [31:0] gd_cur_first;
    logic [31:0] gd_idx0;
    logic [31:0] gd_idx1;
    logic [31:0] gd_idx2;

    // RRECT regs
    logic [31:0] rrect_ptr;
    logic [31:0] rect_x0;
    logic [31:0] rect_y0;
    logic [31:0] rect_x1;
    logic [31:0] rect_y1;
    logic [31:0] rect_color;

    // Latched ROP constant color for an in-flight draw
    logic [31:0] rop_const_color;

    // Latched ROP texture state for an in-flight draw (RDRAW only for now)
    logic        rop_tex_en;
    logic [31:0] rop_tex_base;
    logic [31:0] rop_tex_stride;
    logic [15:0] rop_tex_w;
    logic [15:0] rop_tex_h;
    logic [1:0]  rop_tex_fmt;

    // Sampler pointer helper for TEX descriptor reads
    logic [31:0] sampler_ptr;

`ifndef SYNTHESIS
    integer dbg_gfx_desc_count;
    integer dbg_gdraw_begin_count;
    integer dbg_sentinel_count;
    integer dbg_sentinel_q_events;
`endif

    // Raster/ROP wiring (must be declared before instantiation for Modelsim)
    logic        raster_quad_valid;
    logic        raster_quad_ready;
    logic [31:0] raster_quad_x;
    logic [31:0] raster_quad_y;
    logic [3:0]  raster_quad_mask;
    logic        raster_busy;
    logic        rop_busy;
    logic signed [31:0] raster_tri_area;
    logic [31:0] raster_bary_w0;
    logic [31:0] raster_bary_w1;
    logic [31:0] raster_bary_w2;


    // Per-quad draw attributes latched by raster on DRAW
    logic [31:0] raster_const_color;
    logic        raster_tex_enable;
    logic [31:0] raster_tex_base;
    logic [31:0] raster_tex_stride;
    logic [15:0] raster_tex_w;
    logic [15:0] raster_tex_h;
    logic [1:0]  raster_tex_fmt;
    logic [31:0] raster_v0_u;
    logic [31:0] raster_v0_v;
    logic [31:0] raster_v1_u;
    logic [31:0] raster_v1_v;
    logic [31:0] raster_v2_u;
    logic [31:0] raster_v2_v;

    // Interpolator stage (quad FIFO today; future home for per-quad interpolation)
    logic        interp_quad_valid;
    logic        interp_quad_ready;
    logic [31:0] interp_quad_x;
    logic [31:0] interp_quad_y;
    logic [3:0]  interp_quad_mask;
    logic [31:0] interp_bary_w0;
    logic [31:0] interp_bary_w1;
    logic [31:0] interp_bary_w2;
    logic signed [31:0] interp_tri_area;
    logic        interp_busy;

    // Per-quad draw attributes carried alongside quads (enables overlap across triangles)
    logic [31:0] interp_const_color;
    logic        interp_tex_enable;
    logic [31:0] interp_tex_base;
    logic [31:0] interp_tex_stride;
    logic [15:0] interp_tex_w;
    logic [15:0] interp_tex_h;
    logic [1:0]  interp_tex_fmt;
    logic [31:0] interp_v0_u;
    logic [31:0] interp_v0_v;
    logic [31:0] interp_v1_u;
    logic [31:0] interp_v1_v;
    logic [31:0] interp_v2_u;
    logic [31:0] interp_v2_v;

    // Shade/Interp pipeline stage (barycentric -> UV -> texture address)
    logic        shade_quad_valid;
    logic        shade_quad_ready;
    logic [31:0] shade_quad_x;
    logic [31:0] shade_quad_y;
    logic [3:0]  shade_quad_mask;
    logic [31:0] shade_bary_w0;
    logic [31:0] shade_bary_w1;
    logic [31:0] shade_bary_w2;
    logic signed [31:0] shade_tri_area;
    logic        shade_busy;

    // Consider the pipeline busy if there are queued issues, pending primitives,
    // a non-idle feeder, or any in-flight work in the raster/interp/shade/rop stages.
    assign busy = (gq_count_internal != '0)
               || (primq_count != '0)
               || (feed_state != FEED_IDLE)
               || (gfx_state  != GFX_IDLE)
               || raster_busy
               || interp_busy
               || shade_busy
               || rop_busy;

    logic [31:0] shade_const_color;
    logic        shade_tex_enable;
    logic [31:0] shade_tex_base;
    logic [31:0] shade_tex_stride;
    logic [15:0] shade_tex_w;
    logic [15:0] shade_tex_h;
    logic [1:0]  shade_tex_fmt;
    logic [31:0] shade_v0_u;
    logic [31:0] shade_v0_v;
    logic [31:0] shade_v1_u;
    logic [31:0] shade_v1_v;
    logic [31:0] shade_v2_u;
    logic [31:0] shade_v2_v;
    logic [31:0] shade_tex_addr;

    // Optional ROP texture read channel (currently unused until textured draw is enabled)
    logic        rop_tex_req_valid;
    logic [31:0] rop_tex_req_addr;
    logic        rop_tex_req_ready;
    logic        rop_tex_resp_valid;
    logic [31:0] rop_tex_resp_data;

    // ROP store channel (before optional FIFO)
    logic        rop_st_valid;
    logic [31:0] rop_st_addr;
    logic [31:0] rop_st_wdata;
    logic [3:0]  rop_st_wstrb;
    logic        rop_st_ready;

    // Memory Request Mux
    // Priority: active TEX FSM, then (optionally) ROP texture fetches, otherwise gfx descriptor fetches.
    typedef enum logic [1:0] { MEMSRC_NONE, MEMSRC_TEX, MEMSRC_ROP, MEMSRC_GFX } memsrc_e;
    memsrc_e mem_req_src;

    logic        mem_req_valid_i;
    logic [31:0] mem_req_addr_i;
    logic [4:0]  mem_req_rd_i;

    localparam int unsigned TEXQ_IDX_W = (TEX_REQ_Q_DEPTH <= 1) ? 1 : $clog2(TEX_REQ_Q_DEPTH);
    localparam int unsigned TEXQ_CNT_W = (TEX_REQ_Q_DEPTH <= 1) ? 1 : $clog2(TEX_REQ_Q_DEPTH + 1);
    localparam logic [TEXQ_IDX_W-1:0] TEXQ_LAST = TEXQ_IDX_W'(TEX_REQ_Q_DEPTH - 1);

    logic [31:0] texq_addr [TEX_REQ_Q_DEPTH];
    logic [4:0]  texq_rd   [TEX_REQ_Q_DEPTH];
    logic [TEXQ_IDX_W-1:0] texq_head;
    logic [TEXQ_IDX_W-1:0] texq_tail;
    logic [TEXQ_CNT_W-1:0] texq_count;

    wire texq_out_valid = (texq_count != '0);
    wire [31:0] texq_out_addr = texq_addr[texq_head];
    wire [4:0]  texq_out_rd   = texq_rd[texq_head];

    wire texq_pop = texq_out_valid && tex_req_ready;
    wire texq_full = (texq_count == TEXQ_CNT_W'(TEX_REQ_Q_DEPTH));
    assign texq_can_push = (!texq_full) || texq_pop;

    // Separate FIFO for gfx descriptor fetches (keeps gfx descriptor traffic from thrashing the tex cache)
    logic [31:0] gfxq_addr [TEX_REQ_Q_DEPTH];
    logic [4:0]  gfxq_rd   [TEX_REQ_Q_DEPTH];
    logic [TEXQ_IDX_W-1:0] gfxq_head;
    logic [TEXQ_IDX_W-1:0] gfxq_tail;
    logic [TEXQ_CNT_W-1:0] gfxq_count;

    wire gfxq_out_valid = (gfxq_count != '0);
    wire [31:0] gfxq_out_addr = gfxq_addr[gfxq_head];
    wire [4:0]  gfxq_out_rd   = gfxq_rd[gfxq_head];

    wire gfxq_pop = gfxq_out_valid && gfxd_req_ready;
    wire gfxq_full = (gfxq_count == TEXQ_CNT_W'(TEX_REQ_Q_DEPTH));
    wire gfxq_can_push = (!gfxq_full) || gfxq_pop;

    wire mem_req_can_push = (mem_req_src == MEMSRC_GFX) ? gfxq_can_push : texq_can_push;
    wire mem_req_enq_fire = mem_req_valid_i && mem_req_can_push;
    assign tex_mem_enq_fire = mem_req_enq_fire && (mem_req_src == MEMSRC_TEX);
    assign gfx_mem_enq_fire = mem_req_enq_fire && (mem_req_src == MEMSRC_GFX);

    always_comb begin
        mem_req_valid_i = 1'b0;
        mem_req_addr_i  = 32'h0;
        mem_req_rd_i    = 5'h0;
        mem_req_src     = MEMSRC_NONE;

        rop_tex_req_ready = 1'b0;

         // sampler_ptr reflects the pointer used by the active TEX FSM (latched at start).
         // When TEX is idle, reflect the queue head only if it is a TEX op.
         sampler_ptr = (tex_state != TEX_IDLE) ? tex_sampler_ptr
             : (gq_head_is_tex && (gq[gq_head].op_b < 32)) ? (SAMPLER_TABLE_BASE + (gq[gq_head].op_b << 5))
             : (gq_head_is_tex) ? gq[gq_head].op_b
             : 32'h0;

        if (tex_state != TEX_IDLE) begin
            case (tex_state)
                TEX_DESC_BASE_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = tex_sampler_ptr + 32'h00; mem_req_src = MEMSRC_TEX; end
                TEX_DESC_STRIDE_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = tex_sampler_ptr + 32'h04; mem_req_src = MEMSRC_TEX; end
                TEX_DESC_WIDTH_REQ:  begin mem_req_valid_i = 1'b1; mem_req_addr_i = tex_sampler_ptr + 32'h08; mem_req_src = MEMSRC_TEX; end
                TEX_DESC_HEIGHT_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = tex_sampler_ptr + 32'h0C; mem_req_src = MEMSRC_TEX; end
                TEX_DESC_FMT_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = tex_sampler_ptr + 32'h10; mem_req_src = MEMSRC_TEX; end
                TEX_DESC_WRAP_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = tex_sampler_ptr + 32'h14; mem_req_src = MEMSRC_TEX; end
                TEX_DESC_FILT_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = tex_sampler_ptr + 32'h18; mem_req_src = MEMSRC_TEX; end
                TEX_DESC_MISC_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = tex_sampler_ptr + 32'h1C; mem_req_src = MEMSRC_TEX; end
                TEX_SAMPLE00_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = addr_00; mem_req_src = MEMSRC_TEX; end
                TEX_SAMPLE10_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = addr_10; mem_req_src = MEMSRC_TEX; end
                TEX_SAMPLE01_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = addr_01; mem_req_src = MEMSRC_TEX; end
                TEX_SAMPLE11_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = addr_11; mem_req_src = MEMSRC_TEX; end
                default: ;
            endcase
        end else if (rop_tex_req_valid) begin
            mem_req_valid_i = 1'b1;
            mem_req_addr_i  = rop_tex_req_addr;
            mem_req_src     = MEMSRC_ROP;
            rop_tex_req_ready = texq_can_push;
        end else begin
            case (gfx_state)
                GFX_RSTATE_FB_BASE_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h00; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_FB_STRIDE_REQ:  begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h04; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_FB_FMT_REQ:     begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h08; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_FB_WH_REQ:      begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h0C; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_CONST_COLOR_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h20; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_SAMPLER_HANDLE_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h24; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_SCISSOR_XY_REQ:  begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h28; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_SCISSOR_WH_REQ:  begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h2C; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_FLAGS0_REQ:      begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_ptr + 32'h30; mem_req_src = MEMSRC_GFX; end

                GFX_RSTATE_SAMP_BASE_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_sampler_ptr + 32'h00; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_SAMP_STRIDE_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_sampler_ptr + 32'h04; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_SAMP_WIDTH_REQ:  begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_sampler_ptr + 32'h08; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_SAMP_HEIGHT_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_sampler_ptr + 32'h0C; mem_req_src = MEMSRC_GFX; end
                GFX_RSTATE_SAMP_FMT_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = rstate_sampler_ptr + 32'h10; mem_req_src = MEMSRC_GFX; end

                GFX_GSTATE_VBO_BASE_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = gstate_ptr + 32'h00; mem_req_src = MEMSRC_GFX; end
                GFX_GSTATE_VBO_STRIDE_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = gstate_ptr + 32'h04; mem_req_src = MEMSRC_GFX; end
                GFX_GSTATE_IBO_BASE_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = gstate_ptr + 32'h08; mem_req_src = MEMSRC_GFX; end
                GFX_GSTATE_IBO_FMT_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = gstate_ptr + 32'h0C; mem_req_src = MEMSRC_GFX; end
                GFX_GSTATE_CULL_REQ:       begin mem_req_valid_i = 1'b1; mem_req_addr_i = gstate_ptr + 32'h18; mem_req_src = MEMSRC_GFX; end

                GFX_GPARAM_BASE_VERTEX_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = gparam_ptr + 32'h00; mem_req_src = MEMSRC_GFX; end
                GFX_GPARAM_MAT_COLOR_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = gparam_ptr + 32'h08; mem_req_src = MEMSRC_GFX; end

                GFX_GDRAW_FIRST_REQ:  begin mem_req_valid_i = 1'b1; mem_req_addr_i = gdraw_ptr + 32'h00; mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_COUNT_REQ:  begin mem_req_valid_i = 1'b1; mem_req_addr_i = gdraw_ptr + 32'h04; mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_TOPO_REQ:   begin mem_req_valid_i = 1'b1; mem_req_addr_i = gdraw_ptr + 32'h08; mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_FLAGS_REQ:  begin mem_req_valid_i = 1'b1; mem_req_addr_i = gdraw_ptr + 32'h0C; mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_RSTATE_PTR_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = gdraw_ptr + 32'h10; mem_req_src = MEMSRC_GFX; end

                GFX_GDRAW_V0_X_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx0, 32'h00); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V0_Y_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx0, 32'h04); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V0_U_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx0, 32'h10); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V0_V_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx0, 32'h14); mem_req_src = MEMSRC_GFX; end

                GFX_GDRAW_V1_X_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx1, 32'h00); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V1_Y_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx1, 32'h04); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V1_U_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx1, 32'h10); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V1_V_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx1, 32'h14); mem_req_src = MEMSRC_GFX; end

                GFX_GDRAW_V2_X_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx2, 32'h00); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V2_Y_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx2, 32'h04); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V2_U_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx2, 32'h10); mem_req_src = MEMSRC_GFX; end
                GFX_GDRAW_V2_V_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx2, 32'h14); mem_req_src = MEMSRC_GFX; end
                GFX_RRECT_X0_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = rrect_ptr + 32'h00; mem_req_src = MEMSRC_GFX; end
                GFX_RRECT_Y0_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = rrect_ptr + 32'h04; mem_req_src = MEMSRC_GFX; end
                GFX_RRECT_X1_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = rrect_ptr + 32'h08; mem_req_src = MEMSRC_GFX; end
                GFX_RRECT_Y1_REQ:    begin mem_req_valid_i = 1'b1; mem_req_addr_i = rrect_ptr + 32'h0C; mem_req_src = MEMSRC_GFX; end
                GFX_RRECT_COLOR_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rrect_ptr + 32'h10; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_X0_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h00; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_Y0_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h04; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_U0_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h10; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_V0_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h14; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_C0_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h18; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_X1_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h20; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_Y1_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h24; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_U1_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h30; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_V1_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h34; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_X2_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h40; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_Y2_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h44; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_U2_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h50; mem_req_src = MEMSRC_GFX; end
                GFX_FETCH_V2_REQ: begin mem_req_valid_i = 1'b1; mem_req_addr_i = rsetup_ptr + 32'h54; mem_req_src = MEMSRC_GFX; end
                default: ;
            endcase
        end
    end

    // TEX request FIFO (decouples request generation from downstream tex_req_ready)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            texq_head  <= '0;
            texq_tail  <= '0;
            texq_count <= '0;
            gfxq_head  <= '0;
            gfxq_tail  <= '0;
            gfxq_count <= '0;
        end else if (flush_all) begin
            texq_head  <= '0;
            texq_tail  <= '0;
            texq_count <= '0;
            gfxq_head  <= '0;
            gfxq_tail  <= '0;
            gfxq_count <= '0;
        end else begin
            // Push to the appropriate FIFO based on mem_req_src
            if (mem_req_enq_fire) begin
                if (mem_req_src == MEMSRC_GFX) begin
                    gfxq_addr[gfxq_tail] <= mem_req_addr_i;
                    gfxq_rd[gfxq_tail]   <= mem_req_rd_i;
                    gfxq_tail <= (gfxq_tail == TEXQ_LAST) ? '0 : (gfxq_tail + 1'b1);
                end else begin
                    texq_addr[texq_tail] <= mem_req_addr_i;
                    texq_rd[texq_tail]   <= mem_req_rd_i;
                    texq_tail <= (texq_tail == TEXQ_LAST) ? '0 : (texq_tail + 1'b1);
                end
            end

            // Pop
            if (texq_pop) begin
                texq_head <= (texq_head == TEXQ_LAST) ? '0 : (texq_head + 1'b1);
            end
            if (gfxq_pop) begin
                gfxq_head <= (gfxq_head == TEXQ_LAST) ? '0 : (gfxq_head + 1'b1);
            end

            // Count updates
            unique case ({mem_req_enq_fire && (mem_req_src != MEMSRC_GFX), texq_pop})
                2'b10: texq_count <= texq_count + 1'b1;
                2'b01: texq_count <= texq_count - 1'b1;
                default: ;
            endcase
            unique case ({mem_req_enq_fire && (mem_req_src == MEMSRC_GFX), gfxq_pop})
                2'b10: gfxq_count <= gfxq_count + 1'b1;
                2'b01: gfxq_count <= gfxq_count - 1'b1;
                default: ;
            endcase
        end
    end

    // Drive the external tex_req channel from the FIFO output
    assign tex_req_valid = texq_out_valid;
    assign tex_req_addr  = texq_out_addr;
    assign tex_req_rd    = texq_out_rd;

    // Drive the external gfx descriptor req channel from its FIFO output
    assign gfxd_req_valid = gfxq_out_valid;
    assign gfxd_req_addr  = gfxq_out_addr;
    assign gfxd_req_rd    = gfxq_out_rd;

    wire is_rstate = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b000);
    wire is_rsetup = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b001);
    wire is_rdraw  = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b010);
    wire is_rrect  = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b011);
    wire is_gstate = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b100);
    wire is_gparam = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b101);
    wire is_gdraw  = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b110);

    logic        cmd_valid;
    // Legacy hierarchical debug signal (gfx_console_tb expects dut.u_graphics_pipeline.cmd_ready)
    logic        cmd_ready;
    logic [2:0]  cmd_type;
    logic [31:0] cmd_data_x, cmd_data_y;

    assign cmd_ready = !queue_full;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            gfx_state <= GFX_IDLE;
            gfx_idx <= '0;
            v0x <= 0; v0y <= 0; v1x <= 0; v1y <= 0; v2x <= 0; v2y <= 0;
            v0u <= 0; v0v <= 0; v1u <= 0; v1v <= 0; v2u <= 0; v2v <= 0;
            rsetup_ptr <= 0;
            rsetup_color <= 32'h0;
            rstate_ptr <= 0;
            rrect_ptr  <= 0;
            rect_x0 <= 0;
            rect_y0 <= 0;
            rect_x1 <= 0;
            rect_y1 <= 0;
            rect_color <= 32'h0;
            rop_const_color <= 32'hFFFF00FF;

            rop_tex_en <= 1'b0;
            rop_tex_base   <= 32'h0;
            rop_tex_stride <= 32'h0;
            rop_tex_w      <= 16'h0;
            rop_tex_h      <= 16'h0;
            rop_tex_fmt    <= 2'd0;

            r_fb_base   <= 32'h0;
            r_fb_stride <= 32'h0;
            r_fb_fmt    <= 2'd0;
            r_fb_w      <= 16'h0;
            r_fb_h      <= 16'h0;
            r_const_color <= 32'hFFFF00FF;
            r_sampler_handle <= 32'h0;
            rstate_flags0 <= 32'h0;
            r_scissor_en <= 1'b0;
            r_scissor_x0 <= 16'h0;
            r_scissor_y0 <= 16'h0;
            r_scissor_w  <= 16'h0;
            r_scissor_h  <= 16'h0;

            rstate_sampler_ptr <= 32'h0;
            r_tex_base   <= 32'h0;
            r_tex_stride <= 32'h0;
            r_tex_w      <= 16'h0;
            r_tex_h      <= 16'h0;
            r_tex_fmt    <= 2'd0;

            gstate_ptr <= 32'h0;
            g_vbo_base <= 32'h0;
            g_vbo_stride <= 32'h0;
            g_ibo_base <= 32'h0;
            g_ibo_format <= 32'h0;
            g_cull_clip_flags <= 32'h0;

            gparam_ptr <= 32'h0;
            g_base_vertex <= 32'h0;
            g_material_color <= 32'h0;

            gdraw_ptr <= 32'h0;
            gd_first <= 32'h0;
            gd_count <= 32'h0;
            gd_topology <= 32'h0;
            gd_flags <= 32'h0;
            gd_rstate_ptr <= 32'h0;
            gd_remaining <= 32'h0;
            gd_cur_first <= 32'h0;
            gd_idx0 <= 32'h0;
            gd_idx1 <= 32'h0;
            gd_idx2 <= 32'h0;

`ifndef SYNTHESIS
            dbg_gfx_desc_count <= 0;
            dbg_gdraw_begin_count <= 0;
            dbg_sentinel_count <= 0;
            dbg_sentinel_q_events <= 0;
`endif
        end else if (flush_all) begin
            gfx_state <= GFX_IDLE;
            gfx_idx <= '0;
            rop_tex_en <= 1'b0;
            rsetup_color <= 32'h0;

`ifndef SYNTHESIS
            dbg_gfx_desc_count <= 0;
            dbg_gdraw_begin_count <= 0;
            dbg_sentinel_count <= 0;
            dbg_sentinel_q_events <= 0;
`endif
        end else begin
            case (gfx_state)
                GFX_IDLE: begin
                    // Start GFX from the scheduler-selected FIFO entry.
                    if (gfx_issue_fire) begin
                        gfx_idx <= gfx_issue_idx;
                        unique case (gq[gfx_issue_idx].ctrl.funct3)
                            3'b000: begin // RSTATE
                                rstate_ptr <= gq[gfx_issue_idx].op_a;
`ifndef SYNTHESIS
                                if ((dbg_sentinel_count < 16) && (gq[gfx_issue_idx].op_a[15:0] == 16'h0540)) begin
                                    $display("%0t GFX_SENT start RSTATE2 idx=%0d ptr=%08h", $time, gfx_issue_idx, gq[gfx_issue_idx].op_a);
                                    dbg_sentinel_count <= dbg_sentinel_count + 1;
                                end
`endif
                                gfx_state <= GFX_RSTATE_FB_BASE_REQ;
                            end
                            3'b001: begin // RSETUP
                                rsetup_ptr <= gq[gfx_issue_idx].op_a;
                                rsetup_color <= r_const_color;
                                gfx_state <= GFX_FETCH_X0_REQ;
                            end
                            3'b010: begin // RDRAW
                                rop_const_color <= (rsetup_color != 32'h0) ? rsetup_color : r_const_color;
                                rop_tex_en <= rstate_flags0[1];
                                rop_tex_base   <= r_tex_base;
                                rop_tex_stride <= r_tex_stride;
                                rop_tex_w      <= r_tex_w;
                                rop_tex_h      <= r_tex_h;
                                rop_tex_fmt    <= r_tex_fmt;
                                gfx_state <= GFX_RDRAW_ENQ;
                            end
                            3'b011: begin // RRECT
                                rrect_ptr <= gq[gfx_issue_idx].op_a;
`ifndef SYNTHESIS
                                if ((dbg_sentinel_count < 16) && (gq[gfx_issue_idx].op_a[15:0] == 16'h8000)) begin
                                    $display("%0t GFX_SENT start RRECT idx=%0d ptr=%08h fb_base=%08h stride=%08h wh=%0dx%0d", $time,
                                             gfx_issue_idx, gq[gfx_issue_idx].op_a, r_fb_base, r_fb_stride, r_fb_w, r_fb_h);
                                    dbg_sentinel_count <= dbg_sentinel_count + 1;
                                end
`endif
                                gfx_state <= GFX_RRECT_X0_REQ;
                            end
                            3'b100: begin // GSTATE
                                gstate_ptr <= gq[gfx_issue_idx].op_a;
                                gfx_state <= GFX_GSTATE_VBO_BASE_REQ;
                            end
                            3'b101: begin // GPARAM
                                gparam_ptr <= gq[gfx_issue_idx].op_a;
                                gfx_state <= GFX_GPARAM_BASE_VERTEX_REQ;
                            end
                            3'b110: begin // GDRAW
                                gdraw_ptr <= gq[gfx_issue_idx].op_a;
                                gfx_state <= GFX_GDRAW_FIRST_REQ;
                            end
                            default: begin
                                gfx_state <= GFX_DONE;
                            end
                        endcase
                    end
                end

                // RSTATE descriptor subset
                GFX_RSTATE_FB_BASE_REQ:    if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_FB_BASE_WAIT;
                GFX_RSTATE_FB_BASE_WAIT:   if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    // Focused debug for the sync-buffer RSTATE (RSTATE2)
                    if ((rstate_ptr[15:0] == 16'h0540) && (dbg_sentinel_count < 32)) begin
                        $display("%0t GFX_SENT RSTATE2 fb_base=%08h", $time, gfxd_resp_data);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    r_fb_base <= gfxd_resp_data;
                    gfx_state <= GFX_RSTATE_FB_STRIDE_REQ;
                end

                GFX_RSTATE_FB_STRIDE_REQ:  if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_FB_STRIDE_WAIT;
                GFX_RSTATE_FB_STRIDE_WAIT: if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rstate_ptr[15:0] == 16'h0540) && (dbg_sentinel_count < 32)) begin
                        $display("%0t GFX_SENT RSTATE2 stride=%08h", $time, gfxd_resp_data);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    r_fb_stride <= gfxd_resp_data;
                    gfx_state <= GFX_RSTATE_FB_FMT_REQ;
                end

                GFX_RSTATE_FB_FMT_REQ:     if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_FB_FMT_WAIT;
                GFX_RSTATE_FB_FMT_WAIT:    if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rstate_ptr[15:0] == 16'h0540) && (dbg_sentinel_count < 32)) begin
                        $display("%0t GFX_SENT RSTATE2 fmt=%08h", $time, gfxd_resp_data);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    r_fb_fmt <= gfxd_resp_data[1:0];
                    gfx_state <= GFX_RSTATE_FB_WH_REQ;
                end

                GFX_RSTATE_FB_WH_REQ:      if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_FB_WH_WAIT;
                GFX_RSTATE_FB_WH_WAIT:     if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rstate_ptr[15:0] == 16'h0540) && (dbg_sentinel_count < 32)) begin
                        $display("%0t GFX_SENT RSTATE2 wh=%0dx%0d raw=%08h", $time, gfxd_resp_data[15:0], gfxd_resp_data[31:16], gfxd_resp_data);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    r_fb_w <= gfxd_resp_data[15:0];
                    r_fb_h <= gfxd_resp_data[31:16];
                    gfx_state <= GFX_RSTATE_CONST_COLOR_REQ;
                end

                GFX_RSTATE_CONST_COLOR_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_CONST_COLOR_WAIT;
                GFX_RSTATE_CONST_COLOR_WAIT: if (gfxd_resp_valid) begin r_const_color <= gfxd_resp_data; rop_const_color <= gfxd_resp_data; gfx_state <= GFX_RSTATE_SAMPLER_HANDLE_REQ; end

                GFX_RSTATE_SAMPLER_HANDLE_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_SAMPLER_HANDLE_WAIT;
                GFX_RSTATE_SAMPLER_HANDLE_WAIT: if (gfxd_resp_valid) begin
                    r_sampler_handle <= gfxd_resp_data;
                    rstate_sampler_ptr <= (gfxd_resp_data < 32) ? (SAMPLER_TABLE_BASE + (gfxd_resp_data << 5)) : gfxd_resp_data;
                    gfx_state <= GFX_RSTATE_SCISSOR_XY_REQ;
                end

                GFX_RSTATE_SCISSOR_XY_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_SCISSOR_XY_WAIT;
                GFX_RSTATE_SCISSOR_XY_WAIT: if (gfxd_resp_valid) begin r_scissor_x0 <= gfxd_resp_data[15:0]; r_scissor_y0 <= gfxd_resp_data[31:16]; gfx_state <= GFX_RSTATE_SCISSOR_WH_REQ; end

                GFX_RSTATE_SCISSOR_WH_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_SCISSOR_WH_WAIT;
                GFX_RSTATE_SCISSOR_WH_WAIT: if (gfxd_resp_valid) begin r_scissor_w <= gfxd_resp_data[15:0]; r_scissor_h <= gfxd_resp_data[31:16]; gfx_state <= GFX_RSTATE_FLAGS0_REQ; end

                GFX_RSTATE_FLAGS0_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_FLAGS0_WAIT;
                GFX_RSTATE_FLAGS0_WAIT: if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rstate_ptr[15:0] == 16'h0540) && (dbg_sentinel_count < 32)) begin
                        $display("%0t GFX_SENT RSTATE2 flags0=%08h scissor_en=%0b tex_en=%0b", $time, gfxd_resp_data, gfxd_resp_data[0], gfxd_resp_data[1]);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    rstate_flags0 <= gfxd_resp_data;
                    r_scissor_en <= gfxd_resp_data[0];
                    if (gfxd_resp_data[1]) gfx_state <= GFX_RSTATE_SAMP_BASE_REQ;
                    else gfx_state <= GFX_DONE;
                end

                GFX_RSTATE_SAMP_BASE_REQ:   if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_SAMP_BASE_WAIT;
                GFX_RSTATE_SAMP_BASE_WAIT:  if (gfxd_resp_valid) begin r_tex_base <= gfxd_resp_data; gfx_state <= GFX_RSTATE_SAMP_STRIDE_REQ; end

                GFX_RSTATE_SAMP_STRIDE_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_SAMP_STRIDE_WAIT;
                GFX_RSTATE_SAMP_STRIDE_WAIT: if (gfxd_resp_valid) begin r_tex_stride <= gfxd_resp_data; gfx_state <= GFX_RSTATE_SAMP_WIDTH_REQ; end

                GFX_RSTATE_SAMP_WIDTH_REQ:  if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_SAMP_WIDTH_WAIT;
                GFX_RSTATE_SAMP_WIDTH_WAIT: if (gfxd_resp_valid) begin r_tex_w <= gfxd_resp_data[15:0]; gfx_state <= GFX_RSTATE_SAMP_HEIGHT_REQ; end

                GFX_RSTATE_SAMP_HEIGHT_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_SAMP_HEIGHT_WAIT;
                GFX_RSTATE_SAMP_HEIGHT_WAIT: if (gfxd_resp_valid) begin r_tex_h <= gfxd_resp_data[15:0]; gfx_state <= GFX_RSTATE_SAMP_FMT_REQ; end

                GFX_RSTATE_SAMP_FMT_REQ:    if (gfx_mem_enq_fire) gfx_state <= GFX_RSTATE_SAMP_FMT_WAIT;
                GFX_RSTATE_SAMP_FMT_WAIT:   if (gfxd_resp_valid) begin r_tex_fmt <= gfxd_resp_data[1:0]; gfx_state <= GFX_DONE; end

                // GSTATE minimal
                GFX_GSTATE_VBO_BASE_REQ:   if (gfx_mem_enq_fire) gfx_state <= GFX_GSTATE_VBO_BASE_WAIT;
                GFX_GSTATE_VBO_BASE_WAIT:  if (gfxd_resp_valid) begin
                    g_vbo_base <= gfxd_resp_data;
`ifndef SYNTHESIS
                    if (dbg_gfx_desc_count < 16) begin
                        $display("%0t GFX_DESC GSTATE.vbo_base ptr=%08h data=%08h", $time, gstate_ptr, gfxd_resp_data);
                        dbg_gfx_desc_count <= dbg_gfx_desc_count + 1;
                    end
`endif
                    gfx_state <= GFX_GSTATE_VBO_STRIDE_REQ;
                end

                GFX_GSTATE_VBO_STRIDE_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GSTATE_VBO_STRIDE_WAIT;
                GFX_GSTATE_VBO_STRIDE_WAIT: if (gfxd_resp_valid) begin
                    g_vbo_stride <= gfxd_resp_data;
`ifndef SYNTHESIS
                    if (dbg_gfx_desc_count < 16) begin
                        $display("%0t GFX_DESC GSTATE.vbo_stride ptr=%08h data=%08h", $time, gstate_ptr, gfxd_resp_data);
                        dbg_gfx_desc_count <= dbg_gfx_desc_count + 1;
                    end
`endif
                    gfx_state <= GFX_GSTATE_IBO_BASE_REQ;
                end

                GFX_GSTATE_IBO_BASE_REQ:   if (gfx_mem_enq_fire) gfx_state <= GFX_GSTATE_IBO_BASE_WAIT;
                GFX_GSTATE_IBO_BASE_WAIT:  if (gfxd_resp_valid) begin g_ibo_base <= gfxd_resp_data; gfx_state <= GFX_GSTATE_IBO_FMT_REQ; end

                GFX_GSTATE_IBO_FMT_REQ:    if (gfx_mem_enq_fire) gfx_state <= GFX_GSTATE_IBO_FMT_WAIT;
                GFX_GSTATE_IBO_FMT_WAIT:   if (gfxd_resp_valid) begin g_ibo_format <= gfxd_resp_data; gfx_state <= GFX_GSTATE_CULL_REQ; end

                GFX_GSTATE_CULL_REQ:       if (gfx_mem_enq_fire) gfx_state <= GFX_GSTATE_CULL_WAIT;
                GFX_GSTATE_CULL_WAIT:      if (gfxd_resp_valid) begin g_cull_clip_flags <= gfxd_resp_data; gfx_state <= GFX_DONE; end

                // GPARAM minimal
                GFX_GPARAM_BASE_VERTEX_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GPARAM_BASE_VERTEX_WAIT;
                GFX_GPARAM_BASE_VERTEX_WAIT: if (gfxd_resp_valid) begin g_base_vertex <= gfxd_resp_data; gfx_state <= GFX_GPARAM_MAT_COLOR_REQ; end

                GFX_GPARAM_MAT_COLOR_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GPARAM_MAT_COLOR_WAIT;
                GFX_GPARAM_MAT_COLOR_WAIT: if (gfxd_resp_valid) begin
                    g_material_color <= gfxd_resp_data;
`ifndef SYNTHESIS
                    if (dbg_gfx_desc_count < 16) begin
                        $display("%0t GFX_DESC GPARAM.mat_color ptr=%08h data=%08h", $time, gparam_ptr, gfxd_resp_data);
                        dbg_gfx_desc_count <= dbg_gfx_desc_count + 1;
                    end
`endif
                    gfx_state <= GFX_DONE;
                end

                // GDRAW command block (minimal): non-indexed tri_list
                GFX_GDRAW_FIRST_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_FIRST_WAIT;
                GFX_GDRAW_FIRST_WAIT: if (gfxd_resp_valid) begin gd_first <= gfxd_resp_data; gfx_state <= GFX_GDRAW_COUNT_REQ; end

                GFX_GDRAW_COUNT_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_COUNT_WAIT;
                GFX_GDRAW_COUNT_WAIT: if (gfxd_resp_valid) begin gd_count <= gfxd_resp_data; gfx_state <= GFX_GDRAW_TOPO_REQ; end

                GFX_GDRAW_TOPO_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_TOPO_WAIT;
                GFX_GDRAW_TOPO_WAIT: if (gfxd_resp_valid) begin gd_topology <= gfxd_resp_data; gfx_state <= GFX_GDRAW_FLAGS_REQ; end

                GFX_GDRAW_FLAGS_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_FLAGS_WAIT;
                GFX_GDRAW_FLAGS_WAIT: if (gfxd_resp_valid) begin gd_flags <= gfxd_resp_data; gfx_state <= GFX_GDRAW_RSTATE_PTR_REQ; end

                GFX_GDRAW_RSTATE_PTR_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_RSTATE_PTR_WAIT;
                GFX_GDRAW_RSTATE_PTR_WAIT: if (gfxd_resp_valid) begin gd_rstate_ptr <= gfxd_resp_data; gfx_state <= GFX_GDRAW_BEGIN; end

                GFX_GDRAW_BEGIN: begin
`ifndef SYNTHESIS
                    if (dbg_gdraw_begin_count < 8) begin
                        $display("%0t GDRAW_BEGIN ptr=%08h first=%0d count=%0d topo=%08h flags=%08h vbo_base=%08h vbo_stride=%08h base_vtx=%0d", 
                                 $time, gdraw_ptr, gd_first, gd_count, gd_topology, gd_flags, g_vbo_base, g_vbo_stride, g_base_vertex);
                        dbg_gdraw_begin_count <= dbg_gdraw_begin_count + 1;
                    end
`endif
                    // Minimal support: tri_list (0), non-indexed (FLAGS.bit0=0)
                    gd_cur_first <= gd_first;
                    gd_remaining <= gd_count;

                    if (gd_topology != 32'd0) begin
`ifndef SYNTHESIS
                        if (dbg_gdraw_begin_count < 8) $display("%0t GDRAW_BEGIN bail: unsupported topo=%08h", $time, gd_topology);
`endif
                        gfx_state <= GFX_DONE;
                    end else if (gd_flags[0]) begin
                        // Indexed path not implemented yet
`ifndef SYNTHESIS
                        if (dbg_gdraw_begin_count < 8) $display("%0t GDRAW_BEGIN bail: indexed flags=%08h", $time, gd_flags);
`endif
                        gfx_state <= GFX_DONE;
                    end else if (g_vbo_stride == 32'h0) begin
`ifndef SYNTHESIS
                        if (dbg_gdraw_begin_count < 8) $display("%0t GDRAW_BEGIN bail: vbo_stride=0 vbo_base=%08h", $time, g_vbo_base);
`endif
                        gfx_state <= GFX_DONE;
                    end else begin
                        gfx_state <= GFX_GDRAW_TRI_SETUP;
                    end
                end

                GFX_GDRAW_TRI_SETUP: begin
`ifndef SYNTHESIS
                    if (dbg_gdraw_begin_count < 8) begin
                        $display("%0t GDRAW_TRI_SETUP remaining=%0d cur_first=%0d", $time, gd_remaining, gd_cur_first);
                    end
`endif
                    if (gd_remaining < 32'd3) begin
`ifndef SYNTHESIS
                        if (dbg_gdraw_begin_count < 8) $display("%0t GDRAW_TRI_SETUP bail: remaining=%0d", $time, gd_remaining);
`endif
                        // Drain once at macro-op completion to preserve framebuffer/scissor ordering.
                        gfx_state <= GFX_WAIT_RASTER_DONE;
                    end else begin
                        gd_idx0 <= g_base_vertex + gd_cur_first;
                        gd_idx1 <= g_base_vertex + gd_cur_first + 32'd1;
                        gd_idx2 <= g_base_vertex + gd_cur_first + 32'd2;

                        // Use material color when provided; fallback to RSTATE const color.
                        rop_const_color <= (g_material_color != 32'h0) ? g_material_color : r_const_color;

                        // Enable texture if either RSTATE enables it or GDRAW flags request it.
                        rop_tex_en <= (rstate_flags0[1] | gd_flags[2]);
                        rop_tex_base   <= r_tex_base;
                        rop_tex_stride <= r_tex_stride;
                        rop_tex_w      <= r_tex_w;
                        rop_tex_h      <= r_tex_h;
                        rop_tex_fmt    <= r_tex_fmt;

                        gfx_state <= GFX_GDRAW_V0_X_REQ;
                    end
                end

                // Vertex 0 fetch
                GFX_GDRAW_V0_X_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V0_X_WAIT;
                GFX_GDRAW_V0_X_WAIT: if (gfxd_resp_valid) begin v0x <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V0_Y_REQ; end
                GFX_GDRAW_V0_Y_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V0_Y_WAIT;
                GFX_GDRAW_V0_Y_WAIT: if (gfxd_resp_valid) begin v0y <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V0_U_REQ; end
                GFX_GDRAW_V0_U_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V0_U_WAIT;
                GFX_GDRAW_V0_U_WAIT: if (gfxd_resp_valid) begin v0u <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V0_V_REQ; end
                GFX_GDRAW_V0_V_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V0_V_WAIT;
                GFX_GDRAW_V0_V_WAIT: if (gfxd_resp_valid) begin v0v <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V1_X_REQ; end

                // Vertex 1 fetch
                GFX_GDRAW_V1_X_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V1_X_WAIT;
                GFX_GDRAW_V1_X_WAIT: if (gfxd_resp_valid) begin v1x <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V1_Y_REQ; end
                GFX_GDRAW_V1_Y_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V1_Y_WAIT;
                GFX_GDRAW_V1_Y_WAIT: if (gfxd_resp_valid) begin v1y <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V1_U_REQ; end
                GFX_GDRAW_V1_U_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V1_U_WAIT;
                GFX_GDRAW_V1_U_WAIT: if (gfxd_resp_valid) begin v1u <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V1_V_REQ; end
                GFX_GDRAW_V1_V_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V1_V_WAIT;
                GFX_GDRAW_V1_V_WAIT: if (gfxd_resp_valid) begin v1v <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V2_X_REQ; end

                // Vertex 2 fetch
                GFX_GDRAW_V2_X_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V2_X_WAIT;
                GFX_GDRAW_V2_X_WAIT: if (gfxd_resp_valid) begin v2x <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V2_Y_REQ; end
                GFX_GDRAW_V2_Y_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V2_Y_WAIT;
                GFX_GDRAW_V2_Y_WAIT: if (gfxd_resp_valid) begin v2y <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V2_U_REQ; end
                GFX_GDRAW_V2_U_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V2_U_WAIT;
                GFX_GDRAW_V2_U_WAIT: if (gfxd_resp_valid) begin v2u <= gfxd_resp_data; gfx_state <= GFX_GDRAW_V2_V_REQ; end
                GFX_GDRAW_V2_V_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_GDRAW_V2_V_WAIT;
                GFX_GDRAW_V2_V_WAIT: if (gfxd_resp_valid) begin v2v <= gfxd_resp_data; gfx_state <= GFX_GDRAW_ENQ; end

                // Enqueue triangle packet (decouples fetch/setup from raster acceptance)
                GFX_GDRAW_ENQ: begin
                    if (primq_can_push) begin
                        gd_cur_first <= gd_cur_first + 32'd3;
                        gd_remaining <= gd_remaining - 32'd3;
                        gfx_state <= GFX_GDRAW_TRI_SETUP;
                    end
                end

                // Legacy state kept for compatibility (unused)
                GFX_GDRAW_WAIT: gfx_state <= GFX_GDRAW_TRI_SETUP;

                // RRECT descriptor subset (x0,y0,x1,y1,color) then rasterize as two triangles
                GFX_RRECT_X0_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RRECT_X0_WAIT;
                GFX_RRECT_X0_WAIT: if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rrect_ptr[15:0] == 16'h8000) && (dbg_sentinel_count < 64)) begin
                        $display("%0t GFX_SENT RRECT x0=%08h", $time, gfxd_resp_data);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    rect_x0 <= gfxd_resp_data;
                    gfx_state <= GFX_RRECT_Y0_REQ;
                end

                GFX_RRECT_Y0_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RRECT_Y0_WAIT;
                GFX_RRECT_Y0_WAIT: if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rrect_ptr[15:0] == 16'h8000) && (dbg_sentinel_count < 64)) begin
                        $display("%0t GFX_SENT RRECT y0=%08h", $time, gfxd_resp_data);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    rect_y0 <= gfxd_resp_data;
                    gfx_state <= GFX_RRECT_X1_REQ;
                end

                GFX_RRECT_X1_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RRECT_X1_WAIT;
                GFX_RRECT_X1_WAIT: if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rrect_ptr[15:0] == 16'h8000) && (dbg_sentinel_count < 64)) begin
                        $display("%0t GFX_SENT RRECT x1=%08h", $time, gfxd_resp_data);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    rect_x1 <= gfxd_resp_data;
                    gfx_state <= GFX_RRECT_Y1_REQ;
                end

                GFX_RRECT_Y1_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RRECT_Y1_WAIT;
                GFX_RRECT_Y1_WAIT: if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rrect_ptr[15:0] == 16'h8000) && (dbg_sentinel_count < 64)) begin
                        $display("%0t GFX_SENT RRECT y1=%08h", $time, gfxd_resp_data);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    rect_y1 <= gfxd_resp_data;
                    gfx_state <= GFX_RRECT_COLOR_REQ;
                end

                GFX_RRECT_COLOR_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_RRECT_COLOR_WAIT;
                GFX_RRECT_COLOR_WAIT: if (gfxd_resp_valid) begin
`ifndef SYNTHESIS
                    if ((rrect_ptr[15:0] == 16'h8000) && (dbg_sentinel_count < 64)) begin
                        $display("%0t GFX_SENT RRECT color=%08h (expect lowbyte frame+1) fb_base=%08h stride=%08h", $time, gfxd_resp_data, r_fb_base, r_fb_stride);
                        dbg_sentinel_count <= dbg_sentinel_count + 1;
                    end
`endif
                    rect_color <= gfxd_resp_data;
                    rop_const_color <= gfxd_resp_data;
                    rop_tex_en <= 1'b0;
                    rop_tex_base <= 32'h0;
                    rop_tex_stride <= 32'h0;
                    rop_tex_w <= 16'h0;
                    rop_tex_h <= 16'h0;
                    rop_tex_fmt <= 2'd0;
                    // Triangle 1: (x0,y0) (x1,y0) (x0,y1)
                    v0x <= rect_x0; v0y <= rect_y0;
                    v1x <= rect_x1; v1y <= rect_y0;
                    v2x <= rect_x0; v2y <= rect_y1;
                    v0u <= 32'h0; v0v <= 32'h0;
                    v1u <= 32'h0; v1v <= 32'h0;
                    v2u <= 32'h0; v2v <= 32'h0;
                    gfx_state <= GFX_RRECT_ENQ1;
                end

                GFX_RRECT_ENQ1: begin
                    if (primq_can_push) begin
`ifndef SYNTHESIS
                        if ((rrect_ptr[15:0] == 16'h8000) && (dbg_sentinel_count < 64)) begin
                            $display("%0t GFX_SENT RRECT enq tri1 v0=(%0d,%0d) v1=(%0d,%0d) v2=(%0d,%0d)", $time,
                                     v0x, v0y, v1x, v1y, v2x, v2y);
                            dbg_sentinel_count <= dbg_sentinel_count + 1;
                        end
`endif
                        // Triangle 2: (x1,y0) (x1,y1) (x0,y1)
                        v0x <= rect_x1; v0y <= rect_y0;
                        v1x <= rect_x1; v1y <= rect_y1;
                        v2x <= rect_x0; v2y <= rect_y1;
                        gfx_state <= GFX_RRECT_ENQ2;
                    end
                end

                GFX_RRECT_ENQ2: begin
                    if (primq_can_push) begin
`ifndef SYNTHESIS
                        if ((rrect_ptr[15:0] == 16'h8000) && (dbg_sentinel_count < 64)) begin
                            $display("%0t GFX_SENT RRECT enq tri2 v0=(%0d,%0d) v1=(%0d,%0d) v2=(%0d,%0d)", $time,
                                     v0x, v0y, v1x, v1y, v2x, v2y);
                            dbg_sentinel_count <= dbg_sentinel_count + 1;
                        end
`endif
                        // Restore default color and drain before retiring macro-op.
                        rop_const_color <= r_const_color;
                        gfx_state <= GFX_WAIT_RASTER_DONE;
                    end
                end

                // Legacy states kept for compatibility (unused)
                GFX_RRECT_SEND1_V0: gfx_state <= GFX_RRECT_ENQ1;
                GFX_RRECT_SEND1_V1: gfx_state <= GFX_RRECT_ENQ1;
                GFX_RRECT_SEND1_V2: gfx_state <= GFX_RRECT_ENQ1;
                GFX_RRECT_DRAW1:    gfx_state <= GFX_RRECT_ENQ1;
                GFX_RRECT_WAIT1:    gfx_state <= GFX_RRECT_ENQ2;
                GFX_RRECT_SEND2_V0: gfx_state <= GFX_RRECT_ENQ2;
                GFX_RRECT_SEND2_V1: gfx_state <= GFX_RRECT_ENQ2;
                GFX_RRECT_SEND2_V2: gfx_state <= GFX_RRECT_ENQ2;
                GFX_RRECT_DRAW2:    gfx_state <= GFX_RRECT_ENQ2;
                GFX_RRECT_WAIT2:    gfx_state <= GFX_WAIT_RASTER_DONE;

                GFX_FETCH_X0_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_X0_WAIT;
                GFX_FETCH_X0_WAIT: if (gfxd_resp_valid) begin v0x <= gfxd_resp_data; gfx_state <= GFX_FETCH_Y0_REQ; end

                GFX_FETCH_Y0_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_Y0_WAIT;
                GFX_FETCH_Y0_WAIT: if (gfxd_resp_valid) begin v0y <= gfxd_resp_data; gfx_state <= GFX_FETCH_U0_REQ; end

                GFX_FETCH_U0_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_U0_WAIT;
                GFX_FETCH_U0_WAIT: if (gfxd_resp_valid) begin v0u <= gfxd_resp_data; gfx_state <= GFX_FETCH_V0_REQ; end

                GFX_FETCH_V0_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_V0_WAIT;
                GFX_FETCH_V0_WAIT: if (gfxd_resp_valid) begin v0v <= gfxd_resp_data; gfx_state <= GFX_FETCH_C0_REQ; end

                GFX_FETCH_C0_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_C0_WAIT;
                GFX_FETCH_C0_WAIT: if (gfxd_resp_valid) begin rsetup_color <= gfxd_resp_data; gfx_state <= GFX_FETCH_X1_REQ; end

                GFX_FETCH_X1_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_X1_WAIT;
                GFX_FETCH_X1_WAIT: if (gfxd_resp_valid) begin v1x <= gfxd_resp_data; gfx_state <= GFX_FETCH_Y1_REQ; end

                GFX_FETCH_Y1_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_Y1_WAIT;
                GFX_FETCH_Y1_WAIT: if (gfxd_resp_valid) begin v1y <= gfxd_resp_data; gfx_state <= GFX_FETCH_U1_REQ; end

                GFX_FETCH_U1_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_U1_WAIT;
                GFX_FETCH_U1_WAIT: if (gfxd_resp_valid) begin v1u <= gfxd_resp_data; gfx_state <= GFX_FETCH_V1_REQ; end

                GFX_FETCH_V1_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_V1_WAIT;
                GFX_FETCH_V1_WAIT: if (gfxd_resp_valid) begin v1v <= gfxd_resp_data; gfx_state <= GFX_FETCH_X2_REQ; end

                GFX_FETCH_X2_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_X2_WAIT;
                GFX_FETCH_X2_WAIT: if (gfxd_resp_valid) begin v2x <= gfxd_resp_data; gfx_state <= GFX_FETCH_Y2_REQ; end

                GFX_FETCH_Y2_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_Y2_WAIT;
                GFX_FETCH_Y2_WAIT: if (gfxd_resp_valid) begin v2y <= gfxd_resp_data; gfx_state <= GFX_FETCH_U2_REQ; end

                GFX_FETCH_U2_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_U2_WAIT;
                GFX_FETCH_U2_WAIT: if (gfxd_resp_valid) begin v2u <= gfxd_resp_data; gfx_state <= GFX_FETCH_V2_REQ; end

                GFX_FETCH_V2_REQ: if (gfx_mem_enq_fire) gfx_state <= GFX_FETCH_V2_WAIT;
                GFX_FETCH_V2_WAIT: if (gfxd_resp_valid) begin v2v <= gfxd_resp_data; gfx_state <= GFX_DONE; end

                // Legacy SEND states kept for compatibility (unused)
                GFX_SEND_V0: gfx_state <= GFX_DONE;
                GFX_SEND_V1: gfx_state <= GFX_DONE;
                GFX_SEND_V2: gfx_state <= GFX_DONE;
                GFX_SEND_DRAW: gfx_state <= GFX_WAIT_RASTER_DONE;

                // Enqueue RDRAW triangle and then drain
                GFX_RDRAW_ENQ: begin
                    if (primq_can_push) begin
                        gfx_state <= GFX_WAIT_RASTER_DONE;
                    end
                end

                // Wait for raster + ROP to drain before retiring RDRAW
                GFX_WAIT_RASTER_DONE: begin
                    if ((primq_count == '0) && (feed_state == FEED_IDLE) && !raster_busy && !rop_busy && !interp_busy && !shade_busy) gfx_state <= GFX_DONE;
                end

                GFX_DONE: gfx_state <= GFX_IDLE;
                default: gfx_state <= GFX_IDLE;
            endcase
        end
    end

    // cmd generation (one command per cycle when raster is ready)
    always_comb begin
        cmd_valid  = raster_cmd_valid;
        cmd_type   = raster_cmd_type;
        cmd_data_x = raster_cmd_data_x;
        cmd_data_y = raster_cmd_data_y;
    end

    // Primitive queue push control from gfx_state machine
    always_comb begin
        primq_push = 1'b0;
        primq_push_pkt = '0;

        if (gfx_state == GFX_GDRAW_ENQ) begin
            primq_push = 1'b1;
            primq_push_pkt.v0x = v0x; primq_push_pkt.v0y = v0y;
            primq_push_pkt.v1x = v1x; primq_push_pkt.v1y = v1y;
            primq_push_pkt.v2x = v2x; primq_push_pkt.v2y = v2y;
            primq_push_pkt.v0u = v0u; primq_push_pkt.v0v = v0v;
            primq_push_pkt.v1u = v1u; primq_push_pkt.v1v = v1v;
            primq_push_pkt.v2u = v2u; primq_push_pkt.v2v = v2v;
            primq_push_pkt.const_color_argb = rop_const_color;
            primq_push_pkt.tex_enable       = rop_tex_en;
            primq_push_pkt.tex_base         = rop_tex_base;
            primq_push_pkt.tex_stride_bytes = rop_tex_stride;
            primq_push_pkt.tex_width        = rop_tex_w;
            primq_push_pkt.tex_height       = rop_tex_h;
            primq_push_pkt.tex_format       = rop_tex_fmt;
        end else if (gfx_state == GFX_RRECT_ENQ1 || gfx_state == GFX_RRECT_ENQ2) begin
            primq_push = 1'b1;
            primq_push_pkt.v0x = v0x; primq_push_pkt.v0y = v0y;
            primq_push_pkt.v1x = v1x; primq_push_pkt.v1y = v1y;
            primq_push_pkt.v2x = v2x; primq_push_pkt.v2y = v2y;
            primq_push_pkt.v0u = v0u; primq_push_pkt.v0v = v0v;
            primq_push_pkt.v1u = v1u; primq_push_pkt.v1v = v1v;
            primq_push_pkt.v2u = v2u; primq_push_pkt.v2v = v2v;
            primq_push_pkt.const_color_argb = rop_const_color;
            primq_push_pkt.tex_enable       = 1'b0;
            primq_push_pkt.tex_base         = 32'h0;
            primq_push_pkt.tex_stride_bytes = 32'h0;
            primq_push_pkt.tex_width        = 16'h0;
            primq_push_pkt.tex_height       = 16'h0;
            primq_push_pkt.tex_format       = 2'd0;
        end else if (gfx_state == GFX_RDRAW_ENQ) begin
            primq_push = 1'b1;
            primq_push_pkt.v0x = v0x; primq_push_pkt.v0y = v0y;
            primq_push_pkt.v1x = v1x; primq_push_pkt.v1y = v1y;
            primq_push_pkt.v2x = v2x; primq_push_pkt.v2y = v2y;
            primq_push_pkt.v0u = v0u; primq_push_pkt.v0v = v0v;
            primq_push_pkt.v1u = v1u; primq_push_pkt.v1v = v1v;
            primq_push_pkt.v2u = v2u; primq_push_pkt.v2v = v2v;
            primq_push_pkt.const_color_argb = rop_const_color;
            primq_push_pkt.tex_enable       = rop_tex_en;
            primq_push_pkt.tex_base         = rop_tex_base;
            primq_push_pkt.tex_stride_bytes = rop_tex_stride;
            primq_push_pkt.tex_width        = rop_tex_w;
            primq_push_pkt.tex_height       = rop_tex_h;
            primq_push_pkt.tex_format       = rop_tex_fmt;
        end

        if (!primq_can_push) primq_push = 1'b0;
    end

    raster_unit u_raster (
        .clk(clk),
        .rst_n(rst_n),
        .cmd_valid(cmd_valid),
        .cmd_ready(raster_cmd_ready),
        .cmd_type(cmd_type),
        .cmd_data_x(cmd_data_x),
        .cmd_data_y(cmd_data_y),

        .attr_const_color_argb(hold_prim.const_color_argb),
        .attr_tex_enable(hold_prim.tex_enable),
        .attr_tex_base(hold_prim.tex_base),
        .attr_tex_stride_bytes(hold_prim.tex_stride_bytes),
        .attr_tex_width(hold_prim.tex_width),
        .attr_tex_height(hold_prim.tex_height),
        .attr_tex_format(hold_prim.tex_format),
        .attr_v0_u(hold_prim.v0u),
        .attr_v0_v(hold_prim.v0v),
        .attr_v1_u(hold_prim.v1u),
        .attr_v1_v(hold_prim.v1v),
        .attr_v2_u(hold_prim.v2u),
        .attr_v2_v(hold_prim.v2v),

        .quad_valid(raster_quad_valid),
        .quad_ready(raster_quad_ready),
        .quad_x(raster_quad_x),
        .quad_y(raster_quad_y),
        .quad_mask(raster_quad_mask),
        .quad_bary_w0(raster_bary_w0),
        .quad_bary_w1(raster_bary_w1),
        .quad_bary_w2(raster_bary_w2),

        .quad_const_color_argb(raster_const_color),
        .quad_tex_enable(raster_tex_enable),
        .quad_tex_base(raster_tex_base),
        .quad_tex_stride_bytes(raster_tex_stride),
        .quad_tex_width(raster_tex_w),
        .quad_tex_height(raster_tex_h),
        .quad_tex_format(raster_tex_fmt),
        .quad_v0_u(raster_v0_u),
        .quad_v0_v(raster_v0_v),
        .quad_v1_u(raster_v1_u),
        .quad_v1_v(raster_v1_v),
        .quad_v2_u(raster_v2_u),
        .quad_v2_v(raster_v2_v),

        .tri_area_out(raster_tri_area),
        .busy(raster_busy)
    );

    interpolator_unit #(
        .Q_DEPTH(ROP_QUAD_Q_DEPTH)
    ) u_interp (
        .clk(clk),
        .rst_n(rst_n),
        .flush(flush_all),

        .in_valid(raster_quad_valid),
        .in_ready(raster_quad_ready),
        .in_x(raster_quad_x),
        .in_y(raster_quad_y),
        .in_mask(raster_quad_mask),
        .in_bary_w0(raster_bary_w0),
        .in_bary_w1(raster_bary_w1),
        .in_bary_w2(raster_bary_w2),
        .in_tri_area(raster_tri_area),

        .in_const_color_argb(raster_const_color),
        .in_tex_enable(raster_tex_enable),
        .in_tex_base(raster_tex_base),
        .in_tex_stride_bytes(raster_tex_stride),
        .in_tex_width(raster_tex_w),
        .in_tex_height(raster_tex_h),
        .in_tex_format(raster_tex_fmt),
        .in_v0_u(raster_v0_u),
        .in_v0_v(raster_v0_v),
        .in_v1_u(raster_v1_u),
        .in_v1_v(raster_v1_v),
        .in_v2_u(raster_v2_u),
        .in_v2_v(raster_v2_v),

        .out_valid(interp_quad_valid),
        .out_ready(interp_quad_ready),
        .out_x(interp_quad_x),
        .out_y(interp_quad_y),
        .out_mask(interp_quad_mask),
        .out_bary_w0(interp_bary_w0),
        .out_bary_w1(interp_bary_w1),
        .out_bary_w2(interp_bary_w2),
        .out_tri_area(interp_tri_area),

        .out_const_color_argb(interp_const_color),
        .out_tex_enable(interp_tex_enable),
        .out_tex_base(interp_tex_base),
        .out_tex_stride_bytes(interp_tex_stride),
        .out_tex_width(interp_tex_w),
        .out_tex_height(interp_tex_h),
        .out_tex_format(interp_tex_fmt),
        .out_v0_u(interp_v0_u),
        .out_v0_v(interp_v0_v),
        .out_v1_u(interp_v1_u),
        .out_v1_v(interp_v1_v),
        .out_v2_u(interp_v2_u),
        .out_v2_v(interp_v2_v),

        .busy(interp_busy)
    );

    shade_unit u_shade (
        .clk(clk),
        .rst_n(rst_n),
        .flush(flush_all),

        .in_valid(interp_quad_valid),
        .in_ready(interp_quad_ready),
        .in_x(interp_quad_x),
        .in_y(interp_quad_y),
        .in_mask(interp_quad_mask),
        .in_bary_w0(interp_bary_w0),
        .in_bary_w1(interp_bary_w1),
        .in_bary_w2(interp_bary_w2),
        .in_tri_area(interp_tri_area),

        .in_const_color_argb(interp_const_color),
        .in_tex_enable(interp_tex_enable),
        .in_tex_base(interp_tex_base),
        .in_tex_stride_bytes(interp_tex_stride),
        .in_tex_width(interp_tex_w),
        .in_tex_height(interp_tex_h),
        .in_tex_format(interp_tex_fmt),
        .in_v0_u(interp_v0_u),
        .in_v0_v(interp_v0_v),
        .in_v1_u(interp_v1_u),
        .in_v1_v(interp_v1_v),
        .in_v2_u(interp_v2_u),
        .in_v2_v(interp_v2_v),

        .out_valid(shade_quad_valid),
        .out_ready(shade_quad_ready),
        .out_x(shade_quad_x),
        .out_y(shade_quad_y),
        .out_mask(shade_quad_mask),

        .out_const_color_argb(shade_const_color),
        .out_tex_enable(shade_tex_enable),
        .out_tex_base(shade_tex_base),
        .out_tex_stride_bytes(shade_tex_stride),
        .out_tex_width(shade_tex_w),
        .out_tex_height(shade_tex_h),
        .out_tex_format(shade_tex_fmt),
        .out_v0_u(shade_v0_u),
        .out_v0_v(shade_v0_v),
        .out_v1_u(shade_v1_u),
        .out_v1_v(shade_v1_v),
        .out_v2_u(shade_v2_u),
        .out_v2_v(shade_v2_v),
        .out_bary_w0(shade_bary_w0),
        .out_bary_w1(shade_bary_w1),
        .out_bary_w2(shade_bary_w2),
        .out_tri_area(shade_tri_area),
        .out_tex_addr(shade_tex_addr),

        .busy(shade_busy)
    );

    rop_unit u_rop (
        .clk(clk),
        .rst_n(rst_n),
        .flush(flush_all),

        .fb_base(r_fb_base),
        .fb_stride_bytes(r_fb_stride),
        .fb_format(r_fb_fmt),
        .fb_width(r_fb_w),
        .fb_height(r_fb_h),
        .scissor_en(r_scissor_en),
        .scissor_x0(r_scissor_x0),
        .scissor_y0(r_scissor_y0),
        .scissor_w(r_scissor_w),
        .scissor_h(r_scissor_h),

        .quad_const_color_argb(shade_const_color),
        .quad_tex_enable(shade_tex_enable),
        .quad_tex_base(shade_tex_base),
        .quad_tex_stride_bytes(shade_tex_stride),
        .quad_tex_width(shade_tex_w),
        .quad_tex_height(shade_tex_h),
        .quad_tex_format(shade_tex_fmt),
        .quad_v0_u(shade_v0_u),
        .quad_v0_v(shade_v0_v),
        .quad_v1_u(shade_v1_u),
        .quad_v1_v(shade_v1_v),
        .quad_v2_u(shade_v2_u),
        .quad_v2_v(shade_v2_v),
        .quad_bary_w0(shade_bary_w0),
        .quad_bary_w1(shade_bary_w1),
        .quad_bary_w2(shade_bary_w2),
        .quad_tri_area(shade_tri_area),
        .quad_tex_addr(shade_tex_addr),

        .tex_req_valid(rop_tex_req_valid),
        .tex_req_addr(rop_tex_req_addr),
        .tex_req_ready(rop_tex_req_ready),
        .tex_resp_valid(rop_tex_resp_valid),
        .tex_resp_data(rop_tex_resp_data),

        .quad_valid(shade_quad_valid),
        .quad_ready(shade_quad_ready),
        .quad_x(shade_quad_x),
        .quad_y(shade_quad_y),
        .quad_mask(shade_quad_mask),

        .st_valid(rop_st_valid),
        .st_addr(rop_st_addr),
        .st_wdata(rop_st_wdata),
        .st_wstrb(rop_st_wstrb),
        .st_ready(rop_st_ready),

        .busy(rop_busy)
    );

    // ROP store FIFO (decouples ROP from LSU/WMB backpressure)
    localparam int unsigned STQ_IDX_W = (ROP_STQ_DEPTH <= 1) ? 1 : $clog2(ROP_STQ_DEPTH);
    localparam int unsigned STQ_CNT_W = (ROP_STQ_DEPTH <= 1) ? 1 : $clog2(ROP_STQ_DEPTH + 1);
    localparam logic [STQ_IDX_W-1:0] STQ_LAST = STQ_IDX_W'(ROP_STQ_DEPTH - 1);

    logic [31:0] stq_addr  [ROP_STQ_DEPTH];
    logic [31:0] stq_wdata [ROP_STQ_DEPTH];
    logic [3:0]  stq_wstrb [ROP_STQ_DEPTH];
    logic [STQ_IDX_W-1:0] stq_head;
    logic [STQ_IDX_W-1:0] stq_tail;
    logic [STQ_CNT_W-1:0] stq_count;

    wire stq_out_valid = (stq_count != '0);
    wire stq_pop = stq_out_valid && gfx_st_ready;
    wire stq_full = (stq_count == STQ_CNT_W'(ROP_STQ_DEPTH));
    wire stq_can_push = (!stq_full) || stq_pop;
    wire stq_push = rop_st_valid && stq_can_push;

    assign rop_st_ready = stq_can_push;
    assign gfx_st_valid = stq_out_valid;
    assign gfx_st_addr  = stq_addr[stq_head];
    assign gfx_st_wdata = stq_wdata[stq_head];
    assign gfx_st_wstrb = stq_wstrb[stq_head];

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            stq_head  <= '0;
            stq_tail  <= '0;
            stq_count <= '0;
        end else if (flush_all) begin
            stq_head  <= '0;
            stq_tail  <= '0;
            stq_count <= '0;
        end else begin
            if (stq_push) begin
                stq_addr[stq_tail]  <= rop_st_addr;
                stq_wdata[stq_tail] <= rop_st_wdata;
                stq_wstrb[stq_tail] <= rop_st_wstrb;
                stq_tail <= (stq_tail == STQ_LAST) ? '0 : (stq_tail + 1'b1);
            end

            if (stq_pop) begin
                stq_head <= (stq_head == STQ_LAST) ? '0 : (stq_head + 1'b1);
            end

            unique case ({stq_push, stq_pop})
                2'b10: stq_count <= stq_count + 1'b1;
                2'b01: stq_count <= stq_count - 1'b1;
                default: ;
            endcase
        end
    end

    assign rop_tex_resp_valid = tex_resp_valid;
    assign rop_tex_resp_data  = tex_resp_data;

endmodule
