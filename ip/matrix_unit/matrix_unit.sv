`timescale 1ns/1ps
// ============================================================================
// Matrix Unit (MXU) — Top-level integration
//
// Memory-mapped peripheral for hardware-accelerated tile matrix multiply-
// accumulate (GEMM). Supports C += A × B where tiles are up to 4×4 in
// Q16.16 fixed-point format.
//
// Operation flow:
//   1. Software writes tile A/B addresses + dimensions to registers
//   2. Software writes CTRL[0]=1 to start
//   3. MXU loads tile A from memory, loads tile B, computes C += A × B
//   4. MXU writes result tile C back to memory
//   5. MXU asserts done + optional IRQ / mailbox completion notification
//
// Integration: Connects as a bus-master to the cluster memory system
// (shares an L2 cache port) and has an AXI-MailboxFabric endpoint for
// job dispatch and completion signaling.
//
// Register map: See mat_pkg.sv (MXU_REG_* constants)
// ============================================================================
module matrix_unit
  import mat_pkg::*;
  import mailbox_pkg::*;
#(
    parameter logic [NODE_ID_WIDTH-1:0] MAILBOX_SRC_ID = 16'h0280
)
(
    input  logic        clk,
    input  logic        rst_n,

    // Register access interface (from cluster bus or direct CU access)
    input  logic        reg_wr_valid,
    input  logic [4:0]  reg_wr_addr,
    input  logic [31:0] reg_wr_data,
    output logic        reg_wr_ready,

    input  logic        reg_rd_valid,
    input  logic [4:0]  reg_rd_addr,
    output logic [31:0] reg_rd_data,
    output logic        reg_rd_ready,

    // Interrupt output
    output logic        irq,

    // AXI-MailboxFabric stream interface (to cluster switch)
    output logic                           mb_tx_valid,
    input  logic                           mb_tx_ready,
    output mailbox_flit_t                  mb_tx_data,
    output logic [NODE_ID_WIDTH-1:0]       mb_tx_dest_id,
    input  logic                           mb_rx_valid,
    output logic                           mb_rx_ready,
    input  mailbox_flit_t                  mb_rx_data,
    input  logic [NODE_ID_WIDTH-1:0]       mb_rx_dest_id,

    // Memory interface (to L2 cache port) — supports read and write
    output logic        mem_req_valid,
    output logic        mem_req_rw,      // 0=read, 1=write
    output logic [31:0] mem_req_addr,
    output logic [7:0]  mem_req_size,
    output logic [3:0]  mem_req_qos,
    output logic [7:0]  mem_req_id,
    output logic [63:0] mem_req_wdata,
    output logic [7:0]  mem_req_wstrb,
    input  logic        mem_req_ready,

    input  logic        mem_resp_valid,
    input  logic [63:0] mem_resp_data,
    input  logic [7:0]  mem_resp_id,

    // Status
    output logic        mxu_busy,
    output logic        mxu_done
);

    // -----------------------------------------------------------------------
    // Register file
    // -----------------------------------------------------------------------
    logic [31:0] regs [0:MXU_NUM_REGS-1];

    wire ctrl_start     = regs[MXU_REG_CTRL][0];
    wire ctrl_irq_en    = regs[MXU_REG_CTRL][3];
    wire ctrl_clear_acc = regs[MXU_REG_CTRL][4];

    // -----------------------------------------------------------------------
    // FSM
    // -----------------------------------------------------------------------
    typedef enum logic [3:0] {
        MXU_IDLE      = 4'd0,
        MXU_LOAD_A    = 4'd1,  // Load tile A from memory
        MXU_WAIT_A    = 4'd2,
        MXU_LOAD_B    = 4'd3,  // Load tile B from memory (into weight regs)
        MXU_WAIT_B    = 4'd4,
        MXU_COMPUTE   = 4'd5,  // Stream A through systolic array
        MXU_WAIT_COMP = 4'd6,  // Wait for systolic array to flush
        MXU_STORE_C   = 4'd7,  // Write result tile C back to memory
        MXU_WAIT_WR   = 4'd8,
        MXU_DONE      = 4'd9
    } mxu_state_e;

    mxu_state_e state;

    assign mxu_busy = (state != MXU_IDLE) && (state != MXU_DONE);
    assign mxu_done = (state == MXU_DONE);
    assign irq      = mxu_done && ctrl_irq_en;

    // -----------------------------------------------------------------------
    // Tile buffers
    // -----------------------------------------------------------------------
    logic signed [31:0] tile_a [TILE_DIM-1:0][TILE_DIM-1:0];
    logic signed [31:0] tile_b [TILE_DIM-1:0][TILE_DIM-1:0];

    // Memory beat counters
    logic [3:0] beat_cnt;    // counts 64-bit beats
    logic [3:0] beat_total;  // total beats expected
    logic       mem_req_pending;

    // Tile element index for sequential load
    logic [3:0] elem_row, elem_col;

    // -----------------------------------------------------------------------
    // Systolic array instance
    // -----------------------------------------------------------------------
    logic        sa_clear_acc;
    logic        sa_load_weight;
    logic [1:0]  sa_w_row, sa_w_col;
    logic signed [31:0] sa_w_data;
    logic        sa_compute_valid;
    logic signed [31:0] sa_a_in [TILE_DIM-1:0];
    logic        sa_c_valid;
    logic signed [31:0] sa_c_out [TILE_DIM-1:0][TILE_DIM-1:0];

    systolic_array u_sa (
        .clk(clk), .rst_n(rst_n),
        .clear_acc(sa_clear_acc),
        .load_weight(sa_load_weight),
        .w_row(sa_w_row), .w_col(sa_w_col), .w_data(sa_w_data),
        .compute_valid(sa_compute_valid),
        .a_in(sa_a_in),
        .c_valid(sa_c_valid),
        .c_out(sa_c_out)
    );

    // -----------------------------------------------------------------------
    // Tile load/store and compute sequencer
    // -----------------------------------------------------------------------
    logic [3:0] compute_cycle;
    logic [4:0] weight_load_idx; // Sequential weight load counter (needs 5 bits for TILE_ELEMS=16)
    logic [3:0] store_elem;      // Store counter for C tile writeback

    // Performance counters
    logic [31:0] perf_ops;
    logic [31:0] perf_tiles;

    // Compute state
    assign sa_clear_acc = (state == MXU_IDLE) && ctrl_clear_acc;

    // Memory request generation
    assign mem_req_valid = mem_req_pending;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int i = 0; i < MXU_NUM_REGS; i++) regs[i] <= '0;
            state           <= MXU_IDLE;
            mem_req_pending <= 1'b0;
            beat_cnt        <= '0;
            perf_ops        <= '0;
            perf_tiles      <= '0;
            sa_load_weight  <= 1'b0;
            sa_compute_valid <= 1'b0;
            compute_cycle   <= '0;
            weight_load_idx <= '0;
            store_elem      <= '0;
            mem_req_rw      <= 1'b0;
            mem_req_addr    <= '0;
            mem_req_size    <= '0;
            mem_req_qos     <= 4'h1;
            mem_req_id      <= '0;
            mem_req_wdata   <= '0;
            mem_req_wstrb   <= '0;
            for (int r = 0; r < TILE_DIM; r++)
                for (int c = 0; c < TILE_DIM; c++) begin
                    tile_a[r][c] <= '0;
                    tile_b[r][c] <= '0;
                end
            for (int r = 0; r < TILE_DIM; r++)
                sa_a_in[r] <= '0;
        end else begin
            sa_load_weight   <= 1'b0; // default: one-cycle pulse
            sa_compute_valid <= 1'b0;

            // Register writes (direct slot_reg has priority; mailbox injects when idle)
            if (reg_wr_valid || mb_wr_inject) begin
                automatic logic [4:0]  wr_addr_mux;
                automatic logic [31:0] wr_data_mux;
                wr_addr_mux = reg_wr_valid ? reg_wr_addr : mb_wr_inject_addr;
                wr_data_mux = reg_wr_valid ? reg_wr_data : mb_wr_inject_data;
                case (wr_addr_mux)
                    MXU_REG_CTRL: begin
                        regs[MXU_REG_CTRL] <= wr_data_mux;
                        if (wr_data_mux[0] && (state == MXU_IDLE)) begin
                            state <= MXU_LOAD_A;
                        end
                        if (!wr_data_mux[0] && (state == MXU_DONE)) begin
                            state <= MXU_IDLE;
                        end
                    end
                    MXU_REG_SRC_A_ADDR, MXU_REG_SRC_B_ADDR, MXU_REG_DST_C_ADDR,
                    MXU_REG_TILE_M, MXU_REG_TILE_N, MXU_REG_TILE_K,
                    MXU_REG_STRIDE_A, MXU_REG_STRIDE_B, MXU_REG_STRIDE_C,
                    MXU_REG_ACC_IDX: begin
                        if (state != MXU_IDLE || !mxu_busy)
                            regs[wr_addr_mux] <= wr_data_mux;
                    end
                    default: ;
                endcase
            end

            // State machine
            case (state)
                MXU_IDLE: begin
                    // Wait for start trigger (handled in register write above)
                end

                // --- Load tile A from memory ---
                MXU_LOAD_A: begin
                    mem_req_pending <= 1'b1;
                    mem_req_rw      <= 1'b0;
                    mem_req_addr    <= regs[MXU_REG_SRC_A_ADDR];
                    mem_req_size    <= 8'(TILE_BYTES); // 64 bytes
                    mem_req_id      <= 8'hA0;
                    beat_cnt        <= '0;
                    state           <= MXU_WAIT_A;
                end

                MXU_WAIT_A: begin
                    if (mem_req_pending && mem_req_ready)
                        mem_req_pending <= 1'b0;

                    if (mem_resp_valid && (mem_resp_id == 8'hA0)) begin
                        // Two words per 64-bit beat, row-major order
                        automatic int idx0 = {beat_cnt, 1'b0};
                        automatic int idx1 = {beat_cnt, 1'b1};
                        tile_a[idx0/TILE_DIM][idx0%TILE_DIM] <= $signed(mem_resp_data[31:0]);
                        tile_a[idx1/TILE_DIM][idx1%TILE_DIM] <= $signed(mem_resp_data[63:32]);
                        beat_cnt <= beat_cnt + 1;
                        if (beat_cnt == 4'(TILE_ELEMS/2 - 1))
                            state <= MXU_LOAD_B;
                    end
                end

                // --- Load tile B into systolic array weight registers ---
                MXU_LOAD_B: begin
                    mem_req_pending <= 1'b1;
                    mem_req_rw      <= 1'b0;
                    mem_req_addr    <= regs[MXU_REG_SRC_B_ADDR];
                    mem_req_size    <= 8'(TILE_BYTES);
                    mem_req_id      <= 8'hA1;
                    beat_cnt        <= '0;
                    weight_load_idx <= '0;
                    state           <= MXU_WAIT_B;
                end

                MXU_WAIT_B: begin
                    if (mem_req_pending && mem_req_ready)
                        mem_req_pending <= 1'b0;

                    if (mem_resp_valid && (mem_resp_id == 8'hA1)) begin
                        automatic int idx0 = {beat_cnt, 1'b0};
                        automatic int idx1 = {beat_cnt, 1'b1};
                        tile_b[idx0/TILE_DIM][idx0%TILE_DIM] <= $signed(mem_resp_data[31:0]);
                        tile_b[idx1/TILE_DIM][idx1%TILE_DIM] <= $signed(mem_resp_data[63:32]);
                        beat_cnt <= beat_cnt + 1;
                        if (beat_cnt == 4'(TILE_ELEMS/2 - 1)) begin
                            // All B data received; now load into systolic array weights
                            weight_load_idx <= '0;
                            compute_cycle   <= '0;
                            state <= MXU_COMPUTE; // Will load weights first, then compute
                        end
                    end
                end

                // --- Load weights into SA, then stream A through ---
                MXU_COMPUTE: begin
                    if (weight_load_idx < 5'(TILE_ELEMS)) begin
                        // Load one weight per cycle
                        sa_load_weight <= 1'b1;
                        sa_w_row <= weight_load_idx[3:2];
                        sa_w_col <= weight_load_idx[1:0];
                        sa_w_data <= tile_b[weight_load_idx[3:2]][weight_load_idx[1:0]];
                        weight_load_idx <= weight_load_idx + 1;
                    end else begin
                        // Stream activation rows
                        if (compute_cycle < TILE_DIM[3:0]) begin
                            sa_compute_valid <= 1'b1;
                            for (int r = 0; r < TILE_DIM; r++)
                                sa_a_in[r] <= tile_a[r][compute_cycle];
                            compute_cycle <= compute_cycle + 1;
                        end else begin
                            compute_cycle <= '0;
                            state <= MXU_WAIT_COMP;
                        end
                    end
                end

                MXU_WAIT_COMP: begin
                    // Wait for systolic array to flush (SA_LATENCY cycles from last input)
                    compute_cycle <= compute_cycle + 1;
                    if (compute_cycle >= SA_LATENCY[3:0]) begin
                        perf_ops   <= perf_ops + TILE_ELEMS[31:0];
                        perf_tiles <= perf_tiles + 1;
                        store_elem  <= '0;
                        state       <= MXU_STORE_C;
                    end
                end

                // --- Write result tile C back to memory ---
                MXU_STORE_C: begin
                    mem_req_pending <= 1'b1;
                    mem_req_rw      <= 1'b1;
                    mem_req_addr    <= regs[MXU_REG_DST_C_ADDR] + {store_elem, 3'b000}; // 8 bytes per beat
                    mem_req_size    <= 8'd8; // one 64-bit beat at a time
                    mem_req_id      <= 8'hA2;
                    begin
                        automatic int idx0 = {store_elem, 1'b0};
                        automatic int idx1 = {store_elem, 1'b1};
                        mem_req_wdata <= {sa_c_out[idx1/TILE_DIM][idx1%TILE_DIM],
                                         sa_c_out[idx0/TILE_DIM][idx0%TILE_DIM]};
                    end
                    mem_req_wstrb <= 8'hFF;
                    state         <= MXU_WAIT_WR;
                end

                MXU_WAIT_WR: begin
                    if (mem_req_pending && mem_req_ready) begin
                        mem_req_pending <= 1'b0;
                        store_elem <= store_elem + 1;
                        if (store_elem >= 4'(TILE_ELEMS/2 - 1)) begin
                            // All results written
                            regs[MXU_REG_CTRL][1]   <= 1'b0; // Clear busy
                            regs[MXU_REG_CTRL][2]   <= 1'b1; // Set done
                            regs[MXU_REG_PERF_OPS]  <= perf_ops;
                            regs[MXU_REG_PERF_TILES] <= perf_tiles;
                            state <= MXU_DONE;
                        end else begin
                            state <= MXU_STORE_C;
                        end
                    end
                end

                MXU_DONE: begin
                    // Wait for software to clear via CTRL[0]=0
                    // Re-start handled in register write
                end

                default: state <= MXU_IDLE;
            endcase

            // Update status register
            regs[MXU_REG_STATUS] <= {perf_tiles[15:0], 4'b0, state, 4'b0};
        end
    end

    // -----------------------------------------------------------------------
    // Register read logic
    // -----------------------------------------------------------------------
    assign reg_wr_ready = 1'b1;
    assign reg_rd_ready = 1'b1;

    always_comb begin
        reg_rd_data = '0;
        if (reg_rd_valid && (reg_rd_addr < MXU_NUM_REGS)) begin
            if (reg_rd_addr == MXU_REG_ACC_DATA) begin
                // Direct accumulator access for debug
                automatic int ai = regs[MXU_REG_ACC_IDX][3:0];
                reg_rd_data = sa_c_out[ai/TILE_DIM][ai%TILE_DIM];
            end else begin
                reg_rd_data = regs[reg_rd_addr];
            end
        end
    end

    // -----------------------------------------------------------------------
    // AXI-MailboxFabric bridge (same pattern as RTU)
    //   dest_id[3:0] = 0x0: register write (legacy 1-flit or 2-flit full 32-bit)
    //   dest_id[3:0] = 0x1: register read request
    //   dest_id[3:0] = 0x2: address latch for 2-flit write
    // -----------------------------------------------------------------------
    logic        mb_rd_pending;
    logic [4:0]  mb_rd_addr_latched;
    logic [NODE_ID_WIDTH-1:0] mb_reply_dest;
    logic        mb_tx_completion;
    logic        mb_completion_sent; // stays set until state leaves MXU_DONE
    logic [NODE_ID_WIDTH-1:0] mb_completion_dest;

    // 2-flit write: addr latch register
    logic        mb_wr_addr_valid;
    logic [4:0]  mb_wr_addr_latch;

    // Mailbox-injected register write
    logic        mb_wr_inject;
    logic [4:0]  mb_wr_inject_addr;
    logic [31:0] mb_wr_inject_data;

    assign mb_rx_ready = 1'b1;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            mb_rd_pending      <= 1'b0;
            mb_tx_completion   <= 1'b0;
            mb_completion_sent <= 1'b0;
            mb_completion_dest <= '0;
            mb_wr_addr_valid   <= 1'b0;
            mb_wr_addr_latch   <= '0;
        end else begin
            if (mb_rx_valid && mb_rx_ready) begin
                case (mb_rx_dest_id[3:0])
                    4'h0: begin // Write (clear addr latch after use)
                        if (mb_wr_addr_valid)
                            mb_wr_addr_valid <= 1'b0;
                    end
                    4'h1: begin // Read request
                        mb_rd_pending      <= 1'b1;
                        mb_rd_addr_latched <= mb_rx_data.payload[4:0];
                        mb_reply_dest      <= mb_rx_data.hdr.src_id;
                    end
                    4'h2: begin // Address latch for 2-flit write
                        mb_wr_addr_valid <= 1'b1;
                        mb_wr_addr_latch <= mb_rx_data.payload[4:0];
                    end
                    default: ;
                endcase
            end
            // Latch dest on mailbox-injected CTRL start
            if (mb_wr_inject && mb_wr_inject_addr == 5'(MXU_REG_CTRL) &&
                mb_wr_inject_data[0]) begin
                mb_completion_dest <= mb_rx_data.hdr.src_id;
            end
            if (mb_rd_pending && mb_tx_valid && mb_tx_ready)
                mb_rd_pending <= 1'b0;
            if (state == MXU_DONE && !mb_tx_completion && !mb_completion_sent)
                mb_tx_completion <= 1'b1;
            if (mb_tx_completion && mb_tx_valid && mb_tx_ready && !mb_rd_pending) begin
                mb_tx_completion <= 1'b0;
                mb_completion_sent <= 1'b1;
            end
            if (state != MXU_DONE)
                mb_completion_sent <= 1'b0;
        end
    end

    // Mailbox-injected register write (supports legacy 1-flit and 2-flit)
    always_comb begin
        mb_wr_inject      = 1'b0;
        mb_wr_inject_addr = '0;
        mb_wr_inject_data = '0;
        if (mb_rx_valid && mb_rx_ready && (mb_rx_dest_id[3:0] == 4'h0) && !reg_wr_valid) begin
            mb_wr_inject = 1'b1;
            if (mb_wr_addr_valid) begin
                mb_wr_inject_addr = mb_wr_addr_latch;
                mb_wr_inject_data = mb_rx_data.payload;
            end else begin
                mb_wr_inject_addr = mb_rx_data.payload[31:27];
                mb_wr_inject_data = {5'b0, mb_rx_data.payload[26:0]};
            end
        end
    end

    // TX arbitration
    always_comb begin
        mb_tx_valid   = 1'b0;
        mb_tx_data    = '0;
        mb_tx_dest_id = '0;
        if (mb_rd_pending) begin
            mb_tx_valid = 1'b1;
            mb_tx_data.hdr.src_id = MAILBOX_SRC_ID;
            mb_tx_data.hdr.opcode = OPC_DATA;
            mb_tx_data.hdr.prio   = 2'b01;
            mb_tx_data.hdr.eop    = 1'b1;
            mb_tx_data.hdr.debug  = 1'b0;
            mb_tx_data.payload    = (mb_rd_addr_latched < MXU_NUM_REGS) ?
                                    regs[mb_rd_addr_latched] : 32'hDEAD_BEEF;
            mb_tx_dest_id         = mb_reply_dest;
        end else if (mb_tx_completion) begin
            mb_tx_valid = 1'b1;
            mb_tx_data.hdr.src_id = MAILBOX_SRC_ID;
            mb_tx_data.hdr.opcode = OPC_IRQ;
            mb_tx_data.hdr.prio   = 2'b10;
            mb_tx_data.hdr.eop    = 1'b1;
            mb_tx_data.hdr.debug  = 1'b0;
            mb_tx_data.payload    = {perf_tiles[15:0], perf_ops[15:0]};
            mb_tx_dest_id         = mb_completion_dest;
        end
    end

endmodule : matrix_unit
