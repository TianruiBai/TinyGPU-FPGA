`timescale 1ns/1ps
// ============================================================================
// Ray Tracing Unit (RTU) — Top-level integration
//
// Memory-mapped peripheral for hardware-accelerated ray-BVH intersection.
// CU/host writes ray parameters + BVH root address, starts traversal.
// RTU autonomously walks BVH tree via memory reads (through L2 cache),
// performs AABB and triangle intersection tests, returns closest hit.
//
// Integration: Connects as a bus-master to the cluster memory system
// (shares an L2 cache port) and is addressable via memory-mapped registers.
//
// Register map: See rt_pkg.sv (RTU_REG_* constants)
// ============================================================================
module ray_tracing_unit
  import rt_pkg::*;
  import mailbox_pkg::*;
#(
    parameter logic [NODE_ID_WIDTH-1:0] MAILBOX_SRC_ID = 16'h0180
)
(
    input  logic        clk,
    input  logic        rst_n,

    // Register access interface (from CU LSU or cluster bus)
    input  logic        reg_wr_valid,
    input  logic [4:0]  reg_wr_addr,  // word offset (0..31)
    input  logic [31:0] reg_wr_data,
    output logic        reg_wr_ready,

    input  logic        reg_rd_valid,
    input  logic [4:0]  reg_rd_addr,
    output logic [31:0] reg_rd_data,
    output logic        reg_rd_ready,

    // Interrupt output (active-high pulse on completion)
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

    // Memory read interface (to L2 cache port)
    output logic        mem_req_valid,
    output logic [31:0] mem_req_addr,
    output logic [7:0]  mem_req_size,
    output logic [3:0]  mem_req_qos,
    output logic [7:0]  mem_req_id,
    input  logic        mem_req_ready,

    input  logic        mem_resp_valid,
    input  logic [63:0] mem_resp_data,
    input  logic [7:0]  mem_resp_id,

    // Status visibility
    output logic        rtu_busy,
    output logic        rtu_done
);

    // -----------------------------------------------------------------------
    // Register file
    // -----------------------------------------------------------------------
    logic [31:0] regs [0:RTU_NUM_REGS-1];

    // Control bits decoded from CTRL register
    wire        ctrl_start   = regs[RTU_REG_CTRL][0];
    wire        ctrl_irq_en  = regs[RTU_REG_CTRL][3];

    // BVH engine wiring
    logic        bvh_start;
    logic        bvh_busy;
    logic        bvh_done;
    logic        bvh_irq;
    logic        bvh_result_hit;
    logic signed [31:0] bvh_result_t, bvh_result_u, bvh_result_v;
    logic [31:0] bvh_result_tri_id;
    logic [31:0] bvh_perf_nodes, bvh_perf_tris;

    // Memory interface from BVH engine
    logic        bvh_mem_req_valid;
    logic [31:0] bvh_mem_req_addr;
    logic [7:0]  bvh_mem_req_size;
    logic [7:0]  bvh_mem_req_id;
    logic        bvh_mem_req_ready;

    // FSM
    typedef enum logic [1:0] {
        RTU_IDLE    = 2'd0,
        RTU_RUNNING = 2'd1,
        RTU_DONE    = 2'd2
    } rtu_state_e;

    rtu_state_e rtu_state;

    assign rtu_busy = (rtu_state == RTU_RUNNING);
    assign rtu_done = (rtu_state == RTU_DONE);
    assign irq      = (rtu_state == RTU_DONE) && ctrl_irq_en;

    // -----------------------------------------------------------------------
    // BVH Traversal Engine instance
    // -----------------------------------------------------------------------
    bvh_traversal_engine u_bvh (
        .clk(clk), .rst_n(rst_n),
        .start(bvh_start),
        .busy(bvh_busy),
        .done(bvh_done),
        .irq(),
        .ray_ox($signed(regs[RTU_REG_RAY_OX])),
        .ray_oy($signed(regs[RTU_REG_RAY_OY])),
        .ray_oz($signed(regs[RTU_REG_RAY_OZ])),
        .ray_dx($signed(regs[RTU_REG_RAY_DX])),
        .ray_dy($signed(regs[RTU_REG_RAY_DY])),
        .ray_dz($signed(regs[RTU_REG_RAY_DZ])),
        .inv_dx($signed(regs[RTU_REG_INV_DX])),
        .inv_dy($signed(regs[RTU_REG_INV_DY])),
        .inv_dz($signed(regs[RTU_REG_INV_DZ])),
        .ray_tmin($signed(regs[RTU_REG_RAY_TMIN])),
        .ray_tmax_in($signed(regs[RTU_REG_RAY_TMAX])),
        .bvh_root_addr(regs[RTU_REG_BVH_ROOT]),
        .result_hit(bvh_result_hit),
        .result_t(bvh_result_t),
        .result_u(bvh_result_u),
        .result_v(bvh_result_v),
        .result_tri_id(bvh_result_tri_id),
        .perf_nodes_visited(bvh_perf_nodes),
        .perf_tris_tested(bvh_perf_tris),
        .mem_req_valid(bvh_mem_req_valid),
        .mem_req_addr(bvh_mem_req_addr),
        .mem_req_size(bvh_mem_req_size),
        .mem_req_id(bvh_mem_req_id),
        .mem_req_ready(bvh_mem_req_ready),
        .mem_resp_valid(mem_resp_valid),
        .mem_resp_data(mem_resp_data),
        .mem_resp_id(mem_resp_id)
    );

    // Forward BVH engine memory requests to output
    assign mem_req_valid     = bvh_mem_req_valid;
    assign mem_req_addr      = bvh_mem_req_addr;
    assign mem_req_size      = bvh_mem_req_size;
    assign mem_req_qos       = 4'h2; // Medium priority for RT reads
    assign mem_req_id        = bvh_mem_req_id;
    assign bvh_mem_req_ready = mem_req_ready;

    // -----------------------------------------------------------------------
    // Register write logic
    // -----------------------------------------------------------------------
    assign reg_wr_ready = 1'b1; // Always accept writes (fire-and-forget)

    // Start guard: bvh_done stays high 1 cycle after bvh_start because
    // the BVH engine transitions out of ST_DONE on the next edge.
    logic bvh_start_d;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int i = 0; i < RTU_NUM_REGS; i++) regs[i] <= '0;
            rtu_state   <= RTU_IDLE;
            bvh_start   <= 1'b0;
            bvh_start_d <= 1'b0;
        end else begin
            bvh_start   <= 1'b0; // Default: one-cycle pulse
            bvh_start_d <= bvh_start;

            // Register writes (direct port has priority; mailbox injects when idle)
            if (reg_wr_valid || mb_wr_inject) begin
                logic [4:0]  wr_addr_mux;
                logic [31:0] wr_data_mux;
                wr_addr_mux = reg_wr_valid ? reg_wr_addr : mb_wr_inject_addr;
                wr_data_mux = reg_wr_valid ? reg_wr_data : mb_wr_inject_data;
                case (wr_addr_mux)
                    RTU_REG_CTRL: begin
                        regs[RTU_REG_CTRL] <= wr_data_mux;
                        // Start pulse on write of bit[0]=1 while idle
                        if (wr_data_mux[0] && (rtu_state == RTU_IDLE)) begin
                            bvh_start <= 1'b1;
                            rtu_state <= RTU_RUNNING;
                        end
                        // Writing bit[0]=0 resets done state
                        if (!wr_data_mux[0] && (rtu_state == RTU_DONE)) begin
                            rtu_state <= RTU_IDLE;
                        end
                    end
                    // Ray parameter registers (writable only when idle/done)
                    RTU_REG_RAY_OX,  RTU_REG_RAY_OY,  RTU_REG_RAY_OZ,
                    RTU_REG_RAY_DX,  RTU_REG_RAY_DY,  RTU_REG_RAY_DZ,
                    RTU_REG_RAY_TMIN, RTU_REG_RAY_TMAX,
                    RTU_REG_BVH_ROOT,
                    RTU_REG_INV_DX, RTU_REG_INV_DY, RTU_REG_INV_DZ: begin
                        if (rtu_state != RTU_RUNNING)
                            regs[wr_addr_mux] <= wr_data_mux;
                    end
                    default: ; // Other regs are read-only
                endcase
            end

            // State transitions
            case (rtu_state)
                RTU_RUNNING: begin
                    // Guard: don't check done for 2 cycles after start
                    // (BVH engine transitions out of ST_DONE 1 cycle after start)
                    if (bvh_done && !bvh_start && !bvh_start_d) begin
                        // Latch results into register file
                        regs[RTU_REG_HIT_FLAG]   <= {31'b0, bvh_result_hit};
                        regs[RTU_REG_HIT_T]      <= bvh_result_t;
                        regs[RTU_REG_HIT_U]      <= bvh_result_u;
                        regs[RTU_REG_HIT_V]      <= bvh_result_v;
                        regs[RTU_REG_HIT_TRI_ID] <= bvh_result_tri_id;
                        regs[RTU_REG_PERF_NODES] <= bvh_perf_nodes;
                        regs[RTU_REG_PERF_TESTS] <= bvh_perf_tris;
                        regs[RTU_REG_PERF_RAYS]  <= regs[RTU_REG_PERF_RAYS] + 1;
                        regs[RTU_REG_CTRL][1]    <= 1'b0; // Clear busy
                        regs[RTU_REG_CTRL][2]    <= 1'b1; // Set done
                        rtu_state <= RTU_DONE;
                    end
                end
                default: ;
            endcase

            // Update status register continuously
            regs[RTU_REG_STATUS] <= {bvh_perf_nodes[15:0], 8'(rtu_state), 8'b0};
        end
    end

    // -----------------------------------------------------------------------
    // Register read logic
    // -----------------------------------------------------------------------
    assign reg_rd_ready = 1'b1;

    always_comb begin
        reg_rd_data = '0;
        if (reg_rd_valid && (reg_rd_addr < RTU_NUM_REGS))
            reg_rd_data = regs[reg_rd_addr];
    end

    // -----------------------------------------------------------------------
    // AXI-MailboxFabric bridge
    //
    // Incoming flits (OPC_DATA): payload is {addr[4:0], data[26:0]} for writes
    //   addr encoded in payload[31:27], data in payload[26:0] (lower 27 bits).
    //   Full 32-bit data writes: use two-flit sequence (low word then high word).
    //   Simplified: CSR index 0 of dest_id[3:0] = data write to reg addr,
    //               dest_id[3:0] encodes operation:
    //                 0x0 = register write (payload = {5'b reg_addr, 27'b data})
    //                 0x1 = register read request (payload[4:0] = reg_addr)
    //
    // Outgoing flits: completion notification or read response to src_id.
    //
    // Protocol: fire-and-forget writes; for reads, RTU sends OPC_DATA
    // response flit to the requesting CU's src_id.
    // -----------------------------------------------------------------------
    // Mailbox RX: decode incoming flits → register writes or read requests
    //   dest_id[3:0] = 0x0: register write (1-flit legacy: {5'b addr, 27'b data}
    //                        OR 2-flit full 32-bit if addr was latched via 0x2)
    //   dest_id[3:0] = 0x1: register read request (payload[4:0] = reg addr)
    //   dest_id[3:0] = 0x2: address latch for 2-flit write (payload[4:0] = reg addr)
    logic        mb_rd_pending;
    logic [4:0]  mb_rd_addr_latched;
    logic [NODE_ID_WIDTH-1:0] mb_reply_dest;

    // 2-flit write: addr latch register
    logic        mb_wr_addr_valid;
    logic [4:0]  mb_wr_addr_latch;

    assign mb_rx_ready = 1'b1; // Always accept (fire-and-forget)

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            mb_rd_pending    <= 1'b0;
            mb_wr_addr_valid <= 1'b0;
            mb_wr_addr_latch <= '0;
        end else begin
            // Process incoming mailbox flit
            if (mb_rx_valid && mb_rx_ready) begin
                case (mb_rx_dest_id[3:0])
                    4'h0: begin // Register write (handled below via mb_wr_inject)
                        // Clear addr latch after use
                        if (mb_wr_addr_valid)
                            mb_wr_addr_valid <= 1'b0;
                    end
                    4'h1: begin // Register read request
                        mb_rd_pending     <= 1'b1;
                        mb_rd_addr_latched <= mb_rx_data.payload[4:0];
                        mb_reply_dest     <= mb_rx_data.hdr.src_id;
                    end
                    4'h2: begin // Address latch for 2-flit write
                        mb_wr_addr_valid <= 1'b1;
                        mb_wr_addr_latch <= mb_rx_data.payload[4:0];
                    end
                    default: ;
                endcase
            end

            // Clear read pending after response sent
            if (mb_rd_pending && mb_tx_valid && mb_tx_ready)
                mb_rd_pending <= 1'b0;
        end
    end

    // Mailbox-injected register write (only when direct reg_wr is idle)
    // Supports both legacy 1-flit and new 2-flit full 32-bit protocol
    logic        mb_wr_inject;
    logic [4:0]  mb_wr_inject_addr;
    logic [31:0] mb_wr_inject_data;

    always_comb begin
        mb_wr_inject      = 1'b0;
        mb_wr_inject_addr = '0;
        mb_wr_inject_data = '0;
        if (mb_rx_valid && mb_rx_ready && (mb_rx_dest_id[3:0] == 4'h0) && !reg_wr_valid) begin
            mb_wr_inject = 1'b1;
            if (mb_wr_addr_valid) begin
                // 2-flit mode: addr from latch, data is full 32-bit payload
                mb_wr_inject_addr = mb_wr_addr_latch;
                mb_wr_inject_data = mb_rx_data.payload;
            end else begin
                // Legacy 1-flit mode: {5'b addr, 27'b data}
                mb_wr_inject_addr = mb_rx_data.payload[31:27];
                mb_wr_inject_data = {5'b0, mb_rx_data.payload[26:0]};
            end
        end
    end

    // Mailbox TX: send read response or completion notification
    logic                     mb_tx_completion;
    logic                     mb_completion_sent;
    logic [NODE_ID_WIDTH-1:0] mb_completion_dest;

    // Latch requester's src_id on start for completion notification
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            mb_tx_completion  <= 1'b0;
            mb_completion_sent <= 1'b0;
            mb_completion_dest <= '0;
        end else begin
            // Latch dest on mailbox-injected CTRL start (both 1-flit and 2-flit)
            if (mb_wr_inject && mb_wr_inject_addr == 5'(RTU_REG_CTRL) &&
                mb_wr_inject_data[0]) begin
                mb_completion_dest <= mb_rx_data.hdr.src_id;
            end
            // Send completion when transitioning to DONE (once only)
            if (rtu_state == RTU_RUNNING && bvh_done && !bvh_start && !mb_completion_sent)
                mb_tx_completion <= 1'b1;
            if (mb_tx_completion && mb_tx_valid && mb_tx_ready) begin
                mb_tx_completion <= 1'b0;
                mb_completion_sent <= 1'b1;
            end
            // Reset sent flag when returning to idle (ready for next invocation)
            if (rtu_state == RTU_IDLE)
                mb_completion_sent <= 1'b0;
        end
    end

    // TX arbitration: read response has priority over completion
    always_comb begin
        mb_tx_valid   = 1'b0;
        mb_tx_data    = '0;
        mb_tx_dest_id = '0;
        if (mb_rd_pending) begin
            // Read response
            mb_tx_valid = 1'b1;
            mb_tx_data.hdr.src_id = MAILBOX_SRC_ID;
            mb_tx_data.hdr.opcode = OPC_DATA;
            mb_tx_data.hdr.prio   = 2'b01;
            mb_tx_data.hdr.eop    = 1'b1;
            mb_tx_data.hdr.debug  = 1'b0;
            mb_tx_data.payload    = (mb_rd_addr_latched < RTU_NUM_REGS) ?
                                    regs[mb_rd_addr_latched] : 32'hDEAD_BEEF;
            mb_tx_dest_id         = mb_reply_dest;
        end else if (mb_tx_completion) begin
            // Completion notification
            mb_tx_valid = 1'b1;
            mb_tx_data.hdr.src_id = MAILBOX_SRC_ID;
            mb_tx_data.hdr.opcode = OPC_IRQ;
            mb_tx_data.hdr.prio   = 2'b10;
            mb_tx_data.hdr.eop    = 1'b1;
            mb_tx_data.hdr.debug  = 1'b0;
            mb_tx_data.payload    = {regs[RTU_REG_HIT_FLAG][0], 15'b0,
                                     regs[RTU_REG_HIT_TRI_ID][15:0]};
            mb_tx_dest_id         = mb_completion_dest;
        end
    end

endmodule : ray_tracing_unit
