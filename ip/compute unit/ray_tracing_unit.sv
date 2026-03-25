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
    assign irq      = bvh_done && ctrl_irq_en;

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

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int i = 0; i < RTU_NUM_REGS; i++) regs[i] <= '0;
            rtu_state <= RTU_IDLE;
            bvh_start <= 1'b0;
        end else begin
            bvh_start <= 1'b0; // Default: one-cycle pulse

            // Register writes
            if (reg_wr_valid) begin
                case (reg_wr_addr)
                    RTU_REG_CTRL: begin
                        regs[RTU_REG_CTRL] <= reg_wr_data;
                        // Start pulse on write of bit[0]=1 while idle
                        if (reg_wr_data[0] && (rtu_state == RTU_IDLE)) begin
                            bvh_start <= 1'b1;
                            rtu_state <= RTU_RUNNING;
                        end
                        // Writing bit[0]=0 resets done state
                        if (!reg_wr_data[0] && (rtu_state == RTU_DONE)) begin
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
                            regs[reg_wr_addr] <= reg_wr_data;
                    end
                    default: ; // Other regs are read-only
                endcase
            end

            // State transitions
            case (rtu_state)
                RTU_RUNNING: begin
                    if (bvh_done) begin
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

endmodule : ray_tracing_unit
