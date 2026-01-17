`include "isa_pkg.sv"
module compute_unit_top #(
    parameter logic [31:0] CORE_ID     = 32'h0,
    parameter logic [31:0] TILE_OFFSET = 32'h0
)(
    input  logic        clk,
    input  logic        rst_n,
    // Instruction memory interface
    input  logic [63:0] inst_rdata,
    output logic [31:0] inst_addr,
    // Data memory interface (32-bit width)
    output logic        data_req_valid,
    output logic        data_req_is_load,
    output logic [31:0] data_req_addr,
    output logic [31:0] data_req_wdata,
    output logic [4:0]  data_req_rd,

    // Error reporting from FP/Vector units
    output logic        err_fp_overflow,
    output logic        err_fp_invalid,
    output logic        err_vec_overflow,
    output logic        err_vec_invalid,

    // CSR sideband outputs (optional external visibility)
    output logic [31:0] csr_status,
    output logic [31:0] csr_fstatus,
    output logic [31:0] csr_vstatus,

    input  logic        data_req_ready,
    input  logic        data_resp_valid,
    input  logic [4:0]  data_resp_rd,
    input  logic [31:0] data_resp_data
);
    import isa_pkg::*;

    // IF stage
    logic [31:0] if_pc;
    logic [31:0] if_inst0;
    logic [31:0] if_inst1;
    logic        if_valid;
    logic        if_inst0_valid;
    logic        if_inst1_valid;

    logic [2:0]  pc_advance_bytes;

    // Decode (combinational)
    decode_ctrl_t d0_ctrl;
    decode_ctrl_t d1_ctrl;

    // RR stage
    decode_ctrl_t rr_ctrl;
    logic         rr_valid;

    // RR lane 1 (dual issue: vector-ALU or gfx only)
    decode_ctrl_t rr1_ctrl;
    logic         rr1_valid;

    logic [4:0]   rr1_scalar_raddr;

    logic rr_is_vec_alu;
    logic rr_is_gfx;
    logic rr1_is_vec_alu;
    logic rr1_is_gfx;

    // Vector issue queue (decoupled from scalar/fp/lsu pipe)
    typedef struct packed {
        decode_ctrl_t ctrl;
        logic [127:0] src_a;
        logic [127:0] src_b;
        logic [31:0]  scalar_mask;
    } vector_issue_t;

    localparam int VQ_DEPTH = 2;
    vector_issue_t vq [VQ_DEPTH];
    logic [VQ_DEPTH-1:0] vq_valid;
    logic [$clog2(VQ_DEPTH)-1:0] vq_head;
    logic [$clog2(VQ_DEPTH)-1:0] vq_tail;
    logic [$clog2(VQ_DEPTH+1)-1:0] vq_count;

    // Graphics pipeline instance
    logic        gfx_queue_full;
    logic [3:0]  gfx_queue_count;

    // Texture interface wiring to cache
    logic        tex_gp_req_valid;
    logic [31:0] tex_gp_req_addr;
    logic [4:0]  tex_gp_req_rd;
    logic        tex_gp_req_ready;
    logic        tex_gp_resp_valid;
    logic [31:0] tex_gp_resp_data;
    logic [4:0]  tex_gp_resp_rd;

    // Texture pipeline signals (shared between graphics_pipeline and texture_cache)
    logic        tex_req_valid;
    logic [31:0] tex_req_addr;
    logic [4:0]  tex_req_rd;
    logic        tex_req_ready;
    logic        tex_resp_valid;
    logic [31:0] tex_resp_data;
    logic [4:0]  tex_resp_rd;

    // Branch redirect flag (used early for flush)
    logic        ex_branch_taken;

    // Graphics Pipeline Writeback wiring
    logic        gp_wb_valid;
    logic [4:0]  gp_wb_rd;
    logic [127:0] gp_wb_data;
    logic        gp_wb_is_scalar;

    // Graphics/ROP store interface (into LSU)
    logic        gp_st_valid;
    logic [31:0] gp_st_addr;
    logic [31:0] gp_st_wdata;
    logic [3:0]  gp_st_wstrb;
    logic        gp_st_ready;

    // Mux issue source for graphics pipeline (rr lane0 or rr lane1)
    decode_ctrl_t gp_issue_ctrl;
    logic [31:0]  gp_issue_op_a;
    logic [31:0]  gp_issue_op_b;
    logic [127:0] gp_issue_vec_a;
    logic [127:0] gp_issue_vec_b;
    logic         gp_issue_valid;

    graphics_pipeline u_graphics_pipeline (
        .clk(clk),
        .rst_n(rst_n),
        .flush_all(ex_branch_taken),
        // Issue
        .issue_valid(gp_issue_valid),
        .issue_ctrl(gp_issue_ctrl),
        .issue_op_a(gp_issue_op_a),
        .issue_op_b(gp_issue_op_b),
        .issue_vec_a(gp_issue_vec_a),
        .issue_vec_b(gp_issue_vec_b),
        .queue_full(gfx_queue_full),
        .queue_count(gfx_queue_count),
        // Writeback
        .wb_valid(gp_wb_valid),
        .wb_rd(gp_wb_rd),
        .wb_data(gp_wb_data),
        .wb_is_scalar(gp_wb_is_scalar),
        // Texture Cache
        .tex_req_valid(tex_gp_req_valid),
        .tex_req_addr(tex_gp_req_addr),
        .tex_req_rd(tex_gp_req_rd),
        .tex_req_ready(tex_gp_req_ready),
        .tex_resp_valid(tex_gp_resp_valid),
        .tex_resp_data(tex_gp_resp_data),
        .tex_resp_rd(tex_gp_resp_rd),

        .gfx_st_valid(gp_st_valid),
        .gfx_st_addr(gp_st_addr),
        .gfx_st_wdata(gp_st_wdata),
        .gfx_st_wstrb(gp_st_wstrb),
        .gfx_st_ready(gp_st_ready)
    );

    // (gfx issue select + tex wiring moved below, after stall_pipe is defined)

    // EX stage
    decode_ctrl_t ex_ctrl;
    logic         ex_valid;
    logic [31:0]  ex_op_a;
    logic [31:0]  ex_op_b;
    logic [31:0]  ex_mask_scalar;
    logic [15:0]  ex_fp_a;
    logic [15:0]  ex_fp_b;
    logic [31:0]  ex_fp_scalar;
    logic [127:0] ex_vec_a;
    logic [127:0] ex_vec_b;
    logic [31:0]  ex_scalar_res;
    logic [15:0]  ex_fp_res;
    logic [31:0]  ex_addr;

    // MEM stage
    decode_ctrl_t mem_ctrl;
    logic         mem_valid;
    logic [31:0]  mem_scalar_res;
    logic [15:0]  mem_fp_res;
    logic [31:0]  mem_addr;
    logic [127:0] mem_vec_wdata;
    logic [31:0]  mem_scalar_wdata;

    // WB stage
    decode_ctrl_t wb_ctrl;
    logic         wb_valid;
    logic [31:0]  wb_scalar_res;
    logic [15:0]  wb_fp_res;

    // Scoreboard / pipeline stall
    logic stall_scoreboard;
    logic stall_sb0, stall_sb1;
    logic issue0_valid, issue1_valid;
    logic accept0, accept1;
    logic lsu_stall;
    logic lsu_busy;
    logic stall_membar;
    logic stall_any;

    // Register files
    logic [31:0]  s_rdata_a, s_rdata_b, s_rdata_c;
    logic [31:0]  s_wdata;
    logic         s_we;
    logic [4:0]   s_waddr;

    logic [15:0]  f_rdata_a, f_rdata_b;
    logic [15:0]  f_wdata;
    logic         f_we;
    logic [4:0]   f_waddr;

    logic [127:0] v_rdata_a, v_rdata_b;
    logic [127:0] v_wdata;
    logic         v_we;
    logic [4:0]   v_waddr;

    // Vector ALU
    logic         valuv_ready;
    logic         valuv_wb_valid;
    logic [4:0]   valuv_wb_rd;
    logic [127:0] valuv_wb_data;
    logic         valuv_wb_is_scalar;
    logic         valuv_err_overflow;
    logic         valuv_err_invalid;

    // FP ALU
    logic         fp_wb_valid;
    logic [4:0]   fp_wb_rd;
    logic [15:0]  fp_alu_wb_data;
    logic         fp_scalar_wb_valid;
    logic [4:0]   fp_scalar_wb_rd;
    logic [31:0]  fp_scalar_wb_data;
    logic         fp_wb_err_overflow;
    logic         fp_wb_err_invalid;

    // LSU wiring to external data interface and local memory
    logic         lsu_wb_valid;
    logic         lsu_wb_is_vector;
    logic [4:0]   lsu_wb_rd;
    logic [127:0] lsu_wb_data;

    // Writeback arbitration helpers
    logic         pending_scalar_wb;
    logic [4:0]   pending_scalar_rd;
    logic [31:0]  pending_scalar_data;
    logic         pending_vector_wb;
    logic [4:0]   pending_vector_rd;
    logic [127:0] pending_vector_data;

    logic         local_req_valid;
    logic         local_we;
    logic         local_req_is_vector;
    logic [31:0]  local_addr;
    logic [127:0] local_wdata;
    logic [127:0] local_rdata;
    logic [1:0]   local_bank_sel;

    // CSR wiring
    logic         csr_en;
    logic         csr_csrrs;
    logic [11:0]  csr_addr_ex;
    logic [31:0]  csr_wdata_ex;
    logic [31:0]  csr_rdata;
    logic [15:0]  csr_vmask;

    // Graphics/texture pipe stall is handled via queue backpressure, not global stall

    // MEMBAR waits for LSU/texture traffic to drain before allowing forward progress.
    // Important: only begin flushing/serializing once MEMBAR reaches the MEM stage.
    // If we flush earlier (RR/EX), we can block an older in-flight store from
    // enqueuing into the write-merge buffer in the same cycle.
    wire rr_is_membar  = rr_valid  && rr_ctrl.is_system && (rr_ctrl.funct3 == 3'b000);
    wire ex_is_membar  = ex_valid  && ex_ctrl.is_system && (ex_ctrl.funct3 == 3'b000);
    wire mem_is_membar = mem_valid && mem_ctrl.is_system && (mem_ctrl.funct3 == 3'b000);

    assign stall_membar      = mem_is_membar && lsu_busy;
    wire vector_queue_full   = (vq_count == VQ_DEPTH);
    wire stall_pipe          = lsu_stall || stall_membar;
    // Frontend stalls when it cannot accept slot0.
    // During reset force stall low to avoid X-propagation into fetch/PC
    assign stall_any         = rst_n ? stall_pipe : 1'b0;
    wire stall_issue         = stall_any || (if_valid && !accept0);

    // ---------------------------------------------------------------------
    // Graphics issue select + texture wiring (must appear after stall_pipe is defined)
    // ---------------------------------------------------------------------
    always_comb begin
        gp_issue_valid = 1'b0;
        gp_issue_ctrl  = '0;
        gp_issue_op_a  = 32'h0;
        gp_issue_op_b  = 32'h0;
        gp_issue_vec_a = 128'h0;
        gp_issue_vec_b = 128'h0;

        // Lane0 gfx
        if (rr_is_gfx && !stall_pipe && !gfx_queue_full) begin
            gp_issue_valid = 1'b1;
            gp_issue_ctrl  = rr_ctrl;
            gp_issue_op_a  = s_rdata_a;
            gp_issue_op_b  = s_rdata_b;
            gp_issue_vec_a = v_rdata_a;
            gp_issue_vec_b = v_rdata_b;
        end

        // Lane1 gfx (dual-issue)
        if (!gp_issue_valid && rr1_is_gfx && !stall_pipe && !gfx_queue_full) begin
            gp_issue_valid = 1'b1;
            gp_issue_ctrl  = rr1_ctrl;
            gp_issue_vec_a = v_rdata_a;
            gp_issue_vec_b = v_rdata_b;
            // Only one scalar operand is supported on lane1: route it to the operand the op needs.
            gp_issue_op_a  = (rr1_ctrl.rs1_class == CLASS_SCALAR) ? s_rdata_c : 32'h0;
            gp_issue_op_b  = (rr1_ctrl.rs2_class == CLASS_SCALAR) ? s_rdata_c : 32'h0;
        end
    end

    // Wire Texture Cache to Graphics Pipeline (descriptor/texture reads share this path)
    assign tex_gp_req_ready  = tex_req_ready;
    assign tex_req_valid     = tex_gp_req_valid;
    assign tex_req_addr      = tex_gp_req_addr;
    assign tex_req_rd        = tex_gp_req_rd;

    assign tex_gp_resp_valid = tex_resp_valid;
    assign tex_gp_resp_data  = tex_resp_data;
    assign tex_gp_resp_rd    = tex_resp_rd;

    always_comb begin
        stall_scoreboard = stall_sb0 || stall_sb1;
    end

    // Fetch unit
    fetch_unit u_fetch (
        .clk(clk),
        .rst_n(rst_n),
        .stall(stall_issue),
        .branch_taken(ex_branch_taken),
        .branch_target(ex_scalar_res),
        .pc_advance_bytes(pc_advance_bytes),
        .pc(if_pc),
        .inst_addr(inst_addr),
        .inst_rdata(inst_rdata),
        .inst_valid(if_valid),
        .inst0_valid(if_inst0_valid),
        .inst1_valid(if_inst1_valid),
        .inst0(if_inst0),
        .inst1(if_inst1)
    );

    // Dual decode (combinational)
    decoder u_decoder0 (
        .inst(if_inst0),
        .ctrl(d0_ctrl)
    );

    decoder u_decoder1 (
        .inst(if_inst1),
        .ctrl(d1_ctrl)
    );

    scoreboard u_scoreboard (
        .clk(clk),
        .rst_n(rst_n),
        .issue0_valid(issue0_valid),
        .issue0_rs1_valid(d0_ctrl.uses_rs1),
        .issue0_rs2_valid(d0_ctrl.uses_rs2),
        .issue0_rs1_class(d0_ctrl.rs1_class),
        .issue0_rs2_class(d0_ctrl.rs2_class),
        .issue0_rs1(d0_ctrl.rs1),
        .issue0_rs2(d0_ctrl.rs2),
        .issue0_rd_valid(d0_ctrl.uses_rd),
        .issue0_rd_class(d0_ctrl.rd_class),
        .issue0_rd(d0_ctrl.rd),
        .stall0(stall_sb0),

        .issue1_valid(issue1_valid),
        .issue1_rs1_valid(d1_ctrl.uses_rs1),
        .issue1_rs2_valid(d1_ctrl.uses_rs2),
        .issue1_rs1_class(d1_ctrl.rs1_class),
        .issue1_rs2_class(d1_ctrl.rs2_class),
        .issue1_rs1(d1_ctrl.rs1),
        .issue1_rs2(d1_ctrl.rs2),
        .issue1_rd_valid(d1_ctrl.uses_rd),
        .issue1_rd_class(d1_ctrl.rd_class),
        .issue1_rd(d1_ctrl.rd),
        .stall1(stall_sb1),

        .flush_rr(ex_branch_taken),
        .flush_rr_rd_class(rr_ctrl.rd_class),
        .flush_rr_rd_valid(rr_ctrl.uses_rd),
        .flush_rr_rd(rr_ctrl.rd),
        .wb_scalar_valid(s_we),
        .wb_scalar_rd(s_waddr),
        .wb_fp_valid(f_we),
        .wb_fp_rd(f_waddr),
        .wb_vec_valid(v_we),
        .wb_vec_rd(v_waddr),
        .flush_all(ex_branch_taken)
    );

    // Issue classification (slot0/slot1)
    wire d0_is_vec_alu = if_inst0_valid && d0_ctrl.is_vector && !d0_ctrl.is_load && !d0_ctrl.is_store && !d0_ctrl.is_tex && !d0_ctrl.is_atomic;
    wire d0_is_gfx     = if_inst0_valid && (d0_ctrl.is_tex || d0_ctrl.is_gfx);
    wire d0_is_scalar_pipe = if_inst0_valid && !d0_is_vec_alu && !d0_is_gfx;

    wire d1_is_vec_alu = if_inst1_valid && d1_ctrl.is_vector && !d1_ctrl.is_load && !d1_ctrl.is_store && !d1_ctrl.is_tex && !d1_ctrl.is_atomic;
    wire d1_is_gfx     = if_inst1_valid && (d1_ctrl.is_tex || d1_ctrl.is_gfx);

    wire can_dual = if_inst0_valid && if_inst1_valid && d0_is_scalar_pipe && (d1_is_vec_alu || d1_is_gfx);

    assign issue0_valid = if_valid && if_inst0_valid && !stall_pipe && !ex_branch_taken
                      && !(d0_is_vec_alu && vector_queue_full)
                      && !(d0_is_gfx && gfx_queue_full);

    assign issue1_valid = if_valid && can_dual && !stall_pipe && !ex_branch_taken
                      && ((d1_is_vec_alu && !vector_queue_full) || (d1_is_gfx && !gfx_queue_full));

    always_comb begin
        accept0 = issue0_valid && !stall_sb0;
        accept1 = issue1_valid && !stall_sb1 && accept0;
    end

    // PC step is driven by accepted slots
    always_comb begin
        if (!accept0) pc_advance_bytes = 3'd0;
        else if (accept1) pc_advance_bytes = 3'd8;
        else pc_advance_bytes = 3'd4;
    end

    // RR stage registers
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            rr_valid <= 1'b0;
            rr_ctrl  <= '0;
            rr1_valid <= 1'b0;
            rr1_ctrl  <= '0;
        end else if (ex_branch_taken) begin
             rr_valid <= 1'b0; // Flush on branch taken
             rr1_valid <= 1'b0;
        end else if (!stall_pipe) begin
            rr_valid  <= accept0;
            rr_ctrl   <= d0_ctrl;
            rr1_valid <= accept1;
            rr1_ctrl  <= d1_ctrl;
        end
    end

    // Regfile instances
    regfile_scalar u_regfile_scalar (
        .clk(clk),
        .rst_n(rst_n),
        .raddr_a(rr_ctrl.rs1),
        .raddr_b(rr_ctrl.rs2),
        .raddr_c(rr1_scalar_raddr),
        .rdata_a(s_rdata_a),
        .rdata_b(s_rdata_b),
        .rdata_c(s_rdata_c),
        .we(s_we),
        .waddr(s_waddr),
        .wdata(s_wdata)
    );

    regfile_fp u_regfile_fp (
        .clk(clk),
        .rst_n(rst_n),
        .raddr_a(rr_ctrl.rs1),
        .raddr_b(rr_ctrl.rs2),
        .rdata_a(f_rdata_a),
        .rdata_b(f_rdata_b),
        .we(f_we),
        .waddr(f_waddr),
        .wdata(f_wdata)
    );

    // Vector operands are only needed for rr (vector/gfx) and rr1 (dual-issued vector/gfx).
    wire vrf_use_rr1 = rr1_valid && (rr1_is_vec_alu || rr1_is_gfx);
    wire [4:0] vrf_raddr_a = vrf_use_rr1 ? rr1_ctrl.rs1 : rr_ctrl.rs1;
    wire [4:0] vrf_raddr_b = vrf_use_rr1 ? rr1_ctrl.rs2 : rr_ctrl.rs2;

    regfile_vector u_regfile_vector (
        .clk(clk),
        .rst_n(rst_n),
        .raddr_a(vrf_raddr_a),
        .raddr_b(vrf_raddr_b),
        .rdata_a(v_rdata_a),
        .rdata_b(v_rdata_b),
        .we(v_we),
        .waddr(v_waddr),
        .wdata(v_wdata)
    );

    always_comb begin
        rr_is_vec_alu  = rr_valid  && rr_ctrl.is_vector  && !rr_ctrl.is_load  && !rr_ctrl.is_store  && !rr_ctrl.is_tex && !rr_ctrl.is_atomic;
        rr_is_gfx      = rr_valid  && (rr_ctrl.is_tex || rr_ctrl.is_gfx);
        rr1_is_vec_alu = rr1_valid && rr1_ctrl.is_vector && !rr1_ctrl.is_load && !rr1_ctrl.is_store && !rr1_ctrl.is_tex && !rr1_ctrl.is_atomic;
        rr1_is_gfx     = rr1_valid && (rr1_ctrl.is_tex || rr1_ctrl.is_gfx);
    end

    // Slot1 scalar operand capture: only one scalar operand is supported in the dual-issue lane.
    // Prefer rs2 if it is scalar (e.g., TEX sampler handle). Otherwise use rs1 if it is scalar.
    always_comb begin
        if (rr1_valid && rr1_ctrl.uses_rs2 && (rr1_ctrl.rs2_class == CLASS_SCALAR)) rr1_scalar_raddr = rr1_ctrl.rs2;
        else if (rr1_valid && rr1_ctrl.uses_rs1 && (rr1_ctrl.rs1_class == CLASS_SCALAR)) rr1_scalar_raddr = rr1_ctrl.rs1;
        else rr1_scalar_raddr = 5'd0;
    end

    // EX stage registers (scalar/fp/lsu path only)
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            ex_valid       <= 1'b0;
            ex_ctrl        <= '0;
            ex_op_a        <= 32'h0;
            ex_op_b        <= 32'h0;
            ex_fp_scalar   <= 32'h0;
            ex_fp_a        <= 16'h0;
            ex_fp_b        <= 16'h0;
            ex_vec_a       <= '0;
            ex_vec_b       <= '0;
        end else if (!stall_pipe) begin
            ex_valid        <= rr_valid && !rr_is_vec_alu && !rr_is_gfx;
            ex_ctrl         <= (rr_is_vec_alu || rr_is_gfx) ? '0 : rr_ctrl;
            ex_op_a         <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : s_rdata_a;
            ex_op_b         <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : s_rdata_b;
            ex_fp_scalar    <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : s_rdata_b;
            ex_mask_scalar  <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : s_rdata_b;
            ex_fp_a         <= (rr_is_vec_alu || rr_is_gfx) ? 16'h0 : f_rdata_a;
            ex_fp_b         <= (rr_is_vec_alu || rr_is_gfx) ? 16'h0 : f_rdata_b;
            ex_vec_a        <= (rr_is_vec_alu || rr_is_gfx) ? '0 : v_rdata_a;
            ex_vec_b        <= (rr_is_vec_alu || rr_is_gfx) ? '0 : v_rdata_b;
        end
    end

    // CSR access control (evaluated in EX stage)
    assign csr_addr_ex  = ex_ctrl.imm[11:0];
    assign csr_wdata_ex = ex_op_a; // rs1 value
    assign csr_en       = ex_valid && ex_ctrl.is_system && (ex_ctrl.funct3 == 3'b001 || ex_ctrl.funct3 == 3'b010);
    assign csr_csrrs    = (ex_ctrl.funct3 == 3'b010);

    // Address generation for loads/stores/texture
    agu u_agu (
        .base_addr(ex_op_a),
        .offset(ex_ctrl.imm),
        .effective_addr(ex_addr)
    );

    alu_scalar u_alu_scalar (
        .op_a(ex_op_a),
        .op_b((ex_ctrl.is_load || ex_ctrl.is_store || ex_ctrl.is_branch || !ex_ctrl.uses_rs2) ? ex_ctrl.imm : ex_op_b),
        .funct3(ex_ctrl.funct3),
        .is_sub(ex_ctrl.funct7 == 7'b0100000),
        .opcode(ex_ctrl.is_branch ? OP_BRANCH : OP_INT),
        .result(ex_scalar_res),
        .branch_taken(ex_branch_taken)
    );

    fp_alu u_fp_alu (
        .clk(clk),
        .rst_n(rst_n),
        .valid(ex_valid && ex_ctrl.is_scalar_fp && !stall_any),
        .funct3(ex_ctrl.funct3),
        .src_a(ex_fp_a),
        .src_b(ex_fp_b),
        .scalar_src(ex_op_a),
        .src_c(ex_fp_scalar[15:0]),
        .rd_idx(ex_ctrl.rd),
        .wb_valid(fp_wb_valid),
        .wb_rd(fp_wb_rd),
        .wb_data(fp_alu_wb_data),
        .wb_scalar_valid(fp_scalar_wb_valid),
        .wb_scalar_rd(fp_scalar_wb_rd),
        .wb_scalar_data(fp_scalar_wb_data),
        .wb_err_overflow(fp_wb_err_overflow),
        .wb_err_invalid(fp_wb_err_invalid)
    );

    // Vector issue queue management (2-entry skid). Allows scalar+vector overlap.
    wire valuv_issue_valid = vq_valid[vq_head] && !stall_any;
    wire valuv_fire        = valuv_issue_valid && valuv_ready;

    wire push_vec_alu0     = rr_is_vec_alu  && !stall_pipe && !vector_queue_full;
    wire push_vec_alu1     = rr1_is_vec_alu && !stall_pipe && !vector_queue_full;
    wire push_vec_alu      = push_vec_alu0 || push_vec_alu1;

    wire [127:0] push_vec_src_a = push_vec_alu1 ? v_rdata_a : v_rdata_a;
    wire [127:0] push_vec_src_b = push_vec_alu1 ? v_rdata_b : v_rdata_b;
    wire [31:0]  push_vec_scalar = push_vec_alu1 ? s_rdata_c : s_rdata_b;
    wire decode_ctrl_t push_vec_ctrl = push_vec_alu1 ? rr1_ctrl : rr_ctrl;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            vq_valid <= '0;
            vq_head  <= '0;
            vq_tail  <= '0;
            vq_count <= '0;
        end else if (ex_branch_taken) begin
            vq_valid <= '0;
            vq_head  <= '0;
            vq_tail  <= '0;
            vq_count <= '0;
        end else begin
            // Pop first to free slot
            if (valuv_fire && vq_valid[vq_head]) begin
                vq_valid[vq_head] <= 1'b0;
                vq_head           <= vq_head + 1'b1;
            end

            // Push RR VALU op into queue when space available
            if (push_vec_alu) begin
                vq[vq_tail].ctrl        <= push_vec_ctrl;
                vq[vq_tail].src_a       <= push_vec_src_a;
                vq[vq_tail].src_b       <= push_vec_src_b;
                vq[vq_tail].scalar_mask <= push_vec_scalar;
                vq_valid[vq_tail]       <= 1'b1;
                vq_tail                 <= vq_tail + 1'b1;
            end

            case ({push_vec_alu, (valuv_fire && vq_count != 0)})
                2'b10: vq_count <= vq_count + 1'b1;
                2'b01: vq_count <= vq_count - 1'b1;
                default: vq_count <= vq_count;
            endcase
        end
    end

    alu_vector u_alu_vector (
        .clk(clk),
        .rst_n(rst_n),
        .valid(valuv_issue_valid),
        .funct6(vq[vq_head].ctrl.funct7[6:1]),
        .funct3(vq[vq_head].ctrl.funct3),
        .vm_enable(vq[vq_head].ctrl.vm_enable),
        .vmask(csr_vmask),
        .rd_idx(vq[vq_head].ctrl.rd),
        .dest_is_scalar(vq[vq_head].ctrl.rd_class == CLASS_SCALAR),
        .src_a(vq[vq_head].src_a),
        .src_b(vq[vq_head].src_b),
        .scalar_mask(vq[vq_head].scalar_mask),
        .ready(valuv_ready),
        .wb_valid(valuv_wb_valid),
        .wb_rd(valuv_wb_rd),
        .wb_is_scalar(valuv_wb_is_scalar),
        .wb_data(valuv_wb_data),
        .wb_err_overflow(valuv_err_overflow),
        .wb_err_invalid(valuv_err_invalid)
    );

    // Texture Miss signals
    logic         tex_miss_req_valid;
    logic [31:0]  tex_miss_req_addr;
    logic         tex_miss_req_ready;
    logic         tex_miss_resp_valid;
    logic [127:0] tex_miss_resp_data;
    
    texture_cache #(
        .LINE_BYTES(16),  // Match 128-bit global bus width
        .LINES(64)
    ) u_texture_cache (
        .clk(clk),
        .rst_n(rst_n),
        .req_valid(tex_req_valid),
        .req_addr(tex_req_addr),
        .req_rd(tex_req_rd),
        .req_ready(tex_req_ready),
        .resp_valid(tex_resp_valid),
        .resp_data(tex_resp_data),
        .resp_rd(tex_resp_rd),
        // Miss interface
        .miss_req_valid(tex_miss_req_valid),
        .miss_req_addr(tex_miss_req_addr),
        .miss_req_ready(tex_miss_req_ready),
        .miss_resp_valid(tex_miss_resp_valid),
        .miss_resp_data(tex_miss_resp_data)
    );

    // Texture logic moved to graphics_pipeline module


    // Texture logic moved to graphics_pipeline module
    // Maintained connection to u_texture_cache via tex_gp_* signals




    // Shared local memory (banked BRAM)
    local_mem_banked u_local_mem (
        .clk(clk),
        .rst_n(rst_n),
        .req_valid(local_req_valid),
        .req_we(local_we),
        .req_is_vector(local_req_is_vector),
        .req_bank_sel(local_bank_sel),
        .req_addr(local_addr),
        .req_wdata(local_wdata),
        .resp_rdata(local_rdata)
    );

    // CSR file (status/config + error capture)
    csr_file u_csr (
        .clk(clk),
        .rst_n(rst_n),
        .csr_en(csr_en),
        .csr_csrrs(csr_csrrs),
        .csr_addr(csr_addr_ex),
        .csr_wdata(csr_wdata_ex),
        .csr_rdata(csr_rdata),
        .core_id(CORE_ID),
        .tile_offset(TILE_OFFSET),
        .fp_err_overflow(err_fp_overflow),
        .fp_err_invalid(err_fp_invalid),
        .vec_err_overflow(err_vec_overflow),
        .vec_err_invalid(err_vec_invalid),
        .status_out(csr_status),
        .fstatus_out(csr_fstatus),
        .vstatus_out(csr_vstatus),
        .vmask_out(csr_vmask)
    );

    // Split-path LSU handles local vs global and blocking scalar behavior
    lsu u_lsu (
        .clk(clk),
        .rst_n(rst_n),
        .valid_in(mem_valid && (mem_ctrl.is_load || mem_ctrl.is_store || mem_ctrl.is_atomic)),
        .is_vector(mem_ctrl.is_vector),
        .is_store(mem_ctrl.is_store),
        .is_atomic(mem_ctrl.is_atomic),
        .atomic_op(mem_ctrl.funct3),
        .vec_mode(mem_ctrl.funct3[1:0]),
        .vec_stride(mem_scalar_wdata),
        .vec_index(mem_vec_wdata),
        .addr(mem_addr),
        .write_data(mem_ctrl.is_vector ? mem_vec_wdata : {96'h0, mem_scalar_wdata}),
        .flush_wmb(mem_is_membar),
        .dest_reg_idx(mem_ctrl.rd),
        .stall_pipeline(lsu_stall),
        .busy(lsu_busy),

        .gfx_st_valid(gp_st_valid),
        .gfx_st_addr(gp_st_addr),
        .gfx_st_wdata(gp_st_wdata),
        .gfx_st_wstrb(gp_st_wstrb),
        .gfx_st_ready(gp_st_ready),
        
        // Texture Cache Miss Interface
        .tex_req_valid(tex_miss_req_valid),
        .tex_req_addr(tex_miss_req_addr),
        .tex_req_ready(tex_miss_req_ready),
        .tex_resp_valid(tex_miss_resp_valid),
        .tex_resp_data(tex_miss_resp_data),

        .local_req_valid(local_req_valid),
        .local_we(local_we),
        .local_req_is_vector(local_req_is_vector),
        .local_addr(local_addr),
        .local_wdata(local_wdata),
        .local_bank_sel(local_bank_sel),
        .local_rdata(local_rdata),
        
        // Unified 32-bit Global Interface
        .global_req_valid(data_req_valid),
        .global_req_is_load(data_req_is_load),
        .global_req_addr(data_req_addr),
        .global_req_wdata(data_req_wdata),
        .global_req_rd(data_req_rd),
        .global_req_ready(data_req_ready),
        
        .global_resp_valid(data_resp_valid),
        .global_resp_rd(data_resp_rd),
        .global_resp_data(data_resp_data),
        
        .wb_valid(lsu_wb_valid),
        .wb_is_vector(lsu_wb_is_vector),
        .wb_reg_idx(lsu_wb_rd),
        .wb_data(lsu_wb_data)
    );

    // ------------------------------------------------------------------------

    // MEM stage registers
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            mem_valid        <= 1'b0;
            mem_ctrl         <= '0;
            mem_scalar_res   <= 32'h0;
            mem_fp_res       <= 16'h0;
            mem_addr         <= 32'h0;
            mem_vec_wdata    <= '0;
            mem_scalar_wdata <= 32'h0;
        end else if (lsu_wb_valid && mem_valid && mem_ctrl.is_load && !mem_ctrl.is_vector) begin
            // Drop the in-flight scalar load once its response returns to avoid re-issuing it
            mem_valid        <= 1'b0;
            mem_ctrl         <= '0;
            mem_scalar_res   <= 32'h0;
            mem_fp_res       <= 16'h0;
            mem_addr         <= 32'h0;
            mem_vec_wdata    <= '0;
            mem_scalar_wdata <= 32'h0;
        end else if (!stall_pipe) begin
            mem_valid        <= ex_valid;
            mem_ctrl         <= ex_ctrl;
            mem_scalar_res   <= ex_ctrl.is_system ? csr_rdata : ex_scalar_res;
            mem_fp_res       <= ex_fp_res;
            mem_addr         <= ex_addr;
            mem_vec_wdata    <= ex_vec_b;
            mem_scalar_wdata <= ex_op_b;
        end
    end

    // WB stage registers
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            wb_valid      <= 1'b0;
            wb_ctrl       <= '0;
            wb_scalar_res <= 32'h0;
            wb_fp_res     <= 16'h0;
        end else if (!stall_pipe) begin
            wb_valid      <= mem_valid;
            wb_ctrl       <= mem_ctrl;
            wb_scalar_res <= mem_scalar_res;
            wb_fp_res     <= mem_fp_res;
        end
    end

    // Writeback selection with simple arbitration (LSU priority, ALU buffered)
    wire lsu_scalar_wb = lsu_wb_valid && !lsu_wb_is_vector;
    wire lsu_vector_wb = lsu_wb_valid && lsu_wb_is_vector;
    wire alu_scalar_wb = wb_valid && wb_ctrl.uses_rd && !wb_ctrl.rd_is_vec && !wb_ctrl.rd_is_fp && !wb_ctrl.is_load && !wb_ctrl.is_vector;
    wire fp_scalar_wb  = fp_scalar_wb_valid;      // FP path now 1-cycle registered
    // Mask X on VALU valid to avoid stalling early scalar writebacks
    wire valuv_wb_valid_masked = (valuv_wb_valid === 1'b1);
    wire valuv_scalar_wb = valuv_wb_valid_masked && valuv_wb_is_scalar;

    // Deterministic scalar writeback priority: Pending > LSU > FP scalar > VALU scalar > ALU scalar
    wire scalar_wb_from_pending = pending_scalar_wb && !lsu_scalar_wb;
    wire scalar_wb_from_lsu     = lsu_scalar_wb;
    wire scalar_wb_from_fp      = fp_scalar_wb && !scalar_wb_from_pending && !scalar_wb_from_lsu;
    wire scalar_wb_from_valu    = valuv_scalar_wb && !scalar_wb_from_pending && !scalar_wb_from_lsu && !scalar_wb_from_fp;
    wire scalar_wb_from_alu     = alu_scalar_wb && !scalar_wb_from_pending && !scalar_wb_from_lsu && !scalar_wb_from_fp && !scalar_wb_from_valu;

    assign s_we    = scalar_wb_from_pending | scalar_wb_from_lsu | scalar_wb_from_fp | scalar_wb_from_valu | scalar_wb_from_alu;
    assign s_waddr = scalar_wb_from_pending ? pending_scalar_rd :
                     scalar_wb_from_lsu     ? lsu_wb_rd         :
                     scalar_wb_from_fp      ? fp_scalar_wb_rd   :
                     scalar_wb_from_valu    ? valuv_wb_rd       : wb_ctrl.rd;
    assign s_wdata = scalar_wb_from_pending ? pending_scalar_data :
                     scalar_wb_from_lsu     ? lsu_wb_data[31:0]   :
                     scalar_wb_from_fp      ? fp_scalar_wb_data   :
                     scalar_wb_from_valu    ? valuv_wb_data[31:0] : wb_scalar_res;

    // FP ALU is 1-cycle latency (registered), separate write port:
    wire fp_wb_fp = fp_wb_valid && !fp_scalar_wb_valid;
    assign f_we    = fp_wb_fp;
    assign f_waddr = fp_wb_rd;
    assign f_wdata = fp_alu_wb_data;

    // Error flags are pulsed with respective wb_valid
    assign err_fp_overflow = fp_wb_valid && fp_wb_err_overflow;
    assign err_fp_invalid  = fp_wb_valid && fp_wb_err_invalid;
    assign err_vec_overflow = valuv_wb_valid && valuv_err_overflow;
    assign err_vec_invalid  = valuv_wb_valid && valuv_err_invalid;

    wire valuv_vector_wb = valuv_wb_valid_masked && !valuv_wb_is_scalar;
    wire vector_wb_from_pending = pending_vector_wb && !lsu_vector_wb;
    wire vector_fire = vector_wb_from_pending
                    || lsu_vector_wb
                    || ((gp_wb_valid || valuv_vector_wb) && !lsu_vector_wb && !pending_vector_wb);

    // Vector writeback: priority Pending > LSU > TEX > VALU; capture collisions into pending
    assign v_we    = vector_fire;
    assign v_waddr = vector_wb_from_pending ? pending_vector_rd :
                     (lsu_vector_wb ? lsu_wb_rd :
                     (gp_wb_valid ? gp_wb_rd : valuv_wb_rd));

    assign v_wdata = vector_wb_from_pending ? pending_vector_data :
                     (lsu_vector_wb ? lsu_wb_data :
                     (gp_wb_valid ? gp_wb_data : valuv_wb_data));

    // Pending writeback skid buffers to avoid port collisions
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pending_scalar_wb   <= 1'b0;
            pending_scalar_rd   <= '0;
            pending_scalar_data <= '0;
            pending_vector_wb   <= 1'b0;
            pending_vector_rd   <= '0;
            pending_vector_data <= '0;
        end else begin
            // Scalar pending clear when consumed
            if (scalar_wb_from_pending) begin
                pending_scalar_wb <= 1'b0;
            end
            // Capture scalar ALU result if LSU steals the port this cycle
            if ((alu_scalar_wb || fp_scalar_wb || valuv_scalar_wb) && lsu_scalar_wb && !pending_scalar_wb) begin
                pending_scalar_wb   <= 1'b1;
                pending_scalar_rd   <= alu_scalar_wb ? wb_ctrl.rd : (fp_scalar_wb ? fp_scalar_wb_rd : valuv_wb_rd);
                pending_scalar_data <= alu_scalar_wb ? wb_scalar_res : (fp_scalar_wb ? fp_scalar_wb_data : valuv_wb_data[31:0]);
            end

            // Vector pending clear when consumed
            if (vector_wb_from_pending) begin
                pending_vector_wb <= 1'b0;
            end
            // Capture vector results if LSU steals the port this cycle (tex preferred over VALU)
            if ((gp_wb_valid || valuv_wb_valid) && lsu_vector_wb && !pending_vector_wb) begin
                pending_vector_wb   <= 1'b1;
                pending_vector_rd   <= gp_wb_valid ? gp_wb_rd   : valuv_wb_rd;
                pending_vector_data <= gp_wb_valid ? gp_wb_data : valuv_wb_data;
            end
        end
    end

endmodule
