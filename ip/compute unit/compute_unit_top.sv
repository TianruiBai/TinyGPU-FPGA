module compute_unit_top #(
    parameter logic [31:0] CORE_ID     = 32'h0,
    parameter logic [31:0] TILE_OFFSET = 32'h0,
    parameter int GFX_ISSUE_Q_DEPTH    = 8,
    parameter int TEX_REQ_Q_DEPTH      = 2,
    parameter int TEX_CACHE_LINE_BYTES = 16,
    parameter int TEX_CACHE_LINES      = 64,
    parameter int ROP_WCACHE_ENTRIES   = 8,
    parameter int ROP_QUAD_Q_DEPTH     = 2,
    parameter int ROP_STQ_DEPTH        = 2,
    parameter bit MAILBOX_ENABLE       = 1'b0,
    parameter logic [7:0] MAILBOX_SRC_ID = 8'h00
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
    input  logic [31:0] data_resp_data,

    // Mailbox AXI‑MailboxFabric stream link
    output logic                           mailbox_tx_valid,
    input  logic                           mailbox_tx_ready,
    output mailbox_pkg::mailbox_flit_t     mailbox_tx_data,
    output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_tx_dest_id,

    input  logic                           mailbox_rx_valid,
    output logic                           mailbox_rx_ready,
    input  mailbox_pkg::mailbox_flit_t     mailbox_rx_data,
    input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_rx_dest_id
);
    import mailbox_pkg::*;
    import isa_pkg::*;

    // IF stage
    logic [31:0] if_pc;
    logic [31:0] if_inst0;
    logic [31:0] if_inst1;
    logic        if_valid;
    logic        if_inst0_valid;
    logic        if_inst1_valid;

            assign mailbox_tx_ready_int = ep_tx_ready;
            assign mailbox_rd_ready_int = !rd_pending;

                mailbox_endpoint_stream #(
                    .SRC_ID({8'h00, MAILBOX_SRC_ID})
                ) u_mailbox_ep (
                    .clk(clk),
                    .rst_n(rst_n),

                    .tx_valid(lsu_mailbox_tx_valid),
                    .tx_ready(ep_tx_ready),
                    .tx_data(lsu_mailbox_tx_data),
                    .tx_dest_id(lsu_mailbox_tx_dest),
                    .tx_opcode(lsu_mailbox_tx_opcode),
                    .tx_prio({1'b0, lsu_mailbox_tx_prio}),
                    .tx_eop(lsu_mailbox_tx_eop),
                    .tx_debug(1'b0),

                    .rx_valid(ep_rx_valid),
                    .rx_ready(ep_rx_ready_int),
                    .rx_data(ep_rx_data),
                    .rx_hdr(ep_rx_hdr),
                    .rx_irq(ep_rx_irq),
                    .rx_error(ep_rx_err),
                    .rx_dest_id(ep_rx_dest_id),

                    .link_tx_valid(mailbox_tx_valid),
                    .link_tx_ready(mailbox_tx_ready),
                    .link_tx_data(mailbox_tx_data),
                    .link_tx_dest_id(mailbox_tx_dest_id),

                    .link_rx_valid(mailbox_rx_valid),
                    .link_rx_ready(mailbox_rx_ready),
                    .link_rx_data(mailbox_rx_data),
                    .link_rx_dest_id(mailbox_rx_dest_id)
                );
    logic [4:0]  tex_resp_rd;

                assign mailbox_rd_ready_int = 1'b1;
                assign lsu_mailbox_rd_resp_valid = 1'b0;
                assign lsu_mailbox_rd_resp_data  = 32'h0;
                assign lsu_mailbox_rd_resp_tag   = '0;
    logic        gfxd_req_ready;
                assign mailbox_tx_valid  = 1'b0;
                assign mailbox_tx_data   = '0;
                assign mailbox_tx_dest_id = '0;
                assign mailbox_rx_ready  = 1'b1;
    logic [31:0] ex_pred_target;

                assign ep_rx_hdr        = '0;
                assign ep_rx_dest_id    = '0;
    logic        ex_cf_taken;
                assign ep_rx_err        = 1'b0;
    logic [31:0] ex_cf_target;

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

    // Graphics issue (core -> graphics_pipeline FIFO)
    logic        gp_issue0_valid;
    decode_ctrl_t gp_issue0_ctrl;
    logic [31:0] gp_issue0_op_a;
    logic [31:0] gp_issue0_op_b;
    logic [127:0] gp_issue0_vec_a;
    logic [127:0] gp_issue0_vec_b;

    logic        gp_issue1_valid;
    decode_ctrl_t gp_issue1_ctrl;
    logic [31:0] gp_issue1_op_a;
    logic [31:0] gp_issue1_op_b;
    logic [127:0] gp_issue1_vec_a;
    logic [127:0] gp_issue1_vec_b;

    // Legacy single-issue debug visibility (used by gfx_console_tb via hierarchical refs)
    decode_ctrl_t gp_issue_ctrl;
    logic [31:0]  gp_issue_op_a;
    logic [31:0]  gp_issue_op_b;
    logic         gp_issue_valid;

    graphics_pipeline #(
        .GQ_DEPTH(GFX_ISSUE_Q_DEPTH),
        .TEX_REQ_Q_DEPTH(TEX_REQ_Q_DEPTH),
        .ROP_QUAD_Q_DEPTH(ROP_QUAD_Q_DEPTH),
        .ROP_STQ_DEPTH(ROP_STQ_DEPTH)
    ) u_graphics_pipeline (
        .clk(clk),
        .rst_n(rst_n),
        // NOTE: Do not flush the graphics queue on scalar control-flow redirects.
        // GFX/TEX ops are architecturally "fire-and-forget" once issued; flushing here
        // would incorrectly drop in-flight work whenever the core executes a taken branch
        // (including simple halt loops).
        .flush_all(1'b0),
        // Core-issued gfx/tex ops enqueue into the graphics FIFO
        .issue0_valid(gp_issue0_valid),
        .issue0_ctrl(gp_issue0_ctrl),
        .issue0_op_a(gp_issue0_op_a),
        .issue0_op_b(gp_issue0_op_b),
        .issue0_vec_a(gp_issue0_vec_a),
        .issue0_vec_b(gp_issue0_vec_b),

        .issue1_valid(gp_issue1_valid),
        .issue1_ctrl(gp_issue1_ctrl),
        .issue1_op_a(gp_issue1_op_a),
        .issue1_op_b(gp_issue1_op_b),
        .issue1_vec_a(gp_issue1_vec_a),
        .issue1_vec_b(gp_issue1_vec_b),
        .queue_full(gfx_queue_full),
        .queue_afull(gfx_queue_afull),
        .queue_count(gfx_queue_count),
        .busy(gfx_busy),
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

        // GFX descriptor cache
        .gfxd_req_valid(gfxd_gp_req_valid),
        .gfxd_req_addr(gfxd_gp_req_addr),
        .gfxd_req_rd(gfxd_gp_req_rd),
        .gfxd_req_ready(gfxd_gp_req_ready),
        .gfxd_resp_valid(gfxd_gp_resp_valid),
        .gfxd_resp_data(gfxd_gp_resp_data),
        .gfxd_resp_rd(gfxd_gp_resp_rd),

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
    logic [31:0]  ex_pc;
    logic [31:0]  ex_op_a;
    logic [31:0]  ex_op_b;
    logic [31:0]  mem_pc;
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
    logic [31:0]  s_rdata_a_raw, s_rdata_b_raw, s_rdata_c_raw;
    logic [31:0]  s_rdata_a_gfx, s_rdata_b_gfx, s_rdata_c_gfx;
    logic [31:0]  s_rdata_b_vec, s_rdata_c_vec;
    logic [31:0]  s_wdata;
    logic         s_we;
    logic [4:0]   s_waddr;

    logic [15:0]  f_rdata_a, f_rdata_b;
    logic [15:0]  f_rdata_a_raw, f_rdata_b_raw;
    logic [15:0]  f_wdata;
    logic         f_we;
    logic [4:0]   f_waddr;

    logic [127:0] v_rdata_a, v_rdata_b;
    logic [127:0] v_rdata_a_raw, v_rdata_b_raw;
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
    logic         fp_in_ready;

    // Scalar WB backpressure + commit metadata (for CSR-aligned error pulses)
    logic         fp_scalar_ready;
    logic         valuv_scalar_ready;
    logic         alu_scalar_ready;
    logic         s_commit_from_fp;
    logic         s_commit_from_valu;
    logic         s_commit_err_overflow;
    logic         s_commit_err_invalid;

    // Vector WB commit metadata (for CSR-aligned error pulses)
    logic         v_commit_from_valuv;
    logic         v_commit_err_overflow;
    logic         v_commit_err_invalid;

    // LSU wiring to external data interface and local memory
    logic         lsu_wb_valid;
    logic         lsu_wb_is_vector;
    logic [4:0]   lsu_wb_rd;
    logic [127:0] lsu_wb_data;

    // Writeback-classification helpers (declared early so stall_pipe can reference them)
    logic         lsu_scalar_wb;
    logic         lsu_vector_wb;
    logic         alu_scalar_wb;
    logic         fp_scalar_wb;
    logic         valuv_wb_valid_masked;
    logic         valuv_scalar_wb;

    // Vector writeback pending queue (buffers non-LSU vector writebacks)
    // Sized conservatively to cover max in-flight TEX/GFX completions + any VALU results.
    localparam int VWBQ_DEPTH = 32;
    logic [$clog2(VWBQ_DEPTH+1)-1:0] vwbq_count;
    logic [$clog2(VWBQ_DEPTH)-1:0]   vwbq_head;
    logic [$clog2(VWBQ_DEPTH)-1:0]   vwbq_tail;
    logic [4:0]                      vwbq_rd   [VWBQ_DEPTH];
    logic [127:0]                    vwbq_data [VWBQ_DEPTH];
    logic                            vwbq_from_valuv [VWBQ_DEPTH];
    logic                            vwbq_err_ovf    [VWBQ_DEPTH];
    logic                            vwbq_err_inv    [VWBQ_DEPTH];

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

    // Command streamer CSR-config outputs
    logic         csr_cmd_enable;
    logic [31:0]  csr_cmd_ring_base;
    logic [31:0]  csr_cmd_ring_size_bytes;
    logic [31:0]  csr_cmd_cons_ptr_bytes;
    logic [31:0]  csr_cmd_completion_base;

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

    // Backpressure for graphics/texture macro-ops:
    // GFX/TEX ops enqueue into the graphics pipeline issue queue without a per-op ready/ack.
    // To avoid silently dropping ops when the queue is full, stall the core pipeline while a
    // GFX/TEX op is sitting in RR (or RR1) and there isn't enough queue space.
    wire rr0_is_gp = rr_valid  && (rr_ctrl.is_gfx  || rr_ctrl.is_tex);
    wire rr1_is_gp = rr1_valid && (rr1_ctrl.is_gfx || rr1_ctrl.is_tex);
    wire stall_gfxq_rr = (rr0_is_gp && rr1_is_gp) ? gfx_queue_afull
                        : (rr0_is_gp || rr1_is_gp) ? gfx_queue_full
                        : 1'b0;

    // FP conversions to scalar regs can be backpressured by scalar WB arbitration.
    // When fp_alu is holding an unaccepted result, stall the core if an FP op is in EX.
    wire stall_fp_ex = ex_valid && ex_ctrl.is_scalar_fp && !fp_in_ready;

    // Scalar WB arbitration provides ready/backpressure signals to prevent dropping results.
    wire stall_scalar_wb = (fp_scalar_wb && !fp_scalar_ready)
                        || (valuv_scalar_wb && !valuv_scalar_ready)
                        || (alu_scalar_wb && !alu_scalar_ready);

    // Load-use interlock (scalar + vector ALU). Uses registered EX/MEM state to avoid long comb paths.
    wire rr_is_scalar_pipe = rr_valid && !rr_is_vec_alu && !rr_is_gfx;
    wire rr_uses_scalar_rs1 = rr_is_scalar_pipe && rr_ctrl.uses_rs1 && (rr_ctrl.rs1_class == CLASS_SCALAR) && (rr_ctrl.rs1 != 5'd0);
    wire rr_uses_scalar_rs2 = rr_is_scalar_pipe && rr_ctrl.uses_rs2 && (rr_ctrl.rs2_class == CLASS_SCALAR) && (rr_ctrl.rs2 != 5'd0);

    wire rr0_uses_vec_rs1 = rr_is_vec_alu && rr_ctrl.uses_rs1 && (rr_ctrl.rs1_class == CLASS_VEC) && (rr_ctrl.rs1 != 5'd0);
    wire rr0_uses_vec_rs2 = rr_is_vec_alu && rr_ctrl.uses_rs2 && (rr_ctrl.rs2_class == CLASS_VEC) && (rr_ctrl.rs2 != 5'd0);
    wire rr1_uses_vec_rs1 = rr1_is_vec_alu && rr1_ctrl.uses_rs1 && (rr1_ctrl.rs1_class == CLASS_VEC) && (rr1_ctrl.rs1 != 5'd0);
    wire rr1_uses_vec_rs2 = rr1_is_vec_alu && rr1_ctrl.uses_rs2 && (rr1_ctrl.rs2_class == CLASS_VEC) && (rr1_ctrl.rs2 != 5'd0);

    wire ex_is_scalar_load = ex_valid && ex_ctrl.is_load && !ex_ctrl.is_vector && ex_ctrl.uses_rd && (ex_ctrl.rd != 5'd0);
    wire mem_is_scalar_load = mem_valid && mem_ctrl.is_load && !mem_ctrl.is_vector && mem_ctrl.uses_rd && (mem_ctrl.rd != 5'd0);

    wire ex_is_vec_load  = ex_valid && ex_ctrl.is_load && ex_ctrl.is_vector && ex_ctrl.uses_rd && (ex_ctrl.rd != 5'd0);
    wire mem_is_vec_load = mem_valid && mem_ctrl.is_load && mem_ctrl.is_vector && mem_ctrl.uses_rd && (mem_ctrl.rd != 5'd0);

    wire hazard_ex_load = ex_is_scalar_load && ((rr_uses_scalar_rs1 && (rr_ctrl.rs1 == ex_ctrl.rd))
                                             || (rr_uses_scalar_rs2 && (rr_ctrl.rs2 == ex_ctrl.rd)));

    wire mem_load_data_ready = lsu_wb_valid && !lsu_wb_is_vector && (lsu_wb_rd == mem_ctrl.rd);
    wire hazard_mem_load = mem_is_scalar_load && !mem_load_data_ready
                        && ((rr_uses_scalar_rs1 && (rr_ctrl.rs1 == mem_ctrl.rd))
                         || (rr_uses_scalar_rs2 && (rr_ctrl.rs2 == mem_ctrl.rd)));

    wire hazard_ex_vload = ex_is_vec_load && ((rr0_uses_vec_rs1 && (rr_ctrl.rs1 == ex_ctrl.rd))
                                           || (rr0_uses_vec_rs2 && (rr_ctrl.rs2 == ex_ctrl.rd))
                                           || (rr1_uses_vec_rs1 && (rr1_ctrl.rs1 == ex_ctrl.rd))
                                           || (rr1_uses_vec_rs2 && (rr1_ctrl.rs2 == ex_ctrl.rd)));

    wire mem_vec_load_data_ready = lsu_wb_valid && lsu_wb_is_vector && (lsu_wb_rd == mem_ctrl.rd);
    wire hazard_mem_vload = mem_is_vec_load && !mem_vec_load_data_ready
                            && ((rr0_uses_vec_rs1 && (rr_ctrl.rs1 == mem_ctrl.rd))
                             || (rr0_uses_vec_rs2 && (rr_ctrl.rs2 == mem_ctrl.rd))
                             || (rr1_uses_vec_rs1 && (rr1_ctrl.rs1 == mem_ctrl.rd))
                             || (rr1_uses_vec_rs2 && (rr1_ctrl.rs2 == mem_ctrl.rd)));

    wire stall_load_use = hazard_ex_load || hazard_mem_load || hazard_ex_vload || hazard_mem_vload;

    wire stall_pipe          = lsu_stall || stall_membar || stall_gfxq_rr || stall_fp_ex || stall_scalar_wb || stall_load_use;
    // Frontend stalls when it cannot accept slot0.
    // During reset force stall low to avoid X-propagation into fetch/PC
    assign stall_any         = rst_n ? stall_pipe : 1'b0;
    wire stall_issue         = stall_any || (if_valid && !accept0);

    // ---------------------------------------------------------------------
    // RR forwarding availability (vector + gfx/tex scalar operands)
    // ---------------------------------------------------------------------
    wire valuv_vector_wb_issue = (valuv_wb_valid === 1'b1) && !valuv_wb_is_scalar;
    function automatic logic vec_pending_rd_hit(input logic [4:0] r);
        begin
            vec_pending_rd_hit = 1'b0;
            if (vq_valid[0] && vq[0].ctrl.uses_rd && (vq[0].ctrl.rd == r)) vec_pending_rd_hit = 1'b1;
            if (vq_valid[1] && vq[1].ctrl.uses_rd && (vq[1].ctrl.rd == r)) vec_pending_rd_hit = 1'b1;
        end
    endfunction

    function automatic logic vec_fwd_hit(input logic [4:0] r);
        begin
            vec_fwd_hit = 1'b0;
            if (vec_pending_rd_hit(r)) begin
                if (valuv_vector_wb_issue && (valuv_wb_rd == r)) vec_fwd_hit = 1'b1;
            end else begin
                if (lsu_wb_valid && lsu_wb_is_vector && (lsu_wb_rd == r)) vec_fwd_hit = 1'b1;
                else if ((vwbq_count != '0) && (vwbq_rd[vwbq_head] == r)) vec_fwd_hit = 1'b1;
                else if (gp_wb_valid && (gp_wb_rd == r)) vec_fwd_hit = 1'b1;
                else if (valuv_vector_wb_issue && (valuv_wb_rd == r)) vec_fwd_hit = 1'b1;
            end
        end
    endfunction

    function automatic logic scalar_fwd_hit(input logic [4:0] r);
        begin
            scalar_fwd_hit = 1'b0;
            if (fwd_ex_valid && (fwd_ex_rd == r)) scalar_fwd_hit = 1'b1;
            else if (lsu_wb_valid && !lsu_wb_is_vector && (lsu_wb_rd == r)) scalar_fwd_hit = 1'b1;
            else if (mem_valid && mem_ctrl.uses_rd && !mem_ctrl.is_load && !mem_ctrl.is_vector
                     && !mem_ctrl.rd_is_vec && !mem_ctrl.rd_is_fp && !mem_ctrl.is_scalar_fp
                     && (mem_ctrl.rd == r)) scalar_fwd_hit = 1'b1;
            else if (wb_valid && wb_ctrl.uses_rd && !wb_ctrl.is_load && !wb_ctrl.is_vector
                     && !wb_ctrl.rd_is_vec && !wb_ctrl.rd_is_fp && !wb_ctrl.is_scalar_fp
                     && (wb_ctrl.rd == r)) scalar_fwd_hit = 1'b1;
        end
    endfunction

    wire issue0_rs1_fwd = (d0_ctrl.uses_rs1 && (d0_ctrl.rs1_class == CLASS_VEC) && vec_fwd_hit(d0_ctrl.rs1))
                       || (d0_ctrl.uses_rs1 && (d0_ctrl.rs1_class == CLASS_SCALAR) && scalar_fwd_hit(d0_ctrl.rs1));
    wire issue0_rs2_fwd = (d0_ctrl.uses_rs2 && (d0_ctrl.rs2_class == CLASS_VEC) && vec_fwd_hit(d0_ctrl.rs2))
                       || (d0_ctrl.uses_rs2 && (d0_ctrl.rs2_class == CLASS_SCALAR) && scalar_fwd_hit(d0_ctrl.rs2));
    wire issue1_rs1_fwd = (d1_ctrl.uses_rs1 && (d1_ctrl.rs1_class == CLASS_VEC) && vec_fwd_hit(d1_ctrl.rs1))
                       || (d1_ctrl.uses_rs1 && (d1_ctrl.rs1_class == CLASS_SCALAR) && scalar_fwd_hit(d1_ctrl.rs1));
    wire issue1_rs2_fwd = (d1_ctrl.uses_rs2 && (d1_ctrl.rs2_class == CLASS_VEC) && vec_fwd_hit(d1_ctrl.rs2))
                       || (d1_ctrl.uses_rs2 && (d1_ctrl.rs2_class == CLASS_SCALAR) && scalar_fwd_hit(d1_ctrl.rs2));

    // ---------------------------------------------------------------------
    // Graphics issue select + texture wiring (must appear after stall_pipe is defined)
    // ---------------------------------------------------------------------
    always_comb begin
        // Drive both enqueue slots into the graphics FIFO.
        gp_issue0_valid = rr_is_gfx && !stall_pipe && !ex_redirect_valid;
        gp_issue0_ctrl  = rr_ctrl;
        gp_issue0_op_a  = s_rdata_a_gfx;
        gp_issue0_op_b  = s_rdata_b_gfx;
        gp_issue0_vec_a = v_rdata_a;
        gp_issue0_vec_b = v_rdata_b;

        gp_issue1_valid = rr1_is_gfx && !stall_pipe && !ex_redirect_valid;
        gp_issue1_ctrl  = rr1_ctrl;
        gp_issue1_vec_a = v_rdata_a;
        gp_issue1_vec_b = v_rdata_b;
        // Only one scalar operand is supported on lane1.
        // Route it to the field the engine expects:
        // - GFX ops consume the descriptor pointer from op_a
        // - TEX ops consume the sampler handle/pointer from op_b
        gp_issue1_op_a  = (rr1_ctrl.is_gfx && (rr1_ctrl.rs1_class == CLASS_SCALAR)) ? s_rdata_c_gfx : 32'h0;
        gp_issue1_op_b  = (rr1_ctrl.is_tex && (rr1_ctrl.rs2_class == CLASS_SCALAR)) ? s_rdata_c_gfx : 32'h0;
    end

    // Legacy single-issue debug visibility (used by gfx_console_tb via hierarchical refs)
    assign gp_issue_valid = gp_issue0_valid || gp_issue1_valid;
    assign gp_issue_ctrl  = gp_issue0_valid ? gp_issue0_ctrl : gp_issue1_ctrl;
    assign gp_issue_op_a  = gp_issue0_valid ? gp_issue0_op_a : gp_issue1_op_a;
    assign gp_issue_op_b  = gp_issue0_valid ? gp_issue0_op_b : gp_issue1_op_b;

    // Wire Texture Cache to Graphics Pipeline (descriptor/texture reads share this path)
    assign tex_gp_req_ready  = tex_req_ready;
    assign tex_req_valid     = tex_gp_req_valid;
    assign tex_req_addr      = tex_gp_req_addr;
    assign tex_req_rd        = tex_gp_req_rd;

    assign tex_gp_resp_valid = tex_resp_valid;
    assign tex_gp_resp_data  = tex_resp_data;
    assign tex_gp_resp_rd    = tex_resp_rd;

    // Wire GFX descriptor cache to Graphics Pipeline
    assign gfxd_gp_req_ready  = gfxd_req_ready;
    assign gfxd_req_valid     = gfxd_gp_req_valid;
    assign gfxd_req_addr      = gfxd_gp_req_addr;
    assign gfxd_req_rd        = gfxd_gp_req_rd;

    assign gfxd_gp_resp_valid = gfxd_resp_valid;
    assign gfxd_gp_resp_data  = gfxd_resp_data;
    assign gfxd_gp_resp_rd    = gfxd_resp_rd;

    always_comb begin
        stall_scoreboard = stall_sb0 || stall_sb1;
    end

    // Fetch unit
    logic        bp_pred_taken;
    logic [31:0] bp_pred_target;

    // Dynamic predictor (2-bit counters). Query on IF slot0, but only redirect when slot0 is accepted.
    branch_predictor_bht #(
        .ENTRIES(64)
    ) u_branch_pred (
        .clk(clk),
        .rst_n(rst_n),
        .query_valid(if_valid && if_inst0_valid),
        .query_ctrl(d0_ctrl),
        .query_pc(if_pc),
        .pred_taken(bp_pred_taken),
        .pred_target(bp_pred_target),
        .update_valid(ex_valid && ex_ctrl.is_branch),
        .update_ctrl(ex_ctrl),
        .update_pc(ex_pc),
        .update_taken(ex_cf_taken)
    );

    // Only redirect the fetch PC when the predicted instruction is actually entering the pipe.
    assign if_pred_taken  = accept0 && bp_pred_taken;
    assign if_pred_target = bp_pred_target;

    fetch_unit u_fetch (
        .clk(clk),
        .rst_n(rst_n),
        .stall(stall_issue),
        .pred_taken(if_pred_taken),
        .pred_target(if_pred_target),
        .branch_taken(ex_redirect_valid),
        .branch_target(ex_redirect_target),
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
        .issue0_rs1_fwd(issue0_rs1_fwd),
        .issue0_rs2_fwd(issue0_rs2_fwd),
        .issue0_rd_valid(d0_ctrl.uses_rd),
        .issue0_rd_class(d0_ctrl.rd_class),
        .issue0_rd(d0_ctrl.rd),
        .accept0(accept0),
        .stall0(stall_sb0),

        .issue1_valid(issue1_valid),
        .issue1_rs1_valid(d1_ctrl.uses_rs1),
        .issue1_rs2_valid(d1_ctrl.uses_rs2),
        .issue1_rs1_class(d1_ctrl.rs1_class),
        .issue1_rs2_class(d1_ctrl.rs2_class),
        .issue1_rs1(d1_ctrl.rs1),
        .issue1_rs2(d1_ctrl.rs2),
        .issue1_rs1_fwd(issue1_rs1_fwd),
        .issue1_rs2_fwd(issue1_rs2_fwd),
        .issue1_rd_valid(d1_ctrl.uses_rd),
        .issue1_rd_class(d1_ctrl.rd_class),
        .issue1_rd(d1_ctrl.rd),
        .accept1(accept1),
        .stall1(stall_sb1),

        .flush_rr(ex_redirect_valid),
        .flush_rr_rd_class(rr_ctrl.rd_class),
        .flush_rr_rd_valid(rr_valid && rr_ctrl.uses_rd),
        .flush_rr_rd(rr_ctrl.rd),
        .wb_scalar_valid(s_we),
        .wb_scalar_rd(s_waddr),
        .wb_fp_valid(f_we),
        .wb_fp_rd(f_waddr),
        .wb_vec_valid(v_we),
        .wb_vec_rd(v_waddr),
        // Do not clear all busy bits on redirect: older in-flight ops must complete.
        .flush_all(1'b0)
    );

    // Issue classification (slot0/slot1)
    wire d0_is_vec_alu = if_inst0_valid && d0_ctrl.is_vector && !d0_ctrl.is_load && !d0_ctrl.is_store && !d0_ctrl.is_tex && !d0_ctrl.is_atomic;
    wire d0_is_gfx     = if_inst0_valid && (d0_ctrl.is_tex || d0_ctrl.is_gfx);
    wire d0_is_scalar_pipe = if_inst0_valid && !d0_is_vec_alu && !d0_is_gfx;

    wire d1_is_vec_alu = if_inst1_valid && d1_ctrl.is_vector && !d1_ctrl.is_load && !d1_ctrl.is_store && !d1_ctrl.is_tex && !d1_ctrl.is_atomic;
    wire d1_is_gfx     = if_inst1_valid && (d1_ctrl.is_tex || d1_ctrl.is_gfx);

    // Slot0 can be a scalar-pipe op (including vector loads/stores/atomics), and slot1 can be vec-ALU/gfx.
    // However, vector stores/atomics in slot0 need the vector regfile read ports, which would conflict with
    // slot1 vec-ALU/gfx operand reads (we only have one 2R port set). Disallow that dual-issue case.
    wire d0_needs_vrf = if_inst0_valid && d0_ctrl.is_vector && (d0_ctrl.is_store || d0_ctrl.is_atomic);
    // Do not dual-issue behind control-flow: gfx/tex work is not flushed on redirects.
    wire can_dual_raw = if_inst0_valid && if_inst1_valid && d0_is_scalar_pipe && !d0_ctrl.is_branch && !d0_ctrl.is_system && !d0_needs_vrf && (d1_is_vec_alu || d1_is_gfx);
    wire can_dual = can_dual_raw;

    assign issue0_valid = if_valid && if_inst0_valid && !stall_pipe && !ex_redirect_valid
                      && !(d0_is_vec_alu && vector_queue_full)
                      && !(d0_is_gfx && gfx_queue_full);

    assign issue1_valid = if_valid && can_dual && !stall_pipe && !ex_redirect_valid
                      && ((d1_is_vec_alu && !vector_queue_full) || (d1_is_gfx && !gfx_queue_full));

    always_comb begin
        accept0 = issue0_valid && !stall_sb0;
        accept1 = issue1_valid && !stall_sb1 && accept0;
    end

    // PC step is driven by accepted slots
    always_comb begin
        if (!accept0) pc_advance_bytes = 4'd0;
        else if (accept1) pc_advance_bytes = 4'd8;
        else pc_advance_bytes = 4'd4;
    end

    // NOTE: pc_advance_bytes is intentionally combinational; fetch_unit consumes it when advancing pc_reg.

    // RR stage registers
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            rr_valid <= 1'b0;
            rr_ctrl  <= '0;
            rr1_valid <= 1'b0;
            rr1_ctrl  <= '0;
            rr_pc    <= 32'h0;
            rr_pred_taken  <= 1'b0;
            rr_pred_target <= 32'h0;
        end else if (ex_redirect_valid) begin
             // Flush younger ops on redirect
             rr_valid <= 1'b0;
             rr_ctrl  <= '0;
             rr1_valid <= 1'b0;
             rr1_ctrl  <= '0;
             rr_pc    <= 32'h0;
             rr_pred_taken  <= 1'b0;
             rr_pred_target <= 32'h0;
        end else if (!stall_pipe) begin
            rr_valid  <= accept0;
            rr_ctrl   <= d0_ctrl;
            rr1_valid <= accept1;
            rr1_ctrl  <= d1_ctrl;
            rr_pc     <= accept0 ? if_pc : 32'h0;
            rr_pred_taken  <= accept0 ? if_pred_taken : 1'b0;
            rr_pred_target <= accept0 ? if_pred_target : 32'h0;
        end
    end

    // Regfile instances
    regfile_scalar u_regfile_scalar (
        .clk(clk),
        .rst_n(rst_n),
        .raddr_a(rr_ctrl.rs1),
        .raddr_b(rr_ctrl.rs2),
        .raddr_c(rr1_scalar_raddr),
        .rdata_a(s_rdata_a_raw),
        .rdata_b(s_rdata_b_raw),
        .rdata_c(s_rdata_c_raw),
        .we(s_we),
        .waddr(s_waddr),
        .wdata(s_wdata)
    );

    // Scalar writeback bypass: prefer same-cycle WB data over regfile read.
    always_comb begin
        s_rdata_a = (s_we && (s_waddr != 5'd0) && (s_waddr == rr_ctrl.rs1)) ? s_wdata : s_rdata_a_raw;
        s_rdata_b = (s_we && (s_waddr != 5'd0) && (s_waddr == rr_ctrl.rs2)) ? s_wdata : s_rdata_b_raw;
        s_rdata_c = (s_we && (s_waddr != 5'd0) && (s_waddr == rr1_scalar_raddr)) ? s_wdata : s_rdata_c_raw;
    end

    regfile_fp u_regfile_fp (
        .clk(clk),
        .rst_n(rst_n),
        .raddr_a(rr_ctrl.rs1),
        .raddr_b(rr_ctrl.rs2),
        .rdata_a(f_rdata_a_raw),
        .rdata_b(f_rdata_b_raw),
        .we(f_we),
        .waddr(f_waddr),
        .wdata(f_wdata)
    );

    // Same-cycle writeback bypass for FP reads (to match scoreboard WB relaxation).
    always_comb begin
        f_rdata_a = (f_we && (f_waddr == rr_ctrl.rs1)) ? f_wdata : f_rdata_a_raw;
        f_rdata_b = (f_we && (f_waddr == rr_ctrl.rs2)) ? f_wdata : f_rdata_b_raw;
    end

    // NOTE: Scalar has same-cycle writeback bypass; FP/Vector RAW hazards rely on scoreboard stalls
    // plus same-cycle WB bypass when available.

    // Vector operands are only needed for rr (vector/gfx) and rr1 (dual-issued vector/gfx).
    wire vrf_use_rr1 = rr1_valid && (rr1_is_vec_alu || rr1_is_gfx);
    wire [4:0] vrf_raddr_a = vrf_use_rr1 ? rr1_ctrl.rs1 : rr_ctrl.rs1;
    wire [4:0] vrf_raddr_b = vrf_use_rr1 ? rr1_ctrl.rs2 : rr_ctrl.rs2;

    regfile_vector u_regfile_vector (
        .clk(clk),
        .rst_n(rst_n),
        .raddr_a(vrf_raddr_a),
        .raddr_b(vrf_raddr_b),
        .rdata_a(v_rdata_a_raw),
        .rdata_b(v_rdata_b_raw),
        .we(v_we),
        .waddr(v_waddr),
        .wdata(v_wdata)
    );

    // Vector RR forwarding: allow consuming results from LSU/GP/VALU or pending queue.
    always_comb begin
        v_rdata_a = v_rdata_a_raw;
        if (v_we && (v_waddr == vrf_raddr_a)) v_rdata_a = v_wdata;
        else if (lsu_wb_valid && lsu_wb_is_vector && (lsu_wb_rd == vrf_raddr_a)) v_rdata_a = lsu_wb_data;
        else if ((vwbq_count != '0) && (vwbq_rd[vwbq_head] == vrf_raddr_a)) v_rdata_a = vwbq_data[vwbq_head];
        else if (gp_wb_valid && (gp_wb_rd == vrf_raddr_a)) v_rdata_a = gp_wb_data;
        else if ((valuv_wb_valid === 1'b1) && !valuv_wb_is_scalar && (valuv_wb_rd == vrf_raddr_a)) v_rdata_a = valuv_wb_data;

        v_rdata_b = v_rdata_b_raw;
        if (v_we && (v_waddr == vrf_raddr_b)) v_rdata_b = v_wdata;
        else if (lsu_wb_valid && lsu_wb_is_vector && (lsu_wb_rd == vrf_raddr_b)) v_rdata_b = lsu_wb_data;
        else if ((vwbq_count != '0) && (vwbq_rd[vwbq_head] == vrf_raddr_b)) v_rdata_b = vwbq_data[vwbq_head];
        else if (gp_wb_valid && (gp_wb_rd == vrf_raddr_b)) v_rdata_b = gp_wb_data;
        else if ((valuv_wb_valid === 1'b1) && !valuv_wb_is_scalar && (valuv_wb_rd == vrf_raddr_b)) v_rdata_b = valuv_wb_data;
    end

    always_comb begin
        rr_is_vec_alu  = rr_valid  && rr_ctrl.is_vector  && !rr_ctrl.is_load  && !rr_ctrl.is_store  && !rr_ctrl.is_tex && !rr_ctrl.is_atomic;
        rr_is_gfx      = rr_valid  && (rr_ctrl.is_tex || rr_ctrl.is_gfx);
        rr1_is_vec_alu = rr1_valid && rr1_ctrl.is_vector && !rr1_ctrl.is_load && !rr1_ctrl.is_store && !rr1_ctrl.is_tex && !rr1_ctrl.is_atomic;
        rr1_is_gfx     = rr1_valid && (rr1_ctrl.is_tex || rr1_ctrl.is_gfx);
    end

    // Slot1 scalar operand capture: only one scalar operand is supported in the dual-issue lane.
    // Select the operand based on op type (rs1 for GFX descriptor pointers, rs2 for TEX sampler handles).
    always_comb begin
        if (rr1_valid && rr1_ctrl.is_gfx && rr1_ctrl.uses_rs1 && (rr1_ctrl.rs1_class == CLASS_SCALAR)) rr1_scalar_raddr = rr1_ctrl.rs1;
        else if (rr1_valid && rr1_ctrl.is_tex && rr1_ctrl.uses_rs2 && (rr1_ctrl.rs2_class == CLASS_SCALAR)) rr1_scalar_raddr = rr1_ctrl.rs2;
        else if (rr1_valid && rr1_ctrl.uses_rs2 && (rr1_ctrl.rs2_class == CLASS_SCALAR)) rr1_scalar_raddr = rr1_ctrl.rs2;
        else if (rr1_valid && rr1_ctrl.uses_rs1 && (rr1_ctrl.rs1_class == CLASS_SCALAR)) rr1_scalar_raddr = rr1_ctrl.rs1;
        else rr1_scalar_raddr = 5'd0;
    end

    // EX stage registers (scalar/fp/lsu path only)
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            ex_valid       <= 1'b0;
            ex_ctrl        <= '0;
            ex_pc          <= 32'h0;
            ex_op_a        <= 32'h0;
            ex_op_b        <= 32'h0;
            ex_fp_scalar   <= 32'h0;
            ex_fp_a        <= 16'h0;
            ex_fp_b        <= 16'h0;
            ex_vec_a       <= '0;
            ex_vec_b       <= '0;
            ex_pred_taken  <= 1'b0;
            ex_pred_target <= 32'h0;
        end else if (ex_redirect_valid) begin
            // On a redirect (mispredict), flush younger ops behind EX.
            // Important: do not allow the current RR op to advance into EX on this same edge.
            ex_valid       <= 1'b0;
            ex_ctrl        <= '0;
            ex_pc          <= 32'h0;
            ex_op_a        <= 32'h0;
            ex_op_b        <= 32'h0;
            ex_fp_scalar   <= 32'h0;
            ex_fp_a        <= 16'h0;
            ex_fp_b        <= 16'h0;
            ex_vec_a       <= '0;
            ex_vec_b       <= '0;
            ex_pred_taken  <= 1'b0;
            ex_pred_target <= 32'h0;
        end else if (!stall_pipe) begin
            ex_valid        <= rr_valid && !rr_is_vec_alu && !rr_is_gfx;
            ex_ctrl         <= (rr_is_vec_alu || rr_is_gfx) ? '0 : rr_ctrl;
            ex_pc           <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : rr_pc;
            ex_op_a         <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : s_rdata_a;
            ex_op_b         <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : s_rdata_b;
            ex_fp_scalar    <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : s_rdata_b;
            ex_mask_scalar  <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : s_rdata_b;
            ex_fp_a         <= (rr_is_vec_alu || rr_is_gfx) ? 16'h0 : f_rdata_a;
            ex_fp_b         <= (rr_is_vec_alu || rr_is_gfx) ? 16'h0 : f_rdata_b;
            ex_vec_a        <= (rr_is_vec_alu || rr_is_gfx) ? '0 : v_rdata_a;
            ex_vec_b        <= (rr_is_vec_alu || rr_is_gfx) ? '0 : v_rdata_b;
            ex_pred_taken   <= (rr_is_vec_alu || rr_is_gfx) ? 1'b0 : rr_pred_taken;
            ex_pred_target  <= (rr_is_vec_alu || rr_is_gfx) ? 32'h0 : rr_pred_target;
        end
    end

    // ---------------------------------------------------------------------
    // EX-stage scalar forwarding (from LSU/MEM/WB) + RR gfx/tex scalar forwarding
    // ---------------------------------------------------------------------
    wire fwd_ex_valid = ex_valid && ex_ctrl.uses_rd && !ex_ctrl.is_load && !ex_ctrl.is_vector
                     && !ex_ctrl.rd_is_vec && !ex_ctrl.rd_is_fp && !ex_ctrl.is_scalar_fp
                     && !ex_ctrl.is_system && (ex_ctrl.rd != 5'd0);
    wire [4:0]  fwd_ex_rd   = ex_ctrl.rd;
    wire [31:0] fwd_ex_data = ex_scalar_res;

    wire fwd_mem_valid = mem_valid && mem_ctrl.uses_rd && !mem_ctrl.is_load && !mem_ctrl.is_vector
                      && !mem_ctrl.rd_is_vec && !mem_ctrl.rd_is_fp && !mem_ctrl.is_scalar_fp
                      && (mem_ctrl.rd != 5'd0);
    wire [4:0]  fwd_mem_rd   = mem_ctrl.rd;
    wire [31:0] fwd_mem_data = mem_scalar_res;

    wire fwd_wb_valid = wb_valid && wb_ctrl.uses_rd && !wb_ctrl.is_load && !wb_ctrl.is_vector
                     && !wb_ctrl.rd_is_vec && !wb_ctrl.rd_is_fp && !wb_ctrl.is_scalar_fp
                     && (wb_ctrl.rd != 5'd0);
    wire [4:0]  fwd_wb_rd   = wb_ctrl.rd;
    wire [31:0] fwd_wb_data = wb_scalar_res;

    wire fwd_lsu_valid = lsu_wb_valid && !lsu_wb_is_vector && (lsu_wb_rd != 5'd0);
    wire [4:0]  fwd_lsu_rd   = lsu_wb_rd;
    wire [31:0] fwd_lsu_data = lsu_wb_data[31:0];

    logic [31:0] ex_op_a_fwd;
    logic [31:0] ex_op_b_fwd;

    always_comb begin
        ex_op_a_fwd = ex_op_a;
        if (ex_ctrl.uses_rs1 && (ex_ctrl.rs1_class == CLASS_SCALAR) && (ex_ctrl.rs1 != 5'd0)) begin
            if (fwd_lsu_valid && (fwd_lsu_rd == ex_ctrl.rs1)) ex_op_a_fwd = fwd_lsu_data;
            else if (fwd_mem_valid && (fwd_mem_rd == ex_ctrl.rs1)) ex_op_a_fwd = fwd_mem_data;
            else if (fwd_wb_valid && (fwd_wb_rd == ex_ctrl.rs1)) ex_op_a_fwd = fwd_wb_data;
        end

        ex_op_b_fwd = ex_op_b;
        if (ex_ctrl.uses_rs2 && (ex_ctrl.rs2_class == CLASS_SCALAR) && (ex_ctrl.rs2 != 5'd0)) begin
            if (fwd_lsu_valid && (fwd_lsu_rd == ex_ctrl.rs2)) ex_op_b_fwd = fwd_lsu_data;
            else if (fwd_mem_valid && (fwd_mem_rd == ex_ctrl.rs2)) ex_op_b_fwd = fwd_mem_data;
            else if (fwd_wb_valid && (fwd_wb_rd == ex_ctrl.rs2)) ex_op_b_fwd = fwd_wb_data;
        end
    end

    function automatic logic [31:0] scalar_fwd_data(input logic [4:0] r, input logic [31:0] rf_val);
        logic [31:0] d;
        begin
            d = rf_val;
            if (fwd_ex_valid && (fwd_ex_rd == r)) d = fwd_ex_data;
            else if (fwd_lsu_valid && (fwd_lsu_rd == r)) d = fwd_lsu_data;
            else if (fwd_mem_valid && (fwd_mem_rd == r)) d = fwd_mem_data;
            else if (fwd_wb_valid && (fwd_wb_rd == r)) d = fwd_wb_data;
            scalar_fwd_data = d;
        end
    endfunction

    always_comb begin
        s_rdata_a_gfx = s_rdata_a;
        s_rdata_b_gfx = s_rdata_b;
        s_rdata_c_gfx = s_rdata_c;
        s_rdata_b_vec = s_rdata_b;
        s_rdata_c_vec = s_rdata_c;

        if (rr_is_gfx && rr_ctrl.uses_rs1 && (rr_ctrl.rs1_class == CLASS_SCALAR) && (rr_ctrl.rs1 != 5'd0)) begin
            s_rdata_a_gfx = scalar_fwd_data(rr_ctrl.rs1, s_rdata_a);
        end
        if (rr_is_gfx && rr_ctrl.uses_rs2 && (rr_ctrl.rs2_class == CLASS_SCALAR) && (rr_ctrl.rs2 != 5'd0)) begin
            s_rdata_b_gfx = scalar_fwd_data(rr_ctrl.rs2, s_rdata_b);
        end
        if (rr1_valid && (rr1_ctrl.is_gfx || rr1_ctrl.is_tex) && (rr1_scalar_raddr != 5'd0)) begin
            s_rdata_c_gfx = scalar_fwd_data(rr1_scalar_raddr, s_rdata_c);
        end

        if (rr_is_vec_alu && rr_ctrl.uses_rs2 && (rr_ctrl.rs2_class == CLASS_SCALAR) && (rr_ctrl.rs2 != 5'd0)) begin
            s_rdata_b_vec = scalar_fwd_data(rr_ctrl.rs2, s_rdata_b);
        end
        if (rr1_is_vec_alu && (rr1_scalar_raddr != 5'd0)) begin
            s_rdata_c_vec = scalar_fwd_data(rr1_scalar_raddr, s_rdata_c);
        end
    end

    // CSR access control (evaluated in EX stage)
    assign csr_addr_ex  = ex_ctrl.imm[11:0];
    assign csr_wdata_ex = ex_op_a_fwd; // rs1 value (forwarded)
    assign csr_en       = ex_valid && ex_ctrl.is_system && (ex_ctrl.funct3 == 3'b001 || ex_ctrl.funct3 == 3'b010);
    assign csr_csrrs    = (ex_ctrl.funct3 == 3'b010);

    // Address generation for loads/stores/texture
    agu u_agu (
        .base_addr(ex_op_a_fwd),
        .offset(ex_ctrl.imm),
        .effective_addr(ex_addr)
    );

    logic [31:0] ex_alu_res;
    logic        ex_alu_branch_taken_unused;

    logic        ex_is_link;
    logic [31:0] ex_link_value;

    // Integer ALU result path
    alu_scalar u_alu_scalar_int (
        .op_a(ex_op_a_fwd),
        .op_b((ex_ctrl.is_load || ex_ctrl.is_store || !ex_ctrl.uses_rs2) ? ex_ctrl.imm : ex_op_b_fwd),
        .funct3(ex_ctrl.funct3),
        .is_sub(ex_ctrl.funct7 == 7'b0100000),
        .funct7(ex_ctrl.funct7),
        .opcode(OP_INT),
        .result(ex_alu_res),
        .branch_taken(ex_alu_branch_taken_unused)
    );

    // Centralized branch/jump decision + target + link value
    branch_unit u_branch_unit (
        .valid(ex_valid),
        .ctrl(ex_ctrl),
        .pc(ex_pc),
        .rs1_val(ex_op_a_fwd),
        .rs2_val(ex_op_b_fwd),
        .taken(ex_cf_taken),
        .target(ex_cf_target),
        .is_link(ex_is_link),
        .link_value(ex_link_value)
    );

    wire [31:0] ex_fallthrough = ex_pc + 32'd4;

    // Redirect only on mispredict. (Correct predictions do not flush the pipe.)
    always_comb begin
        ex_redirect_valid  = 1'b0;
        ex_redirect_target = 32'h0;

        if (ex_valid && ex_ctrl.is_branch) begin
            if (ex_pred_taken) begin
                if (!ex_cf_taken) begin
                    ex_redirect_valid  = 1'b1;
                    ex_redirect_target = ex_fallthrough;
                end else if (ex_cf_target != ex_pred_target) begin
                    ex_redirect_valid  = 1'b1;
                    ex_redirect_target = ex_cf_target;
                end
            end else if (ex_cf_taken) begin
                ex_redirect_valid  = 1'b1;
                ex_redirect_target = ex_cf_target;
            end
        end
    end

    // Scalar writeback value (JAL/JALR link = PC+4)
    always_comb begin
        if (ex_ctrl.is_lui) ex_scalar_res = ex_ctrl.imm;
        else if (ex_is_link) ex_scalar_res = ex_link_value;
        else ex_scalar_res = ex_alu_res;
    end

    // FP writeback is always ready for FP-reg writes. FP->scalar conversions can be backpressured.
    wire fp_wb_ready = (!fp_scalar_wb_valid) || fp_scalar_ready;

    fp_alu u_fp_alu (
        .clk(clk),
        .rst_n(rst_n),
        .valid(ex_valid && ex_ctrl.is_scalar_fp && !stall_any),
        .in_ready(fp_in_ready),
        .funct3(ex_ctrl.funct3),
        .src_a(ex_fp_a),
        .src_b(ex_fp_b),
        .scalar_src(ex_op_a_fwd),
        .scalar_src_is_x0(ex_ctrl.uses_rs1 && (ex_ctrl.rs1 == 5'd0)),
        .src_c(ex_op_b_fwd[15:0]),
        .rd_idx(ex_ctrl.rd),
        .wb_ready(fp_wb_ready),
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
    // VALU results may need to be buffered when the vector WB port is busy (pending/LSU/TEX).
    // Prevent issuing VALU vector-producing ops when the vwbq FIFO can't accept the result.
    wire valuv_dest_is_scalar = (vq[vq_head].ctrl.rd_class == CLASS_SCALAR);
    wire vwbq_can_accept_one  = (vwbq_count < VWBQ_DEPTH);
    wire vwbq_can_accept_two  = (vwbq_count < (VWBQ_DEPTH-1));
    wire valuv_issue_allow    = valuv_dest_is_scalar ? 1'b1 : (gp_wb_valid ? vwbq_can_accept_two : vwbq_can_accept_one);

    wire valuv_issue_valid = vq_valid[vq_head] && !stall_any && valuv_issue_allow;
    wire valuv_fire        = valuv_issue_valid && valuv_ready;

    wire push_vec_alu0     = rr_is_vec_alu  && !stall_pipe && !vector_queue_full;
    wire push_vec_alu1     = rr1_is_vec_alu && !stall_pipe && !vector_queue_full;
    wire push_vec_alu      = push_vec_alu0 || push_vec_alu1;

    wire [127:0] push_vec_src_a = push_vec_alu1 ? v_rdata_a : v_rdata_a;
    wire [127:0] push_vec_src_b = push_vec_alu1 ? v_rdata_b : v_rdata_b;
    wire [31:0]  push_vec_scalar = push_vec_alu1 ? s_rdata_c_vec : s_rdata_b_vec;
    decode_ctrl_t push_vec_ctrl;
    assign push_vec_ctrl = push_vec_alu1 ? rr1_ctrl : rr_ctrl;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            vq_valid <= '0;
            vq_head  <= '0;
            vq_tail  <= '0;
            vq_count <= '0;
        end else if (ex_redirect_valid) begin
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

    // GFX descriptor cache miss signals
    logic         gfxd_miss_req_valid;
    logic [31:0]  gfxd_miss_req_addr;
    logic         gfxd_miss_req_ready;
    logic         gfxd_miss_resp_valid;
    logic [127:0] gfxd_miss_resp_data;
    
    texture_cache #(
        .LINE_BYTES(TEX_CACHE_LINE_BYTES),
        .LINES(TEX_CACHE_LINES)
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

    // Separate cache for gfx descriptors/geometry fetches
    texture_cache #(
        .LINE_BYTES(TEX_CACHE_LINE_BYTES),
        .LINES(TEX_CACHE_LINES)
    ) u_gfx_desc_cache (
        .clk(clk),
        .rst_n(rst_n),
        .req_valid(gfxd_req_valid),
        .req_addr(gfxd_req_addr),
        .req_rd(gfxd_req_rd),
        .req_ready(gfxd_req_ready),
        .resp_valid(gfxd_resp_valid),
        .resp_data(gfxd_resp_data),
        .resp_rd(gfxd_resp_rd),
        // Miss interface
        .miss_req_valid(gfxd_miss_req_valid),
        .miss_req_addr(gfxd_miss_req_addr),
        .miss_req_ready(gfxd_miss_req_ready),
        .miss_resp_valid(gfxd_miss_resp_valid),
        .miss_resp_data(gfxd_miss_resp_data)
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
        .vmask_out(csr_vmask),

        .cmd_enable(csr_cmd_enable),
        .cmd_ring_base(csr_cmd_ring_base),
        .cmd_ring_size_bytes(csr_cmd_ring_size_bytes),
        .cmd_cons_ptr_bytes(csr_cmd_cons_ptr_bytes),
        .cmd_completion_base(csr_cmd_completion_base)
    );

    // Internal LSU global port (drives external global data port)
    logic        lsu_global_req_valid;
    logic        lsu_global_req_is_load;
    logic [31:0] lsu_global_req_addr;
    logic [31:0] lsu_global_req_wdata;
    logic [4:0]  lsu_global_req_rd;
    logic        lsu_global_req_ready;
    logic        lsu_global_resp_valid;
    logic [4:0]  lsu_global_resp_rd;
    logic [31:0] lsu_global_resp_data;

    // Mailbox sideband
    logic        lsu_mailbox_tx_valid;
    logic [15:0] lsu_mailbox_tx_dest;
    logic [31:0] lsu_mailbox_tx_data;
    logic        lsu_mailbox_tx_prio;
    logic        lsu_mailbox_tx_eop;
    logic [3:0]  lsu_mailbox_tx_opcode;
    logic        lsu_mailbox_rd_valid;
    logic [15:0] lsu_mailbox_rd_dest;
    logic        lsu_mailbox_rd_prio;
    logic [3:0]  lsu_mailbox_rd_opcode;
    logic        lsu_mailbox_rd_resp_valid;
    logic        lsu_mailbox_rd_resp_ready;
    logic [31:0] lsu_mailbox_rd_resp_data;
    mailbox_tag_t lsu_mailbox_rd_resp_tag;
    logic        mailbox_tx_ready_int;
    logic        mailbox_rd_ready_int;
    logic        mailbox_rd_resp_ready_int;
    logic        ep_tx_ready;

    logic        ep_rx_valid;
    logic [31:0] ep_rx_data;
    mailbox_header_t ep_rx_hdr;
    logic [NODE_ID_WIDTH-1:0] ep_rx_dest_id;
    logic        ep_rx_irq;
    logic        ep_rx_err;
    logic        ep_rx_ready_int;
    logic        rd_pending;

    // RX pop-on-read logic (single outstanding)
    assign ep_rx_ready_int = lsu_mailbox_rd_valid && mailbox_rd_ready_int && (!rd_pending);
    assign mailbox_rd_resp_ready_int = lsu_mailbox_rd_resp_ready;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rd_pending <= 1'b0;
            lsu_mailbox_rd_resp_valid <= 1'b0;
            lsu_mailbox_rd_resp_data  <= 32'h0;
            lsu_mailbox_rd_resp_tag   <= '0;
        end else begin
            if (lsu_mailbox_rd_resp_valid && mailbox_rd_resp_ready_int) begin
                lsu_mailbox_rd_resp_valid <= 1'b0;
                rd_pending <= 1'b0;
            end

            if (lsu_mailbox_rd_valid && mailbox_rd_ready_int && !rd_pending) begin
                rd_pending <= 1'b1;
                lsu_mailbox_rd_resp_valid <= 1'b1;
                lsu_mailbox_rd_resp_data  <= ep_rx_valid ? ep_rx_data : 32'hDEADBEEF;
                lsu_mailbox_rd_resp_tag   <= '0;
            end
        end
    end

    // Split-path LSU handles local vs global and blocking scalar behavior
    lsu #(
        .WMB_ENTRIES(ROP_WCACHE_ENTRIES),
        .MAILBOX_ENABLE(MAILBOX_ENABLE)
    ) u_lsu (
        .clk(clk),
        .rst_n(rst_n),
        .valid_in(mem_valid && (mem_ctrl.is_load || mem_ctrl.is_store || mem_ctrl.is_atomic)),
        .is_vector(mem_ctrl.is_vector),
        .is_store(mem_ctrl.is_store),
        .scalar_funct3(mem_ctrl.funct3),
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

        // GFX Descriptor Cache Miss Interface
        .gfx_req_valid(gfxd_miss_req_valid),
        .gfx_req_addr(gfxd_miss_req_addr),
        .gfx_req_ready(gfxd_miss_req_ready),
        .gfx_resp_valid(gfxd_miss_resp_valid),
        .gfx_resp_data(gfxd_miss_resp_data),

        .local_req_valid(local_req_valid),
        .local_we(local_we),
        .local_req_is_vector(local_req_is_vector),
        .local_addr(local_addr),
        .local_wdata(local_wdata),
        .local_bank_sel(local_bank_sel),
        .local_rdata(local_rdata),
        
        // Unified 32-bit Global Interface (arbitrated externally)
        .global_req_valid(lsu_global_req_valid),
        .global_req_is_load(lsu_global_req_is_load),
        .global_req_addr(lsu_global_req_addr),
        .global_req_wdata(lsu_global_req_wdata),
        .global_req_rd(lsu_global_req_rd),
        .global_req_ready(lsu_global_req_ready),
        
        .global_resp_valid(lsu_global_resp_valid),
        .global_resp_rd(lsu_global_resp_rd),
        .global_resp_data(lsu_global_resp_data),

        .mailbox_tx_valid(lsu_mailbox_tx_valid),
        .mailbox_tx_dest(lsu_mailbox_tx_dest),
        .mailbox_tx_data(lsu_mailbox_tx_data),
        .mailbox_tx_prio(lsu_mailbox_tx_prio),
        .mailbox_tx_eop(lsu_mailbox_tx_eop),
        .mailbox_tx_opcode(lsu_mailbox_tx_opcode),
        .mailbox_tx_ready(mailbox_tx_ready_int),
        .mailbox_rd_valid(lsu_mailbox_rd_valid),
        .mailbox_rd_ready(mailbox_rd_ready_int),
        .mailbox_rd_dest(lsu_mailbox_rd_dest),
        .mailbox_rd_prio(lsu_mailbox_rd_prio),
        .mailbox_rd_opcode(lsu_mailbox_rd_opcode),
        .mailbox_rd_resp_valid(lsu_mailbox_rd_resp_valid),
        .mailbox_rd_resp_ready(lsu_mailbox_rd_resp_ready),
        .mailbox_rd_resp_data(lsu_mailbox_rd_resp_data),
        .mailbox_rd_resp_tag(lsu_mailbox_rd_resp_tag),
        
        .wb_valid(lsu_wb_valid),
        .wb_is_vector(lsu_wb_is_vector),
        .wb_reg_idx(lsu_wb_rd),
        .wb_data(lsu_wb_data)
    );

        // Mailbox endpoint integration (optional)
        generate
            if (MAILBOX_ENABLE) begin : g_mailbox_ep
                assign mailbox_tx_ready_int = ep_tx_ready;

                mailbox_endpoint_stream #(
                    .SRC_ID({8'h00, MAILBOX_SRC_ID})
                ) u_mailbox_ep (
                    .clk(clk),
                    .rst_n(rst_n),

                    .tx_valid(lsu_mailbox_tx_valid),
                    .tx_ready(ep_tx_ready),
                    .tx_data(lsu_mailbox_tx_data),
                    .tx_dest_id(lsu_mailbox_tx_dest),
                    .tx_opcode(lsu_mailbox_tx_opcode),
                    .tx_prio({1'b0, lsu_mailbox_tx_prio}),
                    .tx_eop(lsu_mailbox_tx_eop),
                    .tx_debug(1'b0),

                    .rx_valid(ep_rx_valid),
                    .rx_ready(ep_rx_ready_int),
                    .rx_data(ep_rx_data),
                    .rx_hdr(ep_rx_hdr),
                    .rx_irq(ep_rx_irq),
                    .rx_error(ep_rx_err),
                    .rx_dest_id(ep_rx_dest_id),

                    .link_tx_valid(mailbox_tx_valid),
                    .link_tx_ready(mailbox_tx_ready),
                    .link_tx_data(mailbox_tx_data),
                    .link_tx_dest_id(mailbox_tx_dest_id),

                    .link_rx_valid(mailbox_rx_valid),
                    .link_rx_ready(mailbox_rx_ready),
                    .link_rx_data(mailbox_rx_data),
                    .link_rx_dest_id(mailbox_rx_dest_id)
                );
            end else begin : g_mailbox_tieoff
                assign mailbox_tx_ready_int = 1'b1;
                assign mailbox_rd_ready_int = 1'b1;
                assign lsu_mailbox_rd_resp_valid = 1'b0;
                assign lsu_mailbox_rd_resp_data  = 32'h0;
                assign lsu_mailbox_rd_resp_tag   = '0;
                assign ep_tx_ready = 1'b1;
                assign mailbox_tx_valid  = 1'b0;
                assign mailbox_tx_data   = '0;
                assign mailbox_tx_dest_id = '0;
                assign mailbox_rx_ready  = 1'b1;
                assign ep_rx_valid      = 1'b0;
                assign ep_rx_data       = 32'h0;
                assign ep_rx_hdr        = '0;
                assign ep_rx_dest_id    = '0;
                assign ep_rx_irq        = 1'b0;
                assign ep_rx_err        = 1'b0;
            end
        endgenerate

    // ---------------------------------------------------------------------
    // Global memory interface: direct LSU -> external port
    // ---------------------------------------------------------------------
    assign data_req_valid   = lsu_global_req_valid;
    assign data_req_is_load = lsu_global_req_is_load;
    assign data_req_addr    = lsu_global_req_addr;
    assign data_req_wdata   = lsu_global_req_wdata;
    assign data_req_rd      = lsu_global_req_rd;

    assign lsu_global_req_ready  = data_req_ready;
    assign lsu_global_resp_valid = data_resp_valid;
    assign lsu_global_resp_rd    = data_resp_rd;
    assign lsu_global_resp_data  = data_resp_data;

    // ------------------------------------------------------------------------

    // MEM stage registers
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            mem_valid        <= 1'b0;
            mem_ctrl         <= '0;
            mem_scalar_res   <= 32'h0;
            mem_fp_res       <= 16'h0;
            mem_pc           <= 32'h0;
            mem_addr         <= 32'h0;
            mem_vec_wdata    <= '0;
            mem_scalar_wdata <= 32'h0;
        end else if (!stall_pipe) begin
            mem_valid        <= ex_valid;
            mem_ctrl         <= ex_ctrl;
            mem_scalar_res   <= ex_ctrl.is_system ? csr_rdata : ex_scalar_res;
            mem_fp_res       <= ex_fp_res;
            mem_pc           <= ex_pc;
            mem_addr         <= ex_addr;
            mem_vec_wdata    <= ex_vec_b;
            mem_scalar_wdata <= ex_op_b_fwd;
        end else if (lsu_wb_valid && mem_valid && mem_ctrl.is_load && !mem_ctrl.is_vector) begin
            // Drop the in-flight scalar load once its response returns to avoid re-issuing it
            mem_valid        <= 1'b0;
            mem_ctrl         <= '0;
            mem_scalar_res   <= 32'h0;
            mem_fp_res       <= 16'h0;
            mem_pc           <= 32'h0;
            mem_addr         <= 32'h0;
            mem_vec_wdata    <= '0;
            mem_scalar_wdata <= 32'h0;
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
    assign lsu_scalar_wb = lsu_wb_valid && !lsu_wb_is_vector;
    assign lsu_vector_wb = lsu_wb_valid && lsu_wb_is_vector;
    assign alu_scalar_wb = wb_valid && wb_ctrl.uses_rd && !wb_ctrl.rd_is_vec && !wb_ctrl.rd_is_fp && !wb_ctrl.is_load && !wb_ctrl.is_vector && !wb_ctrl.is_scalar_fp;
    assign fp_scalar_wb  = fp_scalar_wb_valid;      // FP path now 1-cycle registered
    // Mask X on VALU valid to avoid stalling early scalar writebacks
    assign valuv_wb_valid_masked = (valuv_wb_valid === 1'b1);
    assign valuv_scalar_wb = valuv_wb_valid_masked && valuv_wb_is_scalar;

    // Deterministic scalar writeback priority: LSU > Pending > FP scalar > VALU scalar > ALU scalar
    logic scalar_wb_from_pending;
    logic scalar_wb_from_lsu;
    logic scalar_wb_from_fp;
    logic scalar_wb_from_valu;
    logic scalar_wb_from_alu;
    scalar_wb_arb_pending2 u_scalar_wb_arb_pending2 (
        .clk(clk),
        .rst_n(rst_n),

        .lsu_valid(lsu_scalar_wb),
        .lsu_rd(lsu_wb_rd),
        .lsu_data(lsu_wb_data[31:0]),

        .fp_valid(fp_scalar_wb),
        .fp_rd(fp_scalar_wb_rd),
        .fp_data(fp_scalar_wb_data),
        .fp_err_overflow(fp_wb_err_overflow),
        .fp_err_invalid(fp_wb_err_invalid),
        .fp_ready(fp_scalar_ready),

        .valuv_valid(valuv_scalar_wb),
        .valuv_rd(valuv_wb_rd),
        .valuv_data(valuv_wb_data[31:0]),
        .valuv_err_overflow(valuv_err_overflow),
        .valuv_err_invalid(valuv_err_invalid),
        .valuv_ready(valuv_scalar_ready),

        .alu_valid(alu_scalar_wb),
        .alu_rd(wb_ctrl.rd),
        .alu_data(wb_scalar_res),
        .alu_ready(alu_scalar_ready),

        .s_we(s_we),
        .s_waddr(s_waddr),
        .s_wdata(s_wdata),

        .wb_from_fp(s_commit_from_fp),
        .wb_from_valu(s_commit_from_valu),
        .wb_err_overflow(s_commit_err_overflow),
        .wb_err_invalid(s_commit_err_invalid),

        .dbg_from_pending(scalar_wb_from_pending),
        .dbg_from_lsu(scalar_wb_from_lsu),
        .dbg_from_fp(scalar_wb_from_fp),
        .dbg_from_valu(scalar_wb_from_valu),
        .dbg_from_alu(scalar_wb_from_alu)
    );

    // FP ALU is 1-cycle latency (registered), separate write port:
    wire fp_wb_fp = fp_wb_valid && !fp_scalar_wb_valid;
    assign f_we    = fp_wb_fp;
    assign f_waddr = fp_wb_rd;
    assign f_wdata = fp_alu_wb_data;

    // Error flags are pulsed with the committing writeback beat (not merely "produced" valid).
    // FP ops that write FP regs commit with f_we. FP->scalar conversions commit with s_we when sourced from FP.
    assign err_fp_overflow = (f_we && fp_wb_err_overflow) || (s_we && s_commit_from_fp && s_commit_err_overflow);
    assign err_fp_invalid  = (f_we && fp_wb_err_invalid)  || (s_we && s_commit_from_fp && s_commit_err_invalid);
    // Vector errors are only meaningful for VALU vector ops; pulse when that result actually commits to vfile.
    assign err_vec_overflow = v_we && v_commit_from_valuv && v_commit_err_overflow;
    assign err_vec_invalid  = v_we && v_commit_from_valuv && v_commit_err_invalid;

    wire valuv_vector_wb = valuv_wb_valid_masked && !valuv_wb_is_scalar;

    // Vector writeback arbitration: Pending FIFO > LSU > TEX > VALU
    wire v_take_lsu     = lsu_vector_wb;
    wire v_take_pending = (!lsu_vector_wb) && (vwbq_count != '0);
    wire v_take_gp      = (!lsu_vector_wb) && (vwbq_count == '0) && gp_wb_valid;
    wire v_take_valuv   = (!lsu_vector_wb) && (vwbq_count == '0) && !gp_wb_valid && valuv_vector_wb;

    assign v_we    = v_take_lsu || v_take_pending || v_take_gp || v_take_valuv;
    assign v_waddr = v_take_lsu     ? lsu_wb_rd :
                     v_take_pending ? vwbq_rd[vwbq_head] :
                     v_take_gp      ? gp_wb_rd :
                                      valuv_wb_rd;
    assign v_wdata = v_take_lsu     ? lsu_wb_data :
                     v_take_pending ? vwbq_data[vwbq_head] :
                     v_take_gp      ? gp_wb_data :
                                      valuv_wb_data;

    // Commit-beat metadata for CSR error capture (only VALU vector ops produce FP-ish errors).
    assign v_commit_from_valuv   = v_take_valuv ? 1'b1 : (v_take_pending ? vwbq_from_valuv[vwbq_head] : 1'b0);
    assign v_commit_err_overflow = v_take_valuv ? valuv_err_overflow : (v_take_pending ? vwbq_err_ovf[vwbq_head] : 1'b0);
    assign v_commit_err_invalid  = v_take_valuv ? valuv_err_invalid  : (v_take_pending ? vwbq_err_inv[vwbq_head] : 1'b0);

    // Enqueue non-selected vector writebacks (TEX preferred over VALU ordering in the FIFO)
    wire v_push_gp    = gp_wb_valid    && !(v_take_gp);
    wire v_push_valuv = valuv_vector_wb && !(v_take_valuv);

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            vwbq_count <= '0;
            vwbq_head  <= '0;
            vwbq_tail  <= '0;
        end else begin
            logic [$clog2(VWBQ_DEPTH+1)-1:0] cnt;
            logic [$clog2(VWBQ_DEPTH)-1:0]   head;
            logic [$clog2(VWBQ_DEPTH)-1:0]   tail;

            cnt  = vwbq_count;
            head = vwbq_head;
            tail = vwbq_tail;

            // Pop when pending is selected
            if (v_take_pending) begin
                head = head + 1'b1;
                cnt  = cnt - 1'b1;
            end

            // Push TEX (if not selected)
            if (v_push_gp && (cnt < VWBQ_DEPTH)) begin
                vwbq_rd[tail]   <= gp_wb_rd;
                vwbq_data[tail] <= gp_wb_data;
                vwbq_from_valuv[tail] <= 1'b0;
                vwbq_err_ovf[tail]    <= 1'b0;
                vwbq_err_inv[tail]    <= 1'b0;
                tail = tail + 1'b1;
                cnt  = cnt + 1'b1;
            end

            // Push VALU (if not selected)
            if (v_push_valuv && (cnt < VWBQ_DEPTH)) begin
                vwbq_rd[tail]   <= valuv_wb_rd;
                vwbq_data[tail] <= valuv_wb_data;
                vwbq_from_valuv[tail] <= 1'b1;
                vwbq_err_ovf[tail]    <= valuv_err_overflow;
                vwbq_err_inv[tail]    <= valuv_err_invalid;
                tail = tail + 1'b1;
                cnt  = cnt + 1'b1;
            end

            vwbq_count <= cnt;
            vwbq_head  <= head;
            vwbq_tail  <= tail;
        end
    end

endmodule
