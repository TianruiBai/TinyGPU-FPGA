`include "isa_pkg.sv"
module compute_unit_top #(
    parameter logic [31:0] CORE_ID     = 32'h0,
    parameter logic [31:0] TILE_OFFSET = 32'h0
)(
    input  logic        clk,
    input  logic        rst_n,
    // Instruction memory interface
    input  logic [31:0] inst_rdata,
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
    logic [31:0] if_inst;
    logic        if_valid;

    // ID stage
    decode_ctrl_t id_ctrl;
    decode_ctrl_t id_ctrl_dec;
    logic         id_valid;

    // RR stage
    decode_ctrl_t rr_ctrl;
    logic         rr_valid;

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
    logic         ex_branch_taken;
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
    logic lsu_stall;
    logic lsu_busy;
    logic stall_membar;
    logic stall_any;

    // Register files
    logic [31:0]  s_rdata_a, s_rdata_b;
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
    logic        csr_en;
    logic        csr_csrrs;
    logic [11:0] csr_addr_ex;
    logic [31:0] csr_wdata_ex;
    logic [31:0] csr_rdata;

    // Texture stall signal (wire declaration)
    wire tex_stall;

    // MEMBAR waits for LSU/texture traffic to drain before allowing forward progress
    wire rr_is_membar  = rr_valid  && rr_ctrl.is_system && (rr_ctrl.funct3 == 3'b000);
    wire ex_is_membar  = ex_valid  && ex_ctrl.is_system && (ex_ctrl.funct3 == 3'b000);
    wire mem_is_membar = mem_valid && mem_ctrl.is_system && (mem_ctrl.funct3 == 3'b000);

    assign stall_membar      = lsu_busy && (rr_is_membar || ex_is_membar || mem_is_membar);
    // Split stall: pipeline progression should only halt on structural stalls, not scoreboard hazards
    wire stall_pipe          = lsu_stall || tex_stall || stall_membar;
    wire stall_issue         = stall_pipe || stall_scoreboard;
    // During reset force stall low to avoid X-propagation into fetch/PC
    assign stall_any         = rst_n ? stall_pipe : 1'b0;

    // Fetch unit
    fetch_unit u_fetch (
        .clk(clk),
        .rst_n(rst_n),
        .stall(stall_issue),
        .branch_taken(ex_branch_taken),
        .branch_target(ex_scalar_res),
        .pc(if_pc),
        .inst_addr(inst_addr),
        .inst_rdata(inst_rdata),
        .inst_valid(if_valid),
        .inst(if_inst)
    );

    // ID stage registers
    decoder u_decoder (
        .inst(if_inst),
        .ctrl(id_ctrl_dec)
    );

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            id_valid <= 1'b0;
            id_ctrl  <= '0;
        end else if (ex_branch_taken) begin
            id_valid <= 1'b0; // Flush on branch taken
            id_ctrl  <= '0;
        end else if (!stall_issue) begin
            id_valid <= if_valid;
            id_ctrl  <= id_ctrl_dec;
        end
    end

    // Scoreboard issue check
    scoreboard u_scoreboard (
        .clk(clk),
        .rst_n(rst_n),
        .issue_valid(id_valid && !stall_pipe && !ex_branch_taken),
        .issue_rs1_valid(id_ctrl.uses_rs1),
        .issue_rs2_valid(id_ctrl.uses_rs2),
        .issue_rs1_class(id_ctrl.rs1_class),
        .issue_rs2_class(id_ctrl.rs2_class),
        .issue_rs1(id_ctrl.rs1),
        .issue_rs2(id_ctrl.rs2),
        .issue_rd_valid(id_ctrl.uses_rd),
        .issue_rd_class(id_ctrl.rd_class),
        .issue_rd(id_ctrl.rd),
        .stall(stall_scoreboard),
        .flush_rr(ex_branch_taken),
        .flush_rr_rd_class(rr_ctrl.rd_class),
        .flush_rr_rd_valid(rr_ctrl.uses_rd),
        .flush_rr_rd(rr_ctrl.rd),
        .wb_scalar_valid(s_we),
        .wb_scalar_rd(s_waddr),
        .wb_fp_valid(f_we),
        .wb_fp_rd(f_waddr),
        .wb_vec_valid(v_we),
        .wb_vec_rd(v_waddr)
    );

    // RR stage registers
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            rr_valid <= 1'b0;
            rr_ctrl  <= '0;
        end else if (ex_branch_taken) begin
             rr_valid <= 1'b0; // Flush on branch taken
             // rr_ctrl can hold garbage
        end else if (!stall_pipe) begin
            rr_valid <= id_valid & ~stall_scoreboard;
            rr_ctrl  <= id_ctrl;
        end
    end

    // Regfile instances
    regfile_scalar u_regfile_scalar (
        .clk(clk),
        .rst_n(rst_n),
        .raddr_a(rr_ctrl.rs1),
        .raddr_b(rr_ctrl.rs2),
        .rdata_a(s_rdata_a),
        .rdata_b(s_rdata_b),
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

    regfile_vector u_regfile_vector (
        .clk(clk),
        .rst_n(rst_n),
        .raddr_a(rr_ctrl.rs1),
        .raddr_b(rr_ctrl.rs2),
        .rdata_a(v_rdata_a),
        .rdata_b(v_rdata_b),
        .we(v_we),
        .waddr(v_waddr),
        .wdata(v_wdata)
    );

    // EX stage registers
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
            ex_valid        <= rr_valid;
            ex_ctrl         <= rr_ctrl;
            ex_op_a         <= s_rdata_a;
            ex_op_b         <= s_rdata_b;
            ex_fp_scalar    <= s_rdata_b;
            ex_mask_scalar  <= s_rdata_b;
            ex_fp_a         <= f_rdata_a;
            ex_fp_b         <= f_rdata_b;
            ex_vec_a        <= v_rdata_a;
            ex_vec_b        <= v_rdata_b;
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

    alu_vector u_alu_vector (
        .clk(clk),
        .rst_n(rst_n),
        .valid(ex_valid && ex_ctrl.is_vector && !ex_ctrl.is_load && !ex_ctrl.is_store && !ex_ctrl.is_tex && !stall_any),
        .funct6(ex_ctrl.funct7[6:1]),
        .funct3(ex_ctrl.funct3),
        .rd_idx(ex_ctrl.rd),
        .dest_is_scalar(ex_ctrl.rd_class == CLASS_SCALAR),
        .src_a(ex_vec_a),
        .src_b(ex_vec_b),
        .scalar_mask(ex_mask_scalar),
        .ready(valuv_ready),
        .wb_valid(valuv_wb_valid),
        .wb_rd(valuv_wb_rd),
        .wb_is_scalar(valuv_wb_is_scalar),
        .wb_data(valuv_wb_data),
        .wb_err_overflow(valuv_err_overflow),
        .wb_err_invalid(valuv_err_invalid)
    );

    // Texture Cache
    logic         tex_req_ready;
    logic         tex_wb_valid;
    logic [31:0]  tex_wb_data;
    logic [4:0]   tex_wb_rd;
    
    // Texture Miss signals
    logic         tex_miss_req_valid;
    logic [31:0]  tex_miss_req_addr;
    logic         tex_miss_req_ready;
    logic         tex_miss_resp_valid;
    logic [127:0] tex_miss_resp_data;

    // Texture stall logic
    assign tex_stall = ex_valid && ex_ctrl.is_tex && !tex_req_ready;
    
    texture_cache #(
        .LINE_BYTES(16),  // Match 128-bit global bus width
        .LINES(64)
    ) u_texture_cache (
        .clk(clk),
        .rst_n(rst_n),
        .req_valid(ex_valid && ex_ctrl.is_tex && !stall_any),
        .req_addr(ex_addr),
        .req_rd(ex_ctrl.rd),
        .req_ready(tex_req_ready),
        .resp_valid(tex_wb_valid),
        .resp_data(tex_wb_data),
        .resp_rd(tex_wb_rd),
        // Miss interface
        .miss_req_valid(tex_miss_req_valid),
        .miss_req_addr(tex_miss_req_addr),
        .miss_req_ready(tex_miss_req_ready),
        .miss_resp_valid(tex_miss_resp_valid),
        .miss_resp_data(tex_miss_resp_data)
    );

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
        .vstatus_out(csr_vstatus)
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
        .dest_reg_idx(mem_ctrl.rd),
        .stall_pipeline(lsu_stall),
        .busy(lsu_busy),
        
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
                    || ((tex_wb_valid || valuv_vector_wb) && !lsu_vector_wb && !pending_vector_wb);

    // Vector writeback: priority Pending > LSU > TEX > VALU; capture collisions into pending
    assign v_we    = vector_fire;
    assign v_waddr = vector_wb_from_pending ? pending_vector_rd :
                     (lsu_vector_wb ? lsu_wb_rd :
                     (tex_wb_valid ? tex_wb_rd : valuv_wb_rd));

    assign v_wdata = vector_wb_from_pending ? pending_vector_data :
                     (lsu_vector_wb ? lsu_wb_data :
                     (tex_wb_valid ? {96'h0, tex_wb_data} : valuv_wb_data));

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
            if ((tex_wb_valid || valuv_wb_valid) && lsu_vector_wb && !pending_vector_wb) begin
                pending_vector_wb   <= 1'b1;
                pending_vector_rd   <= tex_wb_valid ? tex_wb_rd   : valuv_wb_rd;
                pending_vector_data <= tex_wb_valid ? {96'h0, tex_wb_data} : valuv_wb_data;
            end
        end
    end
endmodule
