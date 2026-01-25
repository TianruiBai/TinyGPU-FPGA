// RISC-V Control Processor (RV32IMA_Zicsr) - simple 5-stage, in-order
module mcu_core #(
    parameter bit MAILBOX_ENABLE = 1'b0,
    parameter logic [7:0] MAILBOX_SRC_ID = 8'h00
) (
    input  logic        clk,
    input  logic        rst_n,

    // Instruction memory (TCM)
    output logic [31:0] imem_addr,
    input  logic [31:0] imem_rdata,

    // Data memory / MMIO
    output logic        dmem_req_valid,
    output logic        dmem_req_write,
    output logic [31:0] dmem_req_addr,
    output logic [31:0] dmem_req_wdata,
    output logic [3:0]  dmem_req_wstrb,
    input  logic        dmem_resp_valid,
    input  logic [31:0] dmem_resp_rdata,

    // Mailbox AXI‑MailboxFabric stream link
    output logic                           mailbox_tx_valid,
    input  logic                           mailbox_tx_ready,
    output mailbox_pkg::mailbox_flit_t     mailbox_tx_data,
    output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_tx_dest_id,

    input  logic                           mailbox_rx_valid,
    output logic                           mailbox_rx_ready,
    input  mailbox_pkg::mailbox_flit_t     mailbox_rx_data,
    input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_rx_dest_id,

    // Interrupts
    input  logic        irq_cmdq_nonempty,
    input  logic        irq_fence_reached,
    input  logic        irq_abort_done,
    input  logic        irq_display_vblank,
    input  logic        irq_mpu_fault,

    // Debug/trace
    output logic [31:0] dbg_pc,
    output logic [31:0] dbg_insn
);

    import mcu_isa_pkg::*;
    import mailbox_pkg::*;

    // -----------------------------
    // Register file
    // -----------------------------
    logic [31:0] rf_rdata1, rf_rdata2;

    // -----------------------------
    // CSR interface
    // -----------------------------
    logic [11:0] csr_raddr, csr_waddr;
    logic [31:0] csr_rdata, csr_wdata;
    logic        csr_we;
    logic        csr_trap_we;
    logic [31:0] csr_trap_mepc, csr_trap_mcause, csr_trap_mtval;
    /* verilator lint_off UNUSEDSIGNAL */
    logic [31:0] csr_mstatus, csr_mtvec;
    /* verilator lint_on UNUSEDSIGNAL */

    // -----------------------------
    // Fetch (no compressed support)
    // -----------------------------
    logic [31:0] pc;
    logic [31:0] if_insn;
    logic        if_insn_valid;
    logic [31:0] pc_next;

    // -----------------------------
    // Execute-stage signals (declared before use)
    // -----------------------------
    logic [31:0] alu_out;
    logic branch_taken;
    logic [31:0] branch_target;
    logic [31:0] imm_i, imm_s, imm_b, imm_u, imm_j;
    logic [6:0]  opcode;
    logic [2:0]  funct3;
    logic [6:0]  funct7;
    logic [31:0] csr_src, csr_old, csr_new;
    logic        csr_write_en;

    // -----------------------------
    // Branch prediction
    // -----------------------------
    logic        if_pred_taken;
    logic        bht_pred_taken;
    logic [31:0] if_pred_target;
    logic        bht_update_valid;
    logic        bht_update_taken;
    logic [31:0] bht_update_pc;

    // -----------------------------
    // Hazard signals
    // -----------------------------
    logic        stall_loaduse;

    // -----------------------------
    // Decode outputs
    // -----------------------------
    decode_ctrl_t dec_ctrl;
    logic [4:0]   dec_rd, dec_rs1, dec_rs2;
    logic [31:0]  dec_imm_i, dec_imm_s, dec_imm_b, dec_imm_u, dec_imm_j;

    // -----------------------------
    // Forwarding signals
    // -----------------------------
    logic [31:0] fwd_rs1, fwd_rs2;

    // -----------------------------
    // Pipeline registers
    // -----------------------------
    logic [31:0] if_id_pc, if_id_insn;
    logic        if_id_valid;

    logic [31:0] id_ex_pc, id_ex_rs1, id_ex_rs2;
    logic [31:0] id_ex_imm_i, id_ex_imm_s, id_ex_imm_b, id_ex_imm_u, id_ex_imm_j;
    logic [4:0]  id_ex_rd, id_ex_rs1_idx, id_ex_rs2_idx;
    logic [6:0]  id_ex_opcode;
    logic [2:0]  id_ex_funct3;
    logic [6:0]  id_ex_funct7;
    logic        id_ex_valid;
    logic        id_ex_mem_read, id_ex_mem_write;
    logic [3:0]  id_ex_wstrb;
    logic        id_ex_wb_en;
    wb_sel_e     id_ex_wb_sel;
    logic [11:0] id_ex_csr_addr;
    csr_op_e     id_ex_csr_op;
    logic        id_ex_csr_imm;
    logic        id_ex_is_amo;
    logic [4:0]  id_ex_amo_funct5;

    logic [31:0] ex_mem_alu, ex_mem_rs2;
    logic [4:0]  ex_mem_rd;
    logic        ex_mem_valid;
    logic [2:0]  ex_mem_funct3;
    logic        ex_mem_wb_en;
    logic        ex_mem_mem_read, ex_mem_mem_write;
    logic [3:0]  ex_mem_wstrb;
    wb_sel_e     ex_mem_wb_sel;
    logic [11:0] ex_mem_csr_addr;
    csr_op_e     ex_mem_csr_op;
    logic [31:0] ex_mem_csr_wdata;
    logic [31:0] ex_mem_csr_old;
    logic        ex_mem_csr_we;
    logic        ex_mem_is_amo;
    logic [4:0]  ex_mem_amo_funct5;

    logic [31:0] mem_wb_data;
    logic [4:0]  mem_wb_rd;
    logic        mem_wb_valid;

    // -----------------------------
    // Control / hazard
    // -----------------------------
    logic stall_fetch, stall_decode, flush_ifid, flush_idex;
    logic mem_wait;
    logic        lsu_wb_valid;
    logic [4:0]  lsu_wb_rd;
    logic [31:0] lsu_wb_data;
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
    logic        lsu_mailbox_rd_ready;
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
    /* verilator lint_off UNUSED */
    logic        unused_mailbox_inputs;
    logic        unused_mailbox_tx;
    /* verilator lint_on UNUSED */

    // -----------------------------
    // IF stage (with compressed support)
    // -----------------------------
    assign imem_addr = {pc[31:2], 2'b00};

    assign unused_mailbox_inputs = MAILBOX_ENABLE ? 1'b0 :
        (mailbox_tx_ready | mailbox_rx_valid | (|mailbox_rx_dest_id) | (|mailbox_rx_data));

    assign unused_mailbox_tx = MAILBOX_ENABLE ? 1'b0 :
        (lsu_mailbox_tx_valid | (|lsu_mailbox_tx_dest) | (|lsu_mailbox_tx_data) | lsu_mailbox_tx_prio |
         lsu_mailbox_tx_eop | (|lsu_mailbox_tx_opcode) | lsu_mailbox_rd_valid | (|lsu_mailbox_rd_dest) |
         lsu_mailbox_rd_prio | (|lsu_mailbox_rd_opcode) | ep_tx_ready | ep_rx_irq | ep_rx_ready_int | ^ep_rx_hdr);

    // RX pop-on-read (single outstanding)
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

    mcu_branch_predictor_bht u_bht (
        .clk(clk),
        .rst_n(rst_n),
        .pc_fetch(pc),
        .predict_taken(bht_pred_taken),
        .update_valid(bht_update_valid),
        .update_pc(bht_update_pc),
        .update_taken(bht_update_taken)
    );

    assign bht_update_valid = 1'b0;
    assign bht_update_pc    = 32'h0;
    assign bht_update_taken = 1'b0;

    always_comb begin
        if_insn_valid = 1'b1;
        if_insn = imem_rdata;
    end

    // predicted target (only for branches/JAL)
    always_comb begin
        if_pred_taken  = 1'b0;
        if_pred_target = pc + 32'd4;

        unique case (if_insn[6:0])
            OPC_JAL: begin
                if_pred_taken  = 1'b1;
                if_pred_target = pc + {{11{if_insn[31]}}, if_insn[31], if_insn[19:12], if_insn[20], if_insn[30:21], 1'b0};
            end
            OPC_BRANCH: begin
                if_pred_taken  = bht_pred_taken;
                if_pred_target = pc + {{19{if_insn[31]}}, if_insn[31], if_insn[7], if_insn[30:25], if_insn[11:8], 1'b0};
            end
            default: ;
        endcase
    end

    // next PC
    always_comb begin
        pc_next = pc + 32'd4;
        if (if_pred_taken) begin
            pc_next = if_pred_target;
        end
        if (branch_taken) begin
            pc_next = branch_target;
        end
    end

    // -----------------------------
    // Regfile
    // -----------------------------
    mcu_regfile u_rf (
        .clk(clk),
        .we(mem_wb_valid),
        .waddr(mem_wb_rd),
        .wdata(mem_wb_data),
        .raddr1(if_id_insn[19:15]),
        .raddr2(if_id_insn[24:20]),
        .rdata1(rf_rdata1),
        .rdata2(rf_rdata2)
    );

    // -----------------------------
    // Decode
    // -----------------------------
    mcu_decode u_decode (
        .insn   (if_id_insn),
        .rd     (dec_rd),
        .rs1    (dec_rs1),
        .rs2    (dec_rs2),
        .imm_i  (dec_imm_i),
        .imm_s  (dec_imm_s),
        .imm_b  (dec_imm_b),
        .imm_u  (dec_imm_u),
        .imm_j  (dec_imm_j),
        .ctrl   (dec_ctrl)
    );

    // -----------------------------
    // Hazard unit (load-use)
    // -----------------------------
    mcu_hazard u_hazard (
        .id_ex_mem_read(id_ex_mem_read),
        .id_ex_rd(id_ex_rd),
        .if_id_rs1(dec_rs1),
        .if_id_rs2(dec_rs2),
        .if_id_uses_rs2(dec_ctrl.uses_rs2),
        .stall_decode(stall_loaduse)
    );

    // -----------------------------
    // CSR block
    // -----------------------------
    mcu_csr u_csr (
        .clk(clk),
        .rst_n(rst_n),
        .csr_addr(csr_raddr),
        .csr_rdata(csr_rdata),
        .csr_we(csr_we),
        .csr_waddr(csr_waddr),
        .csr_wdata(csr_wdata),
        .trap_we(csr_trap_we),
        .trap_mepc(csr_trap_mepc),
        .trap_mcause(csr_trap_mcause),
        .trap_mtval(csr_trap_mtval),
        .csr_mstatus(csr_mstatus),
        .csr_mtvec(csr_mtvec)
    );

    // -----------------------------
    // Main sequential pipeline
    // -----------------------------
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            pc <= 32'h0000_0000;
            if_id_valid <= 1'b0;
            id_ex_valid <= 1'b0;
            ex_mem_valid <= 1'b0;
            csr_trap_we <= 1'b0;
            csr_trap_mepc <= 32'h0;
            csr_trap_mcause <= 32'h0;
            csr_trap_mtval <= 32'h0;

        end else begin
            csr_trap_we <= 1'b0;
            // Writeback handled by mcu_regfile

            // IF
            if (!stall_fetch) begin
                pc <= pc_next;
                if_id_pc <= pc;
                if_id_insn <= if_insn;
                if_id_valid <= if_insn_valid;
            end

            if (flush_ifid) begin
                if_id_valid <= 1'b0;
            end

            // ID/EX
            if (!stall_decode) begin
                id_ex_valid <= if_id_valid;
                id_ex_pc <= if_id_pc;
                id_ex_rs1_idx <= dec_rs1;
                id_ex_rs2_idx <= dec_rs2;
                id_ex_rd <= dec_rd;
                id_ex_opcode <= if_id_insn[6:0];
                id_ex_funct3 <= if_id_insn[14:12];
                id_ex_funct7 <= if_id_insn[31:25];
                id_ex_rs1 <= rf_rdata1;
                id_ex_rs2 <= rf_rdata2;
                id_ex_imm_i <= dec_imm_i;
                id_ex_imm_s <= dec_imm_s;
                id_ex_imm_b <= dec_imm_b;
                id_ex_imm_u <= dec_imm_u;
                id_ex_imm_j <= dec_imm_j;

                id_ex_mem_read <= dec_ctrl.mem_read;
                id_ex_mem_write <= dec_ctrl.mem_write;
                id_ex_wstrb <= dec_ctrl.wstrb;
                id_ex_wb_en <= dec_ctrl.wb_en;
                id_ex_wb_sel <= dec_ctrl.wb_sel;
                id_ex_csr_addr <= dec_ctrl.csr_addr;
                id_ex_csr_op <= dec_ctrl.csr_op;
                id_ex_csr_imm <= dec_ctrl.csr_imm;
                id_ex_is_amo <= dec_ctrl.is_amo;
                id_ex_amo_funct5 <= dec_ctrl.amo_funct5;
            end

            if (flush_idex) begin
                id_ex_valid <= 1'b0;
            end

            // EX/MEM (stall when memory waiting)
            if (!mem_wait) begin
                ex_mem_valid <= id_ex_valid;
                ex_mem_rd <= id_ex_rd;
                ex_mem_rs2 <= (id_ex_mem_write || id_ex_is_amo) ? fwd_rs2 : id_ex_rs2;
                ex_mem_funct3 <= id_ex_funct3;
                ex_mem_alu <= alu_out;
                ex_mem_mem_read <= id_ex_mem_read;
                ex_mem_mem_write <= id_ex_mem_write;
                ex_mem_wstrb <= id_ex_wstrb;
                ex_mem_wb_sel <= id_ex_wb_sel;
                ex_mem_wb_en <= id_ex_wb_en;
                ex_mem_csr_addr <= id_ex_csr_addr;
                ex_mem_csr_op <= id_ex_csr_op;
                ex_mem_csr_old <= csr_old;
                ex_mem_csr_wdata <= csr_new;
                ex_mem_csr_we <= csr_write_en;
                ex_mem_is_amo <= id_ex_is_amo;
                ex_mem_amo_funct5 <= id_ex_amo_funct5;
            end

        end
    end

    // -----------------------------
    // Execute stage combinational
    // -----------------------------
    assign opcode = id_ex_opcode;
    assign funct3 = id_ex_funct3;
    assign funct7 = id_ex_funct7;

    assign imm_i = id_ex_imm_i;
    assign imm_s = id_ex_imm_s;
    assign imm_b = id_ex_imm_b;
    assign imm_u = id_ex_imm_u;
    assign imm_j = id_ex_imm_j;

    assign csr_raddr = id_ex_csr_addr;
    assign csr_we    = ex_mem_valid && (ex_mem_csr_op != CSR_NONE) && ex_mem_csr_we;
    assign csr_waddr = ex_mem_csr_addr;
    assign csr_wdata = ex_mem_csr_wdata;

    // Forwarding
    always_comb begin
        fwd_rs1 = id_ex_rs1;
        fwd_rs2 = id_ex_rs2;
        if (ex_mem_valid && ex_mem_wb_en && (ex_mem_rd != 5'd0) && !ex_mem_mem_read) begin
            if (ex_mem_rd == id_ex_rs1_idx) fwd_rs1 = (ex_mem_wb_sel == WB_CSR) ? ex_mem_csr_old : ex_mem_alu;
            if (ex_mem_rd == id_ex_rs2_idx) fwd_rs2 = (ex_mem_wb_sel == WB_CSR) ? ex_mem_csr_old : ex_mem_alu;
        end
        if (mem_wb_valid && (mem_wb_rd != 5'd0)) begin
            if (mem_wb_rd == id_ex_rs1_idx) fwd_rs1 = mem_wb_data;
            if (mem_wb_rd == id_ex_rs2_idx) fwd_rs2 = mem_wb_data;
        end
    end

    mcu_alu u_alu (
        .rs1(fwd_rs1),
        .rs2(fwd_rs2),
        .pc(id_ex_pc),
        .imm_i(imm_i),
        .imm_s(imm_s),
        .imm_b(imm_b),
        .imm_u(imm_u),
        .imm_j(imm_j),
        .opcode(opcode),
        .funct3(funct3),
        .funct7(funct7),
        .alu_out(alu_out),
        .branch_taken(branch_taken),
        .branch_target(branch_target)
    );

    // CSR op combinational
    always_comb begin
        csr_old = csr_rdata;
        csr_src = id_ex_csr_imm ? {27'b0, id_ex_rs1_idx} : id_ex_rs1;
        csr_new = csr_old;
        csr_write_en = 1'b0;
        case (id_ex_csr_op)
            CSR_W: begin // CSRRW
                csr_new = csr_src;
                csr_write_en = 1'b1;
            end
            CSR_S: begin // CSRRS
                csr_new = csr_old | csr_src;
                csr_write_en = (csr_src != 32'h0);
            end
            CSR_C: begin // CSRRC
                csr_new = csr_old & ~csr_src;
                csr_write_en = (csr_src != 32'h0);
            end
            default: begin
                csr_new = csr_old;
                csr_write_en = 1'b0;
            end
        endcase
    end

    // -----------------------------
    // LSU and WB modules
    // -----------------------------
    mcu_lsu #(
        .MAILBOX_ENABLE(MAILBOX_ENABLE)
    ) u_lsu (
        .clk(clk),
        .rst_n(rst_n),
        .mailbox_tx_valid(lsu_mailbox_tx_valid),
        .mailbox_tx_ready(mailbox_tx_ready_int),
        .mailbox_tx_dest(lsu_mailbox_tx_dest),
        .mailbox_tx_data(lsu_mailbox_tx_data),
        .mailbox_tx_prio(lsu_mailbox_tx_prio),
        .mailbox_tx_eop(lsu_mailbox_tx_eop),
        .mailbox_tx_opcode(lsu_mailbox_tx_opcode),
        .mailbox_rd_valid(lsu_mailbox_rd_valid),
        .mailbox_rd_ready(mailbox_rd_ready_int),
        .mailbox_rd_dest(lsu_mailbox_rd_dest),
        .mailbox_rd_prio(lsu_mailbox_rd_prio),
        .mailbox_rd_opcode(lsu_mailbox_rd_opcode),
        .mailbox_rd_resp_valid(lsu_mailbox_rd_resp_valid),
        .mailbox_rd_resp_ready(lsu_mailbox_rd_resp_ready),
        .mailbox_rd_resp_data(lsu_mailbox_rd_resp_data),
        .mailbox_rd_resp_tag(lsu_mailbox_rd_resp_tag),
        .ex_mem_valid(ex_mem_valid),
        .ex_mem_rd(ex_mem_rd),
        .ex_mem_funct3(ex_mem_funct3),
        .ex_mem_alu(ex_mem_alu),
        .ex_mem_rs2(ex_mem_rs2),
        .ex_mem_mem_read(ex_mem_mem_read),
        .ex_mem_mem_write(ex_mem_mem_write),
        .ex_mem_wstrb(ex_mem_wstrb),
        .ex_mem_is_amo(ex_mem_is_amo),
        .ex_mem_amo_funct5(ex_mem_amo_funct5),
        .dmem_req_valid(dmem_req_valid),
        .dmem_req_write(dmem_req_write),
        .dmem_req_addr(dmem_req_addr),
        .dmem_req_wdata(dmem_req_wdata),
        .dmem_req_wstrb(dmem_req_wstrb),
        .dmem_resp_valid(dmem_resp_valid),
        .dmem_resp_rdata(dmem_resp_rdata),
        .mem_wait(mem_wait),
        .wb_valid(lsu_wb_valid),
        .wb_rd(lsu_wb_rd),
        .wb_data(lsu_wb_data)
    );

    // Mailbox endpoint integration (optional)
    generate
        if (MAILBOX_ENABLE) begin : g_mailbox_ep
            // Endpoint provides ready/backpressure
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
        end else begin : g_mailbox_tieoff
            assign mailbox_tx_ready_int = 1'b1;
            assign mailbox_rd_ready_int = 1'b1;
            assign lsu_mailbox_rd_resp_valid = 1'b0;
            assign lsu_mailbox_rd_resp_data  = 32'h0;
            assign lsu_mailbox_rd_resp_tag   = '0;
            assign ep_tx_ready      = 1'b1;
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

    mcu_wb u_wb (
        .clk(clk),
        .rst_n(rst_n),
        .ex_mem_valid(ex_mem_valid),
        .ex_mem_rd(ex_mem_rd),
        .ex_mem_wb_en(ex_mem_wb_en),
        .ex_mem_wb_sel(ex_mem_wb_sel),
        .ex_mem_alu(ex_mem_alu),
        .ex_mem_csr_old(ex_mem_csr_old),
        .lsu_wb_valid(lsu_wb_valid),
        .lsu_wb_rd(lsu_wb_rd),
        .lsu_wb_data(lsu_wb_data),
        .mem_wb_valid(mem_wb_valid),
        .mem_wb_rd(mem_wb_rd),
        .mem_wb_data(mem_wb_data)
    );

    // -----------------------------
    // Simple trap/interrupt handling (synchronous)
    // -----------------------------
    // TODO: full privilege/CSR semantics. This keeps core robust & deterministic.
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            // handled in reset above
        end else begin
            // Interrupt pending
            if (csr_mstatus[3] && (irq_cmdq_nonempty || irq_fence_reached || irq_abort_done || irq_display_vblank || irq_mpu_fault)) begin
                csr_trap_we <= 1'b1;
                csr_trap_mepc <= pc;
                csr_trap_mcause <= 32'h8000_0000 | (irq_mpu_fault ? 32'd4 : 32'd3);
                csr_trap_mtval <= 32'h0;
                pc <= csr_mtvec;
                if_id_valid <= 1'b0;
                id_ex_valid <= 1'b0;
            end
        end
    end

    // Debug
    assign dbg_pc = pc;
    assign dbg_insn = if_id_insn;

    // Simple stalls
    assign stall_fetch = mem_wait || stall_loaduse;
    assign stall_decode = mem_wait || stall_loaduse;
    assign flush_ifid = branch_taken;
    assign flush_idex = branch_taken;

endmodule
