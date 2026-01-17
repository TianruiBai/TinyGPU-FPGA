// Decode stage (ID)

module mcu_decode (
    input  logic [31:0] insn,
    output logic [4:0]  rd,
    output logic [4:0]  rs1,
    output logic [4:0]  rs2,
    output logic [31:0] imm_i,
    output logic [31:0] imm_s,
    output logic [31:0] imm_b,
    output logic [31:0] imm_u,
    output logic [31:0] imm_j,
    output mcu_isa_pkg::decode_ctrl_t ctrl
);

    import mcu_isa_pkg::*;

    assign rd  = insn[11:7];
    assign rs1 = insn[19:15];
    assign rs2 = insn[24:20];
    assign imm_i = {{20{insn[31]}}, insn[31:20]};
    assign imm_s = {{20{insn[31]}}, insn[31:25], insn[11:7]};
    assign imm_b = {{19{insn[31]}}, insn[31], insn[7], insn[30:25], insn[11:8], 1'b0};
    assign imm_u = {insn[31:12], 12'b0};
    assign imm_j = {{11{insn[31]}}, insn[31], insn[19:12], insn[20], insn[30:21], 1'b0};

    always_comb begin
        ctrl = '0;
        ctrl.wb_en = 1'b1;
        ctrl.wb_sel = WB_ALU;
        ctrl.csr_addr = insn[31:20];
        ctrl.csr_op = CSR_NONE;

        unique case (insn[6:0])
            OPC_LOAD: begin
                ctrl.mem_read = 1'b1;
                ctrl.wb_sel = WB_MEM;
            end
            OPC_STORE: begin
                ctrl.mem_write = 1'b1;
                ctrl.wb_en = 1'b0;
                ctrl.uses_rs2 = 1'b1;
                case (insn[14:12])
                    3'b000: ctrl.wstrb = 4'b0001;
                    3'b001: ctrl.wstrb = 4'b0011;
                    3'b010: ctrl.wstrb = 4'b1111;
                    default: ctrl.wstrb = 4'b0000;
                endcase
            end
            OPC_BRANCH: begin
                ctrl.wb_en = 1'b0;
                ctrl.uses_rs2 = 1'b1;
            end
            OPC_OP: begin
                ctrl.uses_rs2 = 1'b1;
            end
            OPC_AMO: begin
                ctrl.is_amo = 1'b1;
                ctrl.mem_read = 1'b1;
                ctrl.mem_write = 1'b1;
                ctrl.wb_sel = WB_MEM;
                ctrl.uses_rs2 = 1'b1;
                ctrl.amo_funct5 = insn[31:27];
                ctrl.wstrb = 4'b1111;
            end
            OPC_SYSTEM: begin
                if (insn[14:12] != 3'b000) begin
                    ctrl.csr_op = (insn[14:12] == 3'b001) ? CSR_W :
                                  (insn[14:12] == 3'b010) ? CSR_S : CSR_C;
                    ctrl.csr_imm = insn[14];
                    ctrl.wb_sel = WB_CSR;
                end else begin
                    ctrl.wb_en = 1'b0; // ECALL/EBREAK/MRET
                end
            end
            default: ;
        endcase
    end
endmodule
