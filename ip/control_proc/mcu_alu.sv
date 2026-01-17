// ALU / Branch unit (EX)
module mcu_alu (
    input  logic [31:0] rs1,
    input  logic [31:0] rs2,
    input  logic [31:0] pc,
    input  logic [31:0] imm_i,
    input  logic [31:0] imm_s,
    input  logic [31:0] imm_b,
    input  logic [31:0] imm_u,
    input  logic [31:0] imm_j,
    input  logic [6:0]  opcode,
    input  logic [2:0]  funct3,
    input  logic [6:0]  funct7,
    output logic [31:0] alu_out,
    output logic        branch_taken,
    output logic [31:0] branch_target
);
    import mcu_isa_pkg::*;

    always_comb begin
        alu_out = 32'h0;
        branch_taken = 1'b0;
        branch_target = pc + 32'd4;

        unique case (opcode)
            OPC_LUI:   alu_out = imm_u;
            OPC_AUIPC: alu_out = pc + imm_u;
            OPC_JAL: begin
                alu_out = pc + 32'd4;
                branch_taken = 1'b1;
                branch_target = pc + imm_j;
            end
            OPC_JALR: begin
                alu_out = pc + 32'd4;
                branch_taken = 1'b1;
                branch_target = (rs1 + imm_i) & ~32'h1;
            end
            OPC_BRANCH: begin
                case (funct3)
                    3'b000: branch_taken = (rs1 == rs2);
                    3'b001: branch_taken = (rs1 != rs2);
                    3'b100: branch_taken = ($signed(rs1) < $signed(rs2));
                    3'b101: branch_taken = ($signed(rs1) >= $signed(rs2));
                    3'b110: branch_taken = (rs1 < rs2);
                    3'b111: branch_taken = (rs1 >= rs2);
                    default: branch_taken = 1'b0;
                endcase
                branch_target = pc + imm_b;
            end
            OPC_LOAD: begin
                alu_out = rs1 + imm_i;
            end
            OPC_STORE: begin
                alu_out = rs1 + imm_s;
            end
            OPC_AMO: begin
                alu_out = rs1 + imm_i;
            end
            OPC_OPIMM: begin
                case (funct3)
                    3'b000: alu_out = rs1 + imm_i;
                    3'b010: alu_out = ($signed(rs1) < $signed(imm_i)) ? 32'd1 : 32'd0;
                    3'b011: alu_out = (rs1 < imm_i) ? 32'd1 : 32'd0;
                    3'b100: alu_out = rs1 ^ imm_i;
                    3'b110: alu_out = rs1 | imm_i;
                    3'b111: alu_out = rs1 & imm_i;
                    3'b001: alu_out = rs1 << imm_i[4:0];
                    3'b101: alu_out = (funct7[5] ? ($signed(rs1) >>> imm_i[4:0]) : (rs1 >> imm_i[4:0]));
                endcase
            end
            OPC_OP: begin
                case ({funct7, funct3})
                    {7'b0000000,3'b000}: alu_out = rs1 + rs2;
                    {7'b0100000,3'b000}: alu_out = rs1 - rs2;
                    {7'b0000000,3'b111}: alu_out = rs1 & rs2;
                    {7'b0000000,3'b110}: alu_out = rs1 | rs2;
                    {7'b0000000,3'b100}: alu_out = rs1 ^ rs2;
                    {7'b0000000,3'b001}: alu_out = rs1 << rs2[4:0];
                    {7'b0000000,3'b101}: alu_out = rs1 >> rs2[4:0];
                    {7'b0100000,3'b101}: alu_out = $signed(rs1) >>> rs2[4:0];
                    {7'b0000000,3'b010}: alu_out = ($signed(rs1) < $signed(rs2)) ? 32'd1 : 32'd0;
                    {7'b0000000,3'b011}: alu_out = (rs1 < rs2) ? 32'd1 : 32'd0;
                    // M extension
                    {7'b0000001,3'b000}: alu_out = rs1 * rs2;
                    {7'b0000001,3'b001}: alu_out = ($signed(rs1) * $signed(rs2)) >>> 32;
                    {7'b0000001,3'b010}: alu_out = ($signed(rs1) * $unsigned(rs2)) >>> 32;
                    {7'b0000001,3'b011}: alu_out = ($unsigned(rs1) * $unsigned(rs2)) >>> 32;
                    {7'b0000001,3'b100}: alu_out = (rs2 == 0) ? 32'hFFFF_FFFF : $signed(rs1) / $signed(rs2);
                    {7'b0000001,3'b101}: alu_out = (rs2 == 0) ? 32'hFFFF_FFFF : $unsigned(rs1) / $unsigned(rs2);
                    {7'b0000001,3'b110}: alu_out = (rs2 == 0) ? rs1 : $signed(rs1) % $signed(rs2);
                    {7'b0000001,3'b111}: alu_out = (rs2 == 0) ? rs1 : $unsigned(rs1) % $unsigned(rs2);
                    default: alu_out = 32'h0;
                endcase
            end
            default: alu_out = 32'h0;
        endcase
    end
endmodule
