module alu_scalar (
    input  logic [31:0] op_a,
    input  logic [31:0] op_b,
    input  logic [2:0]  funct3,
    input  logic        is_sub,
    input  logic [6:0]  funct7,
    input  logic [6:0]  opcode,
    output logic [31:0] result,
    output logic        branch_taken
);
    import isa_pkg::*;

    logic signed [31:0] s_op_a;
    logic signed [31:0] s_op_b;
    assign s_op_a = op_a;
    assign s_op_b = op_b;

    logic [31:0] srl_res;
    logic [31:0] sra_res;

    // MUL support: signed 32x32 -> 64, take low 32 bits
    logic signed [63:0] mul_prod;
    assign mul_prod = s_op_a * s_op_b;

    always_comb begin
        result = 32'h0;
        branch_taken = 1'b0;

        // Precompute shift results to avoid signedness quirks across tools.
        srl_res = (op_a >> op_b[4:0]);
        sra_res = srl_res;
        if (op_b[4:0] != 5'd0) begin
            if (op_a[31]) sra_res = srl_res | (32'hFFFF_FFFF << (32 - op_b[4:0]));
        end

        if (opcode == OP_BRANCH) begin
            case (funct3)
                3'b000: branch_taken = (op_a == op_b);                  // BEQ
                3'b001: branch_taken = (op_a != op_b);                  // BNE
                3'b100: branch_taken = (s_op_a < s_op_b);               // BLT (signed)
                3'b101: branch_taken = (s_op_a >= s_op_b);              // BGE (signed)
                3'b110: branch_taken = (op_a < op_b);                   // BLTU
                3'b111: branch_taken = (op_a >= op_b);                  // BGEU
                default: branch_taken = 1'b0;
            endcase
        end else if (funct7 == 7'b0000001 && funct3 == 3'b000) begin
            // MUL: low 32 bits of signed multiply
            result = mul_prod[31:0];
        end else begin
            case (funct3)
                3'b000: result = is_sub ? (op_a - op_b) : (op_a + op_b);         // ADD/SUB
                3'b001: result = op_a << op_b[4:0];                              // SLL
                3'b010: result = (s_op_a < s_op_b) ? 32'd1 : 32'd0;              // SLT
                3'b011: result = (op_a < op_b) ? 32'd1 : 32'd0;                  // SLTU
                3'b100: result = op_a ^ op_b;                                    // XOR
                3'b101: result = is_sub ? sra_res : srl_res;                      // SRA/SRL
                3'b110: result = op_a | op_b;                                    // OR
                3'b111: result = op_a & op_b;                                    // AND
            endcase
        end
    end
endmodule
