module branch_unit (
    input  logic                 valid,
    input  isa_pkg::decode_ctrl_t ctrl,
    input  logic [31:0]           pc,
    input  logic [31:0]           rs1_val,
    input  logic [31:0]           rs2_val,
    output logic                 taken,
    output logic [31:0]           target,
    output logic                 is_link,
    output logic [31:0]           link_value
);
    import isa_pkg::*;

    logic cond_taken;
    /* verilator lint_off UNUSED */
    logic [31:0] branch_cmp_result_unused;
    /* verilator lint_on UNUSED */

    // Reuse scalar ALU branch comparator logic for conditional branches.
    alu_scalar u_branch_cmp (
        .op_a(rs1_val),
        .op_b(rs2_val),
        .funct3(ctrl.funct3),
        .is_sub(1'b0),
        .funct7(7'b0000000),
        .opcode(OP_BRANCH),
        .result(branch_cmp_result_unused),
        .branch_taken(cond_taken)
    );

    logic is_jump;
    always_comb begin
        // JAL/JALR are unconditional in this ISA.
        is_jump = ctrl.is_branch && ((ctrl.funct3 == 3'b011) || (ctrl.funct3 == 3'b010));

        taken = 1'b0;
        if (valid && ctrl.is_branch) begin
            taken = is_jump ? 1'b1 : cond_taken;
        end

        target = 32'h0;
        if (valid && ctrl.is_branch) begin
            if (ctrl.funct3 == 3'b010) begin
                // JALR: (rs1 + imm12) & ~1
                target = (rs1_val + ctrl.imm) & 32'hFFFF_FFFE;
            end else begin
                // Conditional branches + JAL: PC-relative
                target = pc + ctrl.imm;
            end
        end

        is_link    = valid && is_jump && ctrl.uses_rd;
        link_value = pc + 32'd4;
    end
endmodule
