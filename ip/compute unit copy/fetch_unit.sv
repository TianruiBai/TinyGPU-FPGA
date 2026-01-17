module fetch_unit (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        stall,
    input  logic        pred_taken,
    input  logic [31:0] pred_target,
    input  logic        branch_taken,
    input  logic [31:0] branch_target,
    input  logic [3:0]  pc_advance_bytes, // 0,4,8 only
    output logic [31:0] pc,
    output logic [31:0] inst_addr,
    input  logic [63:0] inst_rdata,
    output logic        inst_valid,
    output logic        inst0_valid,
    output logic        inst1_valid,
    output logic [31:0] inst0,
    output logic [31:0] inst1
);
    logic [63:0] inst_bundle;
    logic        pc_halfword;

    // pc_reg is the next fetch PC; pc_bundle is the PC associated with inst_bundle.
    logic [31:0] pc_reg;
    logic [31:0] pc_bundle;

    // Export the PC that matches the currently presented instruction(s)
    assign pc = pc_bundle;

    // 64-bit fetch is aligned to 8 bytes; pc[2] selects low/high word.
    assign inst_addr = {pc_reg[31:3], 3'b000};

    logic        redirect;
    logic [31:0] redirect_target;
    always_comb begin
        // Resolved redirect has priority over prediction.
        redirect        = branch_taken | pred_taken;
        redirect_target = branch_taken ? branch_target : pred_target;
    end

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            pc_reg      <= 32'h0;
            pc_bundle   <= 32'h0;
            inst_bundle <= 64'h0;
            pc_halfword <= 1'b0;
            inst_valid  <= 1'b0;
        end else if (redirect) begin
            // Flush current bundle; refetch from branch_target next cycle.
            pc_reg     <= redirect_target;
            pc_bundle  <= redirect_target;
            pc_halfword <= redirect_target[2];
            inst_valid <= 1'b0;
        end else if (!stall) begin
            // Latch the bundle corresponding to pc_reg, then advance pc_reg.
            pc_bundle   <= pc_reg;
            inst_bundle <= inst_rdata;
            pc_halfword <= pc_reg[2];
            inst_valid  <= 1'b1;

            pc_reg <= pc_reg + {{28{1'b0}}, pc_advance_bytes};
        end
    end

    always_comb begin
        inst0       = pc_halfword ? inst_bundle[63:32] : inst_bundle[31:0];
        inst1       = inst_bundle[63:32];
        inst0_valid = inst_valid;
        inst1_valid = inst_valid && !pc_halfword;
    end
endmodule
