`include "../ip/compute unit/isa_pkg.sv"
`include "../ip/compute unit/scoreboard.sv"
module scoreboard_flush_tb;
    reg clk = 0;
    always #5 clk = ~clk;

    reg rst_n = 0;
    reg issue_valid;
    reg issue_rs1_valid;
    reg issue_rs2_valid;
    reg [1:0] issue_rs1_class;
    reg [1:0] issue_rs2_class;
    reg [4:0] issue_rs1;
    reg [4:0] issue_rs2;
    reg issue_rd_valid;
    reg [1:0] issue_rd_class;
    reg [4:0] issue_rd;
    wire stall;
    reg flush_rr;
    reg [1:0] flush_rr_rd_class;
    reg flush_rr_rd_valid;
    reg [4:0] flush_rr_rd;
    reg flush_rr1;
    reg [1:0] flush_rr1_rd_class;
    reg flush_rr1_rd_valid;
    reg [4:0] flush_rr1_rd;
    reg flush_ex1;
    reg [1:0] flush_ex1_rd_class;
    reg flush_ex1_rd_valid;
    reg [4:0] flush_ex1_rd;
    reg wb_scalar_valid0;
    reg [4:0] wb_scalar_rd0;
    reg wb_scalar_valid1;
    reg [4:0] wb_scalar_rd1;
    reg wb_scalar_valid2;
    reg [4:0] wb_scalar_rd2;
    reg wb_fp_valid;
    reg [4:0] wb_fp_rd;
    reg wb_vec_valid;
    reg [4:0] wb_vec_rd;
    reg flush_all;

    scoreboard dut (
        .clk(clk),
        .rst_n(rst_n),
        .issue0_valid(issue_valid),
        .issue0_rs1_valid(issue_rs1_valid),
        .issue0_rs2_valid(issue_rs2_valid),
        .issue0_rs1_class(issue_rs1_class),
        .issue0_rs2_class(issue_rs2_class),
        .issue0_rs1(issue_rs1),
        .issue0_rs2(issue_rs2),
        .issue0_rs1_fwd(1'b0),
        .issue0_rs2_fwd(1'b0),
        .issue0_rd_valid(issue_rd_valid),
        .issue0_rd_class(issue_rd_class),
        .issue0_rd(issue_rd),
        .accept0(issue_valid),
        .stall0(stall),

        .issue1_valid(1'b0),
        .issue1_rs1_valid(1'b0),
        .issue1_rs2_valid(1'b0),
        .issue1_rs1_class(2'b00),
        .issue1_rs2_class(2'b00),
        .issue1_rs1(5'd0),
        .issue1_rs2(5'd0),
        .issue1_rs1_fwd(1'b0),
        .issue1_rs2_fwd(1'b0),
        .issue1_rd_valid(1'b0),
        .issue1_rd_class(2'b00),
        .issue1_rd(5'd0),
        .accept1(1'b0),
        .stall1(),
        .flush_rr(flush_rr),
        .flush_rr_rd_class(flush_rr_rd_class),
        .flush_rr_rd_valid(flush_rr_rd_valid),
        .flush_rr_rd(flush_rr_rd),
        .flush_rr1(flush_rr1),
        .flush_rr1_rd_class(flush_rr1_rd_class),
        .flush_rr1_rd_valid(flush_rr1_rd_valid),
        .flush_rr1_rd(flush_rr1_rd),
        .flush_ex1(flush_ex1),
        .flush_ex1_rd_class(flush_ex1_rd_class),
        .flush_ex1_rd_valid(flush_ex1_rd_valid),
        .flush_ex1_rd(flush_ex1_rd),
        .wb_scalar_valid0(wb_scalar_valid0),
        .wb_scalar_rd0(wb_scalar_rd0),
        .wb_scalar_valid1(wb_scalar_valid1),
        .wb_scalar_rd1(wb_scalar_rd1),
        .wb_scalar_valid2(wb_scalar_valid2),
        .wb_scalar_rd2(wb_scalar_rd2),
        .wb_fp_valid(wb_fp_valid),
        .wb_fp_rd(wb_fp_rd),
        .wb_vec_valid(wb_vec_valid),
        .wb_vec_rd(wb_vec_rd),
        .flush_all(flush_all)
    );

    initial begin
        issue_valid = 0;
        issue_rs1_valid = 0;
        issue_rs2_valid = 0;
        issue_rs1_class = 2'b00;
        issue_rs2_class = 2'b00;
        issue_rs1 = 5'd0;
        issue_rs2 = 5'd0;
        issue_rd_valid = 0;
        issue_rd_class = 2'b00;
        issue_rd = 5'd0;
        flush_rr = 0;
        flush_rr_rd_class = 2'b00;
        flush_rr_rd_valid = 0;
        flush_rr_rd = 5'd0;
        flush_rr1 = 0;
        flush_rr1_rd_class = 2'b00;
        flush_rr1_rd_valid = 0;
        flush_rr1_rd = 5'd0;
        flush_ex1 = 0;
        flush_ex1_rd_class = 2'b00;
        flush_ex1_rd_valid = 0;
        flush_ex1_rd = 5'd0;
        wb_scalar_valid0 = 0;
        wb_scalar_rd0 = 5'd0;
        wb_scalar_valid1 = 0;
        wb_scalar_rd1 = 5'd0;
        wb_scalar_valid2 = 0;
        wb_scalar_rd2 = 5'd0;
        wb_fp_valid = 0;
        wb_fp_rd = 5'd0;
        wb_vec_valid = 0;
        wb_vec_rd = 5'd0;
        flush_all = 0;

        #12 rst_n = 1;

        // Issue rd=5 once to mark busy
        issue_valid = 1; issue_rd_valid = 1; issue_rd_class = 2'b00; issue_rd = 5'd5;
        #10 issue_valid = 0; issue_rd_valid = 0;

        // Re-issue rd=5 should stall (busy)
        issue_valid = 1; issue_rd_valid = 1; issue_rd_class = 2'b00; issue_rd = 5'd5;
        #1 if (!stall) $fatal("Expected stall due to busy rd");
        #9 issue_valid = 0; issue_rd_valid = 0;

        // Flush all busy bits
        flush_all = 1; #10 flush_all = 0;

        // Re-issue rd=5 should now proceed (no stall)
        issue_valid = 1; issue_rd_valid = 1; issue_rd_class = 2'b00; issue_rd = 5'd5;
        #1 if (stall) $fatal("Unexpected stall after flush_all");
        #9 issue_valid = 0; issue_rd_valid = 0;

        $display("scoreboard_flush_tb PASS");
        $finish;
    end
endmodule
