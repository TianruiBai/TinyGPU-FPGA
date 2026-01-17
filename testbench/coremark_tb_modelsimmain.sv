`timescale 1ns/1ps

// ModelSim-only convenience wrapper: preserves the old "no-ports" top-level
// flow by generating clk/rst_n and instantiating the ported coremark_tb.
module coremark_tb_modelsimmain;
    logic clk;
    logic rst_n;

    initial begin
        clk = 1'b0;
        forever #5 clk = ~clk; // 100MHz
    end

    initial begin
        rst_n = 1'b0;
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
    end

    coremark_tb u_tb(
        .clk(clk),
        .rst_n(rst_n)
    );
endmodule
