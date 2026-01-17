module regfile_fp #(
    parameter DATA_W = 16
) (
    input  logic           clk,
    input  logic           rst_n,
    input  logic [4:0]     raddr_a,
    input  logic [4:0]     raddr_b,
    output logic [DATA_W-1:0] rdata_a,
    output logic [DATA_W-1:0] rdata_b,
    input  logic           we,
    input  logic [4:0]     waddr,
    input  logic [DATA_W-1:0] wdata
);
    logic [DATA_W-1:0] mem [31:0];

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            for (int i = 0; i < 32; i++) mem[i] <= '0;
        end else if (we) begin
            mem[waddr] <= wdata;
        end
    end

    always_comb begin
        rdata_a = (raddr_a == 5'd0) ? '0 : mem[raddr_a];
        rdata_b = (raddr_b == 5'd0) ? '0 : mem[raddr_b];
    end
endmodule
