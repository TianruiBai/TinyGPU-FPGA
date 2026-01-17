module regfile_scalar (
    input  logic        clk,
    input  logic        rst_n,
    input  logic [4:0]  raddr_a,
    input  logic [4:0]  raddr_b,
    input  logic [4:0]  raddr_c,
    output logic [31:0] rdata_a,
    output logic [31:0] rdata_b,
    output logic [31:0] rdata_c,
    input  logic        we,
    input  logic [4:0]  waddr,
    input  logic [31:0] wdata
);
    (* ram_style = "distributed" *) logic [31:0] mem [31:0];

    // Synchronous write, combinational read; x0 is hardwired to zero
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            for (int i = 0; i < 32; i++) mem[i] <= 32'h0;
        end else if (we && (waddr != 5'd0)) begin
            mem[waddr] <= wdata;
        end
    end

    always_comb begin
        rdata_a = (raddr_a == 5'd0) ? 32'h0 : mem[raddr_a];
        rdata_b = (raddr_b == 5'd0) ? 32'h0 : mem[raddr_b];
        rdata_c = (raddr_c == 5'd0) ? 32'h0 : mem[raddr_c];
    end
endmodule
