module regfile_vector #(
    parameter DATA_W = 128
) (
    input  logic              clk,
    input  logic              rst_n,
    input  logic [4:0]        raddr_a,
    input  logic [4:0]        raddr_b,
    input  logic [4:0]        raddr_c,
    input  logic [4:0]        raddr_d,
    output logic [DATA_W-1:0] rdata_a,
    output logic [DATA_W-1:0] rdata_b,
    output logic [DATA_W-1:0] rdata_c,
    output logic [DATA_W-1:0] rdata_d,
    input  logic              we0,
    input  logic [4:0]        waddr0,
    input  logic [DATA_W-1:0] wdata0,
    input  logic              we1,
    input  logic [4:0]        waddr1,
    input  logic [DATA_W-1:0] wdata1
);
    (* ram_style = "distributed" *) logic [DATA_W-1:0] mem [31:0];

    // Per-register update (no procedural loops). Priority: we1 > we0.
    generate
        genvar i;
        for (i = 0; i < 32; i++) begin : gen_vec_rf
            localparam logic [4:0] IDX = i;
            wire hit0 = we0 && (waddr0 == IDX);
            wire hit1 = we1 && (waddr1 == IDX);

            wire [DATA_W-1:0] wr_data = hit1 ? wdata1 : hit0 ? wdata0 : mem[i];

            always_ff @(posedge clk or negedge rst_n) begin
                if (!rst_n) begin
                    mem[i] <= '0;
                end else begin
                    mem[i] <= wr_data;
                end
            end
        end
    endgenerate

    always_comb begin
        rdata_a = mem[raddr_a];
        rdata_b = mem[raddr_b];
        rdata_c = mem[raddr_c];
        rdata_d = mem[raddr_d];
    end
endmodule
