// Instruction Fetch stage (IF)
module mcu_ifetch (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        stall,
    input  logic        flush,
    input  logic [31:0] pc_next,
    output logic [31:0] pc,
    output logic [31:0] imem_addr
);
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            pc <= 32'h0000_0000;
        end else if (flush) begin
            pc <= pc_next;
        end else if (!stall) begin
            pc <= pc_next;
        end
    end
    assign imem_addr = pc;
endmodule
