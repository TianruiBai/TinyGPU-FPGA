// Writeback mux (WB)
module mcu_wb (
    input  logic        clk,
    input  logic        rst_n,

    input  logic        ex_mem_valid,
    input  logic [4:0]  ex_mem_rd,
    input  logic        ex_mem_wb_en,
    input  mcu_isa_pkg::wb_sel_e     ex_mem_wb_sel,
    input  logic [31:0] ex_mem_alu,
    input  logic [31:0] ex_mem_csr_old,

    input  logic        lsu_wb_valid,
    input  logic [4:0]  lsu_wb_rd,
    input  logic [31:0] lsu_wb_data,

    output logic        mem_wb_valid,
    output logic [4:0]  mem_wb_rd,
    output logic [31:0] mem_wb_data
);

    import mcu_isa_pkg::*;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            mem_wb_valid <= 1'b0;
            mem_wb_rd <= 5'd0;
            mem_wb_data <= 32'h0;
        end else begin
            if (lsu_wb_valid) begin
                mem_wb_valid <= 1'b1;
                mem_wb_rd <= lsu_wb_rd;
                mem_wb_data <= lsu_wb_data;
            end else if (ex_mem_valid && ex_mem_wb_en && ex_mem_wb_sel == WB_ALU) begin
                mem_wb_valid <= 1'b1;
                mem_wb_rd <= ex_mem_rd;
                mem_wb_data <= ex_mem_alu;
            end else if (ex_mem_valid && ex_mem_wb_en && ex_mem_wb_sel == WB_CSR) begin
                mem_wb_valid <= 1'b1;
                mem_wb_rd <= ex_mem_rd;
                mem_wb_data <= ex_mem_csr_old;
            end else begin
                mem_wb_valid <= 1'b0;
            end
        end
    end
endmodule
