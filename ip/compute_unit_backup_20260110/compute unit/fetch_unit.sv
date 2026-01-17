module fetch_unit (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        stall,
    input  logic        branch_taken,
    input  logic [31:0] branch_target,
    output logic [31:0] pc,
    output logic [31:0] inst_addr,
    input  logic [31:0] inst_rdata,
    output logic        inst_valid,
    output logic [31:0] inst
);
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            pc <= 32'h0;
        end else if (!stall) begin
            if (branch_taken) pc <= branch_target;
            else pc <= pc + 32'd4;
        end
    end

    assign inst_addr = pc;

    // BRAM delivers data one cycle after address; caller handles alignment.
    // Capture instruction only when pipeline can accept it; hold during stall
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            inst       <= 32'h0;
            inst_valid <= 1'b0;
        end else if (!stall) begin
            inst       <= inst_rdata;
            inst_valid <= 1'b1;
        end
        // else: retain inst/valid to avoid losing the fetch during backpressure
    end
endmodule
