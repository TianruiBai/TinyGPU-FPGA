/* verilator lint_off UNUSEDSIGNAL */
// Simple 2-bit saturating counter BHT (64 entries)
module mcu_branch_predictor_bht (
    input  logic        clk,
    input  logic        rst_n,
    input  logic [31:0] pc_fetch,
    output logic        predict_taken,
    input  logic        update_valid,
    input  logic [31:0] update_pc,
    input  logic        update_taken
);
    localparam int ENTRIES = 64;
    logic [1:0] bht [0:ENTRIES-1];
    logic [5:0] idx_fetch;
    logic [5:0] idx_update;

    assign idx_fetch = pc_fetch[7:2];
    assign idx_update = update_pc[7:2];
    assign predict_taken = bht[idx_fetch][1];

    integer i;
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            for (i = 0; i < ENTRIES; i++) begin
                bht[i] <= 2'b01; // weakly not-taken
            end
        end else if (update_valid) begin
            case ({update_taken, bht[idx_update]})
                3'b000: bht[idx_update] <= 2'b00;
                3'b001: bht[idx_update] <= 2'b00;
                3'b010: bht[idx_update] <= 2'b01;
                3'b011: bht[idx_update] <= 2'b10;
                3'b100: bht[idx_update] <= 2'b01;
                3'b101: bht[idx_update] <= 2'b10;
                3'b110: bht[idx_update] <= 2'b11;
                3'b111: bht[idx_update] <= 2'b11;
            endcase
        end
    end
endmodule
/* verilator lint_on UNUSEDSIGNAL */
