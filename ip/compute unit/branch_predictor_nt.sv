module branch_predictor_nt (
    input  logic                 valid,
    input  isa_pkg::decode_ctrl_t ctrl,
    input  logic [31:0]           pc,
    input  logic [31:0]           rs1_val,
    output logic                 pred_taken,
    output logic [31:0]           pred_target
);
    // Static predictor: always predict NOT taken.
    // (Target is don't-care when pred_taken=0.)
    always_comb begin
        pred_taken  = 1'b0;
        pred_target = 32'h0;
    end
endmodule
