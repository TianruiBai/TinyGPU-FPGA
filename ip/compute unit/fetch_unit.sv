module fetch_unit (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        stall,
    input  logic        pred_taken,
    input  logic [31:0] pred_target,
    input  logic        branch_taken,
    input  logic [31:0] branch_target,
    input  logic [3:0]  pc_advance_bytes, // 0,4,8 only
    output logic [31:0] pc,
    // Instruction cache request/response
    output logic        req_valid,
    output logic [31:0] req_addr,
    input  logic        req_ready,
    input  logic        resp_valid,
    input  logic [63:0] resp_data,
    output logic        inst_valid,
    output logic        inst0_valid,
    output logic        inst1_valid,
    output logic [31:0] inst0,
    output logic [31:0] inst1
);
    logic [63:0] inst_bundle;
    logic        pc_halfword;

    // pc_reg is the next fetch PC; pc_bundle is the PC associated with inst_bundle.
    logic [31:0] pc_reg;
    logic [31:0] pc_bundle;
    logic [31:0] req_pc;
    logic        req_pending;
    logic        advance_pending;
    logic [31:0] advance_base_pc;

    // Export the PC that matches the currently presented instruction(s)
    assign pc = pc_bundle;

    logic        redirect;
    logic [31:0] redirect_target;
    always_comb begin
        // Resolved redirect has priority over prediction.
        redirect        = branch_taken | pred_taken;
        redirect_target = branch_taken ? branch_target : pred_target;
    end

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            pc_reg       <= 32'h0;
            pc_bundle    <= 32'h0;
            inst_bundle  <= 64'h0;
            pc_halfword  <= 1'b0;
            inst_valid   <= 1'b0;
            req_pending  <= 1'b0;
            req_pc       <= 32'h0;
            advance_pending <= 1'b0;
            advance_base_pc <= 32'h0;
        end else begin
            // Default: drop inst_valid unless a new response arrives
            inst_valid <= 1'b0;

            if (redirect) begin
                // Flush and restart from redirect target
                pc_reg      <= redirect_target;
                pc_bundle   <= redirect_target;
                pc_halfword <= redirect_target[2];
                req_pending <= 1'b0;
                advance_pending <= 1'b0;
            end else begin
                // Advance PC after the front end decides how many slots were accepted.
                if (advance_pending) begin
                    pc_reg          <= advance_base_pc + {{28{1'b0}}, pc_advance_bytes};
                    advance_pending <= 1'b0;
                    req_pending     <= 1'b0;
                end

                // Issue request when idle, not stalled, and not waiting on an advance
                if (!stall && !req_pending && !advance_pending) begin
                    req_pc      <= pc_reg;
                    req_pending <= req_ready; // latch if accepted this cycle
                end

                // Capture response; hold off issuing the next request until we've advanced PC
                if (resp_valid) begin
                    pc_bundle       <= req_pc;
                    inst_bundle     <= resp_data;
                    pc_halfword     <= req_pc[2];
                    inst_valid      <= 1'b1;
                    advance_pending <= 1'b1;
                    advance_base_pc <= req_pc;
                    req_pending     <= 1'b1;
                end
            end
        end
    end

    // Request drive
    assign req_valid = (!stall) && (!req_pending) && (!advance_pending);
    assign req_addr  = {pc_reg[31:3], 3'b000};

    always_comb begin
        inst0       = pc_halfword ? inst_bundle[63:32] : inst_bundle[31:0];
        inst1       = inst_bundle[63:32];
        inst0_valid = inst_valid;
        inst1_valid = inst_valid && !pc_halfword;
    end
endmodule
