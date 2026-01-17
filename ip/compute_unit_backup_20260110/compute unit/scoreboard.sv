module scoreboard (
    input  logic       clk,
    input  logic       rst_n,
    input  logic       issue_valid,
    input  logic       issue_rs1_valid,
    input  logic       issue_rs2_valid,
    input  logic [1:0] issue_rs1_class,
    input  logic [1:0] issue_rs2_class,
    input  logic [4:0] issue_rs1,
    input  logic [4:0] issue_rs2,
    input  logic       issue_rd_valid,
    input  logic [1:0] issue_rd_class,
    input  logic [4:0] issue_rd,
    output logic       stall,
    // Flush interface for control hazards
    input  logic       flush_rr,
    input  logic [1:0] flush_rr_rd_class,
    input  logic       flush_rr_rd_valid,
    input  logic [4:0] flush_rr_rd,
    // Writeback interface
    input  logic       wb_scalar_valid,
    input  logic [4:0] wb_scalar_rd,
    input  logic       wb_fp_valid,
    input  logic [4:0] wb_fp_rd,
    input  logic       wb_vec_valid,
    input  logic [4:0] wb_vec_rd
);
    logic [31:0] busy_s;
    logic [31:0] busy_f;
    logic [31:0] busy_v;

    function automatic logic src_busy(
        input logic [1:0] cls,
        input logic [4:0] idx
    );
        case (cls)
            2'b00: src_busy = busy_s[idx];
            2'b01: src_busy = busy_f[idx];
            default: src_busy = busy_v[idx];
        endcase
    endfunction

    function automatic logic dest_busy(
        input logic [1:0] cls,
        input logic [4:0] idx
    );
        case (cls)
            2'b00: dest_busy = busy_s[idx];
            2'b01: dest_busy = busy_f[idx];
            default: dest_busy = busy_v[idx];
        endcase
    endfunction

    always_comb begin
        stall = 1'b0;
        if (issue_valid) begin
            if (issue_rs1_valid && src_busy(issue_rs1_class, issue_rs1)) stall = 1'b1;
            if (issue_rs2_valid && src_busy(issue_rs2_class, issue_rs2)) stall = 1'b1;
            if (issue_rd_valid && dest_busy(issue_rd_class, issue_rd)) stall = 1'b1;
        end
    end

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            busy_s <= '0;
            busy_f <= '0;
            busy_v <= '0;
        end else begin
            if (issue_valid && !stall && issue_rd_valid) begin
                case (issue_rd_class)
                    2'b00: busy_s[issue_rd] <= 1'b1;
                    2'b01: busy_f[issue_rd] <= 1'b1;
                    default: busy_v[issue_rd] <= 1'b1;
                endcase
            end

            // Flush logic: clear busy bit set by the instruction currently in RR
            if (flush_rr && flush_rr_rd_valid) begin
                case (flush_rr_rd_class)
                    2'b00: busy_s[flush_rr_rd] <= 1'b0;
                    2'b01: busy_f[flush_rr_rd] <= 1'b0;
                    default: busy_v[flush_rr_rd] <= 1'b0;
                endcase
            end

            if (wb_scalar_valid) busy_s[wb_scalar_rd] <= 1'b0;
            if (wb_fp_valid)     busy_f[wb_fp_rd]    <= 1'b0;
            if (wb_vec_valid)    busy_v[wb_vec_rd]   <= 1'b0;
        end
    end
endmodule
