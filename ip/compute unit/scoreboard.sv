module scoreboard (
    input  logic       clk,
    input  logic       rst_n,
    input  logic       issue0_valid,
    input  logic       issue0_rs1_valid,
    input  logic       issue0_rs2_valid,
    input  logic [1:0] issue0_rs1_class,
    input  logic [1:0] issue0_rs2_class,
    input  logic [4:0] issue0_rs1,
    input  logic [4:0] issue0_rs2,
    input  logic       issue0_rs1_fwd,
    input  logic       issue0_rs2_fwd,
    input  logic       issue0_rd_valid,
    input  logic [1:0] issue0_rd_class,
    input  logic [4:0] issue0_rd,
    // Slot accept qualifiers (true only when the instruction will actually enter the pipe)
    input  logic       accept0,
    output logic       stall0,

    input  logic       issue1_valid,
    input  logic       issue1_rs1_valid,
    input  logic       issue1_rs2_valid,
    input  logic [1:0] issue1_rs1_class,
    input  logic [1:0] issue1_rs2_class,
    input  logic [4:0] issue1_rs1,
    input  logic [4:0] issue1_rs2,
    input  logic       issue1_rs1_fwd,
    input  logic       issue1_rs2_fwd,
    input  logic       issue1_rd_valid,
    input  logic [1:0] issue1_rd_class,
    input  logic [4:0] issue1_rd,
    input  logic       accept1,
    output logic       stall1,
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
    input  logic [4:0] wb_vec_rd,
    // Global flush (e.g., branch taken) clears all busy bits
    input  logic       flush_all
);
    logic [31:0] busy_s;
    logic [31:0] busy_f;
    logic [31:0] busy_v;

    // NOTE: Some simulators don't reliably include signals referenced only inside
    // helper functions in the implicit sensitivity of always_comb. Keep the busy
    // vector references explicit here so stall updates when busy bits change.
    logic issue0_rs1_busy, issue0_rs2_busy, issue0_rd_busy;
    logic issue1_rs1_busy, issue1_rs2_busy, issue1_rd_busy;
    logic wb_match_s_rs1_0, wb_match_s_rs2_0, wb_match_s_rd_0;
    logic wb_match_f_rs1_0, wb_match_f_rs2_0, wb_match_f_rd_0;
    logic wb_match_v_rs1_0, wb_match_v_rs2_0, wb_match_v_rd_0;
    logic wb_match_s_rs1_1, wb_match_s_rs2_1, wb_match_s_rd_1;
    logic wb_match_f_rs1_1, wb_match_f_rs2_1, wb_match_f_rd_1;
    logic wb_match_v_rs1_1, wb_match_v_rs2_1, wb_match_v_rd_1;

    always_comb begin
        // Default
        issue0_rs1_busy = 1'b0;
        issue0_rs2_busy = 1'b0;
        issue0_rd_busy  = 1'b0;
        issue1_rs1_busy = 1'b0;
        issue1_rs2_busy = 1'b0;
        issue1_rd_busy  = 1'b0;

        // Precompute same-cycle writeback matches (used to relax stalls when WB happens now)
        wb_match_s_rs1_0 = wb_scalar_valid && (wb_scalar_rd == issue0_rs1);
        wb_match_s_rs2_0 = wb_scalar_valid && (wb_scalar_rd == issue0_rs2);
        wb_match_s_rd_0  = wb_scalar_valid && (wb_scalar_rd == issue0_rd);
        wb_match_f_rs1_0 = wb_fp_valid     && (wb_fp_rd     == issue0_rs1);
        wb_match_f_rs2_0 = wb_fp_valid     && (wb_fp_rd     == issue0_rs2);
        wb_match_f_rd_0  = wb_fp_valid     && (wb_fp_rd     == issue0_rd);
        wb_match_v_rs1_0 = wb_vec_valid    && (wb_vec_rd    == issue0_rs1);
        wb_match_v_rs2_0 = wb_vec_valid    && (wb_vec_rd    == issue0_rs2);
        wb_match_v_rd_0  = wb_vec_valid    && (wb_vec_rd    == issue0_rd);

        wb_match_s_rs1_1 = wb_scalar_valid && (wb_scalar_rd == issue1_rs1);
        wb_match_s_rs2_1 = wb_scalar_valid && (wb_scalar_rd == issue1_rs2);
        wb_match_s_rd_1  = wb_scalar_valid && (wb_scalar_rd == issue1_rd);
        wb_match_f_rs1_1 = wb_fp_valid     && (wb_fp_rd     == issue1_rs1);
        wb_match_f_rs2_1 = wb_fp_valid     && (wb_fp_rd     == issue1_rs2);
        wb_match_f_rd_1  = wb_fp_valid     && (wb_fp_rd     == issue1_rd);
        wb_match_v_rs1_1 = wb_vec_valid    && (wb_vec_rd    == issue1_rs1);
        wb_match_v_rs2_1 = wb_vec_valid    && (wb_vec_rd    == issue1_rs2);
        wb_match_v_rd_1  = wb_vec_valid    && (wb_vec_rd    == issue1_rd);

        // Slot0 src/dest busy selects
        if (issue0_rs1_valid) begin
            case (issue0_rs1_class)
                2'b00: issue0_rs1_busy = busy_s[issue0_rs1] && !wb_match_s_rs1_0 && !issue0_rs1_fwd;
                2'b01: issue0_rs1_busy = busy_f[issue0_rs1] && !wb_match_f_rs1_0;
                default: issue0_rs1_busy = busy_v[issue0_rs1] && !wb_match_v_rs1_0 && !issue0_rs1_fwd;
            endcase
        end
        if (issue0_rs2_valid) begin
            case (issue0_rs2_class)
                2'b00: issue0_rs2_busy = busy_s[issue0_rs2] && !wb_match_s_rs2_0 && !issue0_rs2_fwd;
                2'b01: issue0_rs2_busy = busy_f[issue0_rs2] && !wb_match_f_rs2_0;
                default: issue0_rs2_busy = busy_v[issue0_rs2] && !wb_match_v_rs2_0 && !issue0_rs2_fwd;
            endcase
        end
        // Only scalar x0 is hardwired to zero. FP0/V0 are treated as normal registers.
        if (issue0_rd_valid && !((issue0_rd_class == 2'b00) && (issue0_rd == 5'd0))) begin
            case (issue0_rd_class)
                2'b00: issue0_rd_busy = busy_s[issue0_rd] && !wb_match_s_rd_0;
                2'b01: issue0_rd_busy = busy_f[issue0_rd] && !wb_match_f_rd_0;
                default: issue0_rd_busy = busy_v[issue0_rd] && !wb_match_v_rd_0;
            endcase
        end

        // Slot1 src/dest busy selects
        if (issue1_rs1_valid) begin
            case (issue1_rs1_class)
                2'b00: issue1_rs1_busy = busy_s[issue1_rs1] && !wb_match_s_rs1_1 && !issue1_rs1_fwd;
                2'b01: issue1_rs1_busy = busy_f[issue1_rs1] && !wb_match_f_rs1_1;
                default: issue1_rs1_busy = busy_v[issue1_rs1] && !wb_match_v_rs1_1 && !issue1_rs1_fwd;
            endcase
        end
        if (issue1_rs2_valid) begin
            case (issue1_rs2_class)
                2'b00: issue1_rs2_busy = busy_s[issue1_rs2] && !wb_match_s_rs2_1 && !issue1_rs2_fwd;
                2'b01: issue1_rs2_busy = busy_f[issue1_rs2] && !wb_match_f_rs2_1;
                default: issue1_rs2_busy = busy_v[issue1_rs2] && !wb_match_v_rs2_1 && !issue1_rs2_fwd;
            endcase
        end
        if (issue1_rd_valid && !((issue1_rd_class == 2'b00) && (issue1_rd == 5'd0))) begin
            case (issue1_rd_class)
                2'b00: issue1_rd_busy = busy_s[issue1_rd] && !wb_match_s_rd_1;
                2'b01: issue1_rd_busy = busy_f[issue1_rd] && !wb_match_f_rd_1;
                default: issue1_rd_busy = busy_v[issue1_rd] && !wb_match_v_rd_1;
            endcase
        end

        stall0 = issue0_valid && (issue0_rs1_busy || issue0_rs2_busy || issue0_rd_busy);
        stall1 = issue1_valid && (issue1_rs1_busy || issue1_rs2_busy || issue1_rd_busy);

        // Same-cycle dependency guard: prevent slot1 from reading a dest written by slot0.
        if (issue0_valid && !stall0 && issue0_rd_valid && !((issue0_rd_class == 2'b00) && (issue0_rd == 5'd0)) && issue1_valid) begin
            if (issue1_rs1_valid && (issue1_rs1_class == issue0_rd_class) && (issue1_rs1 == issue0_rd)) stall1 = 1'b1;
            if (issue1_rs2_valid && (issue1_rs2_class == issue0_rd_class) && (issue1_rs2 == issue0_rd)) stall1 = 1'b1;
            if (issue1_rd_valid  && (issue1_rd_class  == issue0_rd_class) && (issue1_rd  == issue0_rd)
                && !((issue1_rd_class == 2'b00) && (issue1_rd == 5'd0))) stall1 = 1'b1;
        end
    end

    always_ff @(posedge clk) begin
        if (!rst_n || flush_all) begin
            busy_s <= '0;
            busy_f <= '0;
            busy_v <= '0;
        end else begin
            if (wb_scalar_valid) busy_s[wb_scalar_rd] <= 1'b0;
            if (wb_fp_valid)     busy_f[wb_fp_rd]    <= 1'b0;
            if (wb_vec_valid)    busy_v[wb_vec_rd]   <= 1'b0;

            // Flush logic: clear busy bit set by the instruction currently in RR
            if (flush_rr && flush_rr_rd_valid) begin
                case (flush_rr_rd_class)
                    2'b00: busy_s[flush_rr_rd] <= 1'b0;
                    2'b01: busy_f[flush_rr_rd] <= 1'b0;
                    default: busy_v[flush_rr_rd] <= 1'b0;
                endcase
            end

            if (accept0 && issue0_rd_valid && !((issue0_rd_class == 2'b00) && (issue0_rd == 5'd0))) begin
                case (issue0_rd_class)
                    2'b00: busy_s[issue0_rd] <= 1'b1;
                    2'b01: busy_f[issue0_rd] <= 1'b1;
                    default: busy_v[issue0_rd] <= 1'b1;
                endcase
            end

            if (accept1 && issue1_rd_valid && !((issue1_rd_class == 2'b00) && (issue1_rd == 5'd0))) begin
                case (issue1_rd_class)
                    2'b00: busy_s[issue1_rd] <= 1'b1;
                    2'b01: busy_f[issue1_rd] <= 1'b1;
                    default: busy_v[issue1_rd] <= 1'b1;
                endcase
            end
        end
    end
endmodule
