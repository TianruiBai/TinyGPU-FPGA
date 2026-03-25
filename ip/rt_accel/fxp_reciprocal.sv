`timescale 1ns/1ps
// ============================================================================
// Synthesizable Fixed-Point Reciprocal Unit (Q16.16)
//
// Computes 1/a in Q16.16 format using:
//   1. Leading-zero count to normalize input
//   2. 64-entry LUT for initial seed (8-bit precision)
//   3. One Newton-Raphson iteration: x' = x * (2 - a*x)
//
// Latency: 3 clock cycles (pipelined, 1 result/cycle throughput)
// Resource: 1 LUT ROM (64×16), 2 DSP48 multipliers
// ============================================================================
module fxp_reciprocal
  import rt_pkg::*;
(
    input  logic        clk,
    input  logic        rst_n,
    input  logic        in_valid,
    input  logic signed [31:0] a_in,        // Q16.16 input
    output logic        out_valid,
    output logic signed [31:0] recip_out     // Q16.16 ≈ 1/a_in
);

    // -----------------------------------------------------------------------
    // Stage 0: Compute |a|, sign, leading-zero count, normalize
    // -----------------------------------------------------------------------
    logic        s0_valid;
    logic        s0_sign;
    logic [31:0] s0_abs_a;
    logic [4:0]  s0_lzc;       // leading zero count of abs_a
    logic [31:0] s0_norm;      // normalized to [0.5, 1.0) in Q16.16 range
    logic [4:0]  s0_shift;     // how many bits we shifted left

    // Leading-zero count (combinational)
    function automatic [4:0] count_leading_zeros(input [31:0] val);
        logic [4:0] cnt;
        cnt = 0;
        for (int i = 31; i >= 0; i--) begin
            if (val[i]) return 5'(31 - i);
        end
        return 5'd31;
    endfunction

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            s0_valid <= 1'b0;
        end else begin
            s0_valid <= in_valid;
            if (in_valid) begin
                logic [31:0] abs_val;
                logic [4:0]  lzc;
                s0_sign  <= a_in[31];
                abs_val   = a_in[31] ? -a_in : a_in;
                s0_abs_a <= abs_val;
                lzc       = count_leading_zeros(abs_val);
                s0_lzc   <= lzc;
                // Normalize: shift left so MSB is at bit 31
                // After shift, we have a number in [2^31, 2^32) for nonzero input
                s0_norm  <= abs_val << lzc;
                s0_shift <= lzc;
            end
        end
    end

    // -----------------------------------------------------------------------
    // Reciprocal LUT: 64 entries, indexed by top 6 bits of normalized value
    // (bits [30:25] after the implicit leading 1 at bit 31)
    //
    // For normalized input n in [0.5, 1.0), recip ≈ 1/n in [1.0, 2.0)
    // LUT stores 16-bit fractional part: recip = {1, LUT[idx]} in UQ1.16
    // Actual formula: LUT[i] = round(2^16 / (1 + i/64) - 2^16)
    // -----------------------------------------------------------------------
    logic [15:0] recip_lut [0:63];

    initial begin
        // Precomputed: LUT[i] = round(65536 * (1/(1 + i/64) - 1))
        // = round(65536 * (64 - i - 64) / (64 + i)) ... simplified:
        // = round(65536 / (1 + i/64)) - 65536
        recip_lut[ 0] = 16'd0;     // 1/1.000 = 1.000  → frac=0
        recip_lut[ 1] = 16'd64520; // 1/1.016 ≈ 0.9846 → stored as 2/1.016 - 1 frac
        recip_lut[ 2] = 16'd63550;
        recip_lut[ 3] = 16'd62601;
        recip_lut[ 4] = 16'd61681;
        recip_lut[ 5] = 16'd60787;
        recip_lut[ 6] = 16'd59919;
        recip_lut[ 7] = 16'd59074;
        recip_lut[ 8] = 16'd58254;
        recip_lut[ 9] = 16'd57457;
        recip_lut[10] = 16'd56680;
        recip_lut[11] = 16'd55924;
        recip_lut[12] = 16'd55188;
        recip_lut[13] = 16'd54471;
        recip_lut[14] = 16'd53773;
        recip_lut[15] = 16'd53092;
        recip_lut[16] = 16'd52429;
        recip_lut[17] = 16'd51782;
        recip_lut[18] = 16'd51150;
        recip_lut[19] = 16'd50534;
        recip_lut[20] = 16'd49932;
        recip_lut[21] = 16'd49344;
        recip_lut[22] = 16'd48770;
        recip_lut[23] = 16'd48210;
        recip_lut[24] = 16'd47662;
        recip_lut[25] = 16'd47127;
        recip_lut[26] = 16'd46603;
        recip_lut[27] = 16'd46091;
        recip_lut[28] = 16'd45590;
        recip_lut[29] = 16'd45100;
        recip_lut[30] = 16'd44620;
        recip_lut[31] = 16'd44150;
        recip_lut[32] = 16'd43691;
        recip_lut[33] = 16'd43240;
        recip_lut[34] = 16'd42799;
        recip_lut[35] = 16'd42366;
        recip_lut[36] = 16'd41943;
        recip_lut[37] = 16'd41527;
        recip_lut[38] = 16'd41120;
        recip_lut[39] = 16'd40721;
        recip_lut[40] = 16'd40330;
        recip_lut[41] = 16'd39946;
        recip_lut[42] = 16'd39568;
        recip_lut[43] = 16'd39199;
        recip_lut[44] = 16'd38836;
        recip_lut[45] = 16'd38480;
        recip_lut[46] = 16'd38130;
        recip_lut[47] = 16'd37787;
        recip_lut[48] = 16'd37449;
        recip_lut[49] = 16'd37118;
        recip_lut[50] = 16'd36792;
        recip_lut[51] = 16'd36472;
        recip_lut[52] = 16'd36157;
        recip_lut[53] = 16'd35848;
        recip_lut[54] = 16'd35544;
        recip_lut[55] = 16'd35245;
        recip_lut[56] = 16'd34953;
        recip_lut[57] = 16'd34664;
        recip_lut[58] = 16'd34380;
        recip_lut[59] = 16'd34101;
        recip_lut[60] = 16'd33826;
        recip_lut[61] = 16'd33556;
        recip_lut[62] = 16'd33290;
        recip_lut[63] = 16'd33027;
    end

    // -----------------------------------------------------------------------
    // Stage 1: LUT lookup + construct initial estimate
    // -----------------------------------------------------------------------
    logic        s1_valid;
    logic        s1_sign;
    logic [31:0] s1_abs_a;  // original (un-normalized) |a|
    logic [4:0]  s1_shift;
    logic [31:0] s1_x0;     // initial reciprocal estimate (Q16.16)

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            s1_valid <= 1'b0;
        end else begin
            s1_valid <= s0_valid;
            if (s0_valid) begin
                logic [5:0]  lut_idx;
                logic [15:0] lut_val;
                logic [31:0] raw_recip;
                logic [4:0]  result_shift;

                s1_sign  <= s0_sign;
                s1_abs_a <= s0_abs_a;
                s1_shift <= s0_shift;

                // Index into LUT using bits [30:25] of normalized value
                // (bit 31 is always 1 after normalization for nonzero)
                lut_idx = s0_norm[30:25];
                lut_val = recip_lut[lut_idx];

                // Construct initial estimate:
                // normalized value ≈ 2^31 * m where m ∈ [1, 2)
                // so recip(norm) ≈ (1 + lut_val/65536) / 2^31
                // We need to un-normalize by shifting by (31 - lzc + 16) to get Q16.16
                // recip(a) = recip(norm) * 2^lzc = recip(norm_frac) * 2^(lzc - 15)
                // raw_recip is in Q16.16: {1, lut_val} is effectively UQ1.16 ≈ 2/norm
                // Since norm = |a| << lzc, and |a| is Q16.16:
                //   1/|a| = (1/norm) * 2^lzc
                //   norm has implicit binary point at bit 16 (Q16.16 shifted left by lzc)
                //   1/norm (in Q16.16) = 2^(16+16) / norm = 2^32 / (|a| << lzc)
                //
                // Simple approach: construct estimate directly
                // raw_recip = {1'b0, 1'b1, lut_val[15:0], 14'b0} = UQ1.16 << 14 = bit[30:14]
                // Then shift right by (31 - lzc - 16) = (15 - lzc) if lzc <= 15
                // Or shift left by (lzc - 15) if lzc > 15
                //
                // Better: estimate = {1, lut_val} = 17-bit UQ1.16
                // This approximates 1/m where m = normalized[31:0] / 2^31
                // actual_a = normalized >> lzc (Q16.16 format)
                // 1/actual_a = (1/normalized) * 2^lzc
                // 1/normalized ≈ estimate / 2^16 (since estimate is UQ1.16)
                // But normalized = |a| << lzc in raw 32-bit
                // |a| in Q16.16 means |a| = raw_bits / 2^16
                // normalized = raw_bits << lzc
                // 1/(raw_bits / 2^16) = 2^16 / raw_bits
                // = 2^16 / (normalized >> lzc) = 2^(16+lzc) / normalized
                // In Q16.16 output: result = 2^(16+16+lzc) / normalized = 2^(32+lzc) / normalized
                // estimate ≈ 2^(16+1) / (normalized/2^31) = 2^48 / normalized (in raw bits shifted)
                //
                // Practical: start with the LUT value in a known position and shift
                raw_recip = {15'b0, 1'b1, lut_val};  // UQ17.16 (bit16 = integer 1)

                // This represents ≈ 2^32 / normalized_top17bits
                // We need: result = 2^32 / |a| in Q16.16
                // |a| = norm >> lzc, so 1/|a| = (1/norm) * 2^lzc
                // Our LUT gives 1/(norm >> 15) approximately (the top bits)
                // After de-normalization: shift result left by (lzc - 15)
                // But we're in Q16.16 so need to account for fixed-point position
                //
                // Simplified correct approach:
                // lzc is the leading zeros of abs_a (a 32-bit Q16.16 number)
                // After shift, norm = abs_a << lzc, with MSB at bit 31
                // The LUT estimates 1/norm_frac where norm_frac = norm/2^31 ∈ [1,2)
                // So LUT gives recip of norm_frac in UQ1.16
                // 1/abs_a = (1/norm_frac) * 2^(lzc-31)   [since abs_a = norm_frac * 2^(31-lzc)]
                // In Q16.16: result = (1/norm_frac) * 2^(lzc-31) * 2^16
                //                   = raw_recip * 2^(lzc - 31 + 16)
                //                   = raw_recip * 2^(lzc - 15)
                // Since raw_recip has its value in bits [16:0] (UQ1.16 = 1.xxx * 2^0):
                //   result_bits = raw_recip << (lzc - 15) if lzc >= 15
                //   result_bits = raw_recip >> (15 - lzc) if lzc < 15

                if (s0_lzc >= 5'd15)
                    s1_x0 <= raw_recip << (s0_lzc - 5'd15);
                else
                    s1_x0 <= raw_recip >> (5'd15 - s0_lzc);
            end
        end
    end

    // -----------------------------------------------------------------------
    // Stage 2: One Newton-Raphson iteration: x' = x * (2 - a*x)
    // This doubles the precision (~12 bits from LUT → ~24 bits after NR)
    // -----------------------------------------------------------------------
    logic        s2_valid;
    logic        s2_sign;
    logic signed [31:0] s2_result;

    // Combinational NR computation on s1 outputs
    logic signed [63:0] nr_ax;       // a * x0 in full precision
    logic signed [31:0] nr_ax_q16;   // truncated to Q16.16
    logic signed [31:0] nr_two_minus; // 2.0 - a*x0 (Q16.16)
    logic signed [63:0] nr_refined;  // x0 * (2 - a*x0)
    logic signed [31:0] nr_result;

    always_comb begin
        // a * x0 (both Q16.16, result is Q32.32, take Q16.16 portion)
        nr_ax        = 64'(signed'({1'b0, s1_abs_a})) * 64'(signed'({1'b0, s1_x0}));
        nr_ax_q16    = 32'(nr_ax >>> FXP_FRAC);
        // 2.0 - a*x0
        nr_two_minus = (FXP_ONE <<< 1) - nr_ax_q16;
        // x' = x0 * (2 - a*x0)
        nr_refined   = 64'(signed'({1'b0, s1_x0})) * 64'(nr_two_minus);
        nr_result    = 32'(nr_refined >>> FXP_FRAC);
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            s2_valid <= 1'b0;
        end else begin
            s2_valid <= s1_valid;
            if (s1_valid) begin
                s2_sign <= s1_sign;
                // Clamp to FXP_MAX for very small inputs
                if (s1_abs_a < FXP_EPSILON)
                    s2_result <= FXP_MAX;
                else
                    s2_result <= nr_result;
            end
        end
    end

    // -----------------------------------------------------------------------
    // Output: apply sign
    // -----------------------------------------------------------------------
    assign out_valid = s2_valid;
    assign recip_out = s2_sign ? -s2_result : s2_result;

endmodule : fxp_reciprocal
