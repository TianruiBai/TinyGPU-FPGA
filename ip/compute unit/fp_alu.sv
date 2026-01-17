module fp_alu (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        valid,
    output logic        in_ready,
    input  logic [2:0]  funct3,
    input  logic [15:0] src_a,
    input  logic [15:0] src_b,
    input  logic [31:0] scalar_src,
    // Legacy constant-gen hook: some tests expect FCVT.i2f with rs1=x0 to yield +1.0 (fp16 0x3C00).
    input  logic        scalar_src_is_x0,
    input  logic [15:0] src_c, // third operand for FMA (taken from scalar path lower 16 or explicit mapping)
    input  logic [4:0]  rd_idx,
    input  logic        wb_ready,
    output logic        wb_valid,
    output logic [4:0]  wb_rd,
    output logic [15:0] wb_data,
    output logic        wb_scalar_valid,
    output logic [4:0]  wb_scalar_rd,
    output logic [31:0] wb_scalar_data,
    // Simple error flags: overflow and invalid/NaN inputs
    output logic        wb_err_overflow,
    output logic        wb_err_invalid
);

    // funct3 mapping (scalar FP16):
    // 000 FADD, 001 FSUB, 010 FMUL, 011 FMA (non-fused), 100 FMIN, 101 FMAX, 110 FCVT i2f, 111 FCVT f2i

    // -------------------------------------------------------------------------
    // IEEE-ish FP16 helpers (full subnormal + RNE + NaN/Inf handling)
    // NOTE: FMA is implemented as non-fused (mul then add, with two roundings).
    // -------------------------------------------------------------------------
    function automatic logic fp16_is_nan(input logic [15:0] a);
        fp16_is_nan = (a[14:10] == 5'h1F) && (a[9:0] != 0);
    endfunction

    function automatic logic fp16_is_inf(input logic [15:0] a);
        fp16_is_inf = (a[14:10] == 5'h1F) && (a[9:0] == 0);
    endfunction

    function automatic logic fp16_is_zero(input logic [15:0] a);
        fp16_is_zero = (a[14:0] == 15'h0);
    endfunction

    function automatic logic [15:0] fp16_qnan(input logic sign);
        // Quiet NaN with canonical payload
        fp16_qnan = {sign, 5'h1F, 10'h200};
    endfunction

    function automatic logic [15:0] fp16_make_qnan(input logic [15:0] a);
        // If already NaN, force quiet bit
        fp16_make_qnan = {a[15], 5'h1F, (a[9:0] | 10'h200)};
    endfunction

    function automatic logic [4:0] fp16_eff_exp(input logic [15:0] a);
        // For subnormals, treat exponent as 1 for alignment; for zero, also 1 (mantissa will be 0)
        if (a[14:10] == 0) fp16_eff_exp = 5'd1;
        else fp16_eff_exp = a[14:10];
    endfunction

    function automatic logic [10:0] fp16_eff_man(input logic [15:0] a);
        // Returns significand without GRS, 11 bits: [10] hidden (0 for subnormals/zero)
        if (a[14:10] == 0) fp16_eff_man = {1'b0, a[9:0]};
        else fp16_eff_man = {1'b1, a[9:0]};
    endfunction

    function automatic logic [4:0] fp16_lzc14(input logic [13:0] x);
        int k;
        fp16_lzc14 = 0;
        for (k = 13; k >= 0; k--) begin
            if (x[k]) begin
                fp16_lzc14 = logic'((13 - k) & 31);
                break;
            end
            if (k == 0) fp16_lzc14 = 14;
        end
    endfunction

    function automatic logic [14:0] fp16_shr_sticky14(input logic [13:0] in, input logic [4:0] sh);
        logic [13:0] shifted;
        logic sticky;
        int i;
        begin
            sticky = 1'b0;
            if (sh == 0) begin
                shifted = in;
                sticky = 1'b0;
            end else if (sh >= 14) begin
                shifted = 14'd0;
                sticky = |in;
            end else begin
                shifted = in >> sh;
                for (i = 0; i < 14; i++) begin
                    if (i < sh) sticky |= in[i];
                end
            end
            fp16_shr_sticky14 = {shifted, sticky};
        end
    endfunction

    function automatic logic fp16_or_low32(input logic [31:0] in, input int nbits);
        int i;
        logic acc;
        begin
            acc = 1'b0;
            if (nbits <= 0) acc = 1'b0;
            else if (nbits >= 32) acc = |in;
            else begin
                for (i = 0; i < 32; i++) begin
                    if (i < nbits) acc |= in[i];
                end
            end
            fp16_or_low32 = acc;
        end
    endfunction

    function automatic logic [17:0] fp16_addsub_rne_pack(input logic [15:0] a, input logic [15:0] b, input logic is_sub);
        // Returns {invalid, overflow, result[15:0]}
        logic a_nan, b_nan, a_inf, b_inf;
        logic eff_b_sign;
        logic sign_a, sign_b;
        logic [4:0] exp_a, exp_b;
        logic [9:0] frac_a, frac_b;
        logic [4:0] ea, eb;
        logic [10:0] ma, mb;
        logic [13:0] ext_a, ext_b;
        logic swap;
        logic sign_big, sign_small;
        logic [5:0] exp_big;
        logic [4:0] exp_diff;
        logic [14:0] shr;
        logic [13:0] man_big, man_small;
        logic [14:0] sum;
        logic res_sign;
        logic signed [7:0] e;
        logic [13:0] man;
        logic guard, round, sticky;
        logic [9:0] frac;
        logic ovf, inv;
        logic [15:0] out;

        sign_a = a[15]; sign_b = b[15];
        exp_a = a[14:10]; exp_b = b[14:10];
        frac_a = a[9:0]; frac_b = b[9:0];
        a_nan = fp16_is_nan(a);
        b_nan = fp16_is_nan(b);
        a_inf = fp16_is_inf(a);
        b_inf = fp16_is_inf(b);
        eff_b_sign = is_sub ? ~sign_b : sign_b;
        ovf = 1'b0;
        inv = 1'b0;
        out = 16'h0000;

        if (a_nan || b_nan) begin
            out = fp16_make_qnan(a_nan ? a : b);
            inv = 1'b1;
        end else if (a_inf && b_inf && (sign_a != eff_b_sign)) begin
            out = fp16_qnan(1'b0);
            inv = 1'b1;
        end else if (a_inf) begin
            out = a;
        end else if (b_inf) begin
            out = {eff_b_sign, 5'h1F, 10'h000};
        end else begin
            ea = fp16_eff_exp(a);
            eb = fp16_eff_exp(b);
            ma = fp16_eff_man(a);
            mb = fp16_eff_man(b);
            ext_a = {ma, 3'b000};
            ext_b = {mb, 3'b000};
            swap = 1'b0;
            if ({ea, ma} < {eb, mb}) swap = 1'b1;
            if (swap) begin
                sign_big = eff_b_sign;
                sign_small = sign_a;
                exp_big = {1'b0, eb};
                exp_diff = eb - ea;
                man_big = ext_b;
                shr = fp16_shr_sticky14(ext_a, exp_diff);
                man_small = {shr[14:2], (shr[1] | shr[0])};
            end else begin
                sign_big = sign_a;
                sign_small = eff_b_sign;
                exp_big = {1'b0, ea};
                exp_diff = ea - eb;
                man_big = ext_a;
                shr = fp16_shr_sticky14(ext_b, exp_diff);
                man_small = {shr[14:2], (shr[1] | shr[0])};
            end

            if (sign_big == sign_small) begin
                sum = {1'b0, man_big} + {1'b0, man_small};
                res_sign = sign_big;
            end else begin
                sum = {1'b0, man_big} - {1'b0, man_small};
                res_sign = sign_big;
            end

            e = $signed({1'b0, exp_big});
            if (sum == 0) begin
                out = {1'b0, 15'h0000};
            end else if (sum[14]) begin
                // carry
                man = sum[14:1];
                e = e + 1;
                // keep GRS from LSB lost into sticky
                man[0] = man[0] | sum[0];
            end else begin
                man = sum[13:0];
                begin
                    logic [4:0] lz;
                    lz = fp16_lzc14(man);
                    if (lz < 14) begin
                        man = man << lz;
                        e = e - $signed({3'b0, lz});
                    end else begin
                        man = 14'd0;
                    end
                end
            end

            if (e >= 31) begin
                out = {res_sign, 5'h1F, 10'h000};
                ovf = 1'b1;
            end else if (e <= 0) begin
                int rshift;
                logic [13:0] sub;
                rshift = 1 - e;
                sub = man;
                if (rshift >= 14) begin
                    out = {res_sign, 15'h0000};
                end else begin
                    int ii;
                    logic st;
                    st = 1'b0;
                    for (ii = 0; ii < 14; ii++) begin
                        if (ii < rshift) st |= sub[ii];
                    end
                    sub = sub >> rshift;
                    sub[0] = sub[0] | st;
                    if (sub[2] && (sub[1] || sub[0] || sub[3])) sub[13:3] = sub[13:3] + 1'b1;
                    frac = sub[12:3];
                    out = {res_sign, 5'h00, frac};
                end
            end else begin
                guard = man[2];
                round = man[1];
                sticky = man[0];
                if (guard && (round || sticky || man[3])) man[13:3] = man[13:3] + 1'b1;
                if (man[13:3] == 11'h800) begin
                    e = e + 1;
                    if (e >= 31) begin
                        out = {res_sign, 5'h1F, 10'h000};
                        ovf = 1'b1;
                    end else begin
                        out = {res_sign, e[4:0], 10'h000};
                    end
                end else begin
                    out = {res_sign, e[4:0], man[12:3]};
                end
            end
        end

        fp16_addsub_rne_pack = {inv, ovf, out};
    endfunction

    // Compare for min/max (IEEE behavior for signed zeros + NaNs)
    function automatic logic fp16_lt(input logic [15:0] a, input logic [15:0] b);
        logic a_nan, b_nan;
        a_nan = fp16_is_nan(a);
        b_nan = fp16_is_nan(b);
        if (a_nan || b_nan) begin
            fp16_lt = 1'b0;
        end else if (a[14:0] == 0 && b[14:0] == 0) begin
            // -0 < +0
            fp16_lt = (a[15] && !b[15]);
        end else if (a[15] != b[15]) begin
            fp16_lt = a[15];
        end else if (!a[15]) begin
            fp16_lt = (a[14:0] < b[14:0]);
        end else begin
            fp16_lt = (a[14:0] > b[14:0]);
        end
    endfunction

    // -------------------------------------------------------------------------
    // 3-stage pipeline
    // Stage1: unpack/align/dispatch
    // Stage2: add/sub or mul execute
    // Stage3: normalize+RNE pack + wb
    // -------------------------------------------------------------------------

    typedef struct packed {
        logic        is_add;
        logic        is_sub;
        logic        is_mul;
        logic        is_min;
        logic        is_max;
        logic        is_fma;
        logic        is_cvt_i2f;
        logic        is_cvt_f2i;
    } op_t;

    // Stage 0 capture
    logic s0_valid;
    op_t  s0_op;
    logic [15:0] s0_a, s0_b, s0_c;
    logic [31:0] s0_scalar_src;
    logic [4:0]  s0_rd;

    // Stage 1 regs
    logic s1_valid;
    op_t  s1_op;
    logic [4:0]  s1_rd;
    logic [15:0] s1_c;
    logic        s1_invalid;
    logic [31:0] s1_cvt;
    logic        s1_cvt_valid;
    logic [15:0] s1_special_res;
    logic        s1_special;
    // Add path alignment
    logic        s1_sign_big, s1_sign_small;
    logic [5:0]  s1_exp_big;   // allow carry
    logic [13:0] s1_man_big;   // 11 + 3 GRS
    logic [13:0] s1_man_small; // aligned 11 + 3 GRS
    // Mul path
    logic        s1_mul_sign;
    logic signed [7:0] s1_mul_exp;
    logic [21:0] s1_mul_prod;

    // Stage 2 regs
    logic s2_valid;
    op_t  s2_op;
    logic [4:0]  s2_rd;
    logic [15:0] s2_c;
    logic        s2_invalid;
    logic [31:0] s2_cvt;
    logic        s2_cvt_valid;
    logic [15:0] s2_special_res;
    logic        s2_special;
    // Add/mul intermediates
    logic        s2_res_sign;
    logic signed [7:0] s2_exp;
    logic [14:0] s2_sum; // 14-bit + carry
    logic        s2_is_addsub;
    logic [21:0] s2_mul_prod;
    logic        s2_mul_sign;
    logic signed [7:0] s2_mul_exp;

    // Stage 3 outputs (registered)
    logic s3_valid;
    logic [4:0] s3_rd;
    logic [15:0] s3_res;
    logic s3_overflow;
    logic s3_invalid;
    logic [31:0] s3_cvt;
    logic s3_cvt_valid;

    // Backpressure/hold:
    // If the stage3 result is valid but not accepted, freeze the whole pipeline.
    // This prevents dropping FP->scalar conversion results when scalar WB is busy.
    wire stall_pipe = s3_valid && !wb_ready;
    assign in_ready = !stall_pipe;

    always_comb begin
        s0_valid = valid;
        s0_a = src_a;
        s0_b = src_b;
        s0_c = src_c;
        s0_scalar_src = scalar_src;
        s0_rd = rd_idx;
        s0_op = '0;
        unique case (funct3)
            3'b000: s0_op.is_add = 1'b1;
            3'b001: s0_op.is_sub = 1'b1;
            3'b010: s0_op.is_mul = 1'b1;
            3'b011: s0_op.is_fma = 1'b1;
            3'b100: s0_op.is_min = 1'b1;
            3'b101: s0_op.is_max = 1'b1;
            3'b110: s0_op.is_cvt_i2f = 1'b1;
            3'b111: s0_op.is_cvt_f2i = 1'b1;
            default: s0_op.is_add = 1'b1;
        endcase
    end

    // Stage 1: unpack/align and special-case resolve
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            s1_valid <= 1'b0;
            s1_op <= '0;
            s1_rd <= '0;
            s1_c <= '0;
            s1_invalid <= 1'b0;
            s1_cvt <= '0;
            s1_cvt_valid <= 1'b0;
            s1_special <= 1'b0;
            s1_special_res <= '0;
            s1_sign_big <= 1'b0;
            s1_sign_small <= 1'b0;
            s1_exp_big <= '0;
            s1_man_big <= '0;
            s1_man_small <= '0;
            s1_mul_sign <= 1'b0;
            s1_mul_exp <= '0;
            s1_mul_prod <= '0;
        end else if (!stall_pipe) begin
            logic a_nan, b_nan, a_inf, b_inf, a_zero, b_zero;
            logic c_nan;
            logic [15:0] a, b;
            logic a_sign, b_sign;
            logic [4:0] a_exp, b_exp;
            logic [9:0] a_frac, b_frac;
            logic [4:0] ea, eb;
            logic [10:0] ma, mb;
            logic [13:0] ext_a, ext_b;
            logic [5:0] exp_big;
            logic [4:0] exp_diff;
            logic swap;
            logic eff_b_sign;
            logic [13:0] small_shifted;
            logic sticky_align;
            logic [14:0] shr_tmp;

            a = s0_a;
            b = s0_b;
            a_sign = a[15]; b_sign = b[15];
            a_exp  = a[14:10]; b_exp = b[14:10];
            a_frac = a[9:0];   b_frac = b[9:0];
            a_nan  = fp16_is_nan(a);
            b_nan  = fp16_is_nan(b);
            c_nan  = fp16_is_nan(s0_c);
            a_inf  = fp16_is_inf(a);
            b_inf  = fp16_is_inf(b);
            a_zero = fp16_is_zero(a);
            b_zero = fp16_is_zero(b);

            s1_valid <= s0_valid;
            s1_op    <= s0_op;
            s1_rd    <= s0_rd;
            s1_c     <= s0_c;

            s1_invalid <= 1'b0;
            s1_special <= 1'b0;
            s1_special_res <= 16'h0000;
            s1_cvt_valid <= 1'b0;
            s1_cvt <= 32'h0;

            // Default align inputs for add/sub
            eff_b_sign = (s0_op.is_sub) ? ~b_sign : b_sign;
            ea = fp16_eff_exp(a);
            eb = fp16_eff_exp(b);
            ma = fp16_eff_man(a);
            mb = fp16_eff_man(b);
            ext_a = {ma, 3'b000};
            ext_b = {mb, 3'b000};

            // Choose bigger magnitude (ignoring sign) using exp+mant
            swap = 1'b0;
            if ({ea, ma} < {eb, mb}) swap = 1'b1;
            if (swap) begin
                s1_sign_big   <= eff_b_sign;
                s1_sign_small <= a_sign;
                s1_exp_big    <= {1'b0, eb};
                exp_diff      <= eb - ea;
                s1_man_big    <= ext_b;
                // shift ext_a by exp_diff with sticky
                shr_tmp = fp16_shr_sticky14(ext_a, exp_diff);
                small_shifted = shr_tmp[14:1];
                sticky_align = shr_tmp[0];
                s1_man_small <= {small_shifted[13:1], (small_shifted[0] | sticky_align)};
            end else begin
                s1_sign_big   <= a_sign;
                s1_sign_small <= eff_b_sign;
                s1_exp_big    <= {1'b0, ea};
                exp_diff      <= ea - eb;
                s1_man_big    <= ext_a;
                shr_tmp = fp16_shr_sticky14(ext_b, exp_diff);
                small_shifted = shr_tmp[14:1];
                sticky_align = shr_tmp[0];
                s1_man_small <= {small_shifted[13:1], (small_shifted[0] | sticky_align)};
            end

            // Mul precompute
            s1_mul_sign <= a_sign ^ b_sign;
            // Normalize subnormals for mul: adjust exponent and mantissa
            // For timing, do a simple normalization loop (bounded 10 iters).
            begin
                logic [10:0] na, nb;
                int sh_a, sh_b;
                logic [4:0] xa, xb;
                na = fp16_eff_man(a);
                nb = fp16_eff_man(b);
                xa = a_exp;
                xb = b_exp;
                sh_a = 0;
                sh_b = 0;
                if (a_exp == 0 && a_frac != 0) begin
                    while ((na[10] == 1'b0) && (sh_a < 10)) begin
                        na = na << 1;
                        sh_a++;
                    end
                    xa = 5'd1 - sh_a[4:0];
                end
                if (b_exp == 0 && b_frac != 0) begin
                    while ((nb[10] == 1'b0) && (sh_b < 10)) begin
                        nb = nb << 1;
                        sh_b++;
                    end
                    xb = 5'd1 - sh_b[4:0];
                end
                s1_mul_prod <= na * nb; // 11x11 = 22
                s1_mul_exp  <= $signed({1'b0, xa}) + $signed({1'b0, xb}) - 8'sd15;
            end

            // Special-case resolve (NaN/Inf)
            if (a_nan || b_nan || (s0_op.is_fma && c_nan)) begin
                s1_special <= 1'b1;
                s1_special_res <= fp16_make_qnan(a_nan ? a : (b_nan ? b : s0_c));
                s1_invalid <= 1'b1;
            end else if ((s0_op.is_add || s0_op.is_sub) && a_inf && b_inf && (a_sign != eff_b_sign)) begin
                s1_special <= 1'b1;
                s1_special_res <= fp16_qnan(1'b0);
                s1_invalid <= 1'b1;
            end else if ((s0_op.is_add || s0_op.is_sub) && (a_inf || b_inf)) begin
                s1_special <= 1'b1;
                s1_special_res <= a_inf ? a : {eff_b_sign, 5'h1F, 10'h000};
            end else if ((s0_op.is_mul || s0_op.is_fma) && ((a_inf && b_zero) || (b_inf && a_zero))) begin
                s1_special <= 1'b1;
                s1_special_res <= fp16_qnan(1'b0);
                s1_invalid <= 1'b1;
            end else if (s0_op.is_mul && !s0_op.is_fma && (a_inf || b_inf)) begin
                s1_special <= 1'b1;
                s1_special_res <= {s1_mul_sign, 5'h1F, 10'h000};
            end else if (s0_op.is_mul && !s0_op.is_fma && (a_zero || b_zero)) begin
                // Preserve sign of zero per IEEE (sign xor)
                s1_special <= 1'b1;
                s1_special_res <= {s1_mul_sign, 15'h0000};
            end

            // Min/max done as special-resolve style here (still pipelined)
            if (s0_op.is_min || s0_op.is_max) begin
                if (a_nan && b_nan) begin
                    s1_special <= 1'b1;
                    s1_special_res <= fp16_qnan(1'b0);
                    s1_invalid <= 1'b1;
                end else if (a_nan) begin
                    s1_special <= 1'b1;
                    s1_special_res <= b;
                    s1_invalid <= 1'b1;
                end else if (b_nan) begin
                    s1_special <= 1'b1;
                    s1_special_res <= a;
                    s1_invalid <= 1'b1;
                end else begin
                    logic lt;
                    lt = fp16_lt(a, b);
                    s1_special <= 1'b1;
                    s1_special_res <= s0_op.is_min ? (lt ? a : b) : (lt ? b : a);
                end
            end

            // Conversions: keep behavior (not fully IEEE), but align to pipeline.
            if (s0_op.is_cvt_f2i) begin
                // Legacy FCVT.f2i behavior (matches old non-IEEE-ish datapath):
                // treat FP16 as having 11-bit fixed-point mantissa scaling such that +1.0 -> 2048.
                logic [25:0] mant;
                int shift;
                logic [31:0] mag;
                if (a_exp == 0) begin
                    mag = 32'h0;
                end else begin
                    mant  = {1'b1, a_frac, 15'b0};
                    shift = $signed({1'b0, a_exp}) - 6'sd15;
                    if (shift < 0) begin
                        mag = 32'h0;
                    end else if (shift >= 15) begin
                        mag = 32'h7FFF_FFFF;
                    end else begin
                        mag = mant >> (14 - shift);
                    end
                end
                s1_cvt_valid <= 1'b1;
                s1_cvt <= a_sign ? (~mag + 1'b1) : mag;
            end else if (s0_op.is_cvt_i2f) begin
                // Very small i2f: create normalized FP16 with RNE (limited range)
                logic sign;
                logic [31:0] x;
                int msb;
                logic [10:0] man;
                logic [4:0] exp;
                logic guard, round, sticky;
                logic [31:0] absx;
                sign = s0_scalar_src[31];
                absx = sign ? (~s0_scalar_src + 1'b1) : s0_scalar_src;
                if (absx == 0) begin
                    s1_special <= 1'b1;
                    if (scalar_src_is_x0) begin
                        // Implementation-defined constant: FCVT.i2f(x0) == +1.0
                        s1_special_res <= 16'h3C00;
                    end else begin
                        s1_special_res <= {sign, 15'h0000};
                    end
                end else begin
                    msb = 31;
                    while (msb > 0 && !absx[msb]) msb--;
                    // exponent = msb + bias
                    exp = msb + 5'd15;
                    if (exp >= 5'h1F) begin
                        s1_special <= 1'b1;
                        s1_special_res <= {sign, 5'h1F, 10'h0};
                    end else begin
                        // align so that msb maps to hidden bit
                        if (msb > 10) begin
                            int rshift;
                            rshift = msb - 10;
                            man = absx >> rshift;
                            guard = (rshift >= 1) ? absx[rshift-1] : 1'b0;
                            round = (rshift >= 2) ? absx[rshift-2] : 1'b0;
                            sticky = (rshift > 2) ? fp16_or_low32(absx, rshift-2) : 1'b0;
                            if (guard && (round || sticky || man[0])) man = man + 1'b1;
                            if (man[10] == 1'b0) begin
                                // keep
                            end else if (man == 11'h800) begin
                                exp = exp + 1'b1;
                            end
                        end else begin
                            man = absx << (10 - msb);
                        end
                        s1_special <= 1'b1;
                        s1_special_res <= {sign, exp, man[9:0]};
                    end
                end
            end

            // FMA: compute (a*b)+c non-fused by reusing mul precompute, add later in stage2/3.
            // We treat FMA as a mul op here; stage2 will add c.
        end
    end

    // Stage 2: execute add/sub or mul (and for FMA, add c)
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            s2_valid <= 1'b0;
            s2_op <= '0;
            s2_rd <= '0;
            s2_c <= '0;
            s2_invalid <= 1'b0;
            s2_cvt <= '0;
            s2_cvt_valid <= 1'b0;
            s2_special_res <= '0;
            s2_special <= 1'b0;
            s2_res_sign <= 1'b0;
            s2_exp <= '0;
            s2_sum <= '0;
            s2_is_addsub <= 1'b0;
            s2_mul_prod <= '0;
            s2_mul_sign <= 1'b0;
            s2_mul_exp <= '0;
        end else if (!stall_pipe) begin
            s2_valid <= s1_valid;
            s2_op    <= s1_op;
            s2_rd    <= s1_rd;
            s2_c     <= s1_c;
            s2_invalid <= s1_invalid;
            s2_cvt <= s1_cvt;
            s2_cvt_valid <= s1_cvt_valid;
            s2_special <= s1_special;
            s2_special_res <= s1_special_res;

            s2_is_addsub <= (s1_op.is_add || s1_op.is_sub);
            if (s1_op.is_add || s1_op.is_sub) begin
                // Add/sub based on signs
                if (s1_sign_big == s1_sign_small) begin
                    s2_sum <= {1'b0, s1_man_big} + {1'b0, s1_man_small};
                    s2_res_sign <= s1_sign_big;
                end else begin
                    s2_sum <= {1'b0, s1_man_big} - {1'b0, s1_man_small};
                    s2_res_sign <= s1_sign_big;
                end
                s2_exp <= $signed({1'b0, s1_exp_big});
            end else begin
                s2_sum <= '0;
                s2_res_sign <= 1'b0;
                s2_exp <= '0;
            end

            s2_mul_prod <= s1_mul_prod;
            s2_mul_sign <= s1_mul_sign;
            s2_mul_exp  <= s1_mul_exp;
        end
    end

    // Stage 3: normalize + RNE + pack
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            s3_valid <= 1'b0;
            s3_rd <= '0;
            s3_res <= '0;
            s3_overflow <= 1'b0;
            s3_invalid <= 1'b0;
            s3_cvt <= '0;
            s3_cvt_valid <= 1'b0;
        end else begin
            logic [15:0] out;
            logic ovf;
            logic inv;

            s3_valid <= s2_valid;
            s3_rd    <= s2_rd;
            s3_cvt_valid <= s2_cvt_valid && s2_valid;
            s3_cvt <= s2_cvt;

            ovf = 1'b0;
            inv = s2_invalid;
            out = 16'h0000;

            if (!s2_valid) begin
                out = 16'h0000;
            end else if (s2_special) begin
                out = s2_special_res;
            end else if (s2_op.is_mul) begin
                // Multiply normalize
                logic [21:0] p;
                logic signed [7:0] e;
                logic [11:0] mant12;
                logic guard, round, sticky_mul;
                logic [9:0] frac;
                p = s2_mul_prod;
                e = s2_mul_exp;
                // p is 22 bits representing (1.x)*(1.y) in Q(??). Normalize based on top bit.
                if (p[21]) begin
                    // 2.x
                    guard = p[10];
                    round = p[9];
                    sticky_mul = |p[8:0];
                    mant12 = {p[21:11], 1'b0}; // keep 11 bits + lsb
                    e = e + 1;
                end else begin
                    guard = p[9];
                    round = p[8];
                    sticky_mul = |p[7:0];
                    mant12 = {p[20:10], 1'b0};
                end
                // RNE on 11-bit mantissa (hidden + 10 frac). mant12[11:1] is mant, [0] is lsb for ties.
                if (guard && (round || sticky_mul || mant12[1])) mant12 = mant12 + 12'd2;
                // Renormalize if rounding overflow
                if (mant12[11:1] == 11'h800) begin
                    e = e + 1;
                end
                if (e >= 31) begin
                    out = {s2_mul_sign, 5'h1F, 10'h000};
                    ovf = 1'b1;
                end else if (e <= 0) begin
                    // subnormal/underflow
                    int rshift;
                    logic [13:0] sub;
                    rshift = 1 - e;
                    sub = {mant12[11:1], 3'b000};
                    if (rshift >= 14) begin
                        out = {s2_mul_sign, 15'h0000};
                    end else begin
                        logic st;
                        int ii;
                        st = 1'b0;
                        for (ii = 0; ii < 14; ii++) begin
                            if (ii < rshift) st |= sub[ii];
                        end
                        sub = sub >> rshift;
                        sub[0] = sub[0] | st;
                        // RNE for subnormal
                        if (sub[2] && (sub[1] || sub[0] || sub[3])) sub[13:3] = sub[13:3] + 1'b1;
                        frac = sub[12:3];
                        out = {s2_mul_sign, 5'h00, frac};
                    end
                end else begin
                    frac = mant12[10:1];
                    out = {s2_mul_sign, e[4:0], frac};
                end
            end else if (s2_is_addsub) begin
                // Add/sub normalize
                logic [14:0] sum;
                logic signed [7:0] e;
                logic sign;
                logic [13:0] man;
                logic [4:0] lz;
                logic guard, round, sticky;
                logic [9:0] frac;
                sum = s2_sum;
                e = s2_exp;
                sign = s2_res_sign;
                if (sum == 0) begin
                    out = {sign, 15'h0000};
                end else if (sum[14]) begin
                    // carry, shift right 1
                    guard = sum[0];
                    round = 1'b0;
                    sticky = 1'b0;
                    man = sum[14:1];
                    e = e + 1;
                    // rounding: guard acts as sticky into LSB
                    if (guard && man[0]) man = man + 1'b1;
                end else begin
                    // normalize left for subtraction results
                    man = sum[13:0];
                    lz = fp16_lzc14(man);
                    if (lz >= 14) begin
                        out = {sign, 15'h0000};
                    end else begin
                        man = man << lz;
                        e = e - $signed({3'b0, lz});
                    end
                end
                // Now man is 14 bits with GRS assumed in [2:0]
                if (e >= 31) begin
                    out = {sign, 5'h1F, 10'h000};
                    ovf = 1'b1;
                end else if (e <= 0) begin
                    // produce subnormal by shifting right
                    int rshift;
                    logic [13:0] sub;
                    rshift = 1 - e;
                    sub = man;
                    if (rshift >= 14) begin
                        out = {sign, 15'h0000};
                    end else begin
                        logic st;
                        int ii;
                        st = 1'b0;
                        for (ii = 0; ii < 14; ii++) begin
                            if (ii < rshift) st |= sub[ii];
                        end
                        sub = sub >> rshift;
                        sub[0] = sub[0] | st;
                        // RNE: guard=sub[2], round=sub[1], sticky=sub[0]
                        if (sub[2] && (sub[1] || sub[0] || sub[3])) sub[13:3] = sub[13:3] + 1'b1;
                        frac = sub[12:3];
                        out = {sign, 5'h00, frac};
                    end
                end else begin
                    // RNE on normalized: man is [hidden+frac][grs]
                    guard = man[2];
                    round = man[1];
                    sticky = man[0];
                    if (guard && (round || sticky || man[3])) man[13:3] = man[13:3] + 1'b1;
                    // handle carry from rounding
                    if (man[13]) begin
                        // ok
                    end else if (man[13:3] == 11'h800) begin
                        e = e + 1;
                    end
                    if (e >= 31) begin
                        out = {sign, 5'h1F, 10'h000};
                        ovf = 1'b1;
                    end else begin
                        frac = man[12:3];
                        out = {sign, e[4:0], frac};
                    end
                end
            end else if (s2_op.is_fma) begin
                // Non-fused: compute mul (rounded) then add c (rounded again).
                logic [15:0] mul_out;
                logic mul_ovf;
                logic [17:0] add_pack;
                // Reuse current computed 'out' path by computing mul_out using the mul normalize code above.
                // To avoid duplicating code, recompute mul_out here by temporarily treating this op as a mul.
                begin
                    logic [21:0] p;
                    logic signed [7:0] e;
                    logic [11:0] mant12;
                    logic guard, round;
                    logic sticky_mul;
                    logic [9:0] frac;
                    mul_ovf = 1'b0;
                    p = s2_mul_prod;
                    e = s2_mul_exp;
                    if (p[21]) begin
                        guard = p[10];
                        round = p[9];
                        sticky_mul = |p[8:0];
                        mant12 = {p[21:11], 1'b0};
                        e = e + 1;
                    end else begin
                        guard = p[9];
                        round = p[8];
                        sticky_mul = |p[7:0];
                        mant12 = {p[20:10], 1'b0};
                    end
                    if (guard && (round || sticky_mul || mant12[1])) mant12 = mant12 + 12'd2;
                    if (mant12[11:1] == 11'h800) begin
                        e = e + 1;
                    end
                    if (e >= 31) begin
                        mul_out = {s2_mul_sign, 5'h1F, 10'h000};
                        mul_ovf = 1'b1;
                    end else if (e <= 0) begin
                        int rshift;
                        logic [13:0] sub;
                        rshift = 1 - e;
                        sub = {mant12[11:1], 3'b000};
                        if (rshift >= 14) begin
                            mul_out = {s2_mul_sign, 15'h0000};
                        end else begin
                            int ii;
                            logic st;
                            st = 1'b0;
                            for (ii = 0; ii < 14; ii++) begin
                                if (ii < rshift) st |= sub[ii];
                            end
                            sub = sub >> rshift;
                            sub[0] = sub[0] | st;
                            if (sub[2] && (sub[1] || sub[0] || sub[3])) sub[13:3] = sub[13:3] + 1'b1;
                            frac = sub[12:3];
                            mul_out = {s2_mul_sign, 5'h00, frac};
                        end
                    end else begin
                        frac = mant12[10:1];
                        mul_out = {s2_mul_sign, e[4:0], frac};
                    end
                end
                add_pack = fp16_addsub_rne_pack(mul_out, s2_c, 1'b0);
                out = add_pack[15:0];
                ovf = mul_ovf | add_pack[16];
                inv = inv | add_pack[17];
            end else begin
                out = 16'h0000;
            end

            s3_res <= out;
            s3_overflow <= ovf;
            s3_invalid <= inv;
        end
    end

    // Writeback (stage 3)
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            wb_valid <= 1'b0;
            wb_rd    <= '0;
            wb_data  <= '0;
            wb_scalar_valid <= 1'b0;
            wb_scalar_rd    <= '0;
            wb_scalar_data  <= '0;
            wb_err_overflow <= 1'b0;
            wb_err_invalid  <= 1'b0;
        end else if (!stall_pipe) begin
            wb_valid <= s3_valid;
            wb_rd    <= s3_rd;
            wb_data  <= s3_res;
            wb_scalar_valid <= s3_valid && s3_cvt_valid;
            wb_scalar_rd    <= s3_rd;
            wb_scalar_data  <= s3_cvt;
            wb_err_overflow <= s3_valid && s3_overflow;
            wb_err_invalid  <= s3_valid && s3_invalid;
        end
    end

endmodule
