module alu_vector #(
    parameter DATA_W = 128
) (
    input  logic            clk,
    input  logic            rst_n,
    input  logic            valid,
    input  logic [5:0]      funct6,
    input  logic [2:0]      funct3,
    input  logic            vm_enable,
    input  logic [15:0]     vmask,
    input  logic [4:0]      rd_idx,
    input  logic            dest_is_scalar,
    input  logic [DATA_W-1:0] src_a,
    input  logic [DATA_W-1:0] src_b,
    input  logic [31:0]     scalar_mask,
    output logic            ready,
    output logic            wb_valid,
    output logic [4:0]      wb_rd,
    output logic            wb_is_scalar,
    output logic [DATA_W-1:0] wb_data,
    // Error flags (aggregate over lanes for FP types)
    output logic            wb_err_overflow,
    output logic            wb_err_invalid
);

    localparam logic [5:0] VEC_F6_VXOR = 6'b001001;
    localparam logic [5:0] VEC_F6_VAND = 6'b001010;
    localparam logic [5:0] VEC_F6_VOR  = 6'b001011;

    // New ops (see docs/isa_instructions.md)
    localparam logic [5:0] VEC_F6_VPACK  = 6'b001000;
    localparam logic [5:0] VEC_F6_VMUL   = 6'b001100;
    localparam logic [5:0] VEC_F6_VRCP   = 6'b001101;
    localparam logic [5:0] VEC_F6_VRSQRT = 6'b001110;
    localparam logic [5:0] VEC_F6_VUNPK  = 6'b001111;
    // One-cycle vector/tensor ALU. If timing fails, pipeline this block.
    assign ready = 1'b1;
    // Type decoding per updated ISA: 000=I32, 001=I16, 010=I8, 011=FP32, 100=FP16, 101=FP8(E4M3)
    localparam TYPE_I32  = 3'b000;
    localparam TYPE_I16  = 3'b001;
    localparam TYPE_I8   = 3'b010;
    localparam TYPE_FP32 = 3'b011;
    localparam TYPE_FP16 = 3'b100;
    localparam TYPE_FP8  = 3'b101;

    function automatic int elem_width(input [2:0] f3);
        case (f3)
            TYPE_I32, TYPE_FP32: return 32;
            TYPE_I16, TYPE_FP16: return 16;
            default: return 8; // I8 / FP8 fall here
        endcase
    endfunction

    function automatic int lane_count(input int w);
        return DATA_W / w;
    endfunction

    // ---------------------------------------------------------------------
    // Synthesizable FP helpers (FP16, FP8 E4M3) - simplified, flush subnormals
    // ---------------------------------------------------------------------
    function automatic logic [15:0] fp16_addsub(input logic [15:0] a, input logic [15:0] b, input logic is_sub);
        logic sign_a, sign_b, eff_sign_b;
        logic sign_l, sign_s;
        logic [4:0] exp_a, exp_b, exp_l, exp_s;
        logic [9:0] frac_a, frac_b;
        logic [10:0] man_a, man_b, man_l, man_s;
        logic [13:0] man_s_al, man_l_al;
        logic [14:0] man_sum;
        logic [4:0]  exp_diff;
        logic [4:0]  exp_res;
        logic        sign_res;
        logic [15:0] res;
        sign_a = a[15]; exp_a = a[14:10]; frac_a = a[9:0];
        sign_b = b[15]; exp_b = b[14:10]; frac_b = b[9:0];
        eff_sign_b = is_sub ? ~sign_b : sign_b;
        man_a = (exp_a == 0) ? 11'd0 : {1'b1, frac_a};
        man_b = (exp_b == 0) ? 11'd0 : {1'b1, frac_b};
        // sort
        if (a[14:0] >= b[14:0]) begin
            exp_l = exp_a; man_l = man_a; sign_l = sign_a;
            exp_s = exp_b; man_s = man_b; sign_s = eff_sign_b;
        end else begin
            exp_l = exp_b; man_l = man_b; sign_l = eff_sign_b;
            exp_s = exp_a; man_s = man_a; sign_s = sign_a;
        end
        sign_res = sign_l;
        exp_diff = exp_l - exp_s;
        man_s_al = (exp_diff > 13) ? 14'd0 : {man_s,3'b0} >> exp_diff;
        man_l_al = {man_l,3'b0};
        if (sign_l != sign_s) man_sum = man_l_al - man_s_al; else man_sum = man_l_al + man_s_al;
        if (man_sum == 0) begin
            res = 16'h0000;
        end else if (sign_l != sign_s) begin
            // cancellation normalize left
            logic [3:0] lz;
            int exp_tmp;
            if (man_sum[13]) lz = 0; else if (man_sum[12]) lz = 1; else if (man_sum[11]) lz = 2; else if (man_sum[10]) lz = 3;
            else if (man_sum[9]) lz = 4; else if (man_sum[8]) lz = 5; else if (man_sum[7]) lz = 6; else if (man_sum[6]) lz = 7;
            else if (man_sum[5]) lz = 8; else if (man_sum[4]) lz = 9; else if (man_sum[3]) lz = 10; else if (man_sum[2]) lz = 11; else if (man_sum[1]) lz = 12; else lz = 13;
            man_sum = man_sum << lz;
            exp_tmp = exp_l - lz;
            if (exp_tmp <= 0) res = {sign_res,15'b0};
            else if (exp_tmp >= 31) res = {sign_res,5'b11111,10'b0};
            else res = {sign_res, exp_tmp[4:0], man_sum[12:3]};
        end else begin
            // addition normalize right if carry
            if (man_sum[14]) begin
                exp_res = exp_l + 1'b1;
                if (exp_res == 5'b11111) res = {sign_res,5'b11111,10'b0};
                else res = {sign_res, exp_res, man_sum[13:4]};
            end else begin
                res = {sign_res, exp_l, man_sum[12:3]};
            end
        end
        fp16_addsub = res;
    endfunction

    function automatic logic [15:0] fp16_minmax(input logic [15:0] a, input logic [15:0] b, input logic sel_max);
        logic lt;
        if (a[15] != b[15]) lt = a[15];
        else if (a[14:0] == b[14:0]) lt = 1'b0;
        else if (a[15] == 0) lt = (a[14:0] < b[14:0]);
        else lt = (a[14:0] > b[14:0]);
        fp16_minmax = sel_max ? (lt ? b : a) : (lt ? a : b);
    endfunction

    // FP8 E4M3 helpers
    function automatic logic [7:0] fp8_addsub(input logic [7:0] a, input logic [7:0] b, input logic is_sub);
        logic sign_a, sign_b, eff_sign_b;
        logic [3:0] exp_a, exp_b, exp_l, exp_s;
        logic [2:0] frac_a, frac_b;
        logic [3:0] man_a, man_b, man_l, man_s;
        logic [6:0] man_s_al, man_l_al;
        logic [7:0] man_sum;
        logic [3:0] exp_diff, exp_res;
        logic       sign_res;
        logic [7:0] res;
        sign_a = a[7]; exp_a = a[6:3]; frac_a = a[2:0];
        sign_b = b[7]; exp_b = b[6:3]; frac_b = b[2:0];
        eff_sign_b = is_sub ? ~sign_b : sign_b;
        man_a = (exp_a == 0) ? 4'd0 : {1'b1, frac_a};
        man_b = (exp_b == 0) ? 4'd0 : {1'b1, frac_b};
        if (a[6:0] >= b[6:0]) begin
            exp_l = exp_a; man_l = man_a; sign_res = sign_a;
            exp_s = exp_b; man_s = man_b;
        end else begin
            exp_l = exp_b; man_l = man_b; sign_res = eff_sign_b;
            exp_s = exp_a; man_s = man_a;
        end
        exp_diff = exp_l - exp_s;
        man_s_al = (exp_diff > 5) ? 7'd0 : {man_s,3'b0} >> exp_diff;
        man_l_al = {man_l,3'b0};
        if (sign_a != eff_sign_b) man_sum = man_l_al - man_s_al; else man_sum = man_l_al + man_s_al;
        if (man_sum == 0) res = 8'h00;
        else if (sign_a != eff_sign_b) begin
            logic [2:0] lz;
            int exp_tmp;
            if (man_sum[6]) lz = 0; else if (man_sum[5]) lz = 1; else if (man_sum[4]) lz = 2; else if (man_sum[3]) lz = 3;
            else if (man_sum[2]) lz = 4; else if (man_sum[1]) lz = 5; else lz = 6;
            man_sum = man_sum << lz;
            exp_tmp = exp_l - lz;
            if (exp_tmp <= 0) res = {sign_res,7'b0};
            else if (exp_tmp >= 15) res = {sign_res,4'hF,3'b0};
            else res = {sign_res, exp_tmp[3:0], man_sum[5:3]};
        end else begin
            if (man_sum[7]) begin
                exp_res = exp_l + 1'b1;
                if (exp_res == 4'hF) res = {sign_res,4'hF,3'b0};
                else res = {sign_res, exp_res, man_sum[6:4]};
            end else begin
                res = {sign_res, exp_l, man_sum[5:3]};
            end
        end
        fp8_addsub = res;
    endfunction

    function automatic logic [7:0] fp8_minmax(input logic [7:0] a, input logic [7:0] b, input logic sel_max);
        logic lt;
        if (a[7] != b[7]) lt = a[7];
        else if (a[6:0] == b[6:0]) lt = 1'b0;
        else if (a[7] == 0) lt = (a[6:0] < b[6:0]); else lt = (a[6:0] > b[6:0]);
        fp8_minmax = sel_max ? (lt ? b : a) : (lt ? a : b);
    endfunction

    // ---------------------------------------------------------------------
    // FP16/FP8 <-> FP32 conversion helpers (flush subnormals; truncate rounding)
    // These are used to extend VMUL/VRCP/VRSQRT/VPACK/VUNPACK support to FP16/FP8.
    // ---------------------------------------------------------------------
    function automatic logic [31:0] fp16_to_fp32(input logic [15:0] h);
        logic s;
        logic [4:0] e;
        logic [9:0] m;
        logic [7:0] e32;
        s = h[15];
        e = h[14:10];
        m = h[9:0];
        if (e == 0) begin
            fp16_to_fp32 = {s, 8'h00, 23'h0};
        end else if (e == 5'h1F) begin
            fp16_to_fp32 = {s, 8'hFF, {m, 13'h0}};
        end else begin
            e32 = (e - 5'd15) + 8'd127;
            fp16_to_fp32 = {s, e32, {m, 13'h0}};
        end
    endfunction

    function automatic logic [15:0] fp32_to_fp16(input logic [31:0] f);
        logic s;
        logic [7:0] e;
        logic [22:0] m;
        int e16;
        s = f[31];
        e = f[30:23];
        m = f[22:0];
        if (e == 8'h00) begin
            fp32_to_fp16 = {s, 15'h0000};
        end else if (e == 8'hFF) begin
            fp32_to_fp16 = {s, 5'h1F, (m[22:13] == 0) ? 10'h000 : {1'b1, m[21:13]}};
        end else begin
            e16 = (e - 8'd127) + 5'd15;
            if (e16 <= 0) fp32_to_fp16 = {s, 15'h0000};
            else if (e16 >= 31) fp32_to_fp16 = {s, 5'h1F, 10'h000};
            else fp32_to_fp16 = {s, e16[4:0], m[22:13]};
        end
    endfunction

    function automatic logic [31:0] fp8_to_fp32(input logic [7:0] f8);
        logic s;
        logic [3:0] e;
        logic [2:0] m;
        logic [7:0] e32;
        // FP8 E4M3, bias=7 (flush subnormals)
        s = f8[7];
        e = f8[6:3];
        m = f8[2:0];
        if (e == 0) begin
            fp8_to_fp32 = {s, 8'h00, 23'h0};
        end else if (e == 4'hF) begin
            fp8_to_fp32 = {s, 8'hFF, {m, 20'h0}};
        end else begin
            e32 = (e - 4'd7) + 8'd127;
            fp8_to_fp32 = {s, e32, {m, 20'h0}};
        end
    endfunction

    function automatic logic [7:0] fp32_to_fp8(input logic [31:0] f);
        logic s;
        logic [7:0] e;
        logic [22:0] m;
        int e8;
        s = f[31];
        e = f[30:23];
        m = f[22:0];
        if (e == 8'h00) begin
            fp32_to_fp8 = {s, 7'h00};
        end else if (e == 8'hFF) begin
            fp32_to_fp8 = {s, 4'hF, (m != 0) ? 3'b100 : 3'b000};
        end else begin
            e8 = (e - 8'd127) + 4'd7;
            if (e8 <= 0) fp32_to_fp8 = {s, 7'h00};
            else if (e8 >= 15) fp32_to_fp8 = {s, 4'hF, 3'b000};
            else fp32_to_fp8 = {s, e8[3:0], m[22:20]};
        end
    endfunction

    // ---------------------------------------------------------------------
    // FP32 helpers (flush subnormals; simplified rounding)
    // ---------------------------------------------------------------------
    function automatic logic fp32_is_nan(input logic [31:0] a);
        fp32_is_nan = (a[30:23] == 8'hFF) && (a[22:0] != 0);
    endfunction

    function automatic logic fp32_is_inf(input logic [31:0] a);
        fp32_is_inf = (a[30:23] == 8'hFF) && (a[22:0] == 0);
    endfunction

    function automatic logic [31:0] fp32_qnan();
        fp32_qnan = 32'h7FC0_0000;
    endfunction

    function automatic logic [31:0] fp32_addsub(input logic [31:0] a, input logic [31:0] b, input logic is_sub);
        logic sign_a, sign_b, eff_sign_b;
        logic [7:0] exp_a, exp_b, exp_l, exp_s;
        logic [22:0] frac_a, frac_b;
        logic [23:0] man_a, man_b, man_l, man_s;
        logic [26:0] man_l_al, man_s_al;
        logic [27:0] man_sum;
        logic [7:0]  exp_res;
        logic        sign_res;
        logic [31:0] res;

        // NaN propagation
        if (fp32_is_nan(a)) begin
            return a;
        end
        if (fp32_is_nan(b)) begin
            return b;
        end

        // Inf handling (very simple)
        if (fp32_is_inf(a) && fp32_is_inf(b) && (a[31] != (is_sub ? ~b[31] : b[31]))) begin
            return fp32_qnan(); // inf + (-inf)
        end
        if (fp32_is_inf(a)) begin
            return a;
        end
        if (fp32_is_inf(b)) begin
            return {is_sub ? ~b[31] : b[31], b[30:0]};
        end

        sign_a = a[31]; exp_a = a[30:23]; frac_a = a[22:0];
        sign_b = b[31]; exp_b = b[30:23]; frac_b = b[22:0];
        eff_sign_b = is_sub ? ~sign_b : sign_b;

        // Flush subnormals
        man_a = (exp_a == 0) ? 24'd0 : {1'b1, frac_a};
        man_b = (exp_b == 0) ? 24'd0 : {1'b1, frac_b};

        // Sort by magnitude (ignoring sign)
        if (a[30:0] >= b[30:0]) begin
            exp_l = exp_a; man_l = man_a; sign_res = sign_a;
            exp_s = exp_b; man_s = man_b;
        end else begin
            exp_l = exp_b; man_l = man_b; sign_res = eff_sign_b;
            exp_s = exp_a; man_s = man_a;
        end

        man_l_al = {man_l, 3'b0};
        if ((exp_l - exp_s) > 27) man_s_al = 27'd0;
        else man_s_al = {man_s, 3'b0} >> (exp_l - exp_s);

        if (sign_a != eff_sign_b) man_sum = {1'b0, man_l_al} - {1'b0, man_s_al};
        else                      man_sum = {1'b0, man_l_al} + {1'b0, man_s_al};

        if (man_sum == 0) begin
            res = 32'h0000_0000;
        end else if (sign_a != eff_sign_b) begin
            // cancellation normalize left
            int lz;
            lz = 0;
            for (int k = 26; k >= 0; k--) begin
                if (man_sum[k]) begin
                    lz = 26 - k;
                    break;
                end
            end
            if (exp_l <= lz) begin
                res = {sign_res, 31'b0};
            end else begin
                exp_res = exp_l - lz[7:0];
                man_sum = man_sum << lz;
                res = {sign_res, exp_res, man_sum[25:3]};
            end
        end else begin
            // addition normalize right if carry
            exp_res = exp_l;
            if (man_sum[27]) begin
                man_sum = man_sum >> 1;
                exp_res = exp_l + 8'd1;
            end
            if (exp_res == 8'hFF) res = {sign_res, 8'hFF, 23'b0};
            else res = {sign_res, exp_res, man_sum[25:3]};
        end

        fp32_addsub = res;
    endfunction

    function automatic logic [31:0] fp32_mul(input logic [31:0] a, input logic [31:0] b);
        logic sign_a, sign_b, sign_r;
        logic [7:0] exp_a, exp_b;
        logic [22:0] frac_a, frac_b;
        logic [23:0] man_a, man_b;
        logic [47:0] man_p;
        int exp_tmp;

        // NaN propagation
        if (fp32_is_nan(a)) return a;
        if (fp32_is_nan(b)) return b;

        // Inf/zero handling (very simple)
        if (fp32_is_inf(a) && (b[30:0] == 0)) return fp32_qnan();
        if (fp32_is_inf(b) && (a[30:0] == 0)) return fp32_qnan();
        if (fp32_is_inf(a)) return {a[31] ^ b[31], 8'hFF, 23'b0};
        if (fp32_is_inf(b)) return {a[31] ^ b[31], 8'hFF, 23'b0};
        if ((a[30:0] == 0) || (b[30:0] == 0)) return {a[31] ^ b[31], 31'b0};

        sign_a = a[31]; exp_a = a[30:23]; frac_a = a[22:0];
        sign_b = b[31]; exp_b = b[30:23]; frac_b = b[22:0];
        sign_r = sign_a ^ sign_b;

        // Flush subnormals
        if (exp_a == 0 || exp_b == 0) return {sign_r, 31'b0};

        man_a = {1'b1, frac_a};
        man_b = {1'b1, frac_b};
        man_p = man_a * man_b; // 24x24 -> 48

        exp_tmp = (exp_a + exp_b) - 127;

        // Normalize: product is in [1,4)
        if (man_p[47]) begin
            man_p = man_p >> 1;
            exp_tmp += 1;
        end

        if (exp_tmp >= 255) return {sign_r, 8'hFF, 23'b0};
        if (exp_tmp <= 0)   return {sign_r, 31'b0};

        // man_p now has implicit 1 at bit46
        return {sign_r, exp_tmp[7:0], man_p[45:23]};
    endfunction

    function automatic logic [31:0] fp32_from_u8(input logic [7:0] x);
        // Exact conversion of small unsigned integer (0..255) to FP32.
        int msb;
        logic [7:0]  exp;
        logic [22:0] frac;
        logic [23:0] mant;

        if (x == 0) return 32'h0000_0000;

        msb = 0;
        for (int k = 7; k >= 0; k--) begin
            if (x[k]) begin
                msb = k;
                break;
            end
        end

        exp  = 8'(127 + msb);
        mant = {16'h0, x} << (23 - msb); // leading 1 ends up at mant[23]
        frac = mant[22:0];
        return {1'b0, exp, frac};
    endfunction

    function automatic logic [31:0] fp32_to_u32_trunc(input logic [31:0] a);
        // Truncating FP32->U32 (flush subnormals, ignore NaN/Inf).
        logic [7:0] exp;
        logic [22:0] frac;
        logic [23:0] man;
        int shift;

        if (fp32_is_nan(a) || fp32_is_inf(a) || a[31]) return 32'h0;
        exp = a[30:23];
        frac = a[22:0];
        if (exp == 0) return 32'h0;
        man = {1'b1, frac};
        shift = exp - 127;
        if (shift < 0) return 32'h0;
        if (shift > 31) return 32'hFFFF_FFFF;
        if (shift >= 23) return 32'((man) << (shift - 23));
        return 32'(man >> (23 - shift));
    endfunction

    // Initial reciprocal approximation for FP32:
    // x = m*2^(e-127), with m in [1,2). 1/x = (1/m)*2^(127-e).
    // Since 1/m in (0.5,1], we normalize by doubling mantissa and decrementing exponent:
    // 1/x = (2/m)*2^(126-e) => exp_y = 253 - exp_x, mantissa ~ 2/m.
    function automatic logic [31:0] fp32_rcp_init(input logic [31:0] x);
        logic sign_x;
        logic [7:0] exp_x;
        logic [4:0] idx;
        logic [22:0] frac_y;
        logic [22:0] lut [0:31];
        begin
            // Small LUT over mantissa top bits (midpoint samples).
            lut = '{
                23'h7c0fc1, 23'h74898d, 23'h6d7304, 23'h66c2b4, 23'h607038, 23'h5a740e, 23'h54c77b, 23'h4f6475,
                23'h4a4588, 23'h4565c8, 23'h40c0c1, 23'h3c5264, 23'h381703, 23'h340b41, 23'h302c0b, 23'h2c7692,
                23'h28e83f, 23'h257eb5, 23'h2237c3, 23'h1f1166, 23'h1c09c1, 23'h191f1a, 23'h164fda, 23'h139a86,
                23'h10fdbc, 23'h0e7835, 23'h0c08c1, 23'h09ae41, 23'h0767ab, 23'h053408, 23'h03126f, 23'h010204
            };

            // NaN/Inf/0 handled by caller
            sign_x = x[31];
            exp_x  = x[30:23];

            // Flush subnormals to 0 -> return +/-Inf
            if (exp_x == 0) begin
                fp32_rcp_init = {sign_x, 8'hFF, 23'b0};
            end else begin
                idx = x[22:18];
                frac_y = lut[idx];
                fp32_rcp_init = {sign_x, (8'd253 - exp_x), frac_y};
            end
        end
    endfunction

    function automatic logic [31:0] fp32_rcp_nr1(input logic [31:0] x);
        logic [31:0] y0;
        logic [31:0] xy;
        logic [31:0] two_minus_xy;
        begin
            // NaN propagation
            if (fp32_is_nan(x)) return x;

            // Inf/zero
            if (fp32_is_inf(x)) return {x[31], 31'b0};
            if (x[30:0] == 0)   return {x[31], 8'hFF, 23'b0};

            y0 = fp32_rcp_init(x);
            xy = fp32_mul(x, y0);
            two_minus_xy = fp32_addsub(32'h4000_0000, xy, 1'b1); // 2 - x*y0
            fp32_rcp_nr1 = fp32_mul(y0, two_minus_xy);
        end
    endfunction

    function automatic logic [31:0] fp32_rsqrt_nr1(input logic [31:0] x);
        logic [31:0] y0;
        logic [31:0] xhalf;
        logic [31:0] y0_sq;
        logic [31:0] prod;
        logic [31:0] term;
        logic [31:0] y1;
        logic [31:0] i;
        begin
            if (fp32_is_nan(x)) return x;
            if (x[31] && (x[30:0] != 0)) return fp32_qnan();
            if (x[30:0] == 0) return {x[31], 8'hFF, 23'b0};
            if (fp32_is_inf(x)) return 32'h0000_0000;

            // Magic-number initial guess: y0 ~= 1/sqrt(x)
            i  = 32'h5f37_59df - (x >> 1);
            y0 = i;

            // 1 NR iteration: y = y*(1.5 - 0.5*x*y*y)
            xhalf = fp32_mul(x, 32'h3f00_0000); // 0.5*x
            y0_sq = fp32_mul(y0, y0);
            prod  = fp32_mul(xhalf, y0_sq);
            term  = fp32_addsub(32'h3fc0_0000, prod, 1'b1); // 1.5 - prod
            y1    = fp32_mul(y0, term);
            fp32_rsqrt_nr1 = y1;
        end
    endfunction

    function automatic logic [31:0] fp32_minmax(input logic [31:0] a, input logic [31:0] b, input logic sel_max);
        logic lt;
        if (fp32_is_nan(a) && fp32_is_nan(b)) begin
            return fp32_qnan();
        end
        if (fp32_is_nan(a)) begin
            return b;
        end
        if (fp32_is_nan(b)) begin
            return a;
        end

        if (a[31] != b[31]) lt = a[31];
        else if (a[30:0] == b[30:0]) lt = 1'b0;
        else if (a[31] == 1'b0) lt = (a[30:0] < b[30:0]);
        else lt = (a[30:0] > b[30:0]);

        fp32_minmax = sel_max ? (lt ? b : a) : (lt ? a : b);
    endfunction

    // Element buffers (max 16 lanes)
    logic [31:0] a_elem [0:15];
    logic [31:0] b_elem [0:15];
    logic [31:0] r_elem [0:15];

    // Unpack by element width
    integer i;
    integer j;
    logic [15:0] cmp_mask;
    always_comb begin
        int w; int lanes;
        w = elem_width(funct3);
        lanes = lane_count(w);
        cmp_mask = 16'h0;
        for (i = 0; i < 16; i++) begin
            a_elem[i] = 32'h0;
            b_elem[i] = 32'h0;
            r_elem[i] = 32'h0;
        end
        for (i = 0; i < lanes; i++) begin
            int lsb;
            lsb = i * w;
            case (w)
                32: begin
                    a_elem[i] = src_a[lsb +: 32];
                    b_elem[i] = src_b[lsb +: 32];
                end
                16: begin
                    a_elem[i] = {{16{src_a[lsb+15]}}, src_a[lsb +: 16]};
                    b_elem[i] = {{16{src_b[lsb+15]}}, src_b[lsb +: 16]};
                end
                default: begin // 8-bit
                    a_elem[i] = {{24{src_a[lsb+7]}}, src_a[lsb +: 8]};
                    b_elem[i] = {{24{src_b[lsb+7]}}, src_b[lsb +: 8]};
                end
            endcase
        end

        // Compute per-lane
        for (i = 0; i < lanes; i++) begin
            logic signed [31:0] sa, sb;
            sa = a_elem[i];
            sb = b_elem[i];
            case (funct6)
                6'b000000: begin // add/logic/shift
                    case (funct3)
                        3'b000: r_elem[i] = a_elem[i] + b_elem[i];
                        3'b001: r_elem[i] = a_elem[i] << b_elem[i][4:0];
                        3'b100: r_elem[i] = a_elem[i] ^ b_elem[i];
                        3'b101: r_elem[i] = a_elem[i] >>> b_elem[i][4:0];
                        3'b110: r_elem[i] = a_elem[i] | b_elem[i];
                        3'b111: r_elem[i] = a_elem[i] & b_elem[i];
                        default: r_elem[i] = a_elem[i] + b_elem[i];
                    endcase
                end
                6'b000001: r_elem[i] = a_elem[i] - b_elem[i]; // VSUB
                6'b000010: begin // VMIN/VMAX signed
                    case (funct3)
                        3'b010: r_elem[i] = (sa < sb) ? a_elem[i] : b_elem[i];
                        3'b011: r_elem[i] = (sa > sb) ? a_elem[i] : b_elem[i];
                        default: r_elem[i] = a_elem[i];
                    endcase
                end
                6'b000011: begin // VCMP -> mask
                    case (funct3)
                        3'b000: begin r_elem[i] = (a_elem[i] == b_elem[i]) ? {32{1'b1}} : 32'h0; cmp_mask[i] = (a_elem[i] == b_elem[i]); end
                        3'b001: begin r_elem[i] = (sa < sb) ? {32{1'b1}} : 32'h0; cmp_mask[i] = (sa < sb); end
                        3'b010: begin r_elem[i] = (a_elem[i] < b_elem[i]) ? {32{1'b1}} : 32'h0; cmp_mask[i] = (a_elem[i] < b_elem[i]); end
                        default: begin r_elem[i] = 32'h0; cmp_mask[i] = 1'b0; end
                    endcase
                end
                6'b000100: begin // VDOT (int), accumulate later
                    if (vm_enable && !vmask[i]) begin
                        r_elem[i] = 32'h0;
                    end else begin
                        r_elem[i] = $signed(a_elem[i]) * $signed(b_elem[i]);
                    end
                end
                VEC_F6_VMUL: begin // VMUL (int types): element-wise multiply
                    r_elem[i] = a_elem[i] * b_elem[i];
                end
                6'b000110: begin // VSEL: scalar_mask bits select per-lane: 1->rs1, 0->rs2
                    // If vm_enable=1, use CSR vmask for per-lane select (predication-friendly).
                    // Otherwise fall back to scalar_mask bits.
                    r_elem[i] = vm_enable ? (vmask[i] ? a_elem[i] : b_elem[i])
                                         : (scalar_mask[i] ? a_elem[i] : b_elem[i]);
                end
                6'b000111: begin // VSWIZ: simple swizzle using scalar_mask[7:0] (2 bits per lane)
                    int sel;
                    sel = scalar_mask[(i%4)*2 +: 2];
                    r_elem[i] = a_elem[sel];
                end
                6'b000101: begin // VCROSS: lanes 0-2 as 3D vectors (int32)
                    if (i == 0) r_elem[i] = a_elem[1]*b_elem[2] - a_elem[2]*b_elem[1];
                    else if (i == 1) r_elem[i] = a_elem[2]*b_elem[0] - a_elem[0]*b_elem[2];
                    else if (i == 2) r_elem[i] = a_elem[0]*b_elem[1] - a_elem[1]*b_elem[0];
                    else r_elem[i] = 32'h0;
                end
                VEC_F6_VXOR: begin // VXOR: raw-bit XOR (funct3 still selects element width)
                    r_elem[i] = a_elem[i] ^ b_elem[i];
                end
                VEC_F6_VAND: begin // VAND: raw-bit AND
                    r_elem[i] = a_elem[i] & b_elem[i];
                end
                VEC_F6_VOR: begin // VOR: raw-bit OR
                    r_elem[i] = a_elem[i] | b_elem[i];
                end
                VEC_F6_VRCP: begin
                    if (funct3 == TYPE_I32) begin
                        r_elem[i] = fp32_rcp_nr1(a_elem[i]);
                    end else begin
                        r_elem[i] = a_elem[i] + b_elem[i];
                    end
                end
                VEC_F6_VRSQRT: begin
                    if (funct3 == TYPE_I32) begin
                        r_elem[i] = fp32_rsqrt_nr1(a_elem[i]);
                    end else begin
                        r_elem[i] = a_elem[i] + b_elem[i];
                    end
                end
                default: r_elem[i] = a_elem[i] + b_elem[i];
            endcase

            // vm predication: when enabled, masked-off lanes pass through src_a (no write effect)
            // VSEL uses mask for selection, so don't also apply pass-through.
            if ((funct6 != 6'b000110) && vm_enable && !vmask[i]) begin
                r_elem[i] = a_elem[i];
            end
        end

        // VCMP scalar mask packing when dest is scalar
        if (funct6 == 6'b000011 && dest_is_scalar) begin
            for (i = 0; i < 16; i++) r_elem[i] = 32'h0;
            case (lanes)
                1:  r_elem[0][0]      = cmp_mask[0];
                2:  r_elem[0][1:0]    = cmp_mask[1:0];
                4:  r_elem[0][3:0]    = cmp_mask[3:0];
                8:  r_elem[0][7:0]    = cmp_mask[7:0];
                16: r_elem[0][15:0]   = cmp_mask[15:0];
                default: r_elem[0][3:0] = cmp_mask[3:0];
            endcase
        end

        // FP32/FP16/FP8 handling (override for math ops)
        if ((funct3 == TYPE_FP32 || funct3 == TYPE_FP16 || funct3 == TYPE_FP8)
            && (funct6 != VEC_F6_VXOR) && (funct6 != VEC_F6_VAND) && (funct6 != VEC_F6_VOR)) begin
            for (i = 0; i < lanes; i++) begin
                int lsb;
                if (funct3 == TYPE_FP32) begin
                    logic [31:0] a32, b32, r32;
                    lsb = i * 32;
                    a32 = src_a[lsb +: 32];
                    b32 = src_b[lsb +: 32];
                    case (funct6)
                        6'b000000: r32 = fp32_addsub(a32, b32, 1'b0);
                        6'b000001: r32 = fp32_addsub(a32, b32, 1'b1);
                        6'b000010: r32 = fp32_minmax(a32, b32, 1'b0);
                        6'b000011: r32 = fp32_minmax(a32, b32, 1'b1);
                        VEC_F6_VMUL: r32 = fp32_mul(a32, b32);
                        VEC_F6_VRCP: r32 = fp32_rcp_nr1(a32);
                        VEC_F6_VRSQRT: r32 = fp32_rsqrt_nr1(a32);
                        default:   r32 = fp32_addsub(a32, b32, 1'b0);
                    endcase
                    if (vm_enable && !vmask[i]) r32 = a32;
                    r_elem[i] = r32;
                end else if (funct3 == TYPE_FP16) begin
                    logic [15:0] a16, b16, r16;
                    logic [31:0] a32, b32, r32;
                    lsb = i * 16;
                    a16 = src_a[lsb +: 16];
                    b16 = src_b[lsb +: 16];
                    case (funct6)
                        6'b000000: r16 = fp16_addsub(a16, b16, 1'b0);
                        6'b000001: r16 = fp16_addsub(a16, b16, 1'b1);
                        6'b000010: r16 = fp16_minmax(a16, b16, 1'b0);
                        6'b000011: r16 = fp16_minmax(a16, b16, 1'b1);
                        VEC_F6_VMUL: begin
                            a32 = fp16_to_fp32(a16);
                            b32 = fp16_to_fp32(b16);
                            r16 = fp32_to_fp16(fp32_mul(a32, b32));
                        end
                        VEC_F6_VRCP: begin
                            a32 = fp16_to_fp32(a16);
                            r16 = fp32_to_fp16(fp32_rcp_nr1(a32));
                        end
                        VEC_F6_VRSQRT: begin
                            a32 = fp16_to_fp32(a16);
                            r16 = fp32_to_fp16(fp32_rsqrt_nr1(a32));
                        end
                        default:   r16 = fp16_addsub(a16, b16, 1'b0);
                    endcase
                    if (vm_enable && !vmask[i]) r16 = a16;
                    r_elem[i] = {16'h0, r16};
                end else begin
                    logic [7:0] a8, b8, r8;
                    logic [31:0] a32, b32;
                    lsb = i * 8;
                    a8 = src_a[lsb +: 8];
                    b8 = src_b[lsb +: 8];
                    case (funct6)
                        6'b000000: r8 = fp8_addsub(a8, b8, 1'b0);
                        6'b000001: r8 = fp8_addsub(a8, b8, 1'b1);
                        6'b000010: r8 = fp8_minmax(a8, b8, 1'b0);
                        6'b000011: r8 = fp8_minmax(a8, b8, 1'b1);
                        VEC_F6_VMUL: begin
                            a32 = fp8_to_fp32(a8);
                            b32 = fp8_to_fp32(b8);
                            r8 = fp32_to_fp8(fp32_mul(a32, b32));
                        end
                        VEC_F6_VRCP: begin
                            a32 = fp8_to_fp32(a8);
                            r8 = fp32_to_fp8(fp32_rcp_nr1(a32));
                        end
                        VEC_F6_VRSQRT: begin
                            a32 = fp8_to_fp32(a8);
                            r8 = fp32_to_fp8(fp32_rsqrt_nr1(a32));
                        end
                        default:   r8 = fp8_addsub(a8, b8, 1'b0);
                    endcase
                    if (vm_enable && !vmask[i]) r8 = a8;
                    r_elem[i] = {24'h0, r8};
                end
            end
        end

        // VCROSS for FP types: compute in FP32 then convert back (lanes 0..2)
        if (funct6 == 6'b000101 && (funct3 == TYPE_FP32 || funct3 == TYPE_FP16 || funct3 == TYPE_FP8)) begin
            logic [31:0] a0, a1, a2, b0, b1, b2;
            logic [31:0] r0, r1, r2;
            int lsb0, lsb1, lsb2;
            lsb0 = 0;
            lsb1 = (funct3 == TYPE_FP32) ? 32 : (funct3 == TYPE_FP16 ? 16 : 8);
            lsb2 = lsb1 * 2;

            if (funct3 == TYPE_FP32) begin
                a0 = src_a[lsb0 +: 32];
                a1 = src_a[lsb1 +: 32];
                a2 = src_a[lsb2 +: 32];
                b0 = src_b[lsb0 +: 32];
                b1 = src_b[lsb1 +: 32];
                b2 = src_b[lsb2 +: 32];
            end else if (funct3 == TYPE_FP16) begin
                a0 = fp16_to_fp32(src_a[lsb0 +: 16]);
                a1 = fp16_to_fp32(src_a[lsb1 +: 16]);
                a2 = fp16_to_fp32(src_a[lsb2 +: 16]);
                b0 = fp16_to_fp32(src_b[lsb0 +: 16]);
                b1 = fp16_to_fp32(src_b[lsb1 +: 16]);
                b2 = fp16_to_fp32(src_b[lsb2 +: 16]);
            end else begin
                a0 = fp8_to_fp32(src_a[lsb0 +: 8]);
                a1 = fp8_to_fp32(src_a[lsb1 +: 8]);
                a2 = fp8_to_fp32(src_a[lsb2 +: 8]);
                b0 = fp8_to_fp32(src_b[lsb0 +: 8]);
                b1 = fp8_to_fp32(src_b[lsb1 +: 8]);
                b2 = fp8_to_fp32(src_b[lsb2 +: 8]);
            end

            r0 = fp32_addsub(fp32_mul(a1, b2), fp32_mul(a2, b1), 1'b1);
            r1 = fp32_addsub(fp32_mul(a2, b0), fp32_mul(a0, b2), 1'b1);
            r2 = fp32_addsub(fp32_mul(a0, b1), fp32_mul(a1, b0), 1'b1);

            for (i = 0; i < lanes; i++) r_elem[i] = 32'h0;
            if (funct3 == TYPE_FP32) begin
                r_elem[0] = r0;
                r_elem[1] = r1;
                r_elem[2] = r2;
            end else if (funct3 == TYPE_FP16) begin
                r_elem[0] = {16'h0, fp32_to_fp16(r0)};
                r_elem[1] = {16'h0, fp32_to_fp16(r1)};
                r_elem[2] = {16'h0, fp32_to_fp16(r2)};
            end else begin
                r_elem[0] = {24'h0, fp32_to_fp8(r0)};
                r_elem[1] = {24'h0, fp32_to_fp8(r1)};
                r_elem[2] = {24'h0, fp32_to_fp8(r2)};
            end

            // Predication: masked lanes pass through src_a
            if (vm_enable) begin
                if (!vmask[0]) r_elem[0] = (funct3 == TYPE_FP32) ? src_a[lsb0 +: 32]
                                   : (funct3 == TYPE_FP16) ? {16'h0, src_a[lsb0 +: 16]}
                                   : {24'h0, src_a[lsb0 +: 8]};
                if (!vmask[1]) r_elem[1] = (funct3 == TYPE_FP32) ? src_a[lsb1 +: 32]
                                   : (funct3 == TYPE_FP16) ? {16'h0, src_a[lsb1 +: 16]}
                                   : {24'h0, src_a[lsb1 +: 8]};
                if (!vmask[2]) r_elem[2] = (funct3 == TYPE_FP32) ? src_a[lsb2 +: 32]
                                   : (funct3 == TYPE_FP16) ? {16'h0, src_a[lsb2 +: 16]}
                                   : {24'h0, src_a[lsb2 +: 8]};
            end
        end

        // VUNPACK: scalar_mask holds ARGB8888, expand into vector lanes.
        // - TYPE_FP32/FP16/FP8: UNORM8->float in lanes 0..3: r,g,b,a (each scaled by 1/255)
        // - TYPE_I32:  uint32 in lanes 0..3: r,g,b,a
        // - TYPE_I16:  UNORM16 in lanes 0..3: {r,r},{g,g},{b,b},{a,a} (lower 64b), rest 0
        if (funct6 == VEC_F6_VUNPK) begin
            logic [7:0] r8, g8, b8, a8;
            r8 = scalar_mask[7:0];
            g8 = scalar_mask[15:8];
            b8 = scalar_mask[23:16];
            a8 = scalar_mask[31:24];

            for (i = 0; i < 16; i++) r_elem[i] = 32'h0;
            if (funct3 == TYPE_FP32) begin
                logic [31:0] k;
                k = 32'h3b80_8081; // 1/255
                r_elem[0] = fp32_mul(fp32_from_u8(r8), k);
                r_elem[1] = fp32_mul(fp32_from_u8(g8), k);
                r_elem[2] = fp32_mul(fp32_from_u8(b8), k);
                r_elem[3] = fp32_mul(fp32_from_u8(a8), k);
            end else if (funct3 == TYPE_FP16) begin
                logic [31:0] k;
                logic [31:0] rr, gg, bb, aa;
                k = 32'h3b80_8081; // 1/255
                rr = fp32_mul(fp32_from_u8(r8), k);
                gg = fp32_mul(fp32_from_u8(g8), k);
                bb = fp32_mul(fp32_from_u8(b8), k);
                aa = fp32_mul(fp32_from_u8(a8), k);
                r_elem[0] = {16'h0, fp32_to_fp16(rr)};
                r_elem[1] = {16'h0, fp32_to_fp16(gg)};
                r_elem[2] = {16'h0, fp32_to_fp16(bb)};
                r_elem[3] = {16'h0, fp32_to_fp16(aa)};
            end else if (funct3 == TYPE_FP8) begin
                logic [31:0] k;
                logic [31:0] rr, gg, bb, aa;
                k = 32'h3b80_8081; // 1/255
                rr = fp32_mul(fp32_from_u8(r8), k);
                gg = fp32_mul(fp32_from_u8(g8), k);
                bb = fp32_mul(fp32_from_u8(b8), k);
                aa = fp32_mul(fp32_from_u8(a8), k);
                r_elem[0] = {24'h0, fp32_to_fp8(rr)};
                r_elem[1] = {24'h0, fp32_to_fp8(gg)};
                r_elem[2] = {24'h0, fp32_to_fp8(bb)};
                r_elem[3] = {24'h0, fp32_to_fp8(aa)};
            end else if (funct3 == TYPE_I32) begin
                r_elem[0] = {24'h0, r8};
                r_elem[1] = {24'h0, g8};
                r_elem[2] = {24'h0, b8};
                r_elem[3] = {24'h0, a8};
            end else begin
                // default TYPE_I16 layout
                r_elem[0] = {16'h0, r8, r8};
                r_elem[1] = {16'h0, g8, g8};
                r_elem[2] = {16'h0, b8, b8};
                r_elem[3] = {16'h0, a8, a8};
            end
        end

        // VRSQRT FP32: ensure negative finite inputs produce qNaN on active lanes.
        if (funct6 == VEC_F6_VRSQRT && funct3 == TYPE_FP32) begin
            for (i = 0; i < 4; i++) begin
                logic [31:0] a32;
                a32 = src_a[(i*32) +: 32];
                if (!(vm_enable && !vmask[i]) && a32[31] && (a32[30:0] != 0)) begin
                    r_elem[i] = fp32_qnan();
                end
            end
        end

        // VPACK: pack lanes 0..3 into a scalar ARGB8888 in r_elem[0].
        // - TYPE_FP32/FP16/FP8: expects lanes as 0..1 floats; scales by 255 and saturates.
        // - TYPE_I32:  expects lanes 0..255 ints; saturates.
        // - TYPE_I16:  expects lanes as UNORM16; uses high byte.
        if (funct6 == VEC_F6_VPACK) begin
            logic [7:0] r8, g8, b8, a8;
            logic [31:0] r_i, g_i, b_i, a_i;

            // Default 0
            r8 = 8'h0; g8 = 8'h0; b8 = 8'h0; a8 = 8'h0;

            if (funct3 == TYPE_FP32) begin
                logic [31:0] k255;
                k255 = 32'h437f_0000; // 255.0
                r_i = fp32_to_u32_trunc(fp32_mul(src_a[31:0],   k255));
                g_i = fp32_to_u32_trunc(fp32_mul(src_a[63:32],  k255));
                b_i = fp32_to_u32_trunc(fp32_mul(src_a[95:64],  k255));
                a_i = fp32_to_u32_trunc(fp32_mul(src_a[127:96], k255));
            end else if (funct3 == TYPE_FP16) begin
                logic [31:0] k255;
                logic [31:0] rr, gg, bb, aa;
                k255 = 32'h437f_0000; // 255.0
                rr = fp16_to_fp32(src_a[15:0]);
                gg = fp16_to_fp32(src_a[31:16]);
                bb = fp16_to_fp32(src_a[47:32]);
                aa = fp16_to_fp32(src_a[63:48]);
                r_i = fp32_to_u32_trunc(fp32_mul(rr, k255));
                g_i = fp32_to_u32_trunc(fp32_mul(gg, k255));
                b_i = fp32_to_u32_trunc(fp32_mul(bb, k255));
                a_i = fp32_to_u32_trunc(fp32_mul(aa, k255));
            end else if (funct3 == TYPE_FP8) begin
                logic [31:0] k255;
                logic [31:0] rr, gg, bb, aa;
                k255 = 32'h437f_0000; // 255.0
                rr = fp8_to_fp32(src_a[7:0]);
                gg = fp8_to_fp32(src_a[15:8]);
                bb = fp8_to_fp32(src_a[23:16]);
                aa = fp8_to_fp32(src_a[31:24]);
                r_i = fp32_to_u32_trunc(fp32_mul(rr, k255));
                g_i = fp32_to_u32_trunc(fp32_mul(gg, k255));
                b_i = fp32_to_u32_trunc(fp32_mul(bb, k255));
                a_i = fp32_to_u32_trunc(fp32_mul(aa, k255));
            end else if (funct3 == TYPE_I32) begin
                r_i = a_elem[0];
                g_i = a_elem[1];
                b_i = a_elem[2];
                a_i = a_elem[3];
            end else begin
                // TYPE_I16 (signed in core), treat bits as UNORM16 and take high byte.
                r8 = src_a[15:8];
                g8 = src_a[31:24];
                b8 = src_a[47:40];
                a8 = src_a[63:56];
                for (i = 0; i < 16; i++) r_elem[i] = 32'h0;
                r_elem[0] = {a8, b8, g8, r8};
            end

            if (funct3 != TYPE_I16) begin
                if (r_i[31]) r8 = 8'h00; else if (r_i > 32'd255) r8 = 8'hFF; else r8 = r_i[7:0];
                if (g_i[31]) g8 = 8'h00; else if (g_i > 32'd255) g8 = 8'hFF; else g8 = g_i[7:0];
                if (b_i[31]) b8 = 8'h00; else if (b_i > 32'd255) b8 = 8'hFF; else b8 = b_i[7:0];
                if (a_i[31]) a8 = 8'h00; else if (a_i > 32'd255) a8 = 8'hFF; else a8 = a_i[7:0];
                for (i = 0; i < 16; i++) r_elem[i] = 32'h0;
                r_elem[0] = {a8, b8, g8, r8};
            end
        end

        // VDOT reduction: FP and INT paths
        if (funct6 == 6'b000100) begin
            if (funct3 == TYPE_FP32 || funct3 == TYPE_FP16 || funct3 == TYPE_FP8) begin
                logic [31:0] acc;
                acc = 32'h0000_0000;
                for (i = 0; i < lanes; i++) begin
                    if (vm_enable && !vmask[i]) begin
                        // masked-off lane contributes 0
                    end else if (funct3 == TYPE_FP32) begin
                        acc = fp32_addsub(acc, fp32_mul(src_a[(i*32)+:32], src_b[(i*32)+:32]), 1'b0);
                    end else if (funct3 == TYPE_FP16) begin
                        logic [31:0] a32, b32;
                        a32 = fp16_to_fp32(src_a[(i*16)+:16]);
                        b32 = fp16_to_fp32(src_b[(i*16)+:16]);
                        acc = fp32_addsub(acc, fp32_mul(a32, b32), 1'b0);
                    end else begin
                        logic [31:0] a32, b32;
                        a32 = fp8_to_fp32(src_a[(i*8)+:8]);
                        b32 = fp8_to_fp32(src_b[(i*8)+:8]);
                        acc = fp32_addsub(acc, fp32_mul(a32, b32), 1'b0);
                    end
                end
                for (i = 0; i < lanes; i++) r_elem[i] = 32'h0;
                if (funct3 == TYPE_FP32) begin
                    r_elem[0] = acc;
                end else if (funct3 == TYPE_FP16) begin
                    r_elem[0] = {16'h0, fp32_to_fp16(acc)};
                end else begin
                    r_elem[0] = {24'h0, fp32_to_fp8(acc)};
                end
            end else begin
                logic signed [63:0] acc;
                acc = 64'sd0;
                for (i = 0; i < lanes; i++) acc += $signed(r_elem[i]);
                r_elem[0] = acc[31:0];
                for (i = 1; i < lanes; i++) r_elem[i] = 32'h0;
            end
        end
    end

    // Pack results
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            wb_valid <= 1'b0;
            wb_rd    <= '0;
            wb_is_scalar <= 1'b0;
            wb_data  <= '0;
            wb_err_overflow <= 1'b0;
            wb_err_invalid  <= 1'b0;
        end else begin
            int w; int lanes; int lsb;
            logic op_uses_rs2;
            w = elem_width(funct3);
            lanes = lane_count(w);
            wb_valid <= valid;
            wb_rd    <= rd_idx;
            wb_is_scalar <= dest_is_scalar;
            wb_data  <= '0;
            wb_err_overflow <= 1'b0;
            wb_err_invalid  <= 1'b0;
            op_uses_rs2 = !(funct6 == VEC_F6_VRCP || funct6 == VEC_F6_VRSQRT || funct6 == VEC_F6_VUNPK || funct6 == VEC_F6_VPACK);
            for (j = 0; j < lanes; j++) begin
                lsb = j * w;
                case (w)
                    32: wb_data[lsb +: 32] <= r_elem[j];
                    16: wb_data[lsb +: 16] <= r_elem[j][15:0];
                    default: wb_data[lsb +: 8] <= r_elem[j][7:0];
                endcase

                // Error detection for FP lanes
                if (funct3 == TYPE_FP16) begin
                    logic [15:0] lane_a, lane_b, lane_r;
                    lane_a = src_a[lsb +: 16];
                    lane_b = src_b[lsb +: 16];
                    lane_r = r_elem[j][15:0];
                    wb_err_invalid  <= wb_err_invalid  || ((lane_a[14:10] == 5'h1F && lane_a[9:0] != 0) || (op_uses_rs2 && (lane_b[14:10] == 5'h1F && lane_b[9:0] != 0)));
                    wb_err_overflow <= wb_err_overflow || ((lane_r[14:10] == 5'h1F) && (lane_r[9:0] == 0));
                end else if (funct3 == TYPE_FP8) begin
                    logic [7:0] lane_a8, lane_b8, lane_r8;
                    lane_a8 = src_a[lsb +: 8];
                    lane_b8 = src_b[lsb +: 8];
                    lane_r8 = r_elem[j][7:0];
                    wb_err_invalid  <= wb_err_invalid  || ((lane_a8[6:3] == 4'hF && lane_a8[2:0] != 0) || (op_uses_rs2 && (lane_b8[6:3] == 4'hF && lane_b8[2:0] != 0)));
                    wb_err_overflow <= wb_err_overflow || ((lane_r8[6:3] == 4'hF) && (lane_r8[2:0] == 0));
                end else if (funct3 == TYPE_FP32) begin
                    logic [31:0] lane_a32, lane_b32, lane_r32;
                    lane_a32 = src_a[lsb +: 32];
                    lane_b32 = src_b[lsb +: 32];
                    lane_r32 = r_elem[j];
                    wb_err_invalid  <= wb_err_invalid  || fp32_is_nan(lane_a32) || (op_uses_rs2 && fp32_is_nan(lane_b32));
                    wb_err_overflow <= wb_err_overflow || fp32_is_inf(lane_r32);
                end
            end
        end
    end
endmodule
