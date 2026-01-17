module fp_alu (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        valid,
    input  logic [2:0]  funct3,
    input  logic [15:0] src_a,
    input  logic [15:0] src_b,
    input  logic [31:0] scalar_src,
    input  logic [15:0] src_c, // third operand for FMA (taken from scalar path lower 16 or explicit mapping)
    input  logic [4:0]  rd_idx,
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

    // -------------------------------------------------------------------------
    // Synthesizable FP16 ALU (Flush-to-Zero for subnormals)
    // -------------------------------------------------------------------------
    
    // FP16 Format: [15] Sign, [14:10] Exponent, [9:0] Fraction
    // Bias: 15
    
    // Unpack
    logic sign_a, sign_b;
    logic [4:0] exp_a, exp_b;
    logic [9:0] frac_a, frac_b;
    logic [10:0] man_a, man_b; // Includes hidden bit

    assign sign_a = src_a[15];
    assign exp_a  = src_a[14:10];
    assign frac_a = src_a[9:0];
    assign man_a  = (exp_a == 0) ? 11'd0 : {1'b1, frac_a}; // Flush denorms

    assign sign_b = src_b[15];
    assign exp_b  = src_b[14:10];
    assign frac_b = src_b[9:0];
    assign man_b  = (exp_b == 0) ? 11'd0 : {1'b1, frac_b}; // Flush denorms

    // Small FP16 add/sub helper used by FMA path
    function automatic logic [15:0] fp16_addsub(input logic [15:0] a, input logic [15:0] b, input logic is_sub);
        logic sign_a, sign_b, eff_sign_b;
        logic [4:0] exp_a, exp_b, exp_l, exp_s;
        logic [9:0] frac_a, frac_b;
        logic [10:0] man_a_l, man_b_l, man_l, man_s;
        logic [13:0] man_s_al, man_l_al;
        logic [14:0] man_sum;
        logic [4:0]  exp_res;
        logic        sign_res;
        logic [15:0] res;
        sign_a = a[15]; exp_a = a[14:10]; frac_a = a[9:0];
        sign_b = b[15]; exp_b = b[14:10]; frac_b = b[9:0];
        eff_sign_b = is_sub ? ~sign_b : sign_b;
        man_a_l = (exp_a == 0) ? 11'd0 : {1'b1, frac_a};
        man_b_l = (exp_b == 0) ? 11'd0 : {1'b1, frac_b};
        // sort
        if (a[14:0] >= b[14:0]) begin
            exp_l = exp_a; man_l = man_a_l; sign_res = sign_a;
            exp_s = exp_b; man_s = man_b_l;
        end else begin
            exp_l = exp_b; man_l = man_b_l; sign_res = eff_sign_b;
            exp_s = exp_a; man_s = man_a_l;
        end
        exp_res = exp_l - exp_s;
        man_s_al = (exp_res > 13) ? 14'd0 : {man_s,3'b0} >> exp_res;
        man_l_al = {man_l,3'b0};
        if (sign_a != eff_sign_b) man_sum = man_l_al - man_s_al; else man_sum = man_l_al + man_s_al;
        if (man_sum == 0) begin
            res = 16'h0000;
        end else if (sign_a != eff_sign_b) begin
            // cancellation normalize left
            logic [3:0] lz;
            logic [13:0] norm_man_l;
            logic [5:0]  adj_exp_l;
            if (man_sum[13]) lz = 0; else if (man_sum[12]) lz = 1; else if (man_sum[11]) lz = 2;
            else if (man_sum[10]) lz = 3; else if (man_sum[9]) lz = 4; else if (man_sum[8]) lz = 5;
            else if (man_sum[7]) lz = 6; else if (man_sum[6]) lz = 7; else if (man_sum[5]) lz = 8;
            else if (man_sum[4]) lz = 9; else if (man_sum[3]) lz = 10; else if (man_sum[2]) lz = 11;
            else if (man_sum[1]) lz = 12; else lz = 13;
            norm_man_l = man_sum << lz;
            adj_exp_l  = {1'b0, exp_l} - lz;
            if (adj_exp_l <= 0 || exp_l == 0) begin
                 res = {sign_res,15'b0};
            end else begin
                 res = {sign_res, adj_exp_l[4:0], norm_man_l[12:3]};
            end
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

    // Special cases for NaN/Inf detection
    logic is_nan_a, is_nan_b, is_inf_a, is_inf_b, is_zero_a, is_zero_b;
    assign is_nan_a  = (exp_a == 5'b11111) && (frac_a != 0);
    assign is_nan_b  = (exp_b == 5'b11111) && (frac_b != 0);
    assign is_inf_a  = (exp_a == 5'b11111) && (frac_a == 0);
    assign is_inf_b  = (exp_b == 5'b11111) && (frac_b == 0);
    assign is_zero_a = (exp_a == 0) && (frac_a == 0);
    assign is_zero_b = (exp_b == 0) && (frac_b == 0);

    // -------------------------------------------------------------------------
    // MIN / MAX Logic
    // -------------------------------------------------------------------------
    logic a_lt_b;
    logic [15:0] res_min, res_max;
    
    always_comb begin
        if (sign_a != sign_b) begin
            a_lt_b = sign_a; // If A is neg, A < B
        end else begin
            // Same sign
            if (src_a[14:0] == src_b[14:0]) 
                a_lt_b = 1'b0; // Equal
            else if (sign_a == 0) 
                a_lt_b = (src_a[14:0] < src_b[14:0]); // Positive: smaller mag is smaller
            else 
                a_lt_b = (src_a[14:0] > src_b[14:0]); // Negative: larger mag is smaller
        end
        res_min = a_lt_b ? src_a : src_b;
        res_max = a_lt_b ? src_b : src_a;
    end

    // -------------------------------------------------------------------------
    // ADD / SUB Logic
    // -------------------------------------------------------------------------
    logic is_sub;
    assign is_sub = (funct3 == 3'b001);
    
    logic eff_sign_b;
    assign eff_sign_b = is_sub ? ~sign_b : sign_b;
    
    logic operation_sub;
    assign operation_sub = (sign_a != eff_sign_b); // True subtraction if signs differ for ADD
    
    logic [15:0] res_add;
    logic        add_overflow;
    
    // Variables for Adder
    logic [4:0]  exp_l, exp_s;
    logic [10:0] man_l, man_s;
    logic        sign_l;
    logic [4:0]  exp_diff;
    logic [13:0] man_s_aligned; // Extra guard bits
    logic [13:0] man_l_aligned;
    logic [14:0] man_sum;       // 1-bit overflow
    logic [4:0]  add_res_exp;
    logic [9:0]  add_res_frac;
    logic        add_res_sign;
    
    always_comb begin
        // Sort by magnitude for alignment
        if (src_a[14:0] >= src_b[14:0]) begin
            exp_l = exp_a; man_l = man_a; sign_l = sign_a;
            exp_s = exp_b; man_s = man_b;
        end else begin
            exp_l = exp_b; man_l = man_b; sign_l = eff_sign_b;
            exp_s = exp_a; man_s = man_a;
        end
        
        // Alignment
        exp_diff = exp_l - exp_s;
        if (exp_diff > 13) man_s_aligned = 14'd0;
        else man_s_aligned = {man_s, 3'b0} >> exp_diff;
        
        man_l_aligned = {man_l, 3'b0};
        
        // Operation
        if (operation_sub) begin
            man_sum = man_l_aligned - man_s_aligned;
            add_res_sign = sign_l; // Sign is sign of larger
        end else begin
            man_sum = man_l_aligned + man_s_aligned;
            add_res_sign = sign_l;
        end
        
        // Normalization (Simplified)
        if (man_sum == 0) begin
            res_add = 16'h0000; // Zero
            add_overflow = 1'b0;
        end else if (operation_sub) begin
            // Normalization for cancellation (shift left)
            logic [3:0] lead_zeros;
            logic [13:0] norm_man;
            logic [5:0]  adj_exp; // Signed for underflow check
            // Simple priority encoder
            if (man_sum[13]) lead_zeros = 0;
            else if (man_sum[12]) lead_zeros = 1;
            else if (man_sum[11]) lead_zeros = 2;
            else if (man_sum[10]) lead_zeros = 3;
            else if (man_sum[9])  lead_zeros = 4;
            else if (man_sum[8])  lead_zeros = 5;
            else if (man_sum[7])  lead_zeros = 6;
            else if (man_sum[6])  lead_zeros = 7;
            else if (man_sum[5])  lead_zeros = 8;
            else if (man_sum[4])  lead_zeros = 9;
            else if (man_sum[3])  lead_zeros = 10;
            else if (man_sum[2])  lead_zeros = 11;
            else if (man_sum[1])  lead_zeros = 12;
            else lead_zeros = 13;
            
            // Re-normalize magnitude
            norm_man = man_sum << lead_zeros;
            adj_exp  = {1'b0, exp_l} - lead_zeros; 
            
            if (adj_exp <= 0 || exp_l == 0) begin
                 res_add = {add_res_sign, 15'b0}; // Underflow to zero
                 add_overflow = 1'b0;
            end else begin
                 // norm_man[13] is hidden bit. frac is [12:3]
                 res_add = {add_res_sign, adj_exp[4:0], norm_man[12:3]};
                 add_overflow = 1'b0;
            end
        end else begin
            // Normalization for addition (shift right if overflow)
            if (man_sum[14]) begin // 1x.xxx became 1xx.xxx
                if (exp_l == 31) begin
                    res_add = {add_res_sign, 5'b11111, 10'b0}; // Inf
                    add_overflow = 1'b1;
                end else begin
                    res_add = {add_res_sign, exp_l + 5'd1, man_sum[13:4]};
                    add_overflow = (exp_l + 5'd1 == 5'b11111);
                end
            end else begin
                if (exp_l == 31) begin
                    res_add = {add_res_sign, 5'b11111, 10'b0}; // Inf
                    add_overflow = 1'b1;
                end else begin
                    res_add = {add_res_sign, exp_l, man_sum[12:3]};
                    add_overflow = 1'b0;
                end
            end
        end
    end

    // -------------------------------------------------------------------------
    // MUL Logic
    // -------------------------------------------------------------------------
    logic [15:0] res_mul;
    logic        mul_overflow;
    logic [21:0] prod_man;
    logic [5:0]  prod_exp; // Extended for overflow check
    logic signed [6:0] exp_calc;
    logic        prod_sign;
    
    always_comb begin
        prod_sign = sign_a ^ sign_b;
        mul_overflow = 1'b0;
        if (exp_a == 0 || exp_b == 0) begin
            res_mul = {prod_sign, 15'b0};
        end else begin
            prod_man = man_a * man_b;
            exp_calc = {2'b0, exp_a} + {2'b0, exp_b} - 7'd15;

            if (exp_calc <= 0) begin
                 res_mul = {prod_sign, 15'b0}; // Underflow
            end else begin
                 // prod_man is 1.x * 1.y = 1.z or 1x.z
                 // MSB is bit 21. 
                 if (prod_man[21]) begin
                     // 1x.xxx -> Shift right, inc exp
                     if (exp_calc >= 30) begin // 30+1 = 31 (Over)
                        res_mul = {prod_sign, 5'b11111, 10'b0};
                        mul_overflow = 1'b1;
                     end else begin
                        res_mul = {prod_sign, exp_calc[4:0] + 5'd1, prod_man[20:11]};
                        mul_overflow = (exp_calc[4:0] + 5'd1 == 5'b11111);
                     end
                 end else begin
                     // 1.xxx -> Keep exp
                     if (exp_calc >= 31) begin
                        res_mul = {prod_sign, 5'b11111, 10'b0};
                        mul_overflow = 1'b1;
                     end else begin
                        res_mul = {prod_sign, exp_calc[4:0], prod_man[19:10]};
                        mul_overflow = 1'b0;
                     end
                 end
            end
        end
    end

    // -------------------------------------------------------------------------
    // Output Multiplexer & Register
    // -------------------------------------------------------------------------
    logic [15:0] res_comb;
    logic        res_overflow;
    logic        res_invalid;
    logic [31:0] res_cvt;
    logic        res_cvt_valid;
    
    always_comb begin
        // Local temporaries for case arms (must be declared before statements)
        logic [15:0] prod;
        logic        cvt_sign;
        logic [31:0] abs_i;
        int          shift;
        logic [25:0] mant;

        // Invalid if any NaN input
        res_invalid = is_nan_a || is_nan_b;
        res_cvt_valid = 1'b0;
        res_cvt       = 32'h0;
        case (funct3)
            3'b000: begin res_comb = res_add; res_overflow = add_overflow; end // FADD
            3'b001: begin res_comb = res_add; res_overflow = add_overflow; end // FSUB
            3'b010: begin res_comb = res_mul; res_overflow = mul_overflow; end // FMUL
            3'b011: begin // FMA: (a*b)+c
                prod = res_mul;
                // reuse adder with prod and src_c
                res_comb = fp16_addsub(prod, src_c, 1'b0);
                res_overflow = mul_overflow; // simplistic
            end
            3'b100: begin res_comb = res_min; res_overflow = 1'b0; end // FMIN
            3'b101: begin res_comb = res_max; res_overflow = 1'b0; end // FMAX
            3'b110: begin // FCVT i2f
                cvt_sign = scalar_src[31];
                abs_i = cvt_sign ? (~scalar_src + 1'b1) : scalar_src;
                // crude normalization: clamp to max finite if overflow
                if (abs_i[31:15] != 0) begin
                    res_comb = {cvt_sign, 5'h1F, 10'h0};
                    res_overflow = 1'b1;
                end else begin
                    // place integer into mantissa, exponent bias 15+15 (approx)
                    res_comb = {cvt_sign, 5'd15, abs_i[14:5]};
                    res_overflow = 1'b0;
                end
                res_cvt_valid = 1'b0;
            end
            3'b111: begin // FCVT f2i (to scalar int32)
                res_comb = 16'h0;
                res_overflow = 1'b0;
                res_cvt_valid = 1'b1;
                // simple fp16 to int32 (truncate)
                if (exp_a == 0) res_cvt = 32'h0;
                else begin
                    mant = {1'b1, frac_a, 15'b0};
                    shift = exp_a - 5'd15;
                    if (shift >= 16) res_cvt = scalar_src; // overflow fallback
                    else res_cvt = sign_a ? (~(mant >> (14-shift)) + 1'b1) : (mant >> (14-shift));
                end
            end
            default: begin res_comb = 16'h0; res_overflow = 1'b0; end
        endcase
    end

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
        end else begin
            wb_valid <= valid;
            wb_rd    <= rd_idx;
            wb_data  <= res_comb;
            wb_scalar_valid <= valid && res_cvt_valid;
            wb_scalar_rd    <= rd_idx;
            wb_scalar_data  <= res_cvt;
            wb_err_overflow <= res_overflow;
            wb_err_invalid  <= res_invalid;
        end
    end

endmodule
