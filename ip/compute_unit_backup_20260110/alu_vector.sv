module alu_vector #(
    parameter DATA_W = 128
) (
    input  logic            clk,
    input  logic            rst_n,
    input  logic            valid,
    input  logic [5:0]      funct6,
    input  logic [2:0]      funct3,
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
    // One-cycle vector/tensor ALU. If timing fails, pipeline this block.
    assign ready = 1'b1;
    // Type decoding (assumed mapping): 000=I32, 001=I16, 010=I8, 011=FP16, 100=FP8(E4M3)
    localparam TYPE_I32 = 3'b000;
    localparam TYPE_I16 = 3'b001;
    localparam TYPE_I8  = 3'b010;
    localparam TYPE_F16 = 3'b011;
    localparam TYPE_F8  = 3'b100;

    function automatic int elem_width(input [2:0] f3);
        case (f3)
            TYPE_I32: return 32;
            TYPE_I16, TYPE_F16: return 16;
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
            exp_l = exp_a; man_l = man_a; sign_res = sign_a;
            exp_s = exp_b; man_s = man_b;
        end else begin
            exp_l = exp_b; man_l = man_b; sign_res = eff_sign_b;
            exp_s = exp_a; man_s = man_a;
        end
        exp_diff = exp_l - exp_s;
        man_s_al = (exp_diff > 13) ? 14'd0 : {man_s,3'b0} >> exp_diff;
        man_l_al = {man_l,3'b0};
        if (sign_a != eff_sign_b) man_sum = man_l_al - man_s_al; else man_sum = man_l_al + man_s_al;
        if (man_sum == 0) begin
            res = 16'h0000;
        end else if (sign_a != eff_sign_b) begin
            // cancellation normalize left
            logic [3:0] lz;
            if (man_sum[13]) lz = 0; else if (man_sum[12]) lz = 1; else if (man_sum[11]) lz = 2; else if (man_sum[10]) lz = 3;
            else if (man_sum[9]) lz = 4; else if (man_sum[8]) lz = 5; else if (man_sum[7]) lz = 6; else if (man_sum[6]) lz = 7;
            else if (man_sum[5]) lz = 8; else if (man_sum[4]) lz = 9; else if (man_sum[3]) lz = 10; else if (man_sum[2]) lz = 11; else if (man_sum[1]) lz = 12; else lz = 13;
            man_sum = man_sum << lz;
            exp_res = exp_l - lz;
            if (exp_res == 0 || exp_res[4]) res = {sign_res,15'b0};
            else res = {sign_res, exp_res, man_sum[12:3]};
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
            if (man_sum[6]) lz = 0; else if (man_sum[5]) lz = 1; else if (man_sum[4]) lz = 2; else if (man_sum[3]) lz = 3;
            else if (man_sum[2]) lz = 4; else if (man_sum[1]) lz = 5; else lz = 6;
            man_sum = man_sum << lz;
            exp_res = exp_l - lz;
            if (exp_res == 0 || exp_res[3]) res = {sign_res,7'b0};
            else res = {sign_res, exp_res[2:0], man_sum[5:3]};
        end else begin
            if (man_sum[7]) begin
                exp_res = exp_l + 1'b1;
                if (exp_res == 4'hF) res = {sign_res,4'hF,3'b0};
                else res = {sign_res, exp_res[2:0], man_sum[6:4]};
            end else begin
                res = {sign_res, exp_l[2:0], man_sum[5:3]};
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
                6'b000100: begin // VDOT (int only), accumulate later
                    r_elem[i] = a_elem[i] * b_elem[i];
                end
                6'b000110: begin // VSEL: scalar_mask bits select per-lane: 1->rs1, 0->rs2
                    r_elem[i] = scalar_mask[i] ? a_elem[i] : b_elem[i];
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
                default: r_elem[i] = a_elem[i] + b_elem[i];
            endcase
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

        // FP16/FP8 handling (override for math ops when funct6 in ALU group)
        if (funct3 == TYPE_F16 || funct3 == TYPE_F8) begin
            for (i = 0; i < lanes; i++) begin
                int lsb;
                lsb = i * ((funct3 == TYPE_F16) ? 16 : 8);
                if (funct3 == TYPE_F16) begin
                    logic [15:0] a16, b16, r16;
                    a16 = src_a[lsb +: 16];
                    b16 = src_b[lsb +: 16];
                    case (funct6)
                        6'b000000: r16 = fp16_addsub(a16, b16, 1'b0);
                        6'b000001: r16 = fp16_addsub(a16, b16, 1'b1);
                        6'b000010: r16 = fp16_minmax(a16, b16, 1'b0);
                        6'b000011: r16 = fp16_minmax(a16, b16, 1'b1);
                        default:   r16 = fp16_addsub(a16, b16, 1'b0);
                    endcase
                    r_elem[i] = {16'h0, r16};
                end else begin
                    logic [7:0] a8, b8, r8;
                    a8 = src_a[lsb +: 8];
                    b8 = src_b[lsb +: 8];
                    case (funct6)
                        6'b000000: r8 = fp8_addsub(a8, b8, 1'b0);
                        6'b000001: r8 = fp8_addsub(a8, b8, 1'b1);
                        6'b000010: r8 = fp8_minmax(a8, b8, 1'b0);
                        6'b000011: r8 = fp8_minmax(a8, b8, 1'b1);
                        default:   r8 = fp8_addsub(a8, b8, 1'b0);
                    endcase
                    r_elem[i] = {24'h0, r8};
                end
            end
        end

        // VDOT reduction for int types: sum into lane0
        if (funct6 == 6'b000100) begin
            logic signed [63:0] acc;
            acc = 64'sd0;
            for (i = 0; i < lanes; i++) acc += $signed(r_elem[i]);
            r_elem[0] = acc[31:0];
            for (i = 1; i < lanes; i++) r_elem[i] = 32'h0;
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
            w = elem_width(funct3);
            lanes = lane_count(w);
            wb_valid <= valid;
            wb_rd    <= rd_idx;
            wb_is_scalar <= dest_is_scalar;
            wb_data  <= '0;
            wb_err_overflow <= 1'b0;
            wb_err_invalid  <= 1'b0;
            for (j = 0; j < lanes; j++) begin
                lsb = j * w;
                case (w)
                    32: wb_data[lsb +: 32] <= r_elem[j];
                    16: wb_data[lsb +: 16] <= r_elem[j][15:0];
                    default: wb_data[lsb +: 8] <= r_elem[j][7:0];
                endcase

                // Error detection for FP lanes
                if (funct3 == TYPE_F16) begin
                    logic [15:0] lane_a, lane_b, lane_r;
                    lane_a = src_a[lsb +: 16];
                    lane_b = src_b[lsb +: 16];
                    lane_r = r_elem[j][15:0];
                    wb_err_invalid  <= wb_err_invalid  || ((lane_a[14:10] == 5'h1F && lane_a[9:0] != 0) || (lane_b[14:10] == 5'h1F && lane_b[9:0] != 0));
                    wb_err_overflow <= wb_err_overflow || ((lane_r[14:10] == 5'h1F) && (lane_r[9:0] == 0));
                end else if (funct3 == TYPE_F8) begin
                    logic [7:0] lane_a8, lane_b8, lane_r8;
                    lane_a8 = src_a[lsb +: 8];
                    lane_b8 = src_b[lsb +: 8];
                    lane_r8 = r_elem[j][7:0];
                    wb_err_invalid  <= wb_err_invalid  || ((lane_a8[6:3] == 4'hF && lane_a8[2:0] != 0) || (lane_b8[6:3] == 4'hF && lane_b8[2:0] != 0));
                    wb_err_overflow <= wb_err_overflow || ((lane_r8[6:3] == 4'hF) && (lane_r8[2:0] == 0));
                end
            end
        end
    end
endmodule
