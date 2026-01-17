module shade_unit (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        flush,

    // Quad input (from interpolator FIFO)
    input  logic        in_valid,
    output logic        in_ready,
    input  logic [31:0] in_x,
    input  logic [31:0] in_y,
    input  logic [3:0]  in_mask,
    input  logic [31:0] in_bary_w0,
    input  logic [31:0] in_bary_w1,
    input  logic [31:0] in_bary_w2,
    input  logic signed [31:0] in_tri_area,

    // Per-quad attributes
    input  logic [31:0] in_const_color_argb,
    input  logic        in_tex_enable,
    input  logic [31:0] in_tex_base,
    input  logic [31:0] in_tex_stride_bytes,
    input  logic [15:0] in_tex_width,
    input  logic [15:0] in_tex_height,
    input  logic [1:0]  in_tex_format,
    input  logic [31:0] in_v0_u,
    input  logic [31:0] in_v0_v,
    input  logic [31:0] in_v1_u,
    input  logic [31:0] in_v1_v,
    input  logic [31:0] in_v2_u,
    input  logic [31:0] in_v2_v,

    // Quad output (to ROP)
    output logic        out_valid,
    input  logic        out_ready,
    output logic [31:0] out_x,
    output logic [31:0] out_y,
    output logic [3:0]  out_mask,

    // Per-quad attributes (pass-through)
    output logic [31:0] out_const_color_argb,
    output logic        out_tex_enable,
    output logic [31:0] out_tex_base,
    output logic [31:0] out_tex_stride_bytes,
    output logic [15:0] out_tex_width,
    output logic [15:0] out_tex_height,
    output logic [1:0]  out_tex_format,
    output logic [31:0] out_v0_u,
    output logic [31:0] out_v0_v,
    output logic [31:0] out_v1_u,
    output logic [31:0] out_v1_v,
    output logic [31:0] out_v2_u,
    output logic [31:0] out_v2_v,
    output logic [31:0] out_bary_w0,
    output logic [31:0] out_bary_w1,
    output logic [31:0] out_bary_w2,
    output logic signed [31:0] out_tri_area,

    // Precomputed texture address (used by ROP when out_tex_enable)
    output logic [31:0] out_tex_addr,

    output logic        busy
);


    function automatic logic [31:0] clamp_u32(input logic signed [31:0] v, input logic [15:0] max_dim);
        logic signed [31:0] max_s;
        begin
            max_s = $signed({16'h0, max_dim});
            if (v < 0) clamp_u32 = 32'd0;
            else if (v >= max_s) clamp_u32 = (max_dim == 0) ? 32'd0 : {16'h0, (max_dim - 1)};
            else clamp_u32 = $unsigned(v);
        end
    endfunction

    function automatic logic [31:0] compute_tex_addr(
        input logic [31:0] base,
        input logic [31:0] stride_bytes,
        input logic [1:0]  fmt,
        input logic [31:0] u,
        input logic [31:0] v
    );
        logic [31:0] byte_off;
        begin
            if (fmt == 2'd1) byte_off = (v * stride_bytes) + (u << 1);
            else             byte_off = (v * stride_bytes) + (u << 2);
            compute_tex_addr = base + byte_off;
        end
    endfunction

    // 4-stage pipeline (matches recommended Shade/Interp depth more closely):
    //   S1: attribute multiply (products)
    //   S2: accumulate (acc_u, acc_v)
    //   S3: divide by tri_area (u_i, v_i)
    //   S4: clamp + address generation (tex_addr)

    logic s1_valid, s2_valid, s3_valid, s4_valid;

    // Stage 1 regs
    logic [31:0] s1_x, s1_y;
    logic [3:0]  s1_mask;
    logic [31:0] s1_const_color;
    logic        s1_tex_en;
    logic [31:0] s1_tex_base;
    logic [31:0] s1_tex_stride;
    logic [15:0] s1_tex_w;
    logic [15:0] s1_tex_h;
    logic [1:0]  s1_tex_fmt;
    logic [31:0] s1_v0_u, s1_v0_v, s1_v1_u, s1_v1_v, s1_v2_u, s1_v2_v;
    logic [31:0] s1_b0, s1_b1, s1_b2;
    logic signed [31:0] s1_area;
    logic signed [63:0] s1_p0_u;
    logic signed [63:0] s1_p1_u;
    logic signed [63:0] s1_p2_u;
    logic signed [63:0] s1_p0_v;
    logic signed [63:0] s1_p1_v;
    logic signed [63:0] s1_p2_v;

    // Stage 2 regs
    logic [31:0] s2_x, s2_y;
    logic [3:0]  s2_mask;
    logic [31:0] s2_const_color;
    logic        s2_tex_en;
    logic [31:0] s2_tex_base;
    logic [31:0] s2_tex_stride;
    logic [15:0] s2_tex_w;
    logic [15:0] s2_tex_h;
    logic [1:0]  s2_tex_fmt;
    logic [31:0] s2_v0_u, s2_v0_v, s2_v1_u, s2_v1_v, s2_v2_u, s2_v2_v;
    logic [31:0] s2_b0, s2_b1, s2_b2;
    logic signed [31:0] s2_area;
    logic signed [63:0] s2_acc_u;
    logic signed [63:0] s2_acc_v;

    // Stage 3 regs (divide)
    logic [31:0] s3_x, s3_y;
    logic [3:0]  s3_mask;
    logic [31:0] s3_const_color;
    logic        s3_tex_en;
    logic [31:0] s3_tex_base;
    logic [31:0] s3_tex_stride;
    logic [15:0] s3_tex_w;
    logic [15:0] s3_tex_h;
    logic [1:0]  s3_tex_fmt;
    logic [31:0] s3_v0_u, s3_v0_v, s3_v1_u, s3_v1_v, s3_v2_u, s3_v2_v;
    logic [31:0] s3_b0, s3_b1, s3_b2;
    logic signed [31:0] s3_area;
    logic signed [31:0] s3_u_i;
    logic signed [31:0] s3_v_i;

    // Stage 4 regs (clamp+addr)
    logic [31:0] s4_x, s4_y;
    logic [3:0]  s4_mask;
    logic [31:0] s4_const_color;
    logic        s4_tex_en;
    logic [31:0] s4_tex_base;
    logic [31:0] s4_tex_stride;
    logic [15:0] s4_tex_w;
    logic [15:0] s4_tex_h;
    logic [1:0]  s4_tex_fmt;
    logic [31:0] s4_v0_u, s4_v0_v, s4_v1_u, s4_v1_v, s4_v2_u, s4_v2_v;
    logic [31:0] s4_b0, s4_b1, s4_b2;
    logic signed [31:0] s4_area;
    logic [31:0] s4_tex_addr;

    wire s4_ready = out_ready || !s4_valid;
    wire s3_ready = !s3_valid || s4_ready;
    wire s2_ready = !s2_valid || s3_ready;
    wire s1_ready = !s1_valid || s2_ready;

    wire s1_push = in_valid && s1_ready;
    wire s2_push = s1_valid && s2_ready;
    wire s3_push = s2_valid && s3_ready;
    wire s4_push = s3_valid && s4_ready;

    assign in_ready = s1_ready;

    assign out_valid = s4_valid;
    assign out_x     = s4_x;
    assign out_y     = s4_y;
    assign out_mask  = s4_mask;

    assign out_const_color_argb = s4_const_color;
    assign out_tex_enable       = s4_tex_en;
    assign out_tex_base         = s4_tex_base;
    assign out_tex_stride_bytes = s4_tex_stride;
    assign out_tex_width        = s4_tex_w;
    assign out_tex_height       = s4_tex_h;
    assign out_tex_format       = s4_tex_fmt;
    assign out_v0_u             = s4_v0_u;
    assign out_v0_v             = s4_v0_v;
    assign out_v1_u             = s4_v1_u;
    assign out_v1_v             = s4_v1_v;
    assign out_v2_u             = s4_v2_u;
    assign out_v2_v             = s4_v2_v;
    assign out_bary_w0          = s4_b0;
    assign out_bary_w1          = s4_b1;
    assign out_bary_w2          = s4_b2;
    assign out_tri_area         = s4_area;

    assign out_tex_addr = s4_tex_addr;

    assign busy = s1_valid || s2_valid || s3_valid || s4_valid;

    always_ff @(posedge clk or negedge rst_n) begin
        logic [31:0] u_c_tmp;
        logic [31:0] v_c_tmp;
        if (!rst_n) begin
            s1_valid <= 1'b0;
            s2_valid <= 1'b0;
            s3_valid <= 1'b0;
            s4_valid <= 1'b0;
        end else if (flush) begin
            s1_valid <= 1'b0;
            s2_valid <= 1'b0;
            s3_valid <= 1'b0;
            s4_valid <= 1'b0;
        end else begin
            // Stage1 capture
            if (s1_push) begin
                s1_x <= in_x;
                s1_y <= in_y;
                s1_mask <= in_mask;
                s1_const_color <= in_const_color_argb;
                s1_tex_en <= in_tex_enable;
                s1_tex_base <= in_tex_base;
                s1_tex_stride <= in_tex_stride_bytes;
                s1_tex_w <= in_tex_width;
                s1_tex_h <= in_tex_height;
                s1_tex_fmt <= in_tex_format;
                s1_v0_u <= in_v0_u;
                s1_v0_v <= in_v0_v;
                s1_v1_u <= in_v1_u;
                s1_v1_v <= in_v1_v;
                s1_v2_u <= in_v2_u;
                s1_v2_v <= in_v2_v;
                s1_b0 <= in_bary_w0;
                s1_b1 <= in_bary_w1;
                s1_b2 <= in_bary_w2;
                s1_area <= in_tri_area;

                // Multiply stage
                s1_p0_u <= $signed(in_bary_w0) * $signed(in_v0_u);
                s1_p1_u <= $signed(in_bary_w1) * $signed(in_v1_u);
                s1_p2_u <= $signed(in_bary_w2) * $signed(in_v2_u);
                s1_p0_v <= $signed(in_bary_w0) * $signed(in_v0_v);
                s1_p1_v <= $signed(in_bary_w1) * $signed(in_v1_v);
                s1_p2_v <= $signed(in_bary_w2) * $signed(in_v2_v);

                s1_valid <= 1'b1;
            end else if (s1_valid && s2_ready) begin
                // shift forward without new input
                s1_valid <= 1'b0;
            end

            // Stage2 transfer (accumulate)
            if (s2_push) begin
                s2_x <= s1_x;
                s2_y <= s1_y;
                s2_mask <= s1_mask;
                s2_const_color <= s1_const_color;
                s2_tex_en <= s1_tex_en;
                s2_tex_base <= s1_tex_base;
                s2_tex_stride <= s1_tex_stride;
                s2_tex_w <= s1_tex_w;
                s2_tex_h <= s1_tex_h;
                s2_tex_fmt <= s1_tex_fmt;
                s2_v0_u <= s1_v0_u;
                s2_v0_v <= s1_v0_v;
                s2_v1_u <= s1_v1_u;
                s2_v1_v <= s1_v1_v;
                s2_v2_u <= s1_v2_u;
                s2_v2_v <= s1_v2_v;
                s2_b0 <= s1_b0;
                s2_b1 <= s1_b1;
                s2_b2 <= s1_b2;
                s2_area <= s1_area;

                s2_acc_u <= s1_p0_u + s1_p1_u + s1_p2_u;
                s2_acc_v <= s1_p0_v + s1_p1_v + s1_p2_v;

                s2_valid <= 1'b1;
            end else if (s2_valid && s3_ready) begin
                s2_valid <= 1'b0;
            end

            // Stage3 transfer (divide)
            if (s3_push) begin
                s3_x <= s2_x;
                s3_y <= s2_y;
                s3_mask <= s2_mask;
                s3_const_color <= s2_const_color;
                s3_tex_en <= s2_tex_en;
                s3_tex_base <= s2_tex_base;
                s3_tex_stride <= s2_tex_stride;
                s3_tex_w <= s2_tex_w;
                s3_tex_h <= s2_tex_h;
                s3_tex_fmt <= s2_tex_fmt;
                s3_v0_u <= s2_v0_u;
                s3_v0_v <= s2_v0_v;
                s3_v1_u <= s2_v1_u;
                s3_v1_v <= s2_v1_v;
                s3_v2_u <= s2_v2_u;
                s3_v2_v <= s2_v2_v;
                s3_b0 <= s2_b0;
                s3_b1 <= s2_b1;
                s3_b2 <= s2_b2;
                s3_area <= s2_area;

                if (s2_area == 0) begin
                    s3_u_i <= 32'sd0;
                    s3_v_i <= 32'sd0;
                end else begin
                    s3_u_i <= s2_acc_u / $signed(s2_area);
                    s3_v_i <= s2_acc_v / $signed(s2_area);
                end

                s3_valid <= 1'b1;
            end else if (s3_valid && s4_ready) begin
                s3_valid <= 1'b0;
            end

            // Stage4 transfer (clamp + addr)
            if (s4_push) begin
                s4_x <= s3_x;
                s4_y <= s3_y;
                s4_mask <= s3_mask;
                s4_const_color <= s3_const_color;
                s4_tex_en <= s3_tex_en;
                s4_tex_base <= s3_tex_base;
                s4_tex_stride <= s3_tex_stride;
                s4_tex_w <= s3_tex_w;
                s4_tex_h <= s3_tex_h;
                s4_tex_fmt <= s3_tex_fmt;
                s4_v0_u <= s3_v0_u;
                s4_v0_v <= s3_v0_v;
                s4_v1_u <= s3_v1_u;
                s4_v1_v <= s3_v1_v;
                s4_v2_u <= s3_v2_u;
                s4_v2_v <= s3_v2_v;
                s4_b0 <= s3_b0;
                s4_b1 <= s3_b1;
                s4_b2 <= s3_b2;
                s4_area <= s3_area;

                u_c_tmp = clamp_u32(s3_u_i, s3_tex_w);
                v_c_tmp = clamp_u32(s3_v_i, s3_tex_h);
                if (s3_tex_en) s4_tex_addr <= compute_tex_addr(s3_tex_base, s3_tex_stride, s3_tex_fmt, u_c_tmp, v_c_tmp);
                else           s4_tex_addr <= 32'h0;

                s4_valid <= 1'b1;
            end else if (s4_valid && out_ready) begin
                s4_valid <= 1'b0;
            end
        end
    end

endmodule
