module interpolator_unit #(
    parameter int Q_DEPTH = 2
) (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        flush,

    // Quad input (from raster)
    input  logic        in_valid,
    output logic        in_ready,
    input  logic [31:0] in_x,
    input  logic [31:0] in_y,
    input  logic [3:0]  in_mask,
    input  logic [31:0] in_bary_w0,
    input  logic [31:0] in_bary_w1,
    input  logic [31:0] in_bary_w2,
    input  logic signed [31:0] in_tri_area,

    // Per-quad draw attributes (latched per raster output)
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
    output logic [31:0] out_bary_w0,
    output logic [31:0] out_bary_w1,
    output logic [31:0] out_bary_w2,
    output logic signed [31:0] out_tri_area,

    // Per-quad draw attributes
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

    output logic        busy
);

    localparam int unsigned Q_IDX_W = (Q_DEPTH <= 1) ? 1 : $clog2(Q_DEPTH);
    localparam int unsigned Q_CNT_W = (Q_DEPTH <= 1) ? 1 : $clog2(Q_DEPTH + 1);
    localparam logic [Q_IDX_W-1:0] Q_LAST = Q_IDX_W'(Q_DEPTH - 1);

    initial begin
        if (Q_DEPTH < 1) $fatal(1, "interpolator_unit: Q_DEPTH must be >= 1");
    end

    logic [31:0] q_x       [Q_DEPTH];
    logic [31:0] q_y       [Q_DEPTH];
    logic [3:0]  q_mask    [Q_DEPTH];
    logic [31:0] q_bary_w0 [Q_DEPTH];
    logic [31:0] q_bary_w1 [Q_DEPTH];
    logic [31:0] q_bary_w2 [Q_DEPTH];
    logic signed [31:0] q_tri_area [Q_DEPTH];

    logic [31:0] q_const_color [Q_DEPTH];
    logic        q_tex_enable  [Q_DEPTH];
    logic [31:0] q_tex_base    [Q_DEPTH];
    logic [31:0] q_tex_stride  [Q_DEPTH];
    logic [15:0] q_tex_w       [Q_DEPTH];
    logic [15:0] q_tex_h       [Q_DEPTH];
    logic [1:0]  q_tex_fmt     [Q_DEPTH];
    logic [31:0] q_v0_u        [Q_DEPTH];
    logic [31:0] q_v0_v        [Q_DEPTH];
    logic [31:0] q_v1_u        [Q_DEPTH];
    logic [31:0] q_v1_v        [Q_DEPTH];
    logic [31:0] q_v2_u        [Q_DEPTH];
    logic [31:0] q_v2_v        [Q_DEPTH];

    logic [Q_IDX_W-1:0] q_head;
    logic [Q_IDX_W-1:0] q_tail;
    logic [Q_CNT_W-1:0] q_count;

    wire q_out_valid = (q_count != '0);
    wire q_pop = q_out_valid && out_ready;
    wire q_full = (q_count == Q_CNT_W'(Q_DEPTH));
    wire q_can_push = (!q_full) || q_pop;
    wire q_push = in_valid && q_can_push;

    assign in_ready = q_can_push;

    assign out_valid    = q_out_valid;
    assign out_x        = q_x[q_head];
    assign out_y        = q_y[q_head];
    assign out_mask     = q_mask[q_head];
    assign out_bary_w0  = q_bary_w0[q_head];
    assign out_bary_w1  = q_bary_w1[q_head];
    assign out_bary_w2  = q_bary_w2[q_head];
    assign out_tri_area = q_tri_area[q_head];

    assign out_const_color_argb = q_const_color[q_head];
    assign out_tex_enable       = q_tex_enable[q_head];
    assign out_tex_base         = q_tex_base[q_head];
    assign out_tex_stride_bytes = q_tex_stride[q_head];
    assign out_tex_width        = q_tex_w[q_head];
    assign out_tex_height       = q_tex_h[q_head];
    assign out_tex_format       = q_tex_fmt[q_head];
    assign out_v0_u             = q_v0_u[q_head];
    assign out_v0_v             = q_v0_v[q_head];
    assign out_v1_u             = q_v1_u[q_head];
    assign out_v1_v             = q_v1_v[q_head];
    assign out_v2_u             = q_v2_u[q_head];
    assign out_v2_v             = q_v2_v[q_head];

    assign busy = (q_count != '0);

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            q_head  <= '0;
            q_tail  <= '0;
            q_count <= '0;
        end else if (flush) begin
            q_head  <= '0;
            q_tail  <= '0;
            q_count <= '0;
        end else begin
            if (q_push) begin
                q_x[q_tail]       <= in_x;
                q_y[q_tail]       <= in_y;
                q_mask[q_tail]    <= in_mask;
                q_bary_w0[q_tail] <= in_bary_w0;
                q_bary_w1[q_tail] <= in_bary_w1;
                q_bary_w2[q_tail] <= in_bary_w2;
                q_tri_area[q_tail] <= in_tri_area;

                q_const_color[q_tail] <= in_const_color_argb;
                q_tex_enable[q_tail]  <= in_tex_enable;
                q_tex_base[q_tail]    <= in_tex_base;
                q_tex_stride[q_tail]  <= in_tex_stride_bytes;
                q_tex_w[q_tail]       <= in_tex_width;
                q_tex_h[q_tail]       <= in_tex_height;
                q_tex_fmt[q_tail]     <= in_tex_format;
                q_v0_u[q_tail]        <= in_v0_u;
                q_v0_v[q_tail]        <= in_v0_v;
                q_v1_u[q_tail]        <= in_v1_u;
                q_v1_v[q_tail]        <= in_v1_v;
                q_v2_u[q_tail]        <= in_v2_u;
                q_v2_v[q_tail]        <= in_v2_v;

                q_tail <= (q_tail == Q_LAST) ? '0 : (q_tail + 1'b1);
            end

            if (q_pop) begin
                q_head <= (q_head == Q_LAST) ? '0 : (q_head + 1'b1);
            end

            unique case ({q_push, q_pop})
                2'b10: q_count <= q_count + 1'b1;
                2'b01: q_count <= q_count - 1'b1;
                default: ;
            endcase
        end
    end

endmodule
