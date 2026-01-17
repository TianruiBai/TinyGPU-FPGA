module rop_unit (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        flush,

    // Framebuffer state
    input  logic [31:0] fb_base,
    input  logic [31:0] fb_stride_bytes,
    input  logic [1:0]  fb_format,      // 0=ARGB8888, 1=RGB565
    input  logic [15:0] fb_width,
    input  logic [15:0] fb_height,
    input  logic        scissor_en,
    input  logic [15:0] scissor_x0,
    input  logic [15:0] scissor_y0,
    input  logic [15:0] scissor_w,
    input  logic [15:0] scissor_h,

    input  logic [31:0] const_color_argb,

    // Optional texture sampling (minimal)
    input  logic        tex_enable,
    input  logic [31:0] tex_base,
    input  logic [31:0] tex_stride_bytes,
    input  logic [15:0] tex_width,
    input  logic [15:0] tex_height,
    input  logic [1:0]  tex_format,      // 0=ARGB8888, 1=RGB565
    input  logic [31:0] v0_u,
    input  logic [31:0] v0_v,
    input  logic [31:0] v1_u,
    input  logic [31:0] v1_v,
    input  logic [31:0] v2_u,
    input  logic [31:0] v2_v,
    input  logic [31:0] bary_w0,
    input  logic [31:0] bary_w1,
    input  logic [31:0] bary_w2,
    input  logic signed [31:0] tri_area,

    // Texture read interface
    output logic        tex_req_valid,
    output logic [31:0] tex_req_addr,
    input  logic        tex_req_ready,
    input  logic        tex_resp_valid,
    input  logic [31:0] tex_resp_data,

    // Quad input
    input  logic        quad_valid,
    output logic        quad_ready,
    input  logic [31:0] quad_x,
    input  logic [31:0] quad_y,
    input  logic [3:0]  quad_mask,

    // Store interface (to LSU WMB)
    output logic        st_valid,
    output logic [31:0] st_addr,
    output logic [31:0] st_wdata,
    output logic [3:0]  st_wstrb,
    input  logic        st_ready,

    output logic        busy
);
    function automatic logic [15:0] argb_to_rgb565(input logic [31:0] argb);
        logic [7:0] r, g, b;
        begin
            r = argb[7:0];
            g = argb[15:8];
            b = argb[23:16];
            argb_to_rgb565 = {r[7:3], g[7:2], b[7:3]};
        end
    endfunction

    function automatic logic [31:0] rgb565_to_argb8888(input logic [15:0] rgb565);
        logic [7:0] r, g, b;
        begin
            r = {rgb565[15:11], 3'b000};
            g = {rgb565[10:5],  2'b00};
            b = {rgb565[4:0],   3'b000};
            rgb565_to_argb8888 = {8'hFF, b, g, r};
        end
    endfunction

    function automatic logic [31:0] clamp_u32(input logic signed [31:0] v, input logic [15:0] max_dim);
        logic signed [31:0] max_s;
        begin
            max_s = $signed({16'h0, max_dim});
            if (v < 0) clamp_u32 = 32'd0;
            else if (v >= max_s) clamp_u32 = (max_dim == 0) ? 32'd0 : {16'h0, (max_dim - 1)};
            else clamp_u32 = $unsigned(v);
        end
    endfunction

    function automatic logic [31:0] compute_tex_addr(input logic [31:0] u, input logic [31:0] v);
        logic [31:0] byte_off;
        begin
            if (tex_format == 2'd1) byte_off = (v * tex_stride_bytes) + (u << 1);
            else                    byte_off = (v * tex_stride_bytes) + (u << 2);
            compute_tex_addr = tex_base + byte_off;
        end
    endfunction

    function automatic logic pix_enable(
        input logic [31:0] base_x,
        input logic [31:0] base_y,
        input logic [3:0]  mask,
        input logic [1:0]  idx,
        input logic [15:0] fb_w,
        input logic [15:0] fb_h
    );
        logic [31:0] px;
        logic [31:0] py;
        logic in_fb;
        logic in_sc;
        begin
            px = base_x + (idx[0] ? 32'd1 : 32'd0);
            py = base_y + (idx[1] ? 32'd1 : 32'd0);

            in_fb = (px[31:16] == 16'h0) && (py[31:16] == 16'h0)
                 && (px[15:0] < fb_w) && (py[15:0] < fb_h);

            if (!scissor_en) in_sc = 1'b1;
            else begin
                in_sc = (px[15:0] >= scissor_x0) && (py[15:0] >= scissor_y0)
                     && (px[15:0] < (scissor_x0 + scissor_w))
                     && (py[15:0] < (scissor_y0 + scissor_h));
            end

            case (idx)
                2'd0: pix_enable = mask[0] && in_fb && in_sc;
                2'd1: pix_enable = mask[1] && in_fb && in_sc;
                2'd2: pix_enable = mask[2] && in_fb && in_sc;
                default: pix_enable = mask[3] && in_fb && in_sc;
            endcase
        end
    endfunction

    function automatic void compute_store(
        input  logic [31:0] base_x,
        input  logic [31:0] base_y,
        input  logic [1:0]  idx,
        input  logic [31:0] color_argb,
        output logic [31:0] addr,
        output logic [31:0] wdata,
        output logic [3:0]  wstrb
    );
        logic [31:0] px;
        logic [31:0] py;
        logic [31:0] byte_off;
        logic [31:0] paddr;
        logic [15:0] rgb565;
        begin
            px = base_x + (idx[0] ? 32'd1 : 32'd0);
            py = base_y + (idx[1] ? 32'd1 : 32'd0);

            if (fb_format == 2'd1) begin
                byte_off = (py * fb_stride_bytes) + (px << 1);
                paddr = fb_base + byte_off;
                addr = {paddr[31:2], 2'b00};
                rgb565 = argb_to_rgb565(color_argb);

                if (paddr[1]) begin
                    wdata = {rgb565, 16'h0000};
                    wstrb = 4'b1100;
                end else begin
                    wdata = {16'h0000, rgb565};
                    wstrb = 4'b0011;
                end
            end else begin
                byte_off = (py * fb_stride_bytes) + (px << 2);
                paddr = fb_base + byte_off;
                addr = paddr;
                wdata = color_argb;
                wstrb = 4'b1111;
            end
        end
    endfunction

    typedef enum logic [1:0] {
        IDLE,
        TEX_REQ,
        TEX_WAIT,
        EMIT
    } state_t;

    state_t state;

    logic [31:0] hold_x;
    logic [31:0] hold_y;
    logic [3:0]  hold_mask;
    logic [2:0]  pix_idx;

    logic [31:0] quad_color;
    logic [31:0] tex_req_addr_reg;

    always_comb begin
        st_valid = 1'b0;
        st_addr  = 32'h0;
        st_wdata = 32'h0;
        st_wstrb = 4'h0;

        tex_req_valid = 1'b0;
        tex_req_addr  = tex_req_addr_reg;

        quad_ready = (state == IDLE);
        busy = (state != IDLE);

        if (state == TEX_REQ) begin
            tex_req_valid = 1'b1;
        end

        if (state == EMIT && (pix_idx < 3'd4)) begin
            logic [1:0] cur_idx;
            cur_idx = pix_idx[1:0];
            if (pix_enable(hold_x, hold_y, hold_mask, cur_idx, fb_width, fb_height)) begin
                compute_store(hold_x, hold_y, cur_idx, quad_color, st_addr, st_wdata, st_wstrb);
                st_valid = 1'b1;
            end
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state <= IDLE;
            hold_x <= 32'h0;
            hold_y <= 32'h0;
            hold_mask <= 4'h0;
            pix_idx <= 3'd0;
            quad_color <= 32'h0;
            tex_req_addr_reg <= 32'h0;
        end else if (flush) begin
            state <= IDLE;
            hold_x <= 32'h0;
            hold_y <= 32'h0;
            hold_mask <= 4'h0;
            pix_idx <= 3'd0;
            quad_color <= 32'h0;
            tex_req_addr_reg <= 32'h0;
        end else begin
            case (state)
                IDLE: begin
                    if (quad_valid) begin
                        hold_x <= quad_x;
                        hold_y <= quad_y;
                        hold_mask <= quad_mask;
                        pix_idx <= 3'd0;

                        if (tex_enable) begin
                            logic signed [63:0] acc_u;
                            logic signed [63:0] acc_v;
                            logic signed [31:0] u_i;
                            logic signed [31:0] v_i;
                            logic [31:0] u_c;
                            logic [31:0] v_c;

                            if (tri_area == 0) begin
                                u_i = 0;
                                v_i = 0;
                            end else begin
                                acc_u = $signed(bary_w0) * $signed(v0_u) + $signed(bary_w1) * $signed(v1_u) + $signed(bary_w2) * $signed(v2_u);
                                acc_v = $signed(bary_w0) * $signed(v0_v) + $signed(bary_w1) * $signed(v1_v) + $signed(bary_w2) * $signed(v2_v);
                                u_i = acc_u / $signed(tri_area);
                                v_i = acc_v / $signed(tri_area);
                            end

                            u_c = clamp_u32(u_i, tex_width);
                            v_c = clamp_u32(v_i, tex_height);
                            tex_req_addr_reg <= compute_tex_addr(u_c, v_c);
                            state <= TEX_REQ;
                        end else begin
                            quad_color <= const_color_argb;
                            state <= EMIT;
                        end
                    end
                end

                TEX_REQ: begin
                    if (tex_req_ready) begin
                        state <= TEX_WAIT;
                    end
                end

                TEX_WAIT: begin
                    if (tex_resp_valid) begin
                        if (tex_format == 2'd1) quad_color <= rgb565_to_argb8888(tex_resp_data[15:0]);
                        else                    quad_color <= tex_resp_data;
                        state <= EMIT;
                    end
                end

                EMIT: begin
                    logic [1:0] cur_idx;
                    cur_idx = pix_idx[1:0];

                    if (pix_idx >= 3'd4) begin
                        state <= IDLE;
                    end else begin
                        if (pix_enable(hold_x, hold_y, hold_mask, cur_idx, fb_width, fb_height)) begin
                            if (st_ready) pix_idx <= pix_idx + 3'd1;
                        end else begin
                            pix_idx <= pix_idx + 3'd1;
                        end
                    end
                end

                default: state <= IDLE;
            endcase
        end
    end
endmodule
