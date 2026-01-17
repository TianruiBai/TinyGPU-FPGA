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

    // Per-quad draw attributes (latched on quad acceptance)
    input  logic [31:0] quad_const_color_argb,
    input  logic        quad_tex_enable,
    input  logic [31:0] quad_tex_base,
    input  logic [31:0] quad_tex_stride_bytes,
    input  logic [15:0] quad_tex_width,
    input  logic [15:0] quad_tex_height,
    input  logic [1:0]  quad_tex_format,      // 0=ARGB8888, 1=RGB565
    input  logic [31:0] quad_v0_u,
    input  logic [31:0] quad_v0_v,
    input  logic [31:0] quad_v1_u,
    input  logic [31:0] quad_v1_v,
    input  logic [31:0] quad_v2_u,
    input  logic [31:0] quad_v2_v,
    input  logic [31:0] quad_bary_w0,
    input  logic [31:0] quad_bary_w1,
    input  logic [31:0] quad_bary_w2,
    input  logic signed [31:0] quad_tri_area,
    input  logic [31:0] quad_tex_addr,

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
    function automatic logic [31:0] modulate_argb8888(input logic [31:0] texel, input logic [31:0] mod);
        logic [7:0] tr, tg, tb, ta;
        logic [7:0] mr, mg, mb, ma;
        logic [15:0] pr, pg, pb;
        logic [7:0] orr, org, orb;
        begin
            ta = texel[31:24];
            tb = texel[23:16];
            tg = texel[15:8];
            tr = texel[7:0];

            ma = mod[31:24];
            mb = mod[23:16];
            mg = mod[15:8];
            mr = mod[7:0];

            // Use a fast /256 scale (good enough for lighting).
            pr = tr * mr;
            pg = tg * mg;
            pb = tb * mb;
            orr = (pr + 16'h0080) >> 8;
            org = (pg + 16'h0080) >> 8;
            orb = (pb + 16'h0080) >> 8;

            // Keep alpha opaque for now (no blending path).
            modulate_argb8888 = {8'hFF, orb, org, orr};
        end
    endfunction
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

    typedef enum logic [2:0] {
        IDLE,
        TEX_REQ,
        TEX_WAIT,
        EMIT_PREP,
        EMIT_SEND
    } state_t;

    state_t state;

    logic [31:0] hold_x;
    logic [31:0] hold_y;
    logic [3:0]  hold_mask;
    logic [3:0]  rem_mask;
    logic [1:0]  pix_idx;
    logic [1:0]  hold_tex_format;

    logic [31:0] quad_color;
    logic [31:0] tex_req_addr_reg;

    // Internal 2-stage ROP pipeline:
    //   - EMIT_PREP: compute/blend (future) and latch a single store op
    //   - EMIT_SEND: present latched store to write queue (st_*)
    logic        st_hold_valid;
    logic        st_hold_en;
    logic [1:0]  st_hold_idx;
    logic [31:0] st_hold_addr;
    logic [31:0] st_hold_wdata;
    logic [3:0]  st_hold_wstrb;

    function automatic logic [1:0] first_set_idx(input logic [3:0] m);
        if (m[0]) return 2'd0;
        if (m[1]) return 2'd1;
        if (m[2]) return 2'd2;
        return 2'd3;
    endfunction

    always_comb begin
        logic [1:0] cur_idx;

        st_valid = 1'b0;
        st_addr  = 32'h0;
        st_wdata = 32'h0;
        st_wstrb = 4'h0;

        tex_req_valid = 1'b0;
        tex_req_addr  = tex_req_addr_reg;

        quad_ready = (state == IDLE);
        busy = (state != IDLE);

    cur_idx = pix_idx;

        if (state == TEX_REQ) begin
            tex_req_valid = 1'b1;
        end

        if (state == EMIT_SEND && st_hold_valid && st_hold_en) begin
            st_valid = 1'b1;
            st_addr  = st_hold_addr;
            st_wdata = st_hold_wdata;
            st_wstrb = st_hold_wstrb;
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state <= IDLE;
            hold_x <= 32'h0;
            hold_y <= 32'h0;
            hold_mask <= 4'h0;
            rem_mask <= 4'h0;
            pix_idx <= 2'd0;
            hold_tex_format <= 2'd0;
            quad_color <= 32'h0;
            tex_req_addr_reg <= 32'h0;
            st_hold_valid <= 1'b0;
            st_hold_en <= 1'b0;
            st_hold_idx <= 2'd0;
            st_hold_addr <= 32'h0;
            st_hold_wdata <= 32'h0;
            st_hold_wstrb <= 4'h0;
        end else if (flush) begin
            state <= IDLE;
            hold_x <= 32'h0;
            hold_y <= 32'h0;
            hold_mask <= 4'h0;
            rem_mask <= 4'h0;
            pix_idx <= 2'd0;
            hold_tex_format <= 2'd0;
            quad_color <= 32'h0;
            tex_req_addr_reg <= 32'h0;
            st_hold_valid <= 1'b0;
            st_hold_en <= 1'b0;
            st_hold_idx <= 2'd0;
            st_hold_addr <= 32'h0;
            st_hold_wdata <= 32'h0;
            st_hold_wstrb <= 4'h0;
        end else begin
            case (state)
                IDLE: begin
                    st_hold_valid <= 1'b0;
                    if (quad_valid) begin
                        hold_x <= quad_x;
                        hold_y <= quad_y;
                        hold_mask <= quad_mask;
                        rem_mask <= quad_mask;
                        pix_idx <= first_set_idx(quad_mask);

                        hold_tex_format <= quad_tex_format;
                        if (quad_tex_enable) begin
                            tex_req_addr_reg <= quad_tex_addr;
                            state <= TEX_REQ;
                        end else begin
                            quad_color <= quad_const_color_argb;
                            state <= EMIT_PREP;
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
                        logic [31:0] texel_argb;
                        if (hold_tex_format == 2'd1) texel_argb = rgb565_to_argb8888(tex_resp_data[15:0]);
                        else                         texel_argb = tex_resp_data;

                        quad_color <= modulate_argb8888(texel_argb, quad_const_color_argb);
                        state <= EMIT_PREP;
                    end
                end

                EMIT_PREP: begin
                    logic [1:0] cur_idx;
                    logic en;
                    logic [31:0] prep_addr;
                    logic [31:0] prep_wdata;
                    logic [3:0]  prep_wstrb;
                    cur_idx = pix_idx;

                    if (rem_mask == 4'b0000) begin
                        state <= IDLE;
                    end else begin
                        st_hold_idx <= cur_idx;
                        en = pix_enable(hold_x, hold_y, hold_mask, cur_idx, fb_width, fb_height);
                        st_hold_en <= en;
                        if (en) begin
                            compute_store(hold_x, hold_y, cur_idx, quad_color, prep_addr, prep_wdata, prep_wstrb);
                            st_hold_addr <= prep_addr;
                            st_hold_wdata <= prep_wdata;
                            st_hold_wstrb <= prep_wstrb;
                        end else begin
                            st_hold_addr <= 32'h0;
                            st_hold_wdata <= 32'h0;
                            st_hold_wstrb <= 4'h0;
                        end
                        st_hold_valid <= 1'b1;
                        state <= EMIT_SEND;
                    end
                end

                EMIT_SEND: begin
                    logic [3:0] next_mask;
                    if (st_hold_valid) begin
                        if (!st_hold_en) begin
                            next_mask = rem_mask & ~(4'b0001 << st_hold_idx);
                            rem_mask <= next_mask;
                            pix_idx <= first_set_idx(next_mask);
                            st_hold_valid <= 1'b0;
                            state <= EMIT_PREP;
                        end else if (st_ready) begin
                            next_mask = rem_mask & ~(4'b0001 << st_hold_idx);
                            rem_mask <= next_mask;
                            pix_idx <= first_set_idx(next_mask);
                            st_hold_valid <= 1'b0;
                            state <= EMIT_PREP;
                        end
                    end
                end

                default: state <= IDLE;
            endcase
        end
    end
endmodule
