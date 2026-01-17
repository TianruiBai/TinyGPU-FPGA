`include "isa_pkg.sv"

module graphics_pipeline (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        flush_all,

    // Issue Interface
    input  logic        issue_valid,
    input  isa_pkg::decode_ctrl_t issue_ctrl,
    input  logic [31:0] issue_op_a,
    input  logic [31:0] issue_op_b,
    input  logic [127:0] issue_vec_a,
    input  logic [127:0] issue_vec_b,
    output logic        queue_full,
    output logic [3:0]  queue_count, // For debug/status

    // Writeback Interface
    output logic        wb_valid, // For texture results
    output logic [4:0]  wb_rd,
    output logic [127:0] wb_data,
    output logic        wb_is_scalar, // Reserved

    // Texture Cache Interface
    output logic        tex_req_valid,
    output logic [31:0] tex_req_addr,
    output logic [4:0]  tex_req_rd,
    input  logic        tex_req_ready,
    input  logic        tex_resp_valid,
    input  logic [31:0] tex_resp_data,
    input  logic [4:0]  tex_resp_rd,

    // Graphics/ROP store interface (to LSU WMB)
    output logic        gfx_st_valid,
    output logic [31:0] gfx_st_addr,
    output logic [31:0] gfx_st_wdata,
    output logic [3:0]  gfx_st_wstrb,
    input  logic        gfx_st_ready
);
    import isa_pkg::*;

    parameter logic [31:0] SAMPLER_TABLE_BASE = 32'h0;

    // Graphics/Texture Issue Queue
    typedef struct packed {
        decode_ctrl_t ctrl;
        logic [31:0]  op_a;
        logic [31:0]  op_b;
        logic [127:0] vec_a;
        logic [127:0] vec_b;
    } gfx_issue_t;

    // Microarchitecture doc calls out an 8-deep TEX/macro-op queue.
    localparam int GQ_DEPTH = 8;
    gfx_issue_t gq [GQ_DEPTH];
    logic [GQ_DEPTH-1:0] gq_valid;
    logic [$clog2(GQ_DEPTH)-1:0] gq_head;
    logic [$clog2(GQ_DEPTH)-1:0] gq_tail;
    logic [$clog2(GQ_DEPTH+1)-1:0] gq_count_internal;
    
    assign queue_count = gq_count_internal;
    assign queue_full = (gq_count_internal == GQ_DEPTH);

    logic tex_pop;
    logic gfx_pop;
    logic queue_pop;

    // Queue Control Logic
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            gq_head <= '0;
            gq_tail <= '0;
            gq_count_internal <= '0;
            gq_valid <= '0;
        end else if (flush_all) begin
            gq_head <= '0;
            gq_tail <= '0;
            gq_count_internal <= '0;
            gq_valid <= '0;
        end else begin
            // Push
            if (issue_valid && !queue_full) begin
                gq[gq_tail].ctrl  <= issue_ctrl;
                gq[gq_tail].op_a  <= issue_op_a;
                gq[gq_tail].op_b  <= issue_op_b;
                gq[gq_tail].vec_a <= issue_vec_a;
                gq[gq_tail].vec_b <= issue_vec_b;
                gq_valid[gq_tail] <= 1'b1;
                gq_tail <= gq_tail + 1'b1;
                if (!queue_pop) gq_count_internal <= gq_count_internal + 1'b1;
            end
            
            // Pop
            if (queue_pop) begin
                gq_valid[gq_head] <= 1'b0;
                gq_head <= gq_head + 1'b1;
                if (!issue_valid || queue_full) gq_count_internal <= gq_count_internal - 1'b1;
            end
        end
    end

    wire gq_head_is_tex = gq_valid[gq_head] && gq[gq_head].ctrl.is_tex;
    wire gq_head_is_gfx = gq_valid[gq_head] && gq[gq_head].ctrl.is_gfx;

    // Only pop the queue when the pop request matches the current head type.
    // This prevents the TEX and GFX engines from accidentally popping each other's work.
    assign queue_pop = (gq_head_is_tex && tex_pop) || (gq_head_is_gfx && gfx_pop);

    // ------------------------------------------------------------------------
    // Texture Pipeline
    // ------------------------------------------------------------------------

    // Helper Functions
    function automatic logic [7:0] srgb_lin8(input logic [7:0] c);
        // Simple gamma approx x^2.2 or x^2
        // c is 0..255. result is 0..255 linear.
        // x^2 approximation: (c*c)/255.
        // (c*c + 127) >> 8 is a fast way
        return (c * c + 8'd127) >> 8; 
    endfunction

    function automatic logic [31:0] tex_pack_rgba8(input logic [31:0] rgba, input logic srgb);
        logic [7:0] r, g, b, a;
        r = rgba[7:0]; g = rgba[15:8]; b = rgba[23:16]; a = rgba[31:24];
        if (srgb) begin
            r = srgb_lin8(r);
            g = srgb_lin8(g);
            b = srgb_lin8(b);
        end
        return {a, b, g, r};
    endfunction

    function automatic logic [127:0] tex_expand_unorm16(input logic [31:0] rgba, input logic srgb);
        logic [7:0] r, g, b, a;
        r = rgba[7:0]; g = rgba[15:8]; b = rgba[23:16]; a = rgba[31:24];
        if (srgb) begin
            r = srgb_lin8(r);
            g = srgb_lin8(g);
            b = srgb_lin8(b);
        end
        // Expand 0..255 to 0..65535 by replicating high byte: 0xAB -> 0xABAB
        return { 64'h0, {a, a}, {b, b}, {g, g}, {r, r} };
    endfunction

    function automatic logic [31:0] tex_avg4(input logic [31:0] a, input logic [31:0] b);
        // Average two packed RGBA8 colors
        return { ((a[31:24] + b[31:24]) >> 1),
                 ((a[23:16] + b[23:16]) >> 1),
                 ((a[15:8]  + b[15:8])  >> 1),
                 ((a[7:0]   + b[7:0])   >> 1)};
    endfunction

    // ------------------------------------------------------------------------
    // Texture Pipeline
    // ------------------------------------------------------------------------

    // FSM States
    typedef enum logic [4:0] {
        TEX_IDLE,
        // Descriptor words per ISA: 0x00 base, 0x04 stride, 0x08 width, 0x0C height,
        // 0x10 format, 0x14 wrap, 0x18 filter, 0x1C misc.
        TEX_DESC_BASE_REQ,   TEX_DESC_BASE_WAIT,
        TEX_DESC_STRIDE_REQ, TEX_DESC_STRIDE_WAIT,
        TEX_DESC_WIDTH_REQ,  TEX_DESC_WIDTH_WAIT,
        TEX_DESC_HEIGHT_REQ, TEX_DESC_HEIGHT_WAIT,
        TEX_DESC_FMT_REQ,    TEX_DESC_FMT_WAIT,
        TEX_DESC_WRAP_REQ,   TEX_DESC_WRAP_WAIT,
        TEX_DESC_FILT_REQ,   TEX_DESC_FILT_WAIT,
        TEX_DESC_MISC_REQ,   TEX_DESC_MISC_WAIT,
        TEX_CALC_ADDR,                   // Address calculation
        TEX_SAMPLE00_REQ, TEX_SAMPLE00_WAIT,
        TEX_SAMPLE10_REQ, TEX_SAMPLE10_WAIT,
        TEX_SAMPLE01_REQ, TEX_SAMPLE01_WAIT,
        TEX_SAMPLE11_REQ, TEX_SAMPLE11_WAIT,
        TEX_DONE
    } tex_state_e;

    tex_state_e tex_state;
    
    // Descriptor fields
    logic [31:0] tex_desc_base;
    logic [31:0] tex_desc_stride;
    logic [31:0] tex_desc_width;
    logic [31:0] tex_desc_height;
    logic [31:0] tex_desc_format;
    logic [31:0] tex_desc_wrap;
    logic [31:0] tex_desc_filter;
    logic [31:0] tex_desc_misc;

    // Decoded fields
    logic        is_bilinear;
    logic        is_srgb;
    logic        is_unorm_expand;
    logic        wrap_u; // 0=Clamp, 1=Repeat
    logic        wrap_v;
    logic [15:0] tex_w;
    logic [15:0] tex_h;
    logic [1:0]  fmt_code;
    logic [1:0]  bpp_shift;

    // Working vars
    logic [31:0] cur_u, cur_v;
    logic [4:0]  saved_rd;
    logic [31:0] tex_sampler_ptr;
    
    // Sample Storage
    logic [31:0] samp_00, samp_10, samp_01, samp_11;
    
    // Address Calc vars
    logic [31:0] addr_00, addr_10, addr_01, addr_11;

    // Initial U/V from instruction
    logic [31:0] raw_u, raw_v;

    // Output buffering
    logic [127:0] tex_res_vec;
    
    // Address wrapping logic
    function automatic logic [31:0] apply_wrap(
        input logic [31:0] coord,
        input logic [15:0] max_dim,
        input logic        is_wrap
    );
        logic signed [31:0] scoord;
        logic signed [31:0] dim;
        logic signed [31:0] r;
        scoord = signed'(coord);
        dim    = signed'({16'h0, max_dim});

        if (max_dim == 16'h0) return 32'h0;

        if (!is_wrap) begin
            if (scoord < 0) return 32'h0;
            if (scoord >= dim) return logic'(dim - 1);
            return coord;
        end

        // Repeat: modulo with sign fixup
        r = scoord % dim;
        if (r < 0) r = r + dim;
        return logic'(r);
    endfunction

    function automatic logic [31:0] unpack_texel_to_argb8888(
        input logic [31:0] raw,
        input logic [1:0]  format_code
    );
        logic [15:0] rgb565;
        logic [7:0] r8, g8, b8;
        case (format_code)
            2'd0: unpack_texel_to_argb8888 = raw; // ARGB8888
            2'd1: begin
                rgb565 = raw[15:0];
                r8 = {rgb565[15:11], rgb565[15:13]};
                g8 = {rgb565[10:5],  rgb565[10:9]};
                b8 = {rgb565[4:0],   rgb565[4:2]};
                unpack_texel_to_argb8888 = {8'hFF, b8, g8, r8};
            end
            default: unpack_texel_to_argb8888 = raw;
        endcase
    endfunction

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tex_state <= TEX_IDLE;
            tex_desc_base   <= 0;
            tex_desc_stride <= 0;
            tex_desc_width  <= 0;
            tex_desc_height <= 0;
            tex_desc_format <= 0;
            tex_desc_wrap   <= 0;
            tex_desc_filter <= 0;
            tex_desc_misc   <= 0;
            is_bilinear <= 1'b0;
            is_srgb <= 1'b0;
            is_unorm_expand <= 1'b0;
            wrap_u <= 1'b0;
            wrap_v <= 1'b0;
            tex_w <= 16'h0;
            tex_h <= 16'h0;
            fmt_code <= 2'h0;
            bpp_shift <= 2'd2;
            tex_pop <= 0;
            tex_sampler_ptr <= 32'h0;
            wb_valid <= 0;
            wb_rd <= 0;
            wb_data <= 0;
            wb_is_scalar <= 0;
        end else if (flush_all) begin
            tex_state <= TEX_IDLE;
            tex_pop <= 0;
            tex_sampler_ptr <= 32'h0;
            wb_valid <= 0; // Cancel writeback on flush
            is_bilinear <= 1'b0;
            is_srgb <= 1'b0;
            is_unorm_expand <= 1'b0;
        end else begin
            // Default pulse low
            tex_pop <= 0;
            wb_valid <= 0;

            case (tex_state)
                TEX_IDLE: begin
                    // If the queue is being popped this cycle, the head will advance on this
                    // clock edge. Starting TEX in that same cycle can latch fields from the
                    // soon-to-be-popped head while the request mux immediately uses the new head.
                    if (!queue_pop && gq_head_is_tex && tex_req_ready) begin
                        saved_rd <= gq[gq_head].ctrl.rd;
                        raw_u    <= gq[gq_head].vec_a[31:0];
                        raw_v    <= gq[gq_head].vec_a[63:32];
                        tex_sampler_ptr <= (gq[gq_head].op_b < 32) ? (SAMPLER_TABLE_BASE + (gq[gq_head].op_b << 5)) : gq[gq_head].op_b;
                        tex_state <= TEX_DESC_BASE_REQ;
                    end
                end

                // --- Descriptor Fetch Sequence (32B) ---
                TEX_DESC_BASE_REQ:   if (tex_req_valid && tex_req_ready) tex_state <= TEX_DESC_BASE_WAIT;
                TEX_DESC_BASE_WAIT:  if (tex_resp_valid) begin tex_desc_base <= tex_resp_data; tex_state <= TEX_DESC_STRIDE_REQ; end

                TEX_DESC_STRIDE_REQ: if (tex_req_valid && tex_req_ready) tex_state <= TEX_DESC_STRIDE_WAIT;
                TEX_DESC_STRIDE_WAIT: if (tex_resp_valid) begin tex_desc_stride <= tex_resp_data; tex_state <= TEX_DESC_WIDTH_REQ; end

                TEX_DESC_WIDTH_REQ:  if (tex_req_valid && tex_req_ready) tex_state <= TEX_DESC_WIDTH_WAIT;
                TEX_DESC_WIDTH_WAIT: if (tex_resp_valid) begin tex_desc_width <= tex_resp_data; tex_w <= tex_resp_data[15:0]; tex_state <= TEX_DESC_HEIGHT_REQ; end

                TEX_DESC_HEIGHT_REQ: if (tex_req_valid && tex_req_ready) tex_state <= TEX_DESC_HEIGHT_WAIT;
                TEX_DESC_HEIGHT_WAIT: if (tex_resp_valid) begin tex_desc_height <= tex_resp_data; tex_h <= tex_resp_data[15:0]; tex_state <= TEX_DESC_FMT_REQ; end

                TEX_DESC_FMT_REQ:    if (tex_req_valid && tex_req_ready) tex_state <= TEX_DESC_FMT_WAIT;
                TEX_DESC_FMT_WAIT:   if (tex_resp_valid) begin
                    tex_desc_format <= tex_resp_data;
                    fmt_code <= tex_resp_data[1:0];
                    // bytes-per-pixel shift for address calc (ARGB8888=4B, RGB565=2B)
                    bpp_shift <= (tex_resp_data[1:0] == 2'd1) ? 2'd1 : 2'd2;
                    tex_state <= TEX_DESC_WRAP_REQ;
                end

                TEX_DESC_WRAP_REQ:   if (tex_req_valid && tex_req_ready) tex_state <= TEX_DESC_WRAP_WAIT;
                TEX_DESC_WRAP_WAIT:  if (tex_resp_valid) begin
                    tex_desc_wrap <= tex_resp_data;
                    wrap_u <= tex_resp_data[0];
                    wrap_v <= tex_resp_data[1];
                    tex_state <= TEX_DESC_FILT_REQ;
                end

                TEX_DESC_FILT_REQ:   if (tex_req_valid && tex_req_ready) tex_state <= TEX_DESC_FILT_WAIT;
                TEX_DESC_FILT_WAIT:  if (tex_resp_valid) begin
                    tex_desc_filter <= tex_resp_data;
                    is_bilinear <= tex_resp_data[0];
                    tex_state <= TEX_DESC_MISC_REQ;
                end

                TEX_DESC_MISC_REQ:   if (tex_req_valid && tex_req_ready) tex_state <= TEX_DESC_MISC_WAIT;
                TEX_DESC_MISC_WAIT:  if (tex_resp_valid) begin
                    tex_desc_misc <= tex_resp_data;
                    is_srgb <= tex_resp_data[0];
                    is_unorm_expand <= tex_resp_data[1];
                    tex_state <= TEX_CALC_ADDR;
                end

                // --- Address Calculation ---
                TEX_CALC_ADDR: begin
                    // Coordinates are treated as integer texel coordinates for now.
                    // 2x2 taps respect width/height bounds via clamp/repeat.
                    automatic logic [31:0] u0 = apply_wrap(raw_u,       tex_w, wrap_u);
                    automatic logic [31:0] u1 = apply_wrap(raw_u + 1,   tex_w, wrap_u);
                    automatic logic [31:0] v0 = apply_wrap(raw_v,       tex_h, wrap_v);
                    automatic logic [31:0] v1 = apply_wrap(raw_v + 1,   tex_h, wrap_v);

                    addr_00 <= tex_desc_base + (v0 * tex_desc_stride) + (u0 << bpp_shift);
                    addr_10 <= tex_desc_base + (v0 * tex_desc_stride) + (u1 << bpp_shift);
                    addr_01 <= tex_desc_base + (v1 * tex_desc_stride) + (u0 << bpp_shift);
                    addr_11 <= tex_desc_base + (v1 * tex_desc_stride) + (u1 << bpp_shift);

                    tex_state <= TEX_SAMPLE00_REQ;
                end

                // --- Sampling ---
                TEX_SAMPLE00_REQ: if (tex_req_valid && tex_req_ready) tex_state <= TEX_SAMPLE00_WAIT;
                TEX_SAMPLE00_WAIT: if (tex_resp_valid) begin
                    samp_00 <= unpack_texel_to_argb8888(tex_resp_data, fmt_code);
                    if (!is_bilinear) begin
                        // Nearest neighbor only needs 00
                        tex_res_vec = is_unorm_expand ?
                                      tex_expand_unorm16(unpack_texel_to_argb8888(tex_resp_data, fmt_code), is_srgb) :
                                      {96'h0, tex_pack_rgba8(unpack_texel_to_argb8888(tex_resp_data, fmt_code), is_srgb)};
                        tex_state <= TEX_DONE;
                    end else begin
                        tex_state <= TEX_SAMPLE10_REQ;
                    end
                end

                TEX_SAMPLE10_REQ: if (tex_req_valid && tex_req_ready) tex_state <= TEX_SAMPLE10_WAIT;
                TEX_SAMPLE10_WAIT: if (tex_resp_valid) begin
                    samp_10 <= unpack_texel_to_argb8888(tex_resp_data, fmt_code);
                    tex_state <= TEX_SAMPLE01_REQ;
                end


                TEX_SAMPLE01_REQ: if (tex_req_valid && tex_req_ready) tex_state <= TEX_SAMPLE01_WAIT;
                TEX_SAMPLE01_WAIT: if (tex_resp_valid) begin
                    samp_01 <= unpack_texel_to_argb8888(tex_resp_data, fmt_code);
                    tex_state <= TEX_SAMPLE11_REQ;
                end

                TEX_SAMPLE11_REQ: if (tex_req_valid && tex_req_ready) tex_state <= TEX_SAMPLE11_WAIT;
                TEX_SAMPLE11_WAIT: if (tex_resp_valid) begin
                    // Bilinear Average
                    // Simple average of 4 samples for now. True bilinear uses fractional parts.
                    // Given we assumed integer inputs, this really just filters the 2x2 block at that pixel.
                    logic [31:0] samp11;
                    logic [31:0] top_row;
                    logic [31:0] bot_row;
                    logic [31:0] final_color;

                    samp11 = unpack_texel_to_argb8888(tex_resp_data, fmt_code);
                    samp_11 <= samp11;
                    top_row = tex_avg4(samp_00, samp_10);
                    bot_row = tex_avg4(samp_01, samp11);
                    final_color = tex_avg4(top_row, bot_row);

                    tex_res_vec = is_unorm_expand ?
                                  tex_expand_unorm16(final_color, is_srgb) :
                                  {96'h0, tex_pack_rgba8(final_color, is_srgb)};

                    tex_state <= TEX_DONE;
                end

                TEX_DONE: begin
                    wb_valid <= 1'b1;
                    wb_rd <= saved_rd;
                    wb_data <= tex_res_vec;
                    wb_is_scalar <= 1'b0;
                    tex_pop <= gq_head_is_tex;
                    tex_state <= TEX_IDLE;
                end
            endcase
        end
    end

    // ------------------------------------------------------------------------
    // Raster Macro-ops (descriptor-based per ISA)
    // ------------------------------------------------------------------------
    // Implement RSTATE/RSETUP/RDRAW minimally:
    // - RSTATE(rs1=descriptor ptr): fetch FB base/stride/format/wh + scissor + const color (subset)
    // - RSETUP(rs1=descriptor ptr): fetch x0,y0,x1,y1,x2,y2 (subset of 96B vertex block)
    // - RDRAW: kick raster; retire only after raster+ROP are idle

    function automatic logic [31:0] vbo_addr(
        input logic [31:0] base,
        input logic [31:0] stride_bytes,
        input logic [31:0] vtx_idx,
        input logic [31:0] field_off
    );
        vbo_addr = base + (vtx_idx * stride_bytes) + field_off;
    endfunction

    typedef enum logic [7:0] {
        GFX_IDLE,
        GFX_RSTATE_FB_BASE_REQ,   GFX_RSTATE_FB_BASE_WAIT,
        GFX_RSTATE_FB_STRIDE_REQ, GFX_RSTATE_FB_STRIDE_WAIT,
        GFX_RSTATE_FB_FMT_REQ,    GFX_RSTATE_FB_FMT_WAIT,
        GFX_RSTATE_FB_WH_REQ,     GFX_RSTATE_FB_WH_WAIT,
        GFX_RSTATE_CONST_COLOR_REQ, GFX_RSTATE_CONST_COLOR_WAIT,
        GFX_RSTATE_SAMPLER_HANDLE_REQ, GFX_RSTATE_SAMPLER_HANDLE_WAIT,
        GFX_RSTATE_SCISSOR_XY_REQ,  GFX_RSTATE_SCISSOR_XY_WAIT,
        GFX_RSTATE_SCISSOR_WH_REQ,  GFX_RSTATE_SCISSOR_WH_WAIT,
        GFX_RSTATE_FLAGS0_REQ,      GFX_RSTATE_FLAGS0_WAIT,

        GFX_RSTATE_SAMP_BASE_REQ,   GFX_RSTATE_SAMP_BASE_WAIT,
        GFX_RSTATE_SAMP_STRIDE_REQ, GFX_RSTATE_SAMP_STRIDE_WAIT,
        GFX_RSTATE_SAMP_WIDTH_REQ,  GFX_RSTATE_SAMP_WIDTH_WAIT,
        GFX_RSTATE_SAMP_HEIGHT_REQ, GFX_RSTATE_SAMP_HEIGHT_WAIT,
        GFX_RSTATE_SAMP_FMT_REQ,    GFX_RSTATE_SAMP_FMT_WAIT,

        GFX_GSTATE_VBO_BASE_REQ,   GFX_GSTATE_VBO_BASE_WAIT,
        GFX_GSTATE_VBO_STRIDE_REQ, GFX_GSTATE_VBO_STRIDE_WAIT,
        GFX_GSTATE_IBO_BASE_REQ,   GFX_GSTATE_IBO_BASE_WAIT,
        GFX_GSTATE_IBO_FMT_REQ,    GFX_GSTATE_IBO_FMT_WAIT,
        GFX_GSTATE_CULL_REQ,       GFX_GSTATE_CULL_WAIT,

        GFX_GPARAM_BASE_VERTEX_REQ, GFX_GPARAM_BASE_VERTEX_WAIT,
        GFX_GPARAM_MAT_COLOR_REQ,   GFX_GPARAM_MAT_COLOR_WAIT,

        GFX_GDRAW_FIRST_REQ,  GFX_GDRAW_FIRST_WAIT,
        GFX_GDRAW_COUNT_REQ,  GFX_GDRAW_COUNT_WAIT,
        GFX_GDRAW_TOPO_REQ,   GFX_GDRAW_TOPO_WAIT,
        GFX_GDRAW_FLAGS_REQ,  GFX_GDRAW_FLAGS_WAIT,
        GFX_GDRAW_RSTATE_PTR_REQ, GFX_GDRAW_RSTATE_PTR_WAIT,
        GFX_GDRAW_BEGIN,
        GFX_GDRAW_TRI_SETUP,

        GFX_GDRAW_V0_X_REQ, GFX_GDRAW_V0_X_WAIT,
        GFX_GDRAW_V0_Y_REQ, GFX_GDRAW_V0_Y_WAIT,
        GFX_GDRAW_V0_U_REQ, GFX_GDRAW_V0_U_WAIT,
        GFX_GDRAW_V0_V_REQ, GFX_GDRAW_V0_V_WAIT,
        GFX_GDRAW_V1_X_REQ, GFX_GDRAW_V1_X_WAIT,
        GFX_GDRAW_V1_Y_REQ, GFX_GDRAW_V1_Y_WAIT,
        GFX_GDRAW_V1_U_REQ, GFX_GDRAW_V1_U_WAIT,
        GFX_GDRAW_V1_V_REQ, GFX_GDRAW_V1_V_WAIT,
        GFX_GDRAW_V2_X_REQ, GFX_GDRAW_V2_X_WAIT,
        GFX_GDRAW_V2_Y_REQ, GFX_GDRAW_V2_Y_WAIT,
        GFX_GDRAW_V2_U_REQ, GFX_GDRAW_V2_U_WAIT,
        GFX_GDRAW_V2_V_REQ, GFX_GDRAW_V2_V_WAIT,

        GFX_GDRAW_SEND_V0,
        GFX_GDRAW_SEND_V1,
        GFX_GDRAW_SEND_V2,
        GFX_GDRAW_SEND_DRAW,
        GFX_GDRAW_WAIT,

        GFX_RRECT_X0_REQ,    GFX_RRECT_X0_WAIT,
        GFX_RRECT_Y0_REQ,    GFX_RRECT_Y0_WAIT,
        GFX_RRECT_X1_REQ,    GFX_RRECT_X1_WAIT,
        GFX_RRECT_Y1_REQ,    GFX_RRECT_Y1_WAIT,
        GFX_RRECT_COLOR_REQ, GFX_RRECT_COLOR_WAIT,
        GFX_RRECT_SEND1_V0,
        GFX_RRECT_SEND1_V1,
        GFX_RRECT_SEND1_V2,
        GFX_RRECT_DRAW1,
        GFX_RRECT_WAIT1,
        GFX_RRECT_SEND2_V0,
        GFX_RRECT_SEND2_V1,
        GFX_RRECT_SEND2_V2,
        GFX_RRECT_DRAW2,
        GFX_RRECT_WAIT2,

        GFX_FETCH_X0_REQ, GFX_FETCH_X0_WAIT,
        GFX_FETCH_Y0_REQ, GFX_FETCH_Y0_WAIT,
        GFX_FETCH_U0_REQ, GFX_FETCH_U0_WAIT,
        GFX_FETCH_V0_REQ, GFX_FETCH_V0_WAIT,
        GFX_FETCH_X1_REQ, GFX_FETCH_X1_WAIT,
        GFX_FETCH_Y1_REQ, GFX_FETCH_Y1_WAIT,
        GFX_FETCH_U1_REQ, GFX_FETCH_U1_WAIT,
        GFX_FETCH_V1_REQ, GFX_FETCH_V1_WAIT,
        GFX_FETCH_X2_REQ, GFX_FETCH_X2_WAIT,
        GFX_FETCH_Y2_REQ, GFX_FETCH_Y2_WAIT,
        GFX_FETCH_U2_REQ, GFX_FETCH_U2_WAIT,
        GFX_FETCH_V2_REQ, GFX_FETCH_V2_WAIT,
        GFX_SEND_V0,
        GFX_SEND_V1,
        GFX_SEND_V2,
        GFX_SEND_DRAW,
        GFX_WAIT_RASTER_DONE,
        GFX_DONE
    } gfx_state_e;

    gfx_state_e gfx_state;
    logic [31:0] v0x, v0y, v1x, v1y, v2x, v2y;
    logic [31:0] v0u, v0v, v1u, v1v, v2u, v2v;
    logic [31:0] rsetup_ptr;

    // RSTATE regs (subset used by initial triangle fill)
    logic [31:0] r_fb_base;
    logic [31:0] r_fb_stride;
    logic [1:0]  r_fb_fmt;
    logic [15:0] r_fb_w;
    logic [15:0] r_fb_h;
    logic [31:0] r_const_color;
    logic [31:0] r_sampler_handle;
    logic [31:0] rstate_flags0;
    logic        r_scissor_en;
    logic [15:0] r_scissor_x0;
    logic [15:0] r_scissor_y0;
    logic [15:0] r_scissor_w;
    logic [15:0] r_scissor_h;

    logic [31:0] rstate_ptr;

    // RSTATE sampler descriptor (minimal subset for ROP sampling)
    logic [31:0] rstate_sampler_ptr;
    logic [31:0] r_tex_base;
    logic [31:0] r_tex_stride;
    logic [15:0] r_tex_w;
    logic [15:0] r_tex_h;
    logic [1:0]  r_tex_fmt;

    // Geometry state
    logic [31:0] gstate_ptr;
    logic [31:0] g_vbo_base;
    logic [31:0] g_vbo_stride;
    logic [31:0] g_ibo_base;
    logic [31:0] g_ibo_format;
    logic [31:0] g_cull_clip_flags;

    // Per-draw params
    logic [31:0] gparam_ptr;
    logic [31:0] g_base_vertex;
    logic [31:0] g_material_color;

    // GDRAW command
    logic [31:0] gdraw_ptr;
    logic [31:0] gd_first;
    logic [31:0] gd_count;
    logic [31:0] gd_topology;
    logic [31:0] gd_flags;
    logic [31:0] gd_rstate_ptr;

    logic [31:0] gd_remaining;
    logic [31:0] gd_cur_first;
    logic [31:0] gd_idx0;
    logic [31:0] gd_idx1;
    logic [31:0] gd_idx2;

    // RRECT regs
    logic [31:0] rrect_ptr;
    logic [31:0] rect_x0;
    logic [31:0] rect_y0;
    logic [31:0] rect_x1;
    logic [31:0] rect_y1;
    logic [31:0] rect_color;

    // Latched ROP constant color for an in-flight draw
    logic [31:0] rop_const_color;

    // Latched ROP texture state for an in-flight draw (RDRAW only for now)
    logic        rop_tex_en;
    logic [31:0] rop_tex_base;
    logic [31:0] rop_tex_stride;
    logic [15:0] rop_tex_w;
    logic [15:0] rop_tex_h;
    logic [1:0]  rop_tex_fmt;

    // Sampler pointer helper for TEX descriptor reads
    logic [31:0] sampler_ptr;

    // Raster/ROP wiring (must be declared before instantiation for Modelsim)
    logic        raster_quad_valid;
    logic        raster_quad_ready;
    logic [31:0] raster_quad_x;
    logic [31:0] raster_quad_y;
    logic [3:0]  raster_quad_mask;
    logic        raster_busy;
    logic        rop_busy;
    logic signed [31:0] raster_tri_area;
    logic [31:0] raster_bary_w0;
    logic [31:0] raster_bary_w1;
    logic [31:0] raster_bary_w2;

    // Optional ROP texture read channel (currently unused until textured draw is enabled)
    logic        rop_tex_req_valid;
    logic [31:0] rop_tex_req_addr;
    logic        rop_tex_req_ready;
    logic        rop_tex_resp_valid;
    logic [31:0] rop_tex_resp_data;

    // Memory Request Mux
    // Priority: active TEX FSM, then (optionally) ROP texture fetches, otherwise gfx descriptor fetches.
    always_comb begin
        tex_req_valid = 1'b0;
        tex_req_addr  = 32'h0;
        tex_req_rd    = 5'h0;

        rop_tex_req_ready = 1'b0;

        // sampler_ptr reflects the pointer used by the active TEX FSM (latched at start).
        // When TEX is idle, it reflects the current queue head for debug visibility.
        sampler_ptr = (tex_state != TEX_IDLE) ? tex_sampler_ptr
               : (gq[gq_head].op_b < 32) ? (SAMPLER_TABLE_BASE + (gq[gq_head].op_b << 5))
                             : gq[gq_head].op_b;

        if (tex_state != TEX_IDLE) begin
            case (tex_state)
                TEX_DESC_BASE_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = tex_sampler_ptr + 32'h00; end
                TEX_DESC_STRIDE_REQ: begin tex_req_valid = 1'b1; tex_req_addr = tex_sampler_ptr + 32'h04; end
                TEX_DESC_WIDTH_REQ:  begin tex_req_valid = 1'b1; tex_req_addr = tex_sampler_ptr + 32'h08; end
                TEX_DESC_HEIGHT_REQ: begin tex_req_valid = 1'b1; tex_req_addr = tex_sampler_ptr + 32'h0C; end
                TEX_DESC_FMT_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = tex_sampler_ptr + 32'h10; end
                TEX_DESC_WRAP_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = tex_sampler_ptr + 32'h14; end
                TEX_DESC_FILT_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = tex_sampler_ptr + 32'h18; end
                TEX_DESC_MISC_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = tex_sampler_ptr + 32'h1C; end
                TEX_SAMPLE00_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = addr_00; end
                TEX_SAMPLE10_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = addr_10; end
                TEX_SAMPLE01_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = addr_01; end
                TEX_SAMPLE11_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = addr_11; end
                default: ;
            endcase
        end else if (rop_tex_req_valid) begin
            tex_req_valid = 1'b1;
            tex_req_addr  = rop_tex_req_addr;
            rop_tex_req_ready = tex_req_ready;
        end else begin
            case (gfx_state)
                GFX_RSTATE_FB_BASE_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h00; end
                GFX_RSTATE_FB_STRIDE_REQ:  begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h04; end
                GFX_RSTATE_FB_FMT_REQ:     begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h08; end
                GFX_RSTATE_FB_WH_REQ:      begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h0C; end
                GFX_RSTATE_CONST_COLOR_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h20; end
                GFX_RSTATE_SAMPLER_HANDLE_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h24; end
                GFX_RSTATE_SCISSOR_XY_REQ:  begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h28; end
                GFX_RSTATE_SCISSOR_WH_REQ:  begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h2C; end
                GFX_RSTATE_FLAGS0_REQ:      begin tex_req_valid = 1'b1; tex_req_addr = rstate_ptr + 32'h30; end

                GFX_RSTATE_SAMP_BASE_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = rstate_sampler_ptr + 32'h00; end
                GFX_RSTATE_SAMP_STRIDE_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rstate_sampler_ptr + 32'h04; end
                GFX_RSTATE_SAMP_WIDTH_REQ:  begin tex_req_valid = 1'b1; tex_req_addr = rstate_sampler_ptr + 32'h08; end
                GFX_RSTATE_SAMP_HEIGHT_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rstate_sampler_ptr + 32'h0C; end
                GFX_RSTATE_SAMP_FMT_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = rstate_sampler_ptr + 32'h10; end

                GFX_GSTATE_VBO_BASE_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = gstate_ptr + 32'h00; end
                GFX_GSTATE_VBO_STRIDE_REQ: begin tex_req_valid = 1'b1; tex_req_addr = gstate_ptr + 32'h04; end
                GFX_GSTATE_IBO_BASE_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = gstate_ptr + 32'h08; end
                GFX_GSTATE_IBO_FMT_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = gstate_ptr + 32'h0C; end
                GFX_GSTATE_CULL_REQ:       begin tex_req_valid = 1'b1; tex_req_addr = gstate_ptr + 32'h18; end

                GFX_GPARAM_BASE_VERTEX_REQ: begin tex_req_valid = 1'b1; tex_req_addr = gparam_ptr + 32'h00; end
                GFX_GPARAM_MAT_COLOR_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = gparam_ptr + 32'h08; end

                GFX_GDRAW_FIRST_REQ:  begin tex_req_valid = 1'b1; tex_req_addr = gdraw_ptr + 32'h00; end
                GFX_GDRAW_COUNT_REQ:  begin tex_req_valid = 1'b1; tex_req_addr = gdraw_ptr + 32'h04; end
                GFX_GDRAW_TOPO_REQ:   begin tex_req_valid = 1'b1; tex_req_addr = gdraw_ptr + 32'h08; end
                GFX_GDRAW_FLAGS_REQ:  begin tex_req_valid = 1'b1; tex_req_addr = gdraw_ptr + 32'h0C; end
                GFX_GDRAW_RSTATE_PTR_REQ: begin tex_req_valid = 1'b1; tex_req_addr = gdraw_ptr + 32'h10; end

                GFX_GDRAW_V0_X_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx0, 32'h00); end
                GFX_GDRAW_V0_Y_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx0, 32'h04); end
                GFX_GDRAW_V0_U_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx0, 32'h10); end
                GFX_GDRAW_V0_V_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx0, 32'h14); end

                GFX_GDRAW_V1_X_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx1, 32'h00); end
                GFX_GDRAW_V1_Y_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx1, 32'h04); end
                GFX_GDRAW_V1_U_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx1, 32'h10); end
                GFX_GDRAW_V1_V_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx1, 32'h14); end

                GFX_GDRAW_V2_X_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx2, 32'h00); end
                GFX_GDRAW_V2_Y_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx2, 32'h04); end
                GFX_GDRAW_V2_U_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx2, 32'h10); end
                GFX_GDRAW_V2_V_REQ: begin tex_req_valid = 1'b1; tex_req_addr = vbo_addr(g_vbo_base, g_vbo_stride, gd_idx2, 32'h14); end
                GFX_RRECT_X0_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = rrect_ptr + 32'h00; end
                GFX_RRECT_Y0_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = rrect_ptr + 32'h04; end
                GFX_RRECT_X1_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = rrect_ptr + 32'h08; end
                GFX_RRECT_Y1_REQ:    begin tex_req_valid = 1'b1; tex_req_addr = rrect_ptr + 32'h0C; end
                GFX_RRECT_COLOR_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rrect_ptr + 32'h10; end
                GFX_FETCH_X0_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h00; end
                GFX_FETCH_Y0_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h04; end
                GFX_FETCH_U0_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h10; end
                GFX_FETCH_V0_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h14; end
                GFX_FETCH_X1_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h20; end
                GFX_FETCH_Y1_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h24; end
                GFX_FETCH_U1_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h30; end
                GFX_FETCH_V1_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h34; end
                GFX_FETCH_X2_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h40; end
                GFX_FETCH_Y2_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h44; end
                GFX_FETCH_U2_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h50; end
                GFX_FETCH_V2_REQ: begin tex_req_valid = 1'b1; tex_req_addr = rsetup_ptr + 32'h54; end
                default: ;
            endcase
        end
    end

    wire is_rstate = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b000);
    wire is_rsetup = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b001);
    wire is_rdraw  = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b010);
    wire is_rrect  = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b011);
    wire is_gstate = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b100);
    wire is_gparam = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b101);
    wire is_gdraw  = gq_head_is_gfx && (gq[gq_head].ctrl.funct3 == 3'b110);

    logic        cmd_valid;
    logic        cmd_ready;
    logic [2:0]  cmd_type;
    logic [31:0] cmd_data_x, cmd_data_y;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            gfx_state <= GFX_IDLE;
            v0x <= 0; v0y <= 0; v1x <= 0; v1y <= 0; v2x <= 0; v2y <= 0;
            v0u <= 0; v0v <= 0; v1u <= 0; v1v <= 0; v2u <= 0; v2v <= 0;
            rsetup_ptr <= 0;
            rstate_ptr <= 0;
            rrect_ptr  <= 0;
            rect_x0 <= 0;
            rect_y0 <= 0;
            rect_x1 <= 0;
            rect_y1 <= 0;
            rect_color <= 32'h0;
            rop_const_color <= 32'hFFFF00FF;

            rop_tex_en <= 1'b0;
            rop_tex_base   <= 32'h0;
            rop_tex_stride <= 32'h0;
            rop_tex_w      <= 16'h0;
            rop_tex_h      <= 16'h0;
            rop_tex_fmt    <= 2'd0;

            r_fb_base   <= 32'h0;
            r_fb_stride <= 32'h0;
            r_fb_fmt    <= 2'd0;
            r_fb_w      <= 16'h0;
            r_fb_h      <= 16'h0;
            r_const_color <= 32'hFFFF00FF;
            r_sampler_handle <= 32'h0;
            rstate_flags0 <= 32'h0;
            r_scissor_en <= 1'b0;
            r_scissor_x0 <= 16'h0;
            r_scissor_y0 <= 16'h0;
            r_scissor_w  <= 16'h0;
            r_scissor_h  <= 16'h0;

            rstate_sampler_ptr <= 32'h0;
            r_tex_base   <= 32'h0;
            r_tex_stride <= 32'h0;
            r_tex_w      <= 16'h0;
            r_tex_h      <= 16'h0;
            r_tex_fmt    <= 2'd0;

            gstate_ptr <= 32'h0;
            g_vbo_base <= 32'h0;
            g_vbo_stride <= 32'h0;
            g_ibo_base <= 32'h0;
            g_ibo_format <= 32'h0;
            g_cull_clip_flags <= 32'h0;

            gparam_ptr <= 32'h0;
            g_base_vertex <= 32'h0;
            g_material_color <= 32'h0;

            gdraw_ptr <= 32'h0;
            gd_first <= 32'h0;
            gd_count <= 32'h0;
            gd_topology <= 32'h0;
            gd_flags <= 32'h0;
            gd_rstate_ptr <= 32'h0;
            gd_remaining <= 32'h0;
            gd_cur_first <= 32'h0;
            gd_idx0 <= 32'h0;
            gd_idx1 <= 32'h0;
            gd_idx2 <= 32'h0;
        end else if (flush_all) begin
            gfx_state <= GFX_IDLE;
            rop_tex_en <= 1'b0;
        end else begin
            case (gfx_state)
                GFX_IDLE: begin
                    if (queue_pop) begin
                        // Wait for the queue head to advance.
                    end else if (is_rstate) begin
                        rstate_ptr <= gq[gq_head].op_a;
                        gfx_state <= GFX_RSTATE_FB_BASE_REQ;
                    end else if (is_rrect) begin
                        rrect_ptr <= gq[gq_head].op_a;
                        gfx_state <= GFX_RRECT_X0_REQ;
                    end else if (is_rsetup) begin
                        rsetup_ptr <= gq[gq_head].op_a;
                        gfx_state <= GFX_FETCH_X0_REQ;
                    end else if (is_gstate) begin
                        gstate_ptr <= gq[gq_head].op_a;
                        gfx_state <= GFX_GSTATE_VBO_BASE_REQ;
                    end else if (is_gparam) begin
                        gparam_ptr <= gq[gq_head].op_a;
                        gfx_state <= GFX_GPARAM_BASE_VERTEX_REQ;
                    end else if (is_gdraw) begin
                        gdraw_ptr <= gq[gq_head].op_a;
                        gfx_state <= GFX_GDRAW_FIRST_REQ;
                    end else if (is_rdraw) begin
                        rop_const_color <= r_const_color;
                        rop_tex_en <= rstate_flags0[1];
                        rop_tex_base   <= r_tex_base;
                        rop_tex_stride <= r_tex_stride;
                        rop_tex_w      <= r_tex_w;
                        rop_tex_h      <= r_tex_h;
                        rop_tex_fmt    <= r_tex_fmt;
                        gfx_state <= GFX_SEND_DRAW;
                    end
                end

                // RSTATE descriptor subset
                GFX_RSTATE_FB_BASE_REQ:    if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_FB_BASE_WAIT;
                GFX_RSTATE_FB_BASE_WAIT:   if (tex_resp_valid) begin r_fb_base <= tex_resp_data; gfx_state <= GFX_RSTATE_FB_STRIDE_REQ; end

                GFX_RSTATE_FB_STRIDE_REQ:  if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_FB_STRIDE_WAIT;
                GFX_RSTATE_FB_STRIDE_WAIT: if (tex_resp_valid) begin r_fb_stride <= tex_resp_data; gfx_state <= GFX_RSTATE_FB_FMT_REQ; end

                GFX_RSTATE_FB_FMT_REQ:     if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_FB_FMT_WAIT;
                GFX_RSTATE_FB_FMT_WAIT:    if (tex_resp_valid) begin r_fb_fmt <= tex_resp_data[1:0]; gfx_state <= GFX_RSTATE_FB_WH_REQ; end

                GFX_RSTATE_FB_WH_REQ:      if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_FB_WH_WAIT;
                GFX_RSTATE_FB_WH_WAIT:     if (tex_resp_valid) begin r_fb_w <= tex_resp_data[15:0]; r_fb_h <= tex_resp_data[31:16]; gfx_state <= GFX_RSTATE_CONST_COLOR_REQ; end

                GFX_RSTATE_CONST_COLOR_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_CONST_COLOR_WAIT;
                GFX_RSTATE_CONST_COLOR_WAIT: if (tex_resp_valid) begin r_const_color <= tex_resp_data; rop_const_color <= tex_resp_data; gfx_state <= GFX_RSTATE_SAMPLER_HANDLE_REQ; end

                GFX_RSTATE_SAMPLER_HANDLE_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_SAMPLER_HANDLE_WAIT;
                GFX_RSTATE_SAMPLER_HANDLE_WAIT: if (tex_resp_valid) begin
                    r_sampler_handle <= tex_resp_data;
                    rstate_sampler_ptr <= (tex_resp_data < 32) ? (SAMPLER_TABLE_BASE + (tex_resp_data << 5)) : tex_resp_data;
                    gfx_state <= GFX_RSTATE_SCISSOR_XY_REQ;
                end

                GFX_RSTATE_SCISSOR_XY_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_SCISSOR_XY_WAIT;
                GFX_RSTATE_SCISSOR_XY_WAIT: if (tex_resp_valid) begin r_scissor_x0 <= tex_resp_data[15:0]; r_scissor_y0 <= tex_resp_data[31:16]; gfx_state <= GFX_RSTATE_SCISSOR_WH_REQ; end

                GFX_RSTATE_SCISSOR_WH_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_SCISSOR_WH_WAIT;
                GFX_RSTATE_SCISSOR_WH_WAIT: if (tex_resp_valid) begin r_scissor_w <= tex_resp_data[15:0]; r_scissor_h <= tex_resp_data[31:16]; gfx_state <= GFX_RSTATE_FLAGS0_REQ; end

                GFX_RSTATE_FLAGS0_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_FLAGS0_WAIT;
                GFX_RSTATE_FLAGS0_WAIT: if (tex_resp_valid) begin
                    rstate_flags0 <= tex_resp_data;
                    r_scissor_en <= tex_resp_data[0];
                    if (tex_resp_data[1]) gfx_state <= GFX_RSTATE_SAMP_BASE_REQ;
                    else gfx_state <= GFX_DONE;
                end

                GFX_RSTATE_SAMP_BASE_REQ:   if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_SAMP_BASE_WAIT;
                GFX_RSTATE_SAMP_BASE_WAIT:  if (tex_resp_valid) begin r_tex_base <= tex_resp_data; gfx_state <= GFX_RSTATE_SAMP_STRIDE_REQ; end

                GFX_RSTATE_SAMP_STRIDE_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_SAMP_STRIDE_WAIT;
                GFX_RSTATE_SAMP_STRIDE_WAIT: if (tex_resp_valid) begin r_tex_stride <= tex_resp_data; gfx_state <= GFX_RSTATE_SAMP_WIDTH_REQ; end

                GFX_RSTATE_SAMP_WIDTH_REQ:  if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_SAMP_WIDTH_WAIT;
                GFX_RSTATE_SAMP_WIDTH_WAIT: if (tex_resp_valid) begin r_tex_w <= tex_resp_data[15:0]; gfx_state <= GFX_RSTATE_SAMP_HEIGHT_REQ; end

                GFX_RSTATE_SAMP_HEIGHT_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_SAMP_HEIGHT_WAIT;
                GFX_RSTATE_SAMP_HEIGHT_WAIT: if (tex_resp_valid) begin r_tex_h <= tex_resp_data[15:0]; gfx_state <= GFX_RSTATE_SAMP_FMT_REQ; end

                GFX_RSTATE_SAMP_FMT_REQ:    if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RSTATE_SAMP_FMT_WAIT;
                GFX_RSTATE_SAMP_FMT_WAIT:   if (tex_resp_valid) begin r_tex_fmt <= tex_resp_data[1:0]; gfx_state <= GFX_DONE; end

                // GSTATE minimal
                GFX_GSTATE_VBO_BASE_REQ:   if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GSTATE_VBO_BASE_WAIT;
                GFX_GSTATE_VBO_BASE_WAIT:  if (tex_resp_valid) begin g_vbo_base <= tex_resp_data; gfx_state <= GFX_GSTATE_VBO_STRIDE_REQ; end

                GFX_GSTATE_VBO_STRIDE_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GSTATE_VBO_STRIDE_WAIT;
                GFX_GSTATE_VBO_STRIDE_WAIT: if (tex_resp_valid) begin g_vbo_stride <= tex_resp_data; gfx_state <= GFX_GSTATE_IBO_BASE_REQ; end

                GFX_GSTATE_IBO_BASE_REQ:   if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GSTATE_IBO_BASE_WAIT;
                GFX_GSTATE_IBO_BASE_WAIT:  if (tex_resp_valid) begin g_ibo_base <= tex_resp_data; gfx_state <= GFX_GSTATE_IBO_FMT_REQ; end

                GFX_GSTATE_IBO_FMT_REQ:    if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GSTATE_IBO_FMT_WAIT;
                GFX_GSTATE_IBO_FMT_WAIT:   if (tex_resp_valid) begin g_ibo_format <= tex_resp_data; gfx_state <= GFX_GSTATE_CULL_REQ; end

                GFX_GSTATE_CULL_REQ:       if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GSTATE_CULL_WAIT;
                GFX_GSTATE_CULL_WAIT:      if (tex_resp_valid) begin g_cull_clip_flags <= tex_resp_data; gfx_state <= GFX_DONE; end

                // GPARAM minimal
                GFX_GPARAM_BASE_VERTEX_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GPARAM_BASE_VERTEX_WAIT;
                GFX_GPARAM_BASE_VERTEX_WAIT: if (tex_resp_valid) begin g_base_vertex <= tex_resp_data; gfx_state <= GFX_GPARAM_MAT_COLOR_REQ; end

                GFX_GPARAM_MAT_COLOR_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GPARAM_MAT_COLOR_WAIT;
                GFX_GPARAM_MAT_COLOR_WAIT: if (tex_resp_valid) begin g_material_color <= tex_resp_data; gfx_state <= GFX_DONE; end

                // GDRAW command block (minimal): non-indexed tri_list
                GFX_GDRAW_FIRST_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_FIRST_WAIT;
                GFX_GDRAW_FIRST_WAIT: if (tex_resp_valid) begin gd_first <= tex_resp_data; gfx_state <= GFX_GDRAW_COUNT_REQ; end

                GFX_GDRAW_COUNT_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_COUNT_WAIT;
                GFX_GDRAW_COUNT_WAIT: if (tex_resp_valid) begin gd_count <= tex_resp_data; gfx_state <= GFX_GDRAW_TOPO_REQ; end

                GFX_GDRAW_TOPO_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_TOPO_WAIT;
                GFX_GDRAW_TOPO_WAIT: if (tex_resp_valid) begin gd_topology <= tex_resp_data; gfx_state <= GFX_GDRAW_FLAGS_REQ; end

                GFX_GDRAW_FLAGS_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_FLAGS_WAIT;
                GFX_GDRAW_FLAGS_WAIT: if (tex_resp_valid) begin gd_flags <= tex_resp_data; gfx_state <= GFX_GDRAW_RSTATE_PTR_REQ; end

                GFX_GDRAW_RSTATE_PTR_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_RSTATE_PTR_WAIT;
                GFX_GDRAW_RSTATE_PTR_WAIT: if (tex_resp_valid) begin gd_rstate_ptr <= tex_resp_data; gfx_state <= GFX_GDRAW_BEGIN; end

                GFX_GDRAW_BEGIN: begin
                    // Minimal support: tri_list (0), non-indexed (FLAGS.bit0=0)
                    gd_cur_first <= gd_first;
                    gd_remaining <= gd_count;

                    if (gd_topology != 32'd0) begin
                        gfx_state <= GFX_DONE;
                    end else if (gd_flags[0]) begin
                        // Indexed path not implemented yet
                        gfx_state <= GFX_DONE;
                    end else if (g_vbo_stride == 32'h0) begin
                        gfx_state <= GFX_DONE;
                    end else begin
                        gfx_state <= GFX_GDRAW_TRI_SETUP;
                    end
                end

                GFX_GDRAW_TRI_SETUP: begin
                    if (gd_remaining < 32'd3) begin
                        gfx_state <= GFX_DONE;
                    end else begin
                        gd_idx0 <= g_base_vertex + gd_cur_first;
                        gd_idx1 <= g_base_vertex + gd_cur_first + 32'd1;
                        gd_idx2 <= g_base_vertex + gd_cur_first + 32'd2;

                        // Use material color when provided; fallback to RSTATE const color.
                        rop_const_color <= (g_material_color != 32'h0) ? g_material_color : r_const_color;

                        // Enable texture if either RSTATE enables it or GDRAW flags request it.
                        rop_tex_en <= (rstate_flags0[1] | gd_flags[2]);
                        rop_tex_base   <= r_tex_base;
                        rop_tex_stride <= r_tex_stride;
                        rop_tex_w      <= r_tex_w;
                        rop_tex_h      <= r_tex_h;
                        rop_tex_fmt    <= r_tex_fmt;

                        gfx_state <= GFX_GDRAW_V0_X_REQ;
                    end
                end

                // Vertex 0 fetch
                GFX_GDRAW_V0_X_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V0_X_WAIT;
                GFX_GDRAW_V0_X_WAIT: if (tex_resp_valid) begin v0x <= tex_resp_data; gfx_state <= GFX_GDRAW_V0_Y_REQ; end
                GFX_GDRAW_V0_Y_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V0_Y_WAIT;
                GFX_GDRAW_V0_Y_WAIT: if (tex_resp_valid) begin v0y <= tex_resp_data; gfx_state <= GFX_GDRAW_V0_U_REQ; end
                GFX_GDRAW_V0_U_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V0_U_WAIT;
                GFX_GDRAW_V0_U_WAIT: if (tex_resp_valid) begin v0u <= tex_resp_data; gfx_state <= GFX_GDRAW_V0_V_REQ; end
                GFX_GDRAW_V0_V_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V0_V_WAIT;
                GFX_GDRAW_V0_V_WAIT: if (tex_resp_valid) begin v0v <= tex_resp_data; gfx_state <= GFX_GDRAW_V1_X_REQ; end

                // Vertex 1 fetch
                GFX_GDRAW_V1_X_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V1_X_WAIT;
                GFX_GDRAW_V1_X_WAIT: if (tex_resp_valid) begin v1x <= tex_resp_data; gfx_state <= GFX_GDRAW_V1_Y_REQ; end
                GFX_GDRAW_V1_Y_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V1_Y_WAIT;
                GFX_GDRAW_V1_Y_WAIT: if (tex_resp_valid) begin v1y <= tex_resp_data; gfx_state <= GFX_GDRAW_V1_U_REQ; end
                GFX_GDRAW_V1_U_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V1_U_WAIT;
                GFX_GDRAW_V1_U_WAIT: if (tex_resp_valid) begin v1u <= tex_resp_data; gfx_state <= GFX_GDRAW_V1_V_REQ; end
                GFX_GDRAW_V1_V_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V1_V_WAIT;
                GFX_GDRAW_V1_V_WAIT: if (tex_resp_valid) begin v1v <= tex_resp_data; gfx_state <= GFX_GDRAW_V2_X_REQ; end

                // Vertex 2 fetch
                GFX_GDRAW_V2_X_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V2_X_WAIT;
                GFX_GDRAW_V2_X_WAIT: if (tex_resp_valid) begin v2x <= tex_resp_data; gfx_state <= GFX_GDRAW_V2_Y_REQ; end
                GFX_GDRAW_V2_Y_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V2_Y_WAIT;
                GFX_GDRAW_V2_Y_WAIT: if (tex_resp_valid) begin v2y <= tex_resp_data; gfx_state <= GFX_GDRAW_V2_U_REQ; end
                GFX_GDRAW_V2_U_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V2_U_WAIT;
                GFX_GDRAW_V2_U_WAIT: if (tex_resp_valid) begin v2u <= tex_resp_data; gfx_state <= GFX_GDRAW_V2_V_REQ; end
                GFX_GDRAW_V2_V_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_GDRAW_V2_V_WAIT;
                GFX_GDRAW_V2_V_WAIT: if (tex_resp_valid) begin v2v <= tex_resp_data; gfx_state <= GFX_GDRAW_SEND_V0; end

                // Issue triangle to raster
                GFX_GDRAW_SEND_V0: if (cmd_ready) gfx_state <= GFX_GDRAW_SEND_V1;
                GFX_GDRAW_SEND_V1: if (cmd_ready) gfx_state <= GFX_GDRAW_SEND_V2;
                GFX_GDRAW_SEND_V2: if (cmd_ready) gfx_state <= GFX_GDRAW_SEND_DRAW;
                GFX_GDRAW_SEND_DRAW: if (cmd_ready) gfx_state <= GFX_GDRAW_WAIT;

                GFX_GDRAW_WAIT: begin
                    if (!raster_busy && !rop_busy) begin
                        gd_cur_first <= gd_cur_first + 32'd3;
                        gd_remaining <= gd_remaining - 32'd3;
                        gfx_state <= GFX_GDRAW_TRI_SETUP;
                    end
                end

                // RRECT descriptor subset (x0,y0,x1,y1,color) then rasterize as two triangles
                GFX_RRECT_X0_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RRECT_X0_WAIT;
                GFX_RRECT_X0_WAIT: if (tex_resp_valid) begin rect_x0 <= tex_resp_data; gfx_state <= GFX_RRECT_Y0_REQ; end

                GFX_RRECT_Y0_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RRECT_Y0_WAIT;
                GFX_RRECT_Y0_WAIT: if (tex_resp_valid) begin rect_y0 <= tex_resp_data; gfx_state <= GFX_RRECT_X1_REQ; end

                GFX_RRECT_X1_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RRECT_X1_WAIT;
                GFX_RRECT_X1_WAIT: if (tex_resp_valid) begin rect_x1 <= tex_resp_data; gfx_state <= GFX_RRECT_Y1_REQ; end

                GFX_RRECT_Y1_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RRECT_Y1_WAIT;
                GFX_RRECT_Y1_WAIT: if (tex_resp_valid) begin rect_y1 <= tex_resp_data; gfx_state <= GFX_RRECT_COLOR_REQ; end

                GFX_RRECT_COLOR_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_RRECT_COLOR_WAIT;
                GFX_RRECT_COLOR_WAIT: if (tex_resp_valid) begin
                    rect_color <= tex_resp_data;
                    rop_const_color <= tex_resp_data;
                    rop_tex_en <= 1'b0;
                    // Triangle 1: (x0,y0) (x1,y0) (x0,y1)
                    v0x <= rect_x0; v0y <= rect_y0;
                    v1x <= rect_x1; v1y <= rect_y0;
                    v2x <= rect_x0; v2y <= rect_y1;
                    gfx_state <= GFX_RRECT_SEND1_V0;
                end

                GFX_RRECT_SEND1_V0: if (cmd_ready) gfx_state <= GFX_RRECT_SEND1_V1;
                GFX_RRECT_SEND1_V1: if (cmd_ready) gfx_state <= GFX_RRECT_SEND1_V2;
                GFX_RRECT_SEND1_V2: if (cmd_ready) gfx_state <= GFX_RRECT_DRAW1;
                GFX_RRECT_DRAW1:    if (cmd_ready) gfx_state <= GFX_RRECT_WAIT1;
                GFX_RRECT_WAIT1: begin
                    if (!raster_busy && !rop_busy) begin
                        // Triangle 2: (x1,y0) (x1,y1) (x0,y1)
                        v0x <= rect_x1; v0y <= rect_y0;
                        v1x <= rect_x1; v1y <= rect_y1;
                        v2x <= rect_x0; v2y <= rect_y1;
                        gfx_state <= GFX_RRECT_SEND2_V0;
                    end
                end
                GFX_RRECT_SEND2_V0: if (cmd_ready) gfx_state <= GFX_RRECT_SEND2_V1;
                GFX_RRECT_SEND2_V1: if (cmd_ready) gfx_state <= GFX_RRECT_SEND2_V2;
                GFX_RRECT_SEND2_V2: if (cmd_ready) gfx_state <= GFX_RRECT_DRAW2;
                GFX_RRECT_DRAW2:    if (cmd_ready) gfx_state <= GFX_RRECT_WAIT2;
                GFX_RRECT_WAIT2: begin
                    if (!raster_busy && !rop_busy) begin
                        rop_const_color <= r_const_color;
                        gfx_state <= GFX_DONE;
                    end
                end

                GFX_FETCH_X0_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_X0_WAIT;
                GFX_FETCH_X0_WAIT: if (tex_resp_valid) begin v0x <= tex_resp_data; gfx_state <= GFX_FETCH_Y0_REQ; end

                GFX_FETCH_Y0_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_Y0_WAIT;
                GFX_FETCH_Y0_WAIT: if (tex_resp_valid) begin v0y <= tex_resp_data; gfx_state <= GFX_FETCH_U0_REQ; end

                GFX_FETCH_U0_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_U0_WAIT;
                GFX_FETCH_U0_WAIT: if (tex_resp_valid) begin v0u <= tex_resp_data; gfx_state <= GFX_FETCH_V0_REQ; end

                GFX_FETCH_V0_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_V0_WAIT;
                GFX_FETCH_V0_WAIT: if (tex_resp_valid) begin v0v <= tex_resp_data; gfx_state <= GFX_FETCH_X1_REQ; end

                GFX_FETCH_X1_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_X1_WAIT;
                GFX_FETCH_X1_WAIT: if (tex_resp_valid) begin v1x <= tex_resp_data; gfx_state <= GFX_FETCH_Y1_REQ; end

                GFX_FETCH_Y1_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_Y1_WAIT;
                GFX_FETCH_Y1_WAIT: if (tex_resp_valid) begin v1y <= tex_resp_data; gfx_state <= GFX_FETCH_U1_REQ; end

                GFX_FETCH_U1_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_U1_WAIT;
                GFX_FETCH_U1_WAIT: if (tex_resp_valid) begin v1u <= tex_resp_data; gfx_state <= GFX_FETCH_V1_REQ; end

                GFX_FETCH_V1_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_V1_WAIT;
                GFX_FETCH_V1_WAIT: if (tex_resp_valid) begin v1v <= tex_resp_data; gfx_state <= GFX_FETCH_X2_REQ; end

                GFX_FETCH_X2_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_X2_WAIT;
                GFX_FETCH_X2_WAIT: if (tex_resp_valid) begin v2x <= tex_resp_data; gfx_state <= GFX_FETCH_Y2_REQ; end

                GFX_FETCH_Y2_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_Y2_WAIT;
                GFX_FETCH_Y2_WAIT: if (tex_resp_valid) begin v2y <= tex_resp_data; gfx_state <= GFX_FETCH_U2_REQ; end

                GFX_FETCH_U2_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_U2_WAIT;
                GFX_FETCH_U2_WAIT: if (tex_resp_valid) begin v2u <= tex_resp_data; gfx_state <= GFX_FETCH_V2_REQ; end

                GFX_FETCH_V2_REQ: if (tex_req_valid && tex_req_ready) gfx_state <= GFX_FETCH_V2_WAIT;
                GFX_FETCH_V2_WAIT: if (tex_resp_valid) begin v2v <= tex_resp_data; gfx_state <= GFX_SEND_V0; end

                GFX_SEND_V0: if (cmd_ready) gfx_state <= GFX_SEND_V1;
                GFX_SEND_V1: if (cmd_ready) gfx_state <= GFX_SEND_V2;
                GFX_SEND_V2: if (cmd_ready) gfx_state <= GFX_DONE;

                GFX_SEND_DRAW: if (cmd_ready) gfx_state <= GFX_WAIT_RASTER_DONE;

                // Wait for raster + ROP to drain before retiring RDRAW
                GFX_WAIT_RASTER_DONE: begin
                    if (!raster_busy && !rop_busy) gfx_state <= GFX_DONE;
                end

                GFX_DONE: gfx_state <= GFX_IDLE;
                default: gfx_state <= GFX_IDLE;
            endcase
        end
    end

    // cmd generation (one command per cycle when raster is ready)
    always_comb begin
        cmd_valid  = 1'b0;
        cmd_type   = 3'b000;
        cmd_data_x = 32'h0;
        cmd_data_y = 32'h0;
        gfx_pop    = 1'b0;

        if (gfx_state == GFX_SEND_V0 || gfx_state == GFX_GDRAW_SEND_V0) begin
            cmd_valid  = 1'b1;
            cmd_type   = 3'b001;
            cmd_data_x = v0x;
            cmd_data_y = v0y;
        end else if (gfx_state == GFX_SEND_V1 || gfx_state == GFX_GDRAW_SEND_V1) begin
            cmd_valid  = 1'b1;
            cmd_type   = 3'b010;
            cmd_data_x = v1x;
            cmd_data_y = v1y;
        end else if (gfx_state == GFX_SEND_V2 || gfx_state == GFX_GDRAW_SEND_V2) begin
            cmd_valid  = 1'b1;
            cmd_type   = 3'b011;
            cmd_data_x = v2x;
            cmd_data_y = v2y;
        end else if (gfx_state == GFX_SEND_DRAW || gfx_state == GFX_GDRAW_SEND_DRAW) begin
            cmd_valid  = 1'b1;
            cmd_type   = 3'b100;
        end else if (gfx_state == GFX_RRECT_SEND1_V0 || gfx_state == GFX_RRECT_SEND2_V0) begin
            cmd_valid  = 1'b1;
            cmd_type   = 3'b001;
            cmd_data_x = v0x;
            cmd_data_y = v0y;
        end else if (gfx_state == GFX_RRECT_SEND1_V1 || gfx_state == GFX_RRECT_SEND2_V1) begin
            cmd_valid  = 1'b1;
            cmd_type   = 3'b010;
            cmd_data_x = v1x;
            cmd_data_y = v1y;
        end else if (gfx_state == GFX_RRECT_SEND1_V2 || gfx_state == GFX_RRECT_SEND2_V2) begin
            cmd_valid  = 1'b1;
            cmd_type   = 3'b011;
            cmd_data_x = v2x;
            cmd_data_y = v2y;
        end else if (gfx_state == GFX_RRECT_DRAW1 || gfx_state == GFX_RRECT_DRAW2) begin
            cmd_valid  = 1'b1;
            cmd_type   = 3'b100;
        end

        // Pop the queue entry once the macro-op has been accepted.
        if (gfx_state == GFX_DONE && gq_head_is_gfx) gfx_pop = 1'b1;
    end

    raster_unit u_raster (
        .clk(clk),
        .rst_n(rst_n),
        .cmd_valid(cmd_valid),
        .cmd_ready(cmd_ready),
        .cmd_type(cmd_type),
        .cmd_data_x(cmd_data_x),
        .cmd_data_y(cmd_data_y),
        .quad_valid(raster_quad_valid),
        .quad_ready(raster_quad_ready),
        .quad_x(raster_quad_x),
        .quad_y(raster_quad_y),
        .quad_mask(raster_quad_mask),
        .quad_bary_w0(raster_bary_w0),
        .quad_bary_w1(raster_bary_w1),
        .quad_bary_w2(raster_bary_w2),
        .tri_area_out(raster_tri_area),
        .busy(raster_busy)
    );

    rop_unit u_rop (
        .clk(clk),
        .rst_n(rst_n),
        .flush(flush_all),

        .fb_base(r_fb_base),
        .fb_stride_bytes(r_fb_stride),
        .fb_format(r_fb_fmt),
        .fb_width(r_fb_w),
        .fb_height(r_fb_h),
        .scissor_en(r_scissor_en),
        .scissor_x0(r_scissor_x0),
        .scissor_y0(r_scissor_y0),
        .scissor_w(r_scissor_w),
        .scissor_h(r_scissor_h),

        .const_color_argb(rop_const_color),

        .tex_enable(rop_tex_en),
        .tex_base(rop_tex_base),
        .tex_stride_bytes(rop_tex_stride),
        .tex_width(rop_tex_w),
        .tex_height(rop_tex_h),
        .tex_format(rop_tex_fmt),
        .v0_u(v0u),
        .v0_v(v0v),
        .v1_u(v1u),
        .v1_v(v1v),
        .v2_u(v2u),
        .v2_v(v2v),
        .bary_w0(raster_bary_w0),
        .bary_w1(raster_bary_w1),
        .bary_w2(raster_bary_w2),
        .tri_area(raster_tri_area),

        .tex_req_valid(rop_tex_req_valid),
        .tex_req_addr(rop_tex_req_addr),
        .tex_req_ready(rop_tex_req_ready),
        .tex_resp_valid(rop_tex_resp_valid),
        .tex_resp_data(rop_tex_resp_data),

        .quad_valid(raster_quad_valid),
        .quad_ready(raster_quad_ready),
        .quad_x(raster_quad_x),
        .quad_y(raster_quad_y),
        .quad_mask(raster_quad_mask),

        .st_valid(gfx_st_valid),
        .st_addr(gfx_st_addr),
        .st_wdata(gfx_st_wdata),
        .st_wstrb(gfx_st_wstrb),
        .st_ready(gfx_st_ready),

        .busy(rop_busy)
    );

    assign rop_tex_resp_valid = tex_resp_valid;
    assign rop_tex_resp_data  = tex_resp_data;

endmodule