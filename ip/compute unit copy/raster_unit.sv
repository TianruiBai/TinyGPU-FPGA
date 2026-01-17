module raster_unit (
    input  logic        clk,
    input  logic        rst_n,
    
    // Command Interface from Dispatcher
    input  logic        cmd_valid,
    input  logic [2:0]  cmd_type, // 000: NOP, 001: SET_V0, 010: SET_V1, 011: SET_V2, 100: DRAW
    input  logic [31:0] cmd_data_x,
    input  logic [31:0] cmd_data_y,
    output logic        cmd_ready,

    // Per-triangle draw attributes (sampled when DRAW is accepted)
    input  logic [31:0] attr_const_color_argb,
    input  logic        attr_tex_enable,
    input  logic [31:0] attr_tex_base,
    input  logic [31:0] attr_tex_stride_bytes,
    input  logic [15:0] attr_tex_width,
    input  logic [15:0] attr_tex_height,
    input  logic [1:0]  attr_tex_format,
    input  logic [31:0] attr_v0_u,
    input  logic [31:0] attr_v0_v,
    input  logic [31:0] attr_v1_u,
    input  logic [31:0] attr_v1_v,
    input  logic [31:0] attr_v2_u,
    input  logic [31:0] attr_v2_v,

    // Output Interface to Backend (Quad Fragment)
    output logic        quad_valid,
    input  logic        quad_ready,
    output logic [31:0] quad_x,
    output logic [31:0] quad_y,
    output logic [3:0]  quad_mask,
    output logic [31:0] quad_bary_w0,
    output logic [31:0] quad_bary_w1,
    output logic [31:0] quad_bary_w2

    ,
    output logic [31:0] quad_const_color_argb,
    output logic        quad_tex_enable,
    output logic [31:0] quad_tex_base,
    output logic [31:0] quad_tex_stride_bytes,
    output logic [15:0] quad_tex_width,
    output logic [15:0] quad_tex_height,
    output logic [1:0]  quad_tex_format,
    output logic [31:0] quad_v0_u,
    output logic [31:0] quad_v0_v,
    output logic [31:0] quad_v1_u,
    output logic [31:0] quad_v1_v,
    output logic [31:0] quad_v2_u,
    output logic [31:0] quad_v2_v

    ,
    output logic signed [31:0] tri_area_out

    ,
    output logic        busy
);
    // Coverage rule toggle: conservative (inclusive on all edges) tends to avoid cracks
    // when vertex coordinates are quantized/rounded.
    localparam bit USE_TOPLEFT_RULE = 1'b0;
    // Vertex Storage
    logic [31:0] v0_x, v0_y;
    logic [31:0] v1_x, v1_y;
    logic [31:0] v2_x, v2_y;

    // Latched draw attributes for the active triangle
    logic [31:0] r_const_color;
    logic        r_tex_enable;
    logic [31:0] r_tex_base;
    logic [31:0] r_tex_stride_bytes;
    logic [15:0] r_tex_width;
    logic [15:0] r_tex_height;
    logic [1:0]  r_tex_format;
    logic [31:0] r_v0_u;
    logic [31:0] r_v0_v;
    logic [31:0] r_v1_u;
    logic [31:0] r_v1_v;
    logic [31:0] r_v2_u;
    logic [31:0] r_v2_v;

    // Rasterizer State
    // Setup is intentionally split into multiple registered stages to better match
    // a deeper pipeline model:
    //   - vertex fetch (via SET_V* commands)
    //   - SETUP_EDGES (edge deltas / top-left classification)
    //   - SETUP_AREA  (triangle area / winding)
    //   - SETUP_BBOX  (bounding box + raster init)
    typedef enum logic [2:0] {
        IDLE,
        SETUP_EDGES,
        SETUP_AREA,
        SETUP_BBOX,
        RASTER,
        DONE
    } state_t;
    state_t state;

    // 2-stage internal raster pipeline:
    //   Stage1: compute edge functions for a 2x2 quad and register them
    //   Stage2: compute mask/select barycentrics and register output quad
    logic        s1_valid;
    logic signed [63:0] s1_w0_s64[4];
    logic signed [63:0] s1_w1_s64[4];
    logic signed [63:0] s1_w2_s64[4];
    logic signed [31:0] s1_base_x;
    logic signed [31:0] s1_base_y;
    logic        end_reached;

    // Bounding Box
    logic signed [31:0] min_x, max_x, min_y, max_y;
    logic signed [31:0] cur_x, cur_y;

    // Triangle signed area (for winding)
    logic signed [31:0] tri_area;
    logic               tri_flip;

    // Setup stage registers
    logic setup_degenerate;
    logic tl_v1v2, tl_v2v1;
    logic tl_v2v0, tl_v0v2;
    logic tl_v0v1, tl_v1v0;

    function automatic logic signed [31:0] sat_s64_to_s32(input logic signed [63:0] v);
        if (v > 64'sd2147483647)  return 32'sd2147483647;
        if (v < -64'sd2147483648) return -32'sd2147483648;
        return $signed(v[31:0]);
    endfunction

    function automatic logic is_top_left(
        input logic signed [31:0] ax, input logic signed [31:0] ay,
        input logic signed [31:0] bx, input logic signed [31:0] by
    );
        logic signed [31:0] dx;
        logic signed [31:0] dy;
        begin
            dx = bx - ax;
            dy = by - ay;
            // Top-left rule for screen coordinates where +Y points *down*:
            // edge is "top" (dy<0) or "left" (dy==0 && dx>0)
            is_top_left = (dy < 0) || ((dy == 0) && (dx > 0));
        end
    endfunction

    // Helper: Orient 2D (64-bit to avoid overflow)
    function automatic logic signed [63:0] orient2d64(
        input logic signed [31:0] ax, input logic signed [31:0] ay,
        input logic signed [31:0] bx, input logic signed [31:0] by,
        input logic signed [31:0] cx, input logic signed [31:0] cy
    );
        return $signed(bx - ax) * $signed(cy - ay) - $signed(by - ay) * $signed(cx - ax);
    endfunction

    // Pixel-center sampling using a 2x subpixel grid: vertices are shifted <<1,
    // and the sample point is at (x<<1)+1, (y<<1)+1.
    function automatic logic signed [63:0] orient2d_center64(
        input logic signed [31:0] ax, input logic signed [31:0] ay,
        input logic signed [31:0] bx, input logic signed [31:0] by,
        input logic signed [31:0] px, input logic signed [31:0] py
    );
        logic signed [63:0] ax2, ay2, bx2, by2;
        logic signed [63:0] px2, py2;
        begin
            ax2 = $signed(ax) <<< 1;
            ay2 = $signed(ay) <<< 1;
            bx2 = $signed(bx) <<< 1;
            by2 = $signed(by) <<< 1;
            px2 = ($signed(px) <<< 1) + 64'sd1;
            py2 = ($signed(py) <<< 1) + 64'sd1;
            orient2d_center64 = (bx2 - ax2) * (py2 - ay2) - (by2 - ay2) * (px2 - ax2);
        end
    endfunction

    function automatic logic [1:0] first_set_idx(input logic [3:0] m);
        if (m[0]) return 2'd0;
        if (m[1]) return 2'd1;
        if (m[2]) return 2'd2;
        return 2'd3;
    endfunction

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state <= IDLE;
            v0_x <= 0; v0_y <= 0;
            v1_x <= 0; v1_y <= 0;
            v2_x <= 0; v2_y <= 0;
            quad_valid <= 0;
            cmd_ready <= 1'b1;
            tri_area <= 32'sd0;
            tri_flip <= 1'b0;
            setup_degenerate <= 1'b0;
            tl_v1v2 <= 1'b0; tl_v2v1 <= 1'b0;
            tl_v2v0 <= 1'b0; tl_v0v2 <= 1'b0;
            tl_v0v1 <= 1'b0; tl_v1v0 <= 1'b0;
            s1_valid <= 1'b0;
            s1_base_x <= 32'sd0;
            s1_base_y <= 32'sd0;
            end_reached <= 1'b0;
            r_const_color <= 32'h0;
            r_tex_enable <= 1'b0;
            r_tex_base <= 32'h0;
            r_tex_stride_bytes <= 32'h0;
            r_tex_width <= 16'h0;
            r_tex_height <= 16'h0;
            r_tex_format <= 2'd0;
            r_v0_u <= 32'h0;
            r_v0_v <= 32'h0;
            r_v1_u <= 32'h0;
            r_v1_v <= 32'h0;
            r_v2_u <= 32'h0;
            r_v2_v <= 32'h0;
        end else begin
            // Default ready unless busy
            cmd_ready <= (state == IDLE);
            
            // Command Processing
            if (cmd_valid && cmd_ready) begin
                case (cmd_type)
                    3'b001: begin v0_x <= cmd_data_x; v0_y <= cmd_data_y; end
                    3'b010: begin v1_x <= cmd_data_x; v1_y <= cmd_data_y; end
                    3'b011: begin v2_x <= cmd_data_x; v2_y <= cmd_data_y; end
                    3'b100: begin
                        r_const_color <= attr_const_color_argb;
                        r_tex_enable <= attr_tex_enable;
                        r_tex_base <= attr_tex_base;
                        r_tex_stride_bytes <= attr_tex_stride_bytes;
                        r_tex_width <= attr_tex_width;
                        r_tex_height <= attr_tex_height;
                        r_tex_format <= attr_tex_format;
                        r_v0_u <= attr_v0_u;
                        r_v0_v <= attr_v0_v;
                        r_v1_u <= attr_v1_u;
                        r_v1_v <= attr_v1_v;
                        r_v2_u <= attr_v2_u;
                        r_v2_v <= attr_v2_v;

                        state <= SETUP_EDGES;
                        cmd_ready <= 1'b0;
                    end
                    default: ;
                endcase
            end

            // Rasterization Logic
            case (state)
                IDLE: begin
                    quad_valid <= 1'b0;
                    s1_valid <= 1'b0;
                    end_reached <= 1'b0;
                    setup_degenerate <= 1'b0;
                end

                // Setup stage 1: edge deltas / top-left classification (precompute both directions)
                SETUP_EDGES: begin
                    tl_v1v2 <= is_top_left($signed(v1_x), $signed(v1_y), $signed(v2_x), $signed(v2_y));
                    tl_v2v1 <= is_top_left($signed(v2_x), $signed(v2_y), $signed(v1_x), $signed(v1_y));
                    tl_v2v0 <= is_top_left($signed(v2_x), $signed(v2_y), $signed(v0_x), $signed(v0_y));
                    tl_v0v2 <= is_top_left($signed(v0_x), $signed(v0_y), $signed(v2_x), $signed(v2_y));
                    tl_v0v1 <= is_top_left($signed(v0_x), $signed(v0_y), $signed(v1_x), $signed(v1_y));
                    tl_v1v0 <= is_top_left($signed(v1_x), $signed(v1_y), $signed(v0_x), $signed(v0_y));
                    state <= SETUP_AREA;
                end

                // Setup stage 2: triangle area / winding (in 2x subpixel grid)
                SETUP_AREA: begin
                    logic signed [63:0] area64;
                    area64 = orient2d64(
                        $signed(v0_x) <<< 1, $signed(v0_y) <<< 1,
                        $signed(v1_x) <<< 1, $signed(v1_y) <<< 1,
                        $signed(v2_x) <<< 1, $signed(v2_y) <<< 1
                    );

                    setup_degenerate <= (area64 == 64'sd0);
                    tri_flip <= area64[63];
                    tri_area <= sat_s64_to_s32(area64[63] ? -area64 : area64);
                    state <= SETUP_BBOX;
                end

                // Setup stage 3: bounding box + raster init
                SETUP_BBOX: begin
                    logic signed [31:0] min_x_n, max_x_n, min_y_n, max_y_n;

                    min_x_n = (signed'(v0_x) < signed'(v1_x)) ? ((signed'(v0_x) < signed'(v2_x)) ? signed'(v0_x) : signed'(v2_x)) : ((signed'(v1_x) < signed'(v2_x)) ? signed'(v1_x) : signed'(v2_x));
                    max_x_n = (signed'(v0_x) > signed'(v1_x)) ? ((signed'(v0_x) > signed'(v2_x)) ? signed'(v0_x) : signed'(v2_x)) : ((signed'(v1_x) > signed'(v2_x)) ? signed'(v1_x) : signed'(v2_x));
                    min_y_n = (signed'(v0_y) < signed'(v1_y)) ? ((signed'(v0_y) < signed'(v2_y)) ? signed'(v0_y) : signed'(v2_y)) : ((signed'(v1_y) < signed'(v2_y)) ? signed'(v1_y) : signed'(v2_y));
                    max_y_n = (signed'(v0_y) > signed'(v1_y)) ? ((signed'(v0_y) > signed'(v2_y)) ? signed'(v0_y) : signed'(v2_y)) : ((signed'(v1_y) > signed'(v2_y)) ? signed'(v1_y) : signed'(v2_y));

                    min_x <= min_x_n;
                    max_x <= max_x_n;
                    min_y <= min_y_n;
                    max_y <= max_y_n;

                    // Start at exact min corner; emit 2x2 quads.
                    cur_x <= min_x_n;
                    cur_y <= min_y_n;

                    // Reset pipeline before starting a new draw
                    s1_valid <= 1'b0;
                    quad_valid <= 1'b0;
                    end_reached <= 1'b0;

                    // Reject only degenerate triangles (area == 0). Do not enforce a winding order.
                    if (setup_degenerate) state <= DONE;
                    else                  state <= RASTER;
                end

                RASTER: begin
                    // Pipeline ready/accept conditions
                    logic s2_ready;
                    logic s1_ready;
                    logic s1_accept;
                    logic s2_accept;
                    logic last_accept;

                    s2_ready = quad_ready || !quad_valid;
                    s1_ready = (!s1_valid) || s2_ready;
                    s1_accept = s1_ready && !end_reached;
                    s2_accept = s2_ready && s1_valid;
                    last_accept = s1_accept && (cur_x + 2 > max_x) && (cur_y + 2 > max_y);

                    // Stage 2: consume stage1 registered edge values and produce output quad
                    if (s2_accept) begin
                        logic tl0, tl1, tl2;
                        logic [3:0] mask;
                        logic [1:0] sel;
                        logic signed [31:0] px [4];
                        logic signed [31:0] py [4];
                        logic in_bbox [4];
                        logic p_in [4];

                        // Reconstruct per-pixel coordinates from base.
                        px[0] = s1_base_x;
                        py[0] = s1_base_y;
                        px[1] = s1_base_x + 32'sd1;
                        py[1] = s1_base_y;
                        px[2] = s1_base_x;
                        py[2] = s1_base_y + 32'sd1;
                        px[3] = s1_base_x + 32'sd1;
                        py[3] = s1_base_y + 32'sd1;

                        // Use setup-stage precomputed top-left classification
                        tl0 = tri_flip ? tl_v2v1 : tl_v1v2;
                        tl1 = tri_flip ? tl_v0v2 : tl_v2v0;
                        tl2 = tri_flip ? tl_v1v0 : tl_v0v1;

                        for (int i = 0; i < 4; i++) begin
                            in_bbox[i] = (px[i] >= $signed(min_x)) && (px[i] <= $signed(max_x))
                                      && (py[i] >= $signed(min_y)) && (py[i] <= $signed(max_y));

                            if (USE_TOPLEFT_RULE) begin
                                p_in[i] = in_bbox[i]
                                     && (((s1_w0_s64[i] > 0) || ((s1_w0_s64[i] == 0) && tl0))
                                      && ((s1_w1_s64[i] > 0) || ((s1_w1_s64[i] == 0) && tl1))
                                      && ((s1_w2_s64[i] > 0) || ((s1_w2_s64[i] == 0) && tl2)));
                            end else begin
                                p_in[i] = in_bbox[i] && (s1_w0_s64[i] >= 0) && (s1_w1_s64[i] >= 0) && (s1_w2_s64[i] >= 0);
                            end
                        end

                        mask[0] = p_in[0];
                        mask[1] = p_in[1];
                        mask[2] = p_in[2];
                        mask[3] = p_in[3];

                        if (mask != 4'b0000) begin
                            sel = first_set_idx(mask);
                            quad_valid <= 1'b1;
                            quad_x <= $unsigned(s1_base_x);
                            quad_y <= $unsigned(s1_base_y);
                            quad_mask <= mask;
                            quad_bary_w0 <= sat_s64_to_s32(s1_w0_s64[sel]);
                            quad_bary_w1 <= sat_s64_to_s32(s1_w1_s64[sel]);
                            quad_bary_w2 <= sat_s64_to_s32(s1_w2_s64[sel]);

                            quad_const_color_argb <= r_const_color;
                            quad_tex_enable <= r_tex_enable;
                            quad_tex_base <= r_tex_base;
                            quad_tex_stride_bytes <= r_tex_stride_bytes;
                            quad_tex_width <= r_tex_width;
                            quad_tex_height <= r_tex_height;
                            quad_tex_format <= r_tex_format;
                            quad_v0_u <= r_v0_u;
                            quad_v0_v <= r_v0_v;
                            quad_v1_u <= r_v1_u;
                            quad_v1_v <= r_v1_v;
                            quad_v2_u <= r_v2_u;
                            quad_v2_v <= r_v2_v;
                        end else begin
                            quad_valid <= 1'b0;
                        end
                    end

                    // If the current output quad was accepted and we didn't produce a replacement
                    // this cycle, clear quad_valid so the pipeline can drain.
                    if (quad_valid && quad_ready && !s2_accept) begin
                        quad_valid <= 1'b0;
                    end

                    // Stage1->Stage2 transfer bookkeeping for s1_valid
                    if (s2_accept && !s1_accept) begin
                        s1_valid <= 1'b0;
                    end

                    // Stage 1: compute edge functions for current quad and register them
                    if (s1_accept) begin
                        logic signed [63:0] w0_64 [4];
                        logic signed [63:0] w1_64 [4];
                        logic signed [63:0] w2_64 [4];
                        logic signed [31:0] px [4];
                        logic signed [31:0] py [4];

                        s1_base_x <= $signed(cur_x);
                        s1_base_y <= $signed(cur_y);

                        px[0] = $signed(cur_x);
                        py[0] = $signed(cur_y);
                        px[1] = $signed(cur_x) + 32'sd1;
                        py[1] = $signed(cur_y);
                        px[2] = $signed(cur_x);
                        py[2] = $signed(cur_y) + 32'sd1;
                        px[3] = $signed(cur_x) + 32'sd1;
                        py[3] = $signed(cur_y) + 32'sd1;

                        for (int i = 0; i < 4; i++) begin
                            w0_64[i] = orient2d_center64($signed(v1_x), $signed(v1_y), $signed(v2_x), $signed(v2_y), px[i], py[i]);
                            w1_64[i] = orient2d_center64($signed(v2_x), $signed(v2_y), $signed(v0_x), $signed(v0_y), px[i], py[i]);
                            w2_64[i] = orient2d_center64($signed(v0_x), $signed(v0_y), $signed(v1_x), $signed(v1_y), px[i], py[i]);

                            s1_w0_s64[i] <= tri_flip ? -w0_64[i] : w0_64[i];
                            s1_w1_s64[i] <= tri_flip ? -w1_64[i] : w1_64[i];
                            s1_w2_s64[i] <= tri_flip ? -w2_64[i] : w2_64[i];
                        end

                        s1_valid <= 1'b1;

                        if (last_accept) begin
                            end_reached <= 1'b1;
                        end

                        // Scanline stepping (advance when stage1 accepts a new quad)
                        if (cur_x + 2 > max_x) begin
                            cur_x <= min_x;
                            if (cur_y + 2 > max_y) begin
                                cur_y <= cur_y;
                            end else begin
                                cur_y <= cur_y + 2;
                            end
                        end else begin
                            cur_x <= cur_x + 2;
                        end
                    end

                    // Drain/finish once last quad has been accepted and pipeline empties
                    if (end_reached && !s1_valid && !quad_valid) begin
                        state <= DONE;
                    end
                end
                
                DONE: begin
                    state <= IDLE;
                    cmd_ready <= 1'b1;
                end
            endcase
            
            // REMOVE lines 1490-1491 entirely.
            // Do NOT include the separate handshake block here.
            /*
            if (quad_valid && quad_ready) begin
                quad_valid <= 1'b0; 
            end
            */
        end
    end

    assign tri_area_out = tri_area;

    // Busy whenever a draw is active or a quad is pending
    always_comb begin
        busy = (state != IDLE) || quad_valid;
    end

endmodule
