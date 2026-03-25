`timescale 1ns/1ps
// ============================================================================
// Ray-AABB Intersection Unit (Slab Method)
// Pipelined Q16.16 fixed-point, 6-stage pipeline
//
// Algorithm: For each axis compute t_near/t_far via (bound - origin) * inv_dir,
//            then accumulate max(t_near) and min(t_far) across axes.
//            Hit when max(t_near) <= min(t_far) AND t_far > 0 AND t_near < t_max.
//
// Interface: valid/ready handshake, 6-cycle latency, fully pipelined (1 test/cycle)
// ============================================================================
module ray_intersect_aabb
  import rt_pkg::*;
(
    input  logic        clk,
    input  logic        rst_n,

    // Input: ray + AABB (all Q16.16 signed)
    input  logic        in_valid,
    output logic        in_ready,
    input  logic signed [31:0] ray_ox, ray_oy, ray_oz,
    input  logic signed [31:0] inv_dx, inv_dy, inv_dz, // precomputed 1/dir
    input  logic signed [31:0] ray_tmin, ray_tmax,
    input  logic signed [31:0] aabb_min_x, aabb_min_y, aabb_min_z,
    input  logic signed [31:0] aabb_max_x, aabb_max_y, aabb_max_z,
    input  logic        [31:0] tag_in, // passthrough metadata (node ID, etc.)

    // Output: hit/miss + t_near/t_far
    output logic        out_valid,
    input  logic        out_ready,
    output logic        hit,
    output logic signed [31:0] t_near_out,
    output logic signed [31:0] t_far_out,
    output logic        [31:0] tag_out
);

    // Pipeline registers (stages 0-5)
    // Stage 0: Subtract (bound - origin) for all 6 slab boundaries
    // Stage 1-2: Multiply by inv_dir (2-cycle multiplier)
    // Stage 3: Swap if inv_dir negative (ensure t0 < t1 per axis)
    // Stage 4: Accumulate max(t_near) and min(t_far) across X,Y axes
    // Stage 5: Accumulate Z axis, final hit test

    logic [5:0] pipe_valid;
    logic [31:0] pipe_tag [5:0];

    // Stage 0 outputs: differences
    logic signed [31:0] s0_diff_min [2:0]; // min_x-ox, min_y-oy, min_z-oz
    logic signed [31:0] s0_diff_max [2:0]; // max_x-ox, max_y-oy, max_z-oz
    logic signed [31:0] s0_inv_d    [2:0];
    logic signed [31:0] s0_tmin, s0_tmax;

    // Stage 1: multiplier inputs registered
    logic signed [63:0] s1_prod_min [2:0]; // diff_min * inv_d (full precision)
    logic signed [63:0] s1_prod_max [2:0];
    logic signed [31:0] s1_tmin, s1_tmax;

    // Stage 2: truncated products (Q16.16)
    logic signed [31:0] s2_t_min_axis [2:0]; // t values for aabb_min plane
    logic signed [31:0] s2_t_max_axis [2:0]; // t values for aabb_max plane
    logic signed [31:0] s2_tmin, s2_tmax;

    // Stage 3: swapped so t0 <= t1 per axis
    logic signed [31:0] s3_t0 [2:0]; // entry t per axis
    logic signed [31:0] s3_t1 [2:0]; // exit t per axis
    logic signed [31:0] s3_tmin, s3_tmax;

    // Stage 4: partial accumulation (X,Y)
    logic signed [31:0] s4_t_near; // max(t0_x, t0_y)
    logic signed [31:0] s4_t_far;  // min(t1_x, t1_y)
    logic signed [31:0] s4_t0_z, s4_t1_z;
    logic signed [31:0] s4_tmin, s4_tmax;

    // Stage 5: final result
    logic signed [31:0] s5_t_near;
    logic signed [31:0] s5_t_far;
    logic               s5_hit;

    // Backpressure: stall when output can't accept
    wire pipe_stall = pipe_valid[5] && !out_ready;
    assign in_ready = !pipe_stall;

    // Fixed-point multiply helper (combinational, for use in pipeline stages)
    function automatic logic signed [31:0] fxp_mul32(
        input logic signed [31:0] a,
        input logic signed [31:0] b
    );
        logic signed [63:0] full;
        full = 64'(a) * 64'(b);
        return 32'(full >>> FXP_FRAC);
    endfunction

    // ---- Stage 0: Subtract ----
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pipe_valid[0] <= 1'b0;
        end else if (!pipe_stall) begin
            pipe_valid[0] <= in_valid;
            if (in_valid) begin
                s0_diff_min[0] <= aabb_min_x - ray_ox;
                s0_diff_min[1] <= aabb_min_y - ray_oy;
                s0_diff_min[2] <= aabb_min_z - ray_oz;
                s0_diff_max[0] <= aabb_max_x - ray_ox;
                s0_diff_max[1] <= aabb_max_y - ray_oy;
                s0_diff_max[2] <= aabb_max_z - ray_oz;
                s0_inv_d[0]    <= inv_dx;
                s0_inv_d[1]    <= inv_dy;
                s0_inv_d[2]    <= inv_dz;
                s0_tmin        <= ray_tmin;
                s0_tmax        <= ray_tmax;
                pipe_tag[0]    <= tag_in;
            end
        end
    end

    // ---- Stage 1: Multiply (DSP48 inference — full 64-bit product) ----
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pipe_valid[1] <= 1'b0;
        end else if (!pipe_stall) begin
            pipe_valid[1] <= pipe_valid[0];
            for (int i = 0; i < 3; i++) begin
                s1_prod_min[i] <= 64'(s0_diff_min[i]) * 64'(s0_inv_d[i]);
                s1_prod_max[i] <= 64'(s0_diff_max[i]) * 64'(s0_inv_d[i]);
            end
            s1_tmin     <= s0_tmin;
            s1_tmax     <= s0_tmax;
            pipe_tag[1] <= pipe_tag[0];
        end
    end

    // ---- Stage 2: Truncate to Q16.16 ----
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pipe_valid[2] <= 1'b0;
        end else if (!pipe_stall) begin
            pipe_valid[2] <= pipe_valid[1];
            for (int i = 0; i < 3; i++) begin
                s2_t_min_axis[i] <= 32'(s1_prod_min[i] >>> FXP_FRAC);
                s2_t_max_axis[i] <= 32'(s1_prod_max[i] >>> FXP_FRAC);
            end
            s2_tmin     <= s1_tmin;
            s2_tmax     <= s1_tmax;
            pipe_tag[2] <= pipe_tag[1];
        end
    end

    // ---- Stage 3: Swap per axis (t0 <= t1) ----
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pipe_valid[3] <= 1'b0;
        end else if (!pipe_stall) begin
            pipe_valid[3] <= pipe_valid[2];
            for (int i = 0; i < 3; i++) begin
                if (s2_t_min_axis[i] <= s2_t_max_axis[i]) begin
                    s3_t0[i] <= s2_t_min_axis[i];
                    s3_t1[i] <= s2_t_max_axis[i];
                end else begin
                    s3_t0[i] <= s2_t_max_axis[i];
                    s3_t1[i] <= s2_t_min_axis[i];
                end
            end
            s3_tmin     <= s2_tmin;
            s3_tmax     <= s2_tmax;
            pipe_tag[3] <= pipe_tag[2];
        end
    end

    // ---- Stage 4: Accumulate X, Y ----
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pipe_valid[4] <= 1'b0;
        end else if (!pipe_stall) begin
            pipe_valid[4] <= pipe_valid[3];
            // t_near = max(t0_x, t0_y)
            s4_t_near <= (s3_t0[0] > s3_t0[1]) ? s3_t0[0] : s3_t0[1];
            // t_far  = min(t1_x, t1_y)
            s4_t_far  <= (s3_t1[0] < s3_t1[1]) ? s3_t1[0] : s3_t1[1];
            s4_t0_z   <= s3_t0[2];
            s4_t1_z   <= s3_t1[2];
            s4_tmin   <= s3_tmin;
            s4_tmax   <= s3_tmax;
            pipe_tag[4] <= pipe_tag[3];
        end
    end

    // ---- Stage 5: Accumulate Z + hit test ----
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pipe_valid[5] <= 1'b0;
        end else if (!pipe_stall) begin
            pipe_valid[5] <= pipe_valid[4];
            // Final t_near = max(t_near_xy, t0_z, t_min)
            s5_t_near <= max3(s4_t_near, s4_t0_z, s4_tmin);
            // Final t_far  = min(t_far_xy, t1_z, t_max)
            s5_t_far  <= min3(s4_t_far, s4_t1_z, s4_tmax);
            // Hit test (computed on pre-Z-accumulation values for timing;
            // actual hit uses final t_near/t_far via combinational output)
            pipe_tag[5] <= pipe_tag[4];
        end
    end

    // Combinational hit test on stage 5 outputs
    assign s5_hit = (s5_t_near <= s5_t_far) && (s5_t_far > FXP_ZERO);

    // Output
    assign out_valid  = pipe_valid[5];
    assign hit        = s5_hit;
    assign t_near_out = s5_t_near;
    assign t_far_out  = s5_t_far;
    assign tag_out    = pipe_tag[5];

    // Helpers
    function automatic logic signed [31:0] max3(
        input logic signed [31:0] a, b, c
    );
        logic signed [31:0] tmp;
        tmp = (a > b) ? a : b;
        return (tmp > c) ? tmp : c;
    endfunction

    function automatic logic signed [31:0] min3(
        input logic signed [31:0] a, b, c
    );
        logic signed [31:0] tmp;
        tmp = (a < b) ? a : b;
        return (tmp < c) ? tmp : c;
    endfunction

endmodule : ray_intersect_aabb
