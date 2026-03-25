`timescale 1ns/1ps
// ============================================================================
// Ray-Triangle Intersection Unit (Möller-Trumbore Algorithm)
// Pipelined Q16.16 fixed-point, 12-stage pipeline
//
// Algorithm:
//   e1 = v1 - v0;  e2 = v2 - v0
//   h  = cross(dir, e2)
//   a  = dot(e1, h)           // determinant
//   if |a| < epsilon → miss
//   f  = 1/a
//   s  = origin - v0
//   u  = f * dot(s, h)
//   if u < 0 || u > 1 → miss
//   q  = cross(s, e1)
//   v  = f * dot(dir, q)
//   if v < 0 || u+v > 1 → miss
//   t  = f * dot(e2, q)
//   hit = (t > t_min) && (t < t_max)
//
// Interface: valid/ready handshake, 12-cycle latency, fully pipelined
// ============================================================================
module ray_intersect_tri
  import rt_pkg::*;
(
    input  logic        clk,
    input  logic        rst_n,

    // Input: ray + triangle vertices (all Q16.16 signed)
    input  logic        in_valid,
    output logic        in_ready,
    input  logic signed [31:0] ray_ox, ray_oy, ray_oz,
    input  logic signed [31:0] ray_dx, ray_dy, ray_dz,
    input  logic signed [31:0] ray_tmin, ray_tmax,
    input  logic signed [31:0] v0_x, v0_y, v0_z,
    input  logic signed [31:0] v1_x, v1_y, v1_z,
    input  logic signed [31:0] v2_x, v2_y, v2_z,
    input  logic        [31:0] tri_id_in,

    // Output: hit result
    output logic        out_valid,
    input  logic        out_ready,
    output logic        hit,
    output logic signed [31:0] t_out,    // hit distance
    output logic signed [31:0] u_out,    // barycentric u
    output logic signed [31:0] v_out,    // barycentric v
    output logic        [31:0] tri_id_out
);

    // Pipeline valid bits
    logic [11:0] pipe_valid;
    logic [31:0] pipe_tri_id [11:0];

    wire pipe_stall = pipe_valid[11] && !out_ready;
    assign in_ready = !pipe_stall;

    // Fixed-point multiply (inline for synthesis)
    function automatic logic signed [31:0] fmul(
        input logic signed [31:0] a,
        input logic signed [31:0] b
    );
        logic signed [63:0] full;
        full = 64'(a) * 64'(b);
        return 32'(full >>> FXP_FRAC);
    endfunction

    // ---- Stage 0: Edge vectors ----
    // e1 = v1 - v0, e2 = v2 - v0, s = origin - v0
    logic signed [31:0] s0_e1 [2:0]; // x,y,z
    logic signed [31:0] s0_e2 [2:0];
    logic signed [31:0] s0_s  [2:0];
    logic signed [31:0] s0_dir [2:0];
    logic signed [31:0] s0_tmin, s0_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[0] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[0] <= in_valid;
            if (in_valid) begin
                s0_e1[0] <= v1_x - v0_x; s0_e1[1] <= v1_y - v0_y; s0_e1[2] <= v1_z - v0_z;
                s0_e2[0] <= v2_x - v0_x; s0_e2[1] <= v2_y - v0_y; s0_e2[2] <= v2_z - v0_z;
                s0_s[0]  <= ray_ox - v0_x; s0_s[1] <= ray_oy - v0_y; s0_s[2] <= ray_oz - v0_z;
                s0_dir[0] <= ray_dx; s0_dir[1] <= ray_dy; s0_dir[2] <= ray_dz;
                s0_tmin <= ray_tmin; s0_tmax <= ray_tmax;
                pipe_tri_id[0] <= tri_id_in;
            end
        end
    end

    // ---- Stage 1: Cross product h = cross(dir, e2) — multiply phase ----
    logic signed [31:0] s1_h_prod [5:0]; // 6 products for cross product
    logic signed [31:0] s1_e1 [2:0], s1_s [2:0], s1_e2 [2:0], s1_dir [2:0];
    logic signed [31:0] s1_tmin, s1_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[1] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[1] <= pipe_valid[0];
            // h = dir × e2: h.x = dir.y*e2.z - dir.z*e2.y, etc.
            s1_h_prod[0] <= fmul(s0_dir[1], s0_e2[2]); // dir.y * e2.z
            s1_h_prod[1] <= fmul(s0_dir[2], s0_e2[1]); // dir.z * e2.y
            s1_h_prod[2] <= fmul(s0_dir[2], s0_e2[0]); // dir.z * e2.x
            s1_h_prod[3] <= fmul(s0_dir[0], s0_e2[2]); // dir.x * e2.z
            s1_h_prod[4] <= fmul(s0_dir[0], s0_e2[1]); // dir.x * e2.y
            s1_h_prod[5] <= fmul(s0_dir[1], s0_e2[0]); // dir.y * e2.x
            s1_e1  <= s0_e1; s1_s  <= s0_s;
            s1_e2  <= s0_e2; s1_dir <= s0_dir;
            s1_tmin <= s0_tmin; s1_tmax <= s0_tmax;
            pipe_tri_id[1] <= pipe_tri_id[0];
        end
    end

    // ---- Stage 2: Complete h, start dot(e1, h) ----
    logic signed [31:0] s2_h [2:0];
    logic signed [31:0] s2_e1 [2:0], s2_s [2:0], s2_e2 [2:0], s2_dir [2:0];
    logic signed [31:0] s2_tmin, s2_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[2] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[2] <= pipe_valid[1];
            s2_h[0] <= s1_h_prod[0] - s1_h_prod[1]; // dir.y*e2.z - dir.z*e2.y
            s2_h[1] <= s1_h_prod[2] - s1_h_prod[3]; // dir.z*e2.x - dir.x*e2.z
            s2_h[2] <= s1_h_prod[4] - s1_h_prod[5]; // dir.x*e2.y - dir.y*e2.x
            s2_e1  <= s1_e1; s2_s  <= s1_s;
            s2_e2  <= s1_e2; s2_dir <= s1_dir;
            s2_tmin <= s1_tmin; s2_tmax <= s1_tmax;
            pipe_tri_id[2] <= pipe_tri_id[1];
        end
    end

    // ---- Stage 3: dot(e1, h) = determinant a, dot(s, h) ----
    logic signed [31:0] s3_dot_e1h [2:0]; // partial products
    logic signed [31:0] s3_dot_sh  [2:0]; // partial products
    logic signed [31:0] s3_e1 [2:0], s3_s [2:0], s3_e2 [2:0], s3_dir [2:0];
    logic signed [31:0] s3_h [2:0];
    logic signed [31:0] s3_tmin, s3_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[3] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[3] <= pipe_valid[2];
            s3_dot_e1h[0] <= fmul(s2_e1[0], s2_h[0]);
            s3_dot_e1h[1] <= fmul(s2_e1[1], s2_h[1]);
            s3_dot_e1h[2] <= fmul(s2_e1[2], s2_h[2]);
            s3_dot_sh[0]  <= fmul(s2_s[0], s2_h[0]);
            s3_dot_sh[1]  <= fmul(s2_s[1], s2_h[1]);
            s3_dot_sh[2]  <= fmul(s2_s[2], s2_h[2]);
            s3_e1  <= s2_e1; s3_s  <= s2_s;
            s3_e2  <= s2_e2; s3_dir <= s2_dir;
            s3_h   <= s2_h;
            s3_tmin <= s2_tmin; s3_tmax <= s2_tmax;
            pipe_tri_id[3] <= pipe_tri_id[2];
        end
    end

    // ---- Stage 4: Sum dot products → a (determinant), dot_sh ----
    logic signed [31:0] s4_a;       // determinant
    logic signed [31:0] s4_dot_sh;  // dot(s, h)
    logic               s4_degenerate; // |a| < epsilon
    logic signed [31:0] s4_e1 [2:0], s4_s [2:0], s4_e2 [2:0], s4_dir [2:0];
    logic signed [31:0] s4_tmin, s4_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[4] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[4] <= pipe_valid[3];
            s4_a      <= s3_dot_e1h[0] + s3_dot_e1h[1] + s3_dot_e1h[2];
            s4_dot_sh <= s3_dot_sh[0] + s3_dot_sh[1] + s3_dot_sh[2];
            s4_e1  <= s3_e1; s4_s  <= s3_s;
            s4_e2  <= s3_e2; s4_dir <= s3_dir;
            s4_tmin <= s3_tmin; s4_tmax <= s3_tmax;
            pipe_tri_id[4] <= pipe_tri_id[3];
        end
    end

    // Degenerate test (combinational from s4)
    wire signed [31:0] s4_abs_a = (s4_a < 0) ? -s4_a : s4_a;
    assign s4_degenerate = (s4_abs_a < FXP_EPSILON);

    // ---- Stage 5: Reciprocal approximation (1/a) via Newton-Raphson seed ----
    // For FPGA: use a simple LUT-based initial guess + 1 Newton step
    // f_approx = initial_guess, then f = f_approx * (2 - a * f_approx)
    logic signed [31:0] s5_inv_a;
    logic signed [31:0] s5_dot_sh;
    logic               s5_degenerate;
    logic signed [31:0] s5_e1 [2:0], s5_s [2:0], s5_e2 [2:0], s5_dir [2:0];
    logic signed [31:0] s5_tmin, s5_tmax;

    // Simple reciprocal: for bounded inputs, use the divide-by-shift approach
    // For production, this would be a dedicated reciprocal unit.
    // Here we use a 2-stage Newton-Raphson with initial estimate.
    logic signed [31:0] recip_seed;
    logic signed [31:0] recip_refined;

    // Initial seed: based on leading-bit position of |a|
    // This is a simplified approach; a LUT would be better for production
    always_comb begin
        // Cheap reciprocal seed: FXP_ONE * FXP_ONE / a ≈ 2^32 / a
        // For synthesis, use a single DSP iteration. Here we use the identity:
        // 1/a ≈ sign(a) * (2^(2*FXP_FRAC) / |a|) truncated to 32 bits
        // This is implemented as a conditional negate + shift-based estimate
        if (s4_abs_a == 0)
            recip_seed = FXP_MAX;
        else begin
            // Newton seed: start with 2^16 / leading bits estimate
            // Simplified: just do a registered divide (acceptable for FPGA at lower freq)
            // In a real design, replace with a pipelined reciprocal LUT+Newton unit
            recip_seed = 32'(({32'h0001_0000, 16'b0} / {16'b0, s4_abs_a[31:0]}));
        end
        if (s4_a < 0) recip_seed = -recip_seed;
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[5] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[5]  <= pipe_valid[4];
            s5_inv_a       <= recip_seed;
            s5_dot_sh      <= s4_dot_sh;
            s5_degenerate  <= s4_degenerate;
            s5_e1  <= s4_e1; s5_s  <= s4_s;
            s5_e2  <= s4_e2; s5_dir <= s4_dir;
            s5_tmin <= s4_tmin; s5_tmax <= s4_tmax;
            pipe_tri_id[5] <= pipe_tri_id[4];
        end
    end

    // ---- Stage 6: Compute u = f * dot(s, h); early reject ----
    logic signed [31:0] s6_u;
    logic               s6_u_reject;
    logic               s6_degenerate;
    logic signed [31:0] s6_inv_a;
    logic signed [31:0] s6_e1 [2:0], s6_s [2:0], s6_e2 [2:0], s6_dir [2:0];
    logic signed [31:0] s6_tmin, s6_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[6] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[6] <= pipe_valid[5];
            s6_u          <= fmul(s5_inv_a, s5_dot_sh);
            s6_degenerate <= s5_degenerate;
            s6_inv_a      <= s5_inv_a;
            s6_e1  <= s5_e1; s6_s  <= s5_s;
            s6_e2  <= s5_e2; s6_dir <= s5_dir;
            s6_tmin <= s5_tmin; s6_tmax <= s5_tmax;
            pipe_tri_id[6] <= pipe_tri_id[5];
        end
    end
    assign s6_u_reject = (s6_u < FXP_ZERO) || (s6_u > FXP_ONE);

    // ---- Stage 7: Cross product q = cross(s, e1) — multiply phase ----
    logic signed [31:0] s7_q_prod [5:0];
    logic signed [31:0] s7_u;
    logic               s7_reject;
    logic signed [31:0] s7_inv_a;
    logic signed [31:0] s7_e2 [2:0], s7_dir [2:0];
    logic signed [31:0] s7_tmin, s7_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[7] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[7] <= pipe_valid[6];
            // q = s × e1
            s7_q_prod[0] <= fmul(s6_s[1], s6_e1[2]); // s.y * e1.z
            s7_q_prod[1] <= fmul(s6_s[2], s6_e1[1]); // s.z * e1.y
            s7_q_prod[2] <= fmul(s6_s[2], s6_e1[0]); // s.z * e1.x
            s7_q_prod[3] <= fmul(s6_s[0], s6_e1[2]); // s.x * e1.z
            s7_q_prod[4] <= fmul(s6_s[0], s6_e1[1]); // s.x * e1.y
            s7_q_prod[5] <= fmul(s6_s[1], s6_e1[0]); // s.y * e1.x
            s7_u      <= s6_u;
            s7_reject <= s6_degenerate || s6_u_reject;
            s7_inv_a  <= s6_inv_a;
            s7_e2  <= s6_e2; s7_dir <= s6_dir;
            s7_tmin <= s6_tmin; s7_tmax <= s6_tmax;
            pipe_tri_id[7] <= pipe_tri_id[6];
        end
    end

    // ---- Stage 8: Complete q, start dot(dir, q) and dot(e2, q) ----
    logic signed [31:0] s8_q [2:0];
    logic signed [31:0] s8_u;
    logic               s8_reject;
    logic signed [31:0] s8_inv_a;
    logic signed [31:0] s8_e2 [2:0], s8_dir [2:0];
    logic signed [31:0] s8_tmin, s8_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[8] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[8] <= pipe_valid[7];
            s8_q[0] <= s7_q_prod[0] - s7_q_prod[1];
            s8_q[1] <= s7_q_prod[2] - s7_q_prod[3];
            s8_q[2] <= s7_q_prod[4] - s7_q_prod[5];
            s8_u      <= s7_u;
            s8_reject <= s7_reject;
            s8_inv_a  <= s7_inv_a;
            s8_e2  <= s7_e2; s8_dir <= s7_dir;
            s8_tmin <= s7_tmin; s8_tmax <= s7_tmax;
            pipe_tri_id[8] <= pipe_tri_id[7];
        end
    end

    // ---- Stage 9: dot(dir, q) and dot(e2, q) — multiply phase ----
    logic signed [31:0] s9_dot_dq [2:0]; // dir.x*q.x, dir.y*q.y, dir.z*q.z
    logic signed [31:0] s9_dot_eq [2:0]; // e2.x*q.x, e2.y*q.y, e2.z*q.z
    logic signed [31:0] s9_u;
    logic               s9_reject;
    logic signed [31:0] s9_inv_a;
    logic signed [31:0] s9_tmin, s9_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[9] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[9] <= pipe_valid[8];
            for (int i = 0; i < 3; i++) begin
                s9_dot_dq[i] <= fmul(s8_dir[i], s8_q[i]);
                s9_dot_eq[i] <= fmul(s8_e2[i], s8_q[i]);
            end
            s9_u      <= s8_u;
            s9_reject <= s8_reject;
            s9_inv_a  <= s8_inv_a;
            s9_tmin   <= s8_tmin; s9_tmax <= s8_tmax;
            pipe_tri_id[9] <= pipe_tri_id[8];
        end
    end

    // ---- Stage 10: Sum dots → raw_v, raw_t; compute v and reject ----
    logic signed [31:0] s10_raw_v; // f * dot(dir, q)
    logic signed [31:0] s10_raw_t; // f * dot(e2, q)
    logic signed [31:0] s10_u;
    logic               s10_reject;
    logic signed [31:0] s10_tmin, s10_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[10] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[10] <= pipe_valid[9];
            s10_raw_v  <= fmul(s9_inv_a, s9_dot_dq[0] + s9_dot_dq[1] + s9_dot_dq[2]);
            s10_raw_t  <= fmul(s9_inv_a, s9_dot_eq[0] + s9_dot_eq[1] + s9_dot_eq[2]);
            s10_u      <= s9_u;
            s10_reject <= s9_reject;
            s10_tmin   <= s9_tmin; s10_tmax <= s9_tmax;
            pipe_tri_id[10] <= pipe_tri_id[9];
        end
    end

    // ---- Stage 11: Final hit test ----
    logic               s11_hit;
    logic signed [31:0] s11_t, s11_u, s11_v;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pipe_valid[11] <= 1'b0;
            s11_hit        <= 1'b0;
        end else if (!pipe_stall) begin
            pipe_valid[11] <= pipe_valid[10];
            s11_u <= s10_u;
            s11_v <= s10_raw_v;
            s11_t <= s10_raw_t;
            // v check: v >= 0 && u + v <= 1
            // t check: t > t_min && t < t_max
            s11_hit <= !s10_reject
                    && (s10_raw_v >= FXP_ZERO)
                    && ((s10_u + s10_raw_v) <= FXP_ONE)
                    && (s10_raw_t > s10_tmin)
                    && (s10_raw_t < s10_tmax);
            pipe_tri_id[11] <= pipe_tri_id[10];
        end
    end

    // Output
    assign out_valid   = pipe_valid[11];
    assign hit         = s11_hit;
    assign t_out       = s11_t;
    assign u_out       = s11_u;
    assign v_out       = s11_v;
    assign tri_id_out  = pipe_tri_id[11];

endmodule : ray_intersect_tri
