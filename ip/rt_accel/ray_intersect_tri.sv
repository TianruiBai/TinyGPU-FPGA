`timescale 1ns/1ps
// ============================================================================
// Ray-Triangle Intersection Unit (Möller-Trumbore Algorithm)
// Pipelined Q16.16 fixed-point, 16-stage pipeline
//
// Algorithm:
//   e1 = v1 - v0;  e2 = v2 - v0
//   h  = cross(dir, e2)
//   a  = dot(e1, h)           // determinant
//   if |a| < epsilon → miss
//   f  = 1/a                  // via synthesizable LUT+Newton-Raphson reciprocal
//   s  = origin - v0
//   u  = f * dot(s, h)
//   if u < 0 || u > 1 → miss
//   q  = cross(s, e1)
//   v  = f * dot(dir, q)
//   if v < 0 || u+v > 1 → miss
//   t  = f * dot(e2, q)
//   hit = (t > t_min) && (t < t_max)
//
// Stages 0-4:  Edge vectors, cross products, determinant
// Stages 5-8:  Pipelined reciprocal (LUT + Newton-Raphson, 3 cycles) + capture
// Stages 9-15: u/v/t computation and hit test
//
// Interface: valid/ready handshake, 16-cycle latency, fully pipelined
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

    // Total pipeline depth = 16 stages (0..15)
    localparam int PIPE_DEPTH = 16;

    // Pipeline valid bits
    logic [PIPE_DEPTH-1:0] pipe_valid;
    logic [31:0] pipe_tri_id [PIPE_DEPTH-1:0];

    wire pipe_stall = pipe_valid[PIPE_DEPTH-1] && !out_ready;
    assign in_ready = !pipe_stall;

    // Fixed-point multiply (inline for synthesis — maps to DSP48)
    // (* use_dsp = "yes" *) guides Xilinx Vivado to infer DSP48 slices.
    // The constant right-shift is zero-cost wire routing.
    (* use_dsp = "yes" *)
    function automatic logic signed [31:0] fmul(
        input logic signed [31:0] a,
        input logic signed [31:0] b
    );
        logic signed [63:0] full;
        full = 64'(a) * 64'(b);
        return 32'(full >>> FXP_FRAC);
    endfunction

    // ========================================================================
    // Stage 0: Edge vectors
    //   e1 = v1 - v0, e2 = v2 - v0, s = origin - v0
    // ========================================================================
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

    // ========================================================================
    // Stage 1: Cross product h = cross(dir, e2) — multiply phase
    // ========================================================================
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

    // ========================================================================
    // Stage 2: Complete h, start dot(e1, h)
    // ========================================================================
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

    // ========================================================================
    // Stage 3: dot(e1, h) and dot(s, h) — multiply phase
    // ========================================================================
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

    // ========================================================================
    // Stage 4: Sum dot products → determinant (a), dot(s,h)
    // ========================================================================
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

    // ========================================================================
    // Stages 5-7: Pipelined reciprocal (1/a) via fxp_reciprocal module
    //   3-cycle latency: LZC+normalize → LUT+shift → Newton-Raphson
    // ========================================================================
    logic        recip_out_valid;
    logic signed [31:0] recip_out_val;

    fxp_reciprocal u_recip (
        .clk(clk),
        .rst_n(rst_n),
        .in_valid(pipe_valid[4] && !pipe_stall),
        .a_in(s4_a),
        .out_valid(recip_out_valid),
        .recip_out(recip_out_val)
    );

    // Delay-match pipeline data through the 3 reciprocal stages (5, 6, 7)
    // Plus one extra stage (8) to capture the reciprocal output correctly
    logic signed [31:0] s5_dot_sh;
    logic               s5_degenerate;
    logic signed [31:0] s5_e1 [2:0], s5_s [2:0], s5_e2 [2:0], s5_dir [2:0];
    logic signed [31:0] s5_tmin, s5_tmax;

    logic signed [31:0] s6_dot_sh;
    logic               s6_degenerate;
    logic signed [31:0] s6_e1 [2:0], s6_s [2:0], s6_e2 [2:0], s6_dir [2:0];
    logic signed [31:0] s6_tmin, s6_tmax;

    logic signed [31:0] s7_dot_sh;
    logic               s7_degenerate;
    logic signed [31:0] s7_e1 [2:0], s7_s [2:0], s7_e2 [2:0], s7_dir [2:0];
    logic signed [31:0] s7_tmin, s7_tmax;

    logic signed [31:0] s8_dot_sh;
    logic               s8_degenerate;
    logic signed [31:0] s8_inv_a;
    logic signed [31:0] s8_e1 [2:0], s8_s [2:0], s8_e2 [2:0], s8_dir [2:0];
    logic signed [31:0] s8_tmin, s8_tmax;

    // Stage 5 delay
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[5] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[5]  <= pipe_valid[4];
            s5_dot_sh      <= s4_dot_sh;
            s5_degenerate  <= s4_degenerate;
            s5_e1 <= s4_e1; s5_s <= s4_s;
            s5_e2 <= s4_e2; s5_dir <= s4_dir;
            s5_tmin <= s4_tmin; s5_tmax <= s4_tmax;
            pipe_tri_id[5] <= pipe_tri_id[4];
        end
    end

    // Stage 6 delay
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[6] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[6]  <= pipe_valid[5];
            s6_dot_sh      <= s5_dot_sh;
            s6_degenerate  <= s5_degenerate;
            s6_e1 <= s5_e1; s6_s <= s5_s;
            s6_e2 <= s5_e2; s6_dir <= s5_dir;
            s6_tmin <= s5_tmin; s6_tmax <= s5_tmax;
            pipe_tri_id[6] <= pipe_tri_id[5];
        end
    end

    // Stage 7 delay (reciprocal output becomes valid after this edge)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[7] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[7]  <= pipe_valid[6];
            s7_dot_sh      <= s6_dot_sh;
            s7_degenerate  <= s6_degenerate;
            s7_e1 <= s6_e1; s7_s <= s6_s;
            s7_e2 <= s6_e2; s7_dir <= s6_dir;
            s7_tmin <= s6_tmin; s7_tmax <= s6_tmax;
            pipe_tri_id[7] <= pipe_tri_id[6];
        end
    end

    // Stage 8: capture reciprocal result (now properly aligned)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[8] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[8]  <= pipe_valid[7];
            s8_inv_a       <= recip_out_val;
            s8_dot_sh      <= s7_dot_sh;
            s8_degenerate  <= s7_degenerate;
            s8_e1 <= s7_e1; s8_s <= s7_s;
            s8_e2 <= s7_e2; s8_dir <= s7_dir;
            s8_tmin <= s7_tmin; s8_tmax <= s7_tmax;
            pipe_tri_id[8] <= pipe_tri_id[7];
        end
    end

    // ========================================================================
    // Stage 9: Compute u = f * dot(s, h); early reject
    // ========================================================================
    logic signed [31:0] s9_u;
    logic               s9_u_reject;
    logic               s9_degenerate;
    logic signed [31:0] s9_inv_a;
    logic signed [31:0] s9_e1 [2:0], s9_s [2:0], s9_e2 [2:0], s9_dir [2:0];
    logic signed [31:0] s9_tmin, s9_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[9] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[9] <= pipe_valid[8];
            s9_u          <= fmul(s8_inv_a, s8_dot_sh);
            s9_degenerate <= s8_degenerate;
            s9_inv_a      <= s8_inv_a;
            s9_e1 <= s8_e1; s9_s <= s8_s;
            s9_e2 <= s8_e2; s9_dir <= s8_dir;
            s9_tmin <= s8_tmin; s9_tmax <= s8_tmax;
            pipe_tri_id[9] <= pipe_tri_id[8];
        end
    end
    assign s9_u_reject = (s9_u < FXP_ZERO) || (s9_u > FXP_ONE);

    // ========================================================================
    // Stage 10: Cross product q = cross(s, e1) — multiply phase
    // ========================================================================
    logic signed [31:0] s10_q_prod [5:0];
    logic signed [31:0] s10_u;
    logic               s10_reject;
    logic signed [31:0] s10_inv_a;
    logic signed [31:0] s10_e2 [2:0], s10_dir [2:0];
    logic signed [31:0] s10_tmin, s10_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[10] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[10] <= pipe_valid[9];
            // q = s × e1
            s10_q_prod[0] <= fmul(s9_s[1], s9_e1[2]); // s.y * e1.z
            s10_q_prod[1] <= fmul(s9_s[2], s9_e1[1]); // s.z * e1.y
            s10_q_prod[2] <= fmul(s9_s[2], s9_e1[0]); // s.z * e1.x
            s10_q_prod[3] <= fmul(s9_s[0], s9_e1[2]); // s.x * e1.z
            s10_q_prod[4] <= fmul(s9_s[0], s9_e1[1]); // s.x * e1.y
            s10_q_prod[5] <= fmul(s9_s[1], s9_e1[0]); // s.y * e1.x
            s10_u      <= s9_u;
            s10_reject <= s9_degenerate || s9_u_reject;
            s10_inv_a  <= s9_inv_a;
            s10_e2 <= s9_e2; s10_dir <= s9_dir;
            s10_tmin <= s9_tmin; s10_tmax <= s9_tmax;
            pipe_tri_id[10] <= pipe_tri_id[9];
        end
    end

    // ========================================================================
    // Stage 11: Complete q, start dot(dir, q) and dot(e2, q)
    // ========================================================================
    logic signed [31:0] s11_q [2:0];
    logic signed [31:0] s11_u;
    logic               s11_reject;
    logic signed [31:0] s11_inv_a;
    logic signed [31:0] s11_e2 [2:0], s11_dir [2:0];
    logic signed [31:0] s11_tmin, s11_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[11] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[11] <= pipe_valid[10];
            s11_q[0] <= s10_q_prod[0] - s10_q_prod[1];
            s11_q[1] <= s10_q_prod[2] - s10_q_prod[3];
            s11_q[2] <= s10_q_prod[4] - s10_q_prod[5];
            s11_u      <= s10_u;
            s11_reject <= s10_reject;
            s11_inv_a  <= s10_inv_a;
            s11_e2 <= s10_e2; s11_dir <= s10_dir;
            s11_tmin <= s10_tmin; s11_tmax <= s10_tmax;
            pipe_tri_id[11] <= pipe_tri_id[10];
        end
    end

    // ========================================================================
    // Stage 12: dot(dir, q) and dot(e2, q) — multiply phase
    // ========================================================================
    logic signed [31:0] s12_dot_dq [2:0]; // dir.x*q.x, dir.y*q.y, dir.z*q.z
    logic signed [31:0] s12_dot_eq [2:0]; // e2.x*q.x, e2.y*q.y, e2.z*q.z
    logic signed [31:0] s12_u;
    logic               s12_reject;
    logic signed [31:0] s12_inv_a;
    logic signed [31:0] s12_tmin, s12_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[12] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[12] <= pipe_valid[11];
            for (int i = 0; i < 3; i++) begin
                s12_dot_dq[i] <= fmul(s11_dir[i], s11_q[i]);
                s12_dot_eq[i] <= fmul(s11_e2[i], s11_q[i]);
            end
            s12_u      <= s11_u;
            s12_reject <= s11_reject;
            s12_inv_a  <= s11_inv_a;
            s12_tmin   <= s11_tmin; s12_tmax <= s11_tmax;
            pipe_tri_id[12] <= pipe_tri_id[11];
        end
    end

    // ========================================================================
    // Stage 13: Sum dot products (pre-adder stage for timing)
    //   Separates 3-input addition from multiply to break critical path.
    //   DSP48 pre-adder only handles 2 inputs on A port; the 3-input sum
    //   on B must be computed in a prior stage.
    // ========================================================================
    logic signed [31:0] s13_sum_dq; // dot(dir, q) = sum of products
    logic signed [31:0] s13_sum_eq; // dot(e2, q)  = sum of products
    logic signed [31:0] s13_u;
    logic               s13_reject;
    logic signed [31:0] s13_inv_a;
    logic signed [31:0] s13_tmin, s13_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[13] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[13] <= pipe_valid[12];
            s13_sum_dq <= s12_dot_dq[0] + s12_dot_dq[1] + s12_dot_dq[2];
            s13_sum_eq <= s12_dot_eq[0] + s12_dot_eq[1] + s12_dot_eq[2];
            s13_u      <= s12_u;
            s13_reject <= s12_reject;
            s13_inv_a  <= s12_inv_a;
            s13_tmin   <= s12_tmin; s13_tmax <= s12_tmax;
            pipe_tri_id[13] <= pipe_tri_id[12];
        end
    end

    // ========================================================================
    // Stage 14: Multiply inv_a × sums → raw_v, raw_t (DSP48-friendly)
    // ========================================================================
    logic signed [31:0] s14_raw_v; // f * dot(dir, q)
    logic signed [31:0] s14_raw_t; // f * dot(e2, q)
    logic signed [31:0] s14_u;
    logic               s14_reject;
    logic signed [31:0] s14_tmin, s14_tmax;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) pipe_valid[14] <= 1'b0;
        else if (!pipe_stall) begin
            pipe_valid[14] <= pipe_valid[13];
            s14_raw_v  <= fmul(s13_inv_a, s13_sum_dq);
            s14_raw_t  <= fmul(s13_inv_a, s13_sum_eq);
            s14_u      <= s13_u;
            s14_reject <= s13_reject;
            s14_tmin   <= s13_tmin; s14_tmax <= s13_tmax;
            pipe_tri_id[14] <= pipe_tri_id[13];
        end
    end

    // ========================================================================
    // Stage 15: Final hit test (combinational output, registered valid)
    // ========================================================================
    logic               s15_hit;
    logic signed [31:0] s15_t, s15_u, s15_v;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pipe_valid[15] <= 1'b0;
        end else if (!pipe_stall) begin
            pipe_valid[15] <= pipe_valid[14];
            s15_u <= s14_u;
            s15_v <= s14_raw_v;
            s15_t <= s14_raw_t;
            pipe_tri_id[15] <= pipe_tri_id[14];
        end
    end

    // Combinational hit test on registered stage 14 data (avoids BLKANDNBLK)
    assign s15_hit = !s14_reject
                  && (s14_raw_v >= FXP_ZERO)
                  && ((s14_u + s14_raw_v) <= FXP_ONE)
                  && (s14_raw_t > s14_tmin)
                  && (s14_raw_t < s14_tmax);

    // Output
    assign out_valid   = pipe_valid[15];
    assign hit         = s15_hit;
    assign t_out       = s15_t;
    assign u_out       = s15_u;
    assign v_out       = s15_v;
    assign tri_id_out  = pipe_tri_id[15];

endmodule : ray_intersect_tri
