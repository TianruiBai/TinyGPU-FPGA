`timescale 1ns/1ps
// ============================================================================
// Ray-Tracing Package: Types, constants, and helpers for the RT pipeline
// ============================================================================
package rt_pkg;

  // -----------------------------------------------------------------------
  // Fixed-point Q16.16 format for all RT spatial computations
  // -----------------------------------------------------------------------
  localparam int FXP_TOTAL = 32;
  localparam int FXP_FRAC  = 16;
  localparam int FXP_INT   = FXP_TOTAL - FXP_FRAC;

  localparam logic signed [31:0] FXP_ZERO    = 32'sh0000_0000;
  localparam logic signed [31:0] FXP_ONE     = 32'sh0001_0000;  // 1.0
  localparam logic signed [31:0] FXP_NEG_ONE = 32'sh_FFFF_0000; // -1.0
  localparam logic signed [31:0] FXP_EPSILON = 32'sh0000_0010;  // ~0.000244
  localparam logic signed [31:0] FXP_MAX     = 32'sh7FFF_FFFF;  // +32767.9999
  localparam logic signed [31:0] FXP_MIN     = 32'sh8000_0000;  // -32768.0

  // -----------------------------------------------------------------------
  // BVH memory layout (32 bytes per node, aligned)
  // -----------------------------------------------------------------------
  //   Word 0: {node_type[1:0], payload_ptr[29:0]}
  //           Internal: payload_ptr = left_child_word_addr
  //           Leaf:     payload_ptr = triangle_list_word_addr
  //   Word 1: {pad[1:0], secondary[29:0]}
  //           Internal: secondary = right_child_word_addr
  //           Leaf:     secondary = triangle_count
  //   Words 2-7: AABB {min_x, min_y, min_z, max_x, max_y, max_z} (Q16.16)
  //
  // Triangle in memory (48 bytes = 12 words, aligned):
  //   Words 0-2:  v0 (x, y, z)  Q16.16
  //   Words 3-5:  v1 (x, y, z)  Q16.16
  //   Words 6-8:  v2 (x, y, z)  Q16.16
  //   Word 9:     triangle_id
  //   Words 10-11: reserved

  localparam int BVH_NODE_WORDS = 8;
  localparam int BVH_NODE_BYTES = BVH_NODE_WORDS * 4;
  localparam int TRI_WORDS      = 12;
  localparam int TRI_BYTES      = TRI_WORDS * 4;

  typedef enum logic [1:0] {
    BVH_INTERNAL = 2'b00,
    BVH_LEAF     = 2'b01,
    BVH_EMPTY    = 2'b10,
    BVH_RESERVED = 2'b11
  } bvh_node_type_e;

  // -----------------------------------------------------------------------
  // RTU register map (word offsets from RTU base address)
  // -----------------------------------------------------------------------
  localparam int RTU_REG_CTRL        = 0;   // W: [0]=start  R: [0]=start,[1]=busy,[2]=done,[3]=irq_en
  localparam int RTU_REG_STATUS      = 1;   // R: [7:0]=fsm_state, [15:8]=stack_depth, [31:16]=rays_done
  localparam int RTU_REG_RAY_OX      = 2;   // W: ray origin X (Q16.16)
  localparam int RTU_REG_RAY_OY      = 3;   // W: ray origin Y
  localparam int RTU_REG_RAY_OZ      = 4;   // W: ray origin Z
  localparam int RTU_REG_RAY_DX      = 5;   // W: ray direction X (Q16.16, should be normalized)
  localparam int RTU_REG_RAY_DY      = 6;   // W: ray direction Y
  localparam int RTU_REG_RAY_DZ      = 7;   // W: ray direction Z
  localparam int RTU_REG_RAY_TMIN    = 8;   // W: ray t_min (Q16.16)
  localparam int RTU_REG_RAY_TMAX    = 9;   // W: ray t_max (Q16.16)
  localparam int RTU_REG_BVH_ROOT    = 10;  // W: BVH root node byte address in VRAM
  localparam int RTU_REG_HIT_FLAG    = 11;  // R: 1=hit, 0=miss
  localparam int RTU_REG_HIT_T       = 12;  // R: closest hit distance (Q16.16)
  localparam int RTU_REG_HIT_U       = 13;  // R: barycentric u (Q16.16)
  localparam int RTU_REG_HIT_V       = 14;  // R: barycentric v (Q16.16)
  localparam int RTU_REG_HIT_TRI_ID  = 15;  // R: triangle ID of closest hit
  localparam int RTU_REG_PERF_RAYS   = 16;  // R: total rays traced (counter)
  localparam int RTU_REG_PERF_NODES  = 17;  // R: BVH nodes visited (counter)
  localparam int RTU_REG_PERF_TESTS  = 18;  // R: triangle tests performed (counter)
  localparam int RTU_REG_INV_DX      = 19;  // W: precomputed 1/dir.x (Q16.16) — optional
  localparam int RTU_REG_INV_DY      = 20;  // W: precomputed 1/dir.y
  localparam int RTU_REG_INV_DZ      = 21;  // W: precomputed 1/dir.z
  localparam int RTU_NUM_REGS        = 32;
  localparam int RTU_REG_ADDR_BITS   = $clog2(RTU_NUM_REGS);

  // -----------------------------------------------------------------------
  // BVH traversal stack
  // -----------------------------------------------------------------------
  localparam int BVH_STACK_DEPTH = 24; // Max tree depth (covers ~16M nodes)

  // -----------------------------------------------------------------------
  // Intersection pipeline latencies
  // -----------------------------------------------------------------------
  localparam int AABB_PIPE_STAGES  = 6;
  localparam int TRI_PIPE_STAGES   = 16;  // 15 + 1 extra for pre-adder timing split

endpackage : rt_pkg
