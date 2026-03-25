`timescale 1ns/1ps
// ============================================================================
// Matrix Unit Package: Types, constants, and register map
// ============================================================================
package mat_pkg;

  // -----------------------------------------------------------------------
  // Tile dimensions (configurable, keep small for FPGA)
  // -----------------------------------------------------------------------
  localparam int TILE_DIM = 4;   // 4×4 tiles
  localparam int TILE_ELEMS = TILE_DIM * TILE_DIM; // 16 elements

  // -----------------------------------------------------------------------
  // Data format: Q16.16 fixed-point (matches RT pipeline)
  // -----------------------------------------------------------------------
  localparam int FXP_TOTAL = 32;
  localparam int FXP_FRAC  = 16;

  // -----------------------------------------------------------------------
  // Systolic array pipeline latency
  // -----------------------------------------------------------------------
  // For a TILE_DIM×TILE_DIM systolic array with data skew:
  //   Latency = 2*TILE_DIM - 1 + pipeline registers
  localparam int SA_LATENCY = 2 * TILE_DIM + 1; // 9 cycles

  // -----------------------------------------------------------------------
  // Matrix Unit register map (word offsets from MXU base address)
  // -----------------------------------------------------------------------
  localparam int MXU_REG_CTRL        = 0;   // W: [0]=start [1]=load_a [2]=load_b [3]=store_c [4]=clear_acc
                                             // R: [0]=start [1]=busy [2]=done [3]=irq_en
  localparam int MXU_REG_STATUS      = 1;   // R: [7:0]=fsm_state, [15:8]=reserved, [31:16]=ops_done
  localparam int MXU_REG_SRC_A_ADDR  = 2;   // W: byte address of tile A in VRAM
  localparam int MXU_REG_SRC_B_ADDR  = 3;   // W: byte address of tile B in VRAM
  localparam int MXU_REG_DST_C_ADDR  = 4;   // W: byte address for result tile C writeback
  localparam int MXU_REG_TILE_M      = 5;   // W: M dimension (rows of A/C), max TILE_DIM
  localparam int MXU_REG_TILE_N      = 6;   // W: N dimension (cols of B/C), max TILE_DIM
  localparam int MXU_REG_TILE_K      = 7;   // W: K dimension (cols of A / rows of B), max TILE_DIM
  localparam int MXU_REG_STRIDE_A    = 8;   // W: row stride of A in elements (for non-contiguous)
  localparam int MXU_REG_STRIDE_B    = 9;   // W: row stride of B in elements
  localparam int MXU_REG_STRIDE_C    = 10;  // W: row stride of C in elements
  localparam int MXU_REG_PERF_OPS    = 11;  // R: total MACs performed (counter)
  localparam int MXU_REG_PERF_TILES  = 12;  // R: total tiles computed (counter)
  localparam int MXU_REG_ACC_DATA    = 13;  // R/W: direct access to accumulator (for debug)
  localparam int MXU_REG_ACC_IDX     = 14;  // W: accumulator element index for direct access
  localparam int MXU_NUM_REGS        = 32;
  localparam int MXU_REG_ADDR_BITS   = $clog2(MXU_NUM_REGS);

  // -----------------------------------------------------------------------
  // Memory layout for tiles (contiguous, row-major)
  //   Tile A: TILE_DIM × TILE_DIM × 4 bytes
  //   Total tile size = 4 × 4 × 4 = 64 bytes (one cache line!)
  // -----------------------------------------------------------------------
  localparam int TILE_BYTES = TILE_ELEMS * 4;
  localparam int TILE_WORDS = TILE_ELEMS;

endpackage : mat_pkg
