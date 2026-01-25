# Compute Unit Microarchitecture (As-Built RTL)

This document describes the RTL currently implemented under `ip/compute unit/`.

## 1. Top-Level
`compute_unit_top.sv` integrates scalar/FP/vector execution, a unified LSU, and a decoupled graphics pipeline.

**External interfaces** (top ports):
- Instruction fetch: `inst_addr` + `inst_rdata` (64-bit, two 32-bit instructions per fetch)
- Unified global memory: `data_req_*` / `data_resp_*` (32-bit)
- Optional CSR visibility: `csr_status/csr_fstatus/csr_vstatus`

## 2. Front-End + Issue
The front-end fetches two 32-bit instructions per 64-bit beat and decodes both (`decoder.sv`).

**Limited dual-issue**:
- Slot0 (“lane0”) can issue any instruction class.
- Slot1 (“lane1”) is restricted to **vector-ALU** or **graphics/TEX** classes.
- Both lanes are gated by `scoreboard.sv` hazards and by structural availability (vector queue space, graphics queue space, LSU availability).

## 3. Register Files + Scoreboard
Implemented storage:
- `regfile_scalar.sv`: 32×32-bit
- `regfile_fp.sv`: 32×16-bit (FP16)
- `regfile_vector.sv`: 32×128-bit

`scoreboard.sv` tracks busy state for scalar/FP/vector register classes and stalls issue on RAW/WAW hazards until writeback clears the destination.

## 4. Execution Units
- **Scalar integer**: `alu_scalar.sv`
- **FP16 scalar**: `fp_alu.sv` — the RTL now instantiates two FP ALU instances to support dual-issue FP operations and scalar conversions.
- **Vector**: `alu_vector.sv` — two VALU instances are instantiated (u_alu_vector0/u_alu_vector1) to support dual VALU issue and improve vector throughput (subject to VQ/VWBQ capacity).

Dual FP/Vector specifics:
- Dual FP ALUs: two independent FP pipelines produce either FP register writes or scalar conversions. Scalar FP conversions arbitration is deterministic and backpressured by the scalar WB arb (`scalar_wb_arb_pending2.sv`).
- Dual VALUs: VALU dual-issue issues from a 2-entry Vector Issue Queue (`VQ_DEPTH=2`). Each VALU has independent ready/valid handshakes and produces WB results (`valuv0_wb_*`, `valuv1_wb_*`).
- Vector Writeback Buffer Queue (VWBQ): non-committed VALU vector results, texture/gfx writebacks, and LSU vector results can be buffered in a VWBQ FIFO (default depth = 32). Two vector writeback ports (`v_we0` and `v_we1`) arbitrate LSU > pending FIFO > graphics > VALU in a deterministic order to commit vector register updates.
- Legacy testbench signals: for backwards compatibility the top-level exposes single-port legacy probes (e.g., `valuv_wb_valid`, `valuv_wb_rd`, `fp_scalar_wb_*`) which select and expose the active ALU that would be used by older testbenches.

## 5. LSU + Memory System
`lsu.sv` handles:
- Scalar loads/stores on the unified 32-bit global interface
- Vector loads/stores (architecturally 128-bit; adapted/serialized onto the 32-bit global interface)
- Atomics (scalar + vector)
- Local (on-chip) memory accesses via `local_mem_banked.sv`

For global scalar stores (and for graphics/ROP stores), the LSU uses `write_merge_buf.sv` (8 entries) to combine byte-masked 32-bit writes and reduce external transaction overhead.

## 6. Graphics + Texture
Graphics/TEX macro-ops are handled by `graphics_pipeline.sv` with an internal queue (`GQ_DEPTH=8`). It supports:
- `TEX` descriptor fetch + sampling (nearest/bilinear, optional sRGB + UNORM expand)
- Graphics state macro-ops (`RSTATE`, `GSTATE`, `GPARAM`, `GDRAW`) used by the raster path

`texture_cache.sv` provides a simple line-based cache for TEX reads and refills through the LSU.

Raster/ROP blocks are in `raster_unit.sv` and `rop_unit.sv`, and their stores are sent into the LSU via the `gfx_st_*` interface.

### 6.1 Raster → ROP fragment interface (as-built)
`raster_unit.sv` produces fragments into `rop_unit.sv` via a small **ready/valid** interface:
- Payload: `quad_x/quad_y`, `quad_mask`, `quad_bary_w0/w1/w2`, `tri_area_out`.
- The interface is shaped like a 2x2 “quad”, but the current raster path emits **one pixel at a time** with `quad_mask=4'b0001`.

**Handshake rules (normative):**
- When `quad_valid=1` and `quad_ready=0`, the rasterizer must hold the payload stable (no pixel loss).
- The rasterizer advances to the next pixel only when `(quad_ready || !quad_valid)`.

**Math conventions (as-built):**
- Coverage and barycentric edge functions are evaluated in **64-bit** to avoid overflow.
- Sampling is at pixel center using a **2x subpixel grid**.
- Triangle winding is normalized so `tri_area_out` is non-negative; CW triangles are not discarded (only zero-area triangles are rejected).
- `quad_bary_w*` are **unnormalized** edge-function values; `rop_unit.sv` normalizes by dividing accumulated attributes by `tri_area`.

### 6.2 ROP emission model (as-built)
`rop_unit.sv` consumes the quad payload but emits stores only for enabled pixels in `quad_mask`.
- This avoids wasting cycles when the rasterizer provides a single-pixel mask.

## 7. Writeback Arbitration
Multiple units can produce scalar writeback in the same cycle. `scalar_wb_arb_pending2.sv` implements deterministic arbitration plus a 2-deep pending queue for scalar writebacks that lose arbitration.
