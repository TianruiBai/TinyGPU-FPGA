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

**Fetch alignment + bundling (as-built)**:
- `fetch_unit.sv` fetches a 64-bit aligned bundle (8B aligned). `pc[2]` selects low/high 32-bit word.
- `inst1_valid` is only asserted when `pc[2]==0` (i.e., the bundle contains two sequential aligned 32-bit instructions).

**Control-flow + prediction (as-built)**:
- Dynamic branch predictor: `branch_predictor_bht.sv` (64-entry, 2-bit saturating counters).
- Predictor is queried on the IF slot0 instruction; **the predicted redirect is only applied when slot0 is accepted** (prevents redirecting on a stalled instruction).
- Redirects are generated in EX by `branch_unit.sv`; the pipe flushes only on **mispredict** (correct predictions do not flush).

**Limited dual-issue**:
- Slot0 (“lane0”) can issue any instruction class.
- Slot1 (“lane1”) is restricted to **vector-ALU** or **graphics/TEX** classes.
- Both lanes are gated by `scoreboard.sv` hazards and by structural availability (vector queue space, graphics queue space, LSU availability).

**Dual-issue restrictions (as-built)**:
- Slot0 cannot dual-issue a vector store/atomic in parallel with slot1 vec-ALU/gfx, because there is only one set of vector regfile read ports.
- Graphics/TEX work is **not flushed** on scalar control-flow redirects (architecturally “fire-and-forget” once issued).

## 3. Register Files + Scoreboard
Implemented storage:
- `regfile_scalar.sv`: 32×32-bit
- `regfile_fp.sv`: 32×16-bit (FP16)
- `regfile_vector.sv`: 32×128-bit

`scoreboard.sv` tracks busy state for scalar/FP/vector register classes and stalls issue on RAW/WAW hazards until writeback clears the destination.

**Bypass + interlocks (as-built)**:
- **Same-cycle writeback relaxation** in the scoreboard: a busy bit does not stall if the matching WB is occurring in the same cycle.
- RR-stage **scalar/FP/vector** read ports include same-cycle WB bypass.
- **RR vector forwarding** can source values from LSU WB, graphics WB, VALU WB, or the vector pending queue head to reduce stalls.
- **RR gfx/tex scalar forwarding** can source descriptor pointer operands from LSU/MEM/WB (scalar pipe) to avoid gfx queue bubbles.
- **EX-stage scalar forwarding** from LSU/MEM/WB feeds ALU/branch/AGU/CSR/FP scalar inputs.
- **Scalar load-use interlock** stalls when an EX/MEM scalar load result is not yet available (uses registered EX/MEM state to avoid long comb paths).

## 4. Execution Units
- **Scalar integer**: `alu_scalar.sv`
- **FP16 scalar**: `fp_alu.sv` (also produces scalar conversions via `wb_scalar_*`)
- **Vector**: `alu_vector.sv` (128-bit; INT32/INT16/INT8 and FP32/FP16/FP8(E4M3) lanes)

Vector ops are fed from a small decoupling queue in `compute_unit_top.sv` (`VQ_DEPTH=2`) so lane1 issue can enqueue vector work without directly consuming the scalar pipeline.

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

### 6.0 Roadmap: decoupled “command streamer” GPU (planned)
The current RTL treats graphics as a decoupled unit with an internal queue, but it is still ultimately fed by scalar-issued macro-ops and shares the LSU/global memory pipe.

The next architecture step is to remove CPU-side blocking/serialization and improve timing/bandwidth by moving to an **autonomous graphics front-end** plus **pipelined stages**:

- **Command Streamer (front-end DMA):** scalar `RSTATE/RSETUP/GDRAW/...` become *enqueue-only* writes into a VRAM ring. The streamer reads the ring and feeds an internal instruction queue.
- **Shadow State Prefetch:** on `RSTATE/GSTATE/GPARAM` the streamer prefetches descriptors into “shadow” registers to hide VRAM latency before draw execution.
- **2x2 quad rasterization:** raster advances `x+=2,y+=2`, generates a 4-bit `quad_mask`, and pushes whole quads downstream for derivative-friendly timing.
- **Tile buffer ROP:** blend/depth/write happens against on-chip BRAM (tile buffer) with burst flushes to VRAM.

**Memory architecture change (planned):** split the monolithic LSU client role into three MIU ports:
1) **Scalar/Vector Port** (high priority, low latency)
2) **ROP Port** (critical priority, high bandwidth)
3) **Texture/Command Port** (low priority, latency-tolerant; uses caches/FIFOs)

This change is primarily about QoS and timing closure: it prevents ROP writes or texture bursts from starving scalar load/store forward progress.

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

## 7. Diagrams
- Compute-unit microarchitecture: `docs/diagrams/compute_unit_microarch.puml`
- Raster → ROP handshake (sequence): `docs/diagrams/raster_rop_readyvalid.puml`
- TEX miss/refill (sequence): `docs/diagrams/tex_cache_miss_refill.puml`

## 8. Writeback Arbitration
Multiple units can produce scalar writeback in the same cycle. `scalar_wb_arb_pending2.sv` implements deterministic arbitration plus a 2-deep pending queue for scalar writebacks that lose arbitration.
