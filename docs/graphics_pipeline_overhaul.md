# Graphics Pipeline Overhaul (Roadmap)

This document describes the planned transition of the graphics subsystem from a scalar-fed “blocking co-processor” to an **autonomous, pipelined GPU** optimized for FPGA timing and memory QoS.

Goals:
- Preserve **100% ISA binary compatibility** (same opcodes/descriptors).
- Remove CPU-side stalls by making graphics instructions **enqueue-only**.
- Increase throughput with pipeline parallelism + FIFO decoupling.
- Prevent VRAM starvation by splitting memory clients and enforcing QoS.

---

## 1. Old vs new execution model

Old (blocking-ish):
- CPU executes `GDRAW` (and related ops)
- CPU waits/serializes behind fixed-function progress

New (async submit):
- CPU executes `RSTATE/RSETUP/GDRAW/...` and enqueues **32-bit command packets** into a **VRAM ring buffer**
- CPU resumes immediately (except ring-full backpressure)
- A **Command Streamer** consumes the ring and feeds the GPU stages

This is architecturally transparent to software except for improved overlap; ordering is handled via `MEMBAR` (see below).

---

## 2. Triple-port memory interface (MIU)

To avoid timing issues and starvation, replace “everything shares the LSU” with a dedicated **Memory Interface Unit (MIU)** that arbitrates three independent clients:

1) Scalar/Vector port (high priority, low latency)
- CPU loads/stores, stack/spills, atomics

2) ROP port (critical priority, bandwidth)
- Framebuffer/depth writes
- Should not be forced to wait behind texture bursts

3) Texture/Command port (low priority, latency-tolerant)
- Texture fetches and command/descriptors
- Uses caches/FIFOs and tolerates stalls locally

The MIU provides isolation and predictable QoS; it is also a clean boundary for later wider-bus upgrades.

---

## 3. Pipeline stages (planned)

### Stage 0: Command Streamer (front-end)
Module: `gfx_cmd_streamer.sv`
- Consumes VRAM ring packets.
- Prefetches descriptors on state changes into “shadow state”.
- Pushes decoded work items into the setup/raster front of the pipe.

### Stage 1: Setup
Module: `gfx_setup.sv`
- Computes edge coefficients, bounding box.
- Computes `inv_area = 1/tri_area` (LUT-based reciprocal suggested).
- Emits a primitive packet into a raster FIFO.

### Stage 2: Raster (2x2 quads)
Refactor: `raster_unit.sv` (or split into `gfx_raster_quad.sv`)
- Iterates `x += 2`, `y += 2`.
- Evaluates coverage for TL/TR/BL/BR and generates `quad_mask[3:0]`.
- Emits quad payload into an interpolator FIFO.

### Stage 3: Interpolator + Texture
Modules: `gfx_interpolator.sv`, `gfx_texture_unit.sv`
- Normalizes barycentrics using `inv_area`.
- Interpolates UV/attrs for all 4 pixels.
- Texture unit stalls locally on miss; upstream can continue until FIFOs fill.

Cache: `tex_cache_l1.sv` (tiled layout)
- Cache line stores a small 2D block (e.g., 4x4 texels) to match quad locality.

### Stage 4: ROP + Commit
Modules: `gfx_rop_unit.sv`, `tile_buffer_ram.sv`
- Performs depth/blend against on-chip tile buffer.
- Flushes dirty lines in bursts via MIU.

---

## 4. ISA compatibility mapping

The instruction stream is unchanged; the microarchitectural meaning becomes enqueue-only:

- `RSTATE` / `GSTATE` / `GPARAM`: enqueue state-change packets; streamer prefetches.
- `RSETUP`: enqueue “setup” packet (vertex block pointer).
- `GDRAW` / `RDRAW` / `RRECT`: enqueue draw packets.

### Ordering (`MEMBAR`)
`MEMBAR` becomes the architectural “full drain” point:
- Must wait until the GPU ring consumer catches the producer.
- Must wait until in-flight ROP/texture memory traffic completes.

This preserves existing expectations for host/display visibility and makes testbench completion robust.

---

## 5. Suggested migration steps

1) Define the VRAM ring packet format + producer/consumer pointers.
2) Implement tile buffer ROP path (immediate bandwidth win).
3) Convert raster to true 2x2 quads.
4) Add command streamer + shadow state prefetch.
5) Introduce MIU triple-port arbitration and progressively move clients.
