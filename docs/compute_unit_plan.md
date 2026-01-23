# Compute Unit Bring-up Plan (With Current RTL Map)

This plan started as a staged bring-up guide. It now also includes an **as-built module map** to match the current RTL under `ip/compute unit/`.

ISA assumptions (implemented): 32 scalar regs (`x0..x31`), 32 FP regs (`f0..f31` FP16), 32 vector regs (`v0..v31` 128-bit, supports FP32/FP16/FP8(E4M3) and packed int modes), scoreboard-managed hazards, vector predication via `vm` + CSR `vmask`.

## Current RTL Module Hierarchy (as-built)
```
compute_unit_top.sv
├── fetch_unit.sv
│   └── icache.sv
├── decoder.sv
├── csr_file.sv
├── scoreboard.sv
├── regfile_scalar.sv
├── regfile_fp.sv
├── regfile_vector.sv
├── alu_scalar.sv
├── fp_alu.sv
├── alu_vector.sv
├── lsu.sv
│   ├── local_mem_banked.sv
│   └── write_merge_buf.sv
├── graphics_pipeline.sv
│   ├── texture_cache.sv
│   ├── raster_unit.sv
│   └── rop_unit.sv
└── scalar_wb_arb_pending2.sv
```

## Graphics Pipeline Overhaul (Roadmap)
This section captures the planned transition from a “blocking co-processor” style graphics path to a **high-throughput, autonomous GPU pipeline** optimized for FPGA timing.

Key goals:
- Scalar core issues `RSTATE/RSETUP/GDRAW/...` as **enqueue-only** submits into a VRAM ring buffer.
- A **Command Streamer** DMA engine fetches commands + descriptors and feeds the GPU stages.
- Graphics becomes a pipelined subsystem (Setup → Raster → Interpolate/TEX → ROP) with FIFO decoupling.
- Memory traffic is QoS-managed via a **triple-port MIU** so ROP writes never starve scalar loads/stores.

### Planned module hierarchy (target)
Names are suggestions; exact split can evolve as RTL stabilizes.

```
compute_unit_top.sv
├── scalar core (existing)
├── miu_arbiter.sv              (new: triple-port memory interface)
└── graphics_pipeline_top.sv    (new: pipelined GPU wrapper)
    ├── gfx_cmd_streamer.sv     (VRAM ring consumer + prefetch/shadow state)
    ├── gfx_setup.sv            (edges + bbox + inv_area)
    ├── raster_unit.sv          (refactor: true 2x2 quad stepping)
    ├── gfx_interpolator.sv     (normalize barycentrics; UV/attrib interp)
    ├── gfx_texture_unit.sv     (refactor: latency hiding)
    │   └── tex_cache_l1.sv     (tiled cache layout)
    └── gfx_rop_unit.sv         (tile buffer + burst commit)
        └── tile_buffer_ram.sv
```

## Roadmap status (as-built vs planned)
- **miu_arbiter.sv** — implemented (triple-port arbitration behavior present in RTL).
- **gfx_cmd_streamer.sv** — implemented (command ring / fence semantics present).
- **graphics_pipeline.sv** — implemented (pipeline stages: setup, raster, interpolator, ROP exist), but further refactors toward a `graphics_pipeline_top.sv` split are *planned*.
- **texture_cache.sv / rop_unit.sv** — implemented; further microarchitectural improvements (tiling, latency hiding) remain planned.

## Open issues / action items
- [ ] Decide whether VRF should be synchronous (1-cycle read) or leave combinational; record decision and adapt scoreboard/tests.
- [ ] Add RTL-level regression test that exercises multi-bit `quad_mask` patterns and checks ROP handling.
- [ ] Document known unused CSRs / top-level signals (e.g., command streamer CSRs that are present but not externally useful yet).
- [ ] Add a short “synthesis notes” subsection to describe arrays that are combinational vs synchronous and BRAM inference implications.

## Step 1: Minimal Scalar Core
Goal: Execute `ADD x1, x2, x3` and `BEQ` loops.
- Write `isa_pkg.sv` with opcode enums and a decode struct (opcode/funct3/funct7 -> control).
- Implement `regfile_scalar.sv` (32x32, 2R/1W, x0 hardwired zero).
- Implement `alu_scalar.sv` with ADD/SUB/AND/OR plus PC/branch compare.
- Hook fetch->decode->regread->execute->wb in `compute_unit_top.sv` without vector paths.
- Testbench: load a small hex into I-ROM, check PC increments and register writes via assertions.

## Step 2: Scoreboard (hazard cop)
Goal: Stall only when sources/dests are busy; allow latency hiding (vmask-aware).
- Busy masks: one per file (`busy_s[31:0]`, `busy_f[31:0]`, `busy_v[31:0]`). `vmask` is architected, lives in CSR, not scoreboarding.
- On issue: stall if rs1/rs2 busy or rd busy; set rd busy when accepted.
- On writeback: clear busy bit for the completing rd.
- Integrate with scalar pipeline first; add per-class issue_valid and wb_valid.
- Unit test: drive sequences that check RAW/WAW stalls clear on wb.

## Step 3: Vector Unit (data path)
Goal: Execute `VADD v1, v2, v3` with BRAM-backed VRF and respect `vm`/`vmask` predication.
- `regfile_vector.sv`: As-built uses combinational reads (`regfile_vector.sv` presents combinational mem reads), providing data in the same cycle. Synthesis may infer BRAM and therefore introduce target-dependent read latency; the RTL includes forwarding and scoreboard mechanisms to handle these timing variations. If an architectural 1-cycle VRF read latency is desired, consider changing the VRF to synchronous reads and updating scoreboard/issue timing accordingly. `vmask` sits in `csr_file.sv`; V ops gate per-lane behavior when `vm` is enabled.
- `alu_vector.sv`: 4 parallel 32-bit adders (or FP32 adders) for basic VADD; later extend to VCMP/VSEL/VDOT/VCROSS, VPACK/VUNPACK and sat mode.
- Pipeline: Fetch -> Decode -> RegRead (combinational in current RTL; forwarding/scoreboard handle timing variations) -> Execute -> WB; scoreboard covers regread hazards and forwarding.
- Testbench: feed VLD-like preloads or direct writes, then VADD, check result matches lane-wise add.

## Step 4: LSU + Local Memory
Goal: Handle `VLD`/`VST` and shared-memory access.
- Add a 4KB shared BRAM outside the core; address map separates shared vs. global (OSPI) ranges.
- `lsu.sv`: accept requests; shared hits read BRAM in 1–2 cycles; global issues to external bus with long latency.
- Scoreboard: mark rd busy on issue; clear on wb_valid from LSU.
- Testbench: model long-latency global loads (e.g., 100-cycle return) and verify independent scalar ops continue; dependent ops stall until wb. Cover alignment penalties and gather serialization.

## Step 5: Texture Sampler Stub
Goal: Functional `TEX` without full filtering.
- Add coord-to-address helper that maps `{u,v}` to texel address.
- Stub sampler returns a constant color (e.g., 0xFFFF00FF) while plumbing TEX decode/issue/wb.
- Later: connect to texture cache/LSU for real fetch and add wrap/clamp, **nearest + bilinear**, optional sRGB/UNORM decode, and sampler descriptor table.

## Step 6: Hardware Rasterizer & Triangle Setup
Goal: Accelerate triangle edge evaluation and pixel coverage generation.
-   `raster_unit.sv`: Implement edge-function based rasterizer.
    -   Current RTL emits 2×2 quads (scanline stepping by +2) and produces a 4-bit `quad_mask` that indicates coverage for the 2×2 pixels; the mask can contain any combination of bits (not fixed to `4'b0001`).
    -   The rasterizer computes per-quad edge values and samples at pixel center using a 2× subpixel grid to form coverage and barycentric values.
    -   Coverage uses 64-bit edge functions to avoid overflow.
    -   Winding is normalized (`tri_area` exported as non-negative) so triangles render regardless of CW/CCW order; degenerate (zero-area) triangles are rejected.
    -   Barycentric outputs (`quad_bary_w0/w1/w2`) are unnormalized edge-function values in the same scale as `tri_area_out` for downstream interpolation; the implementation selects a representative pixel (first set bit in the mask) to provide barycentrics for the quad payload.

-   Raster→ROP handshake (normative for RTL):
    - `quad_valid` indicates the fragment payload is valid.
    - The rasterizer must hold `quad_*` stable while `quad_valid=1` and `quad_ready=0`.
    - The rasterizer advances to the next pixel only when `(quad_ready || !quad_valid)`.
-   Triangle Setup:
    -   Logic to calculate edge coefficients ($A, B, C$) from vertex positions (X, Y).
    -   May be part of `raster_unit` or a separate pre-processor `setup_unit`.
-   Integration:
    -   Interface for core to push triangles (Setup) and pull fragments (Raster).
    -   Future: increase throughput and per-quad barycentric fidelity (e.g., provide per-pixel barycentrics for all enabled mask bits, improve quad packing) once `rop_unit.sv` is refactored to consume whole-quads efficiently.
-   Testbench:
    -   Feed vertices, verify coverage masks and barycentric values against a reference model.

## Verification Strategy
- Unit tests: ALU (int and vector), scoreboard, VRF BRAM timing, LSU state machine.
- Integration micro-kernels (assembly): latency-hiding loop with VLD busying `v1`, independent scalar adds progress, `VADD v2,v1,v1` stalls until wb.
- Waveform/trace: observe stall signals and busy masks; ensure only dependent ops stall.
- Use Verilator or Vivado sim before FPGA bring-up.

## Checklist
- [ ] `isa_pkg.sv` opcode/structs defined.
- [ ] Scalar core (fetch/decode/regread/execute/wb) runs ADD/BEQ loop.
- [ ] Scoreboard enforces RAW/WAW per s/f/v file.
- [ ] VRF behaviour documented (combinational reads in RTL; synthesis may infer BRAM and change read latency).
- [ ] Vector ALU does 4-lane VADD.
- [ ] LSU handles shared/global, with wb + scoreboard clear.
- [ ] TEX stub returns constant color and exercises TEX pipeline.
- [ ] Raster Unit implements edge-test and fragment generation (2×2 quad stepping; quad_mask capable).
