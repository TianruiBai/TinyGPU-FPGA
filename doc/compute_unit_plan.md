# Compute Unit Bring-up Plan (With Current RTL Map)

This plan started as a staged bring-up guide. It now also includes an **as-built module map** to match the current RTL under `ip/compute unit/`.

ISA assumptions (implemented): 32 scalar regs (`x0..x31`), 32 FP regs (`f0..f31` FP16), 32 vector regs (`v0..v31` 128-bit, supports FP32/FP16/FP8(E4M3) and packed int modes), scoreboard-managed hazards, vector predication via `vm` + CSR `vmask`.

## Current RTL Module Hierarchy (as-built)
```
compute_unit_top.sv
├── fetch_unit.sv
│   └── icache.sv (l1_inst_cache.sv integration via inst_miss_*)
├── decoder.sv
├── csr_file.sv
├── scoreboard.sv (now accepts dual FP/vec writeback ports)
├── regfile_scalar.sv
├── regfile_fp.sv (dual FP write ports driven by two fp_alu instances)
├── regfile_vector.sv
├── alu_scalar.sv
├── fp_alu.sv (two instances: u_fp_alu0, u_fp_alu1)
├── alu_vector.sv (two instances: u_alu_vector0, u_alu_vector1)
├── lsu.sv
│   ├── local_mem_banked.sv
│   └── write_merge_buf.sv
├── l1_inst_cache.sv
├── l1_data_cache.sv (connected to top-level dcache_mem_* interface)
├── graphics_pipeline.sv
│   ├── texture_cache.sv
│   ├── raster_unit.sv
│   └── rop_unit.sv
└── scalar_wb_arb_pending2.sv

Note: The top-level now exposes explicit instruction miss and D-cache memory interfaces (`inst_miss_*`, `dcache_mem_*`) which are modelled in testbenches instead of the legacy `data_req_*` signals.
```

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
Goal: Execute `VADD v1, v2, v3` with BRAM-backed VRF and respect `vm`/`vmask` predication. The design now supports dual-issue VALU (two concurrent vector ALUs) to increase throughput.
- `regfile_vector.sv`: As-built uses combinational reads, providing data in the same cycle. Forwarding and scoreboard mechanisms handle timing variations from BRAM inference. If a synchronous VRF is used in the future, update scoreboard/issue timing as needed.
- `alu_vector.sv`: Two VALU instances (u_alu_vector0/u_alu_vector1) are instantiated and can issue two vector ops per cycle (subject to VQ space and resource availability).
- Vector Issue Queue (VQ): a small 2-entry decoupling queue (`VQ_DEPTH=2`) supports up to two enqueues per cycle and two issues. VALU results that cannot commit immediately (LSU/gp ordering or busy writeback ports) are buffered in the Vector Writeback Buffer Queue (VWBQ) implemented in `compute_unit_top.sv` (configurable depth, default 32 entries).
- Vector writeback arbitration: two vector writeback ports (`v_we0/v_we1`) arbitrate results across LSU, pending FIFO, graphics writebacks, and available VALU outputs; deterministic priorities are designed to avoid live-lock and ensure correctness.
- Legacy debug signals: top-level exposes backward-compatible signals (e.g., `valuv_wb_valid`, `valuv_wb_rd`, `fp_scalar_wb_*`) for testbench visibility; these select from active ALU outputs when both are present.
- Pipeline: Fetch -> Decode -> RegRead -> (VQ enqueue) -> VALU issue -> VWBQ/LSU/GFX commit; scoreboard handles hazardous interactions and same-cycle forwarding from VALU/LSU/GP.
- Testbench: feed VLD-like preloads or direct writes, then dual VADD sequences, check both single and dual-issue results and vwbq arbitration.

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
    -   Future: optionally restore true 2x2 quad stepping for throughput (emit 4 mask bits/pixel barycentrics) once ROP is ready to consume it efficiently.
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
- [ ] VRF infers BRAM; 1-cycle read latency handled.
- [ ] Vector ALU does 4-lane VADD.
- [ ] LSU handles shared/global, with wb + scoreboard clear.
- [ ] TEX stub returns constant color and exercises TEX pipeline.
- [ ] Raster Unit implements edge-test and fragment generation (currently per-pixel; quad-mask capable).
