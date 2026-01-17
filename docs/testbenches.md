# Testbenches

This repo’s SV testbenches live in `testbench/`.

## Compute Unit Regression: `compute_unit_full_tb`
Files:
- `testbench/compute_unit_full_tb.sv`
- `testbench/compute_unit_full_tb.do` (ModelSim/Questa batch script)

What it does:
- Builds a small ROM program in SV (no external hex needed).
- Provides a simple unified 32-bit global memory model (`mem[]`) and a small response FIFO.
- Uses a completion marker: the ROM writes `1` to `BASE_ADDR + DONE_OFF` and executes `MEMBAR`; the TB waits for this, then waits for the DUT to go quiescent.
  - Roadmap note: as graphics becomes asynchronous (VRAM ring + command streamer), `MEMBAR` is the architectural “full drain” point (it must wait for in-flight GPU/ROP/texture memory traffic as well). This keeps the TB completion pattern stable.
- Freezes the DUT (asserts reset) and performs self-checks:
  - Scalar integer ALU + branches + CSR
  - Scalar FP16 ALU + FP16<->int conversions
  - Vector integer ops + vector FP (FP32/FP16/FP8)
  - Vector/scalar loads, stores, and atomics
  - TEX path (descriptor fetch + sample)
  - Graphics smoke test: `RSTATE/GSTATE/GPARAM/GDRAW` and checks known framebuffer writes

Key constants (in `compute_unit_full_tb.sv`):
- `BASE_ADDR = 32'hFFFF_F800` (global-space select uses `addr[31]=1`)
- `MEM_WORDS = 16384` (64KB backing store)
- `DONE_OFF  = 32'h0000_01F0`

Expected result:
- Prints `Full testbench checks passed.` and exits via `$finish`.
- Produces `compute_unit_full_tb.vcd` (VCD dump) in the current directory.

### Run with Verilator
From repo root:

```bash
verilator --binary --sv --timing --trace --trace-structs \
  --top-module compute_unit_full_tb \
  -I"ip/compute unit" \
  ip/compute\ unit/isa_pkg.sv \
  ip/compute\ unit/agu.sv \
  ip/compute\ unit/alu_scalar.sv \
  ip/compute\ unit/alu_vector.sv \
  ip/compute\ unit/csr_file.sv \
  ip/compute\ unit/decoder.sv \
  ip/compute\ unit/fetch_unit.sv \
  ip/compute\ unit/fp_alu.sv \
  ip/compute\ unit/graphics_pipeline.sv \
  ip/compute\ unit/icache.sv \
  ip/compute\ unit/local_mem_banked.sv \
  ip/compute\ unit/lsu.sv \
  ip/compute\ unit/raster_unit.sv \
  ip/compute\ unit/regfile_fp.sv \
  ip/compute\ unit/regfile_scalar.sv \
  ip/compute\ unit/regfile_vector.sv \
  ip/compute\ unit/rop_unit.sv \
  ip/compute\ unit/scalar_wb_arb_pending2.sv \
  ip/compute\ unit/scoreboard.sv \
  ip/compute\ unit/texture_cache.sv \
  ip/compute\ unit/write_merge_buf.sv \
  ip/compute\ unit/compute_unit_top.sv \
  testbench/compute_unit_full_tb.sv

./obj_dir/Vcompute_unit_full_tb
```

### Run with ModelSim/Questa
From repo root:

```bash
vsim -c -do testbench/compute_unit_full_tb.do
```

The `.do` script writes a transcript file (see `testbench/` for the latest transcript name).

## Graphics-Directed Testbenches
These are smaller, graphics-focused tests:
- `testbench/gfx_triangle_tb.sv` + `testbench/gfx_triangle_tb.do`
- `testbench/gfx_gdraw_tb.sv` + `testbench/gfx_gdraw_tb.do`
- `testbench/gfx_textured_triangle_tb.sv` + `testbench/gfx_textured_triangle_tb.do`

## Graphics Console (PPM dump): `gfx_console_tb`
This is a higher-level directed graphics test that renders multiple frames and dumps them as images:
- `testbench/gfx_console_tb.sv`
- Output: `frame_0.ppm` … `frame_23.ppm` (written to repo root by default)

### Run with the provided script
From repo root:

```bash
./scripts/run_gfx_console_verilator.sh +color=0 +maxcyc=400000
```

Run them the same way as above by substituting the `.do` file (ModelSim) or the top module/file (Verilator).
