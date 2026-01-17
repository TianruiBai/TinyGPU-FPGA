#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# 1) Build CoreMark images (ROM + global mem init)
make -C coremark clean >/dev/null
rm -f coremark/tinygpu/*.o coremark/coremark.elf coremark/coremark_tinygpu.memh coremark/coremark_mem_init.memh coremark/coremark_text.bin coremark/coremark_data.bin || true
make -C coremark PORT_DIR=tinygpu ITERATIONS=1 \
  CC=riscv32-unknown-elf-gcc OBJCOPY=riscv32-unknown-elf-objcopy >/dev/null

rom="coremark/coremark_tinygpu.memh"
mem_init="coremark/coremark_mem_init.memh"

# 2) Build Verilator binary
rtl_dir="ip/compute unit"

# Collect RTL (exclude isa_pkg.sv, compiled separately to keep order deterministic)
rtl_files=()
for f in "$rtl_dir"/*.sv; do
  [[ "$f" == "$rtl_dir/isa_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  -sv \
  --cc --exe --build \
  --top-module coremark_tb \
  "$rtl_dir/isa_pkg.sv" \
  "${rtl_files[@]}" \
  testbench/coremark_tb.sv \
  testbench/verilator_coremark_main.cpp \
  -o Vcoremark_tb

# 3) Run
./obj_dir/Vcoremark_tb \
  +rom="$rom" \
  +mem_init="$mem_init" \
  +base=80000000 \
  +max_cycles=50000000
