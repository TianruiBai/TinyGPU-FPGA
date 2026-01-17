#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# 1) Build Dhrystone images (ROM + global mem init)
make -C benchmark-dhrystone/tinygpu clean >/dev/null
make -C benchmark-dhrystone/tinygpu \
  CC=riscv32-unknown-elf-gcc OBJCOPY=riscv32-unknown-elf-objcopy \
  DHRY_ITERS=${DHRY_ITERS:-200} >/dev/null

rom="benchmark-dhrystone/dhrystone_tinygpu.memh"
mem_init="benchmark-dhrystone/dhrystone_mem_init.memh"

# TB knobs (overridable via environment)
base_addr=${BASE_ADDR:-80000000}
max_cycles=${MAX_CYCLES:-50000000}
tb_debug=${TB_DEBUG:-0}
tb_check_bounds=${TB_CHECK_BOUNDS:-0}
done_off=${DONE_OFF:-000001f0}
result_off=${RESULT_OFF:-00000200}

# Optional extra plusargs for the simulator binary (e.g., waves)
tb_extra_args=${TB_EXTRA_ARGS:-}

# 2) Build Verilator binary
rtl_dir="ip/compute unit"

rtl_files=()
for f in "$rtl_dir"/*.sv; do
  [[ "$f" == "$rtl_dir/isa_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  -sv \
  --trace --trace-structs \
  --cc --exe --build \
  --top-module dhrystone_tb \
  "$rtl_dir/isa_pkg.sv" \
  "${rtl_files[@]}" \
  testbench/dhrystone_tb.sv \
  testbench/verilator_dhrystone_main.cpp \
  -o Vdhrystone_tb

# 3) Run
./obj_dir/Vdhrystone_tb \
  +rom="$rom" \
  +mem_init="$mem_init" \
  +base="$base_addr" \
  +done_off="$done_off" \
  +result_off="$result_off" \
  +max_cycles="$max_cycles" \
  +debug="$tb_debug" \
  +check_bounds="$tb_check_bounds" \
  $tb_extra_args
