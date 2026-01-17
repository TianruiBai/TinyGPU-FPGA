#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

asm_src="test/full_testbench.S"
out_base="test_gpu_full"

python_bin="$repo_root/.venv/bin/python"
"$python_bin" tools/tinygpu_asm.py "$asm_src" -o "$out_base"

rtl_dir="ip/compute unit"

rtl_files=()
for f in "$rtl_dir"/*.sv; do
  [[ "$f" == "$rtl_dir/isa_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  --timing \
  --trace \
  --binary \
  --top-module compute_unit_top_tb \
  "$rtl_dir/isa_pkg.sv" \
  "${rtl_files[@]}" \
  testbench/compute_unit_top_tb.sv \
  -o Vcompute_unit_top_tb

./obj_dir/Vcompute_unit_top_tb