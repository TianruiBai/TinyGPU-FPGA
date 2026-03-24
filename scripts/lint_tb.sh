#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

TB_FILE="${1:-testbench/gfx_sw_rt_tb.sv}"
rtl_dir="ip/compute unit"
rtl_files=()
for f in "$rtl_dir"/*.sv; do
  [[ "$f" == "$rtl_dir/isa_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

cache_files=()
for f in ip/cache/*.sv; do
  cache_files+=("$f")
done

verilator --lint-only --timing -Wall -Wno-fatal -sv \
  "$rtl_dir/isa_pkg.sv" \
  ip/mailbox/mailbox_pkg.sv \
  "${rtl_files[@]}" \
  "${cache_files[@]}" \
  "$TB_FILE" \
  2>&1

echo "Lint exit code: $?"
