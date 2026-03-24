#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

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

verilator --lint-only -Wall -Wno-fatal -sv \
  "$rtl_dir/isa_pkg.sv" \
  ip/mailbox/mailbox_pkg.sv \
  "${rtl_files[@]}" \
  "${cache_files[@]}" \
  testbench/dhrystone_tb.sv \
  2>&1

echo "Lint completed with exit code $?"
