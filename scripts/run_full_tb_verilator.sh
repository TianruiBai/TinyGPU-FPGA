#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

rtl_dir="ip/compute unit"
mailbox_dir="ip/mailbox"
mailbox_pkg="ip/mailbox/mailbox_pkg.sv"

# Collect RTL (exclude isa_pkg.sv, compiled separately to keep package order deterministic)
rtl_files=()
for f in "$rtl_dir"/*.sv; do
  [[ "$f" == "$rtl_dir/isa_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

for f in "$mailbox_dir"/*.sv; do
  [[ "$f" == "$mailbox_dir/mailbox_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  --timing \
  --binary \
  -Iip/mailbox \
  --top-module compute_unit_full_tb \
  "$mailbox_pkg" \
  "$rtl_dir/isa_pkg.sv" \
  "${rtl_files[@]}" \
  testbench/compute_unit_full_tb.sv \
  -o Vcompute_unit_full_tb

./obj_dir/Vcompute_unit_full_tb
