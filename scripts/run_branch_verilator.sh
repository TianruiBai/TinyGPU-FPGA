#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

rtl_dir="ip/compute unit"
cache_dir="ip/cache"
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

for f in "$cache_dir"/*.sv; do
  rtl_files+=("$f")
done

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  -sv \
  --cc --exe --build \
  --top-module branch_tb \
  "$mailbox_pkg" \
  "$rtl_dir/isa_pkg.sv" \
  "${rtl_files[@]}" \
  testbench/branch_tb.sv \
  testbench/verilator_branch_main.cpp \
  -o Vbranch_tb

# Run
./obj_dir/Vbranch_tb \
  +max_cycles=500000
