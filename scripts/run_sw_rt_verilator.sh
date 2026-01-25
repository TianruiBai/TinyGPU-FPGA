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
  --timing \
  --trace \
  --trace-structs \
  --binary \
  -Iip/mailbox -Iip/cache \
  --top-module gfx_sw_rt_tb \
  "$mailbox_pkg" \
  "$rtl_dir/isa_pkg.sv" \
  "${rtl_files[@]}" \
  testbench/gfx_sw_rt_tb.sv \
  -o Vgfx_sw_rt_tb

./obj_dir/Vgfx_sw_rt_tb "$@"
