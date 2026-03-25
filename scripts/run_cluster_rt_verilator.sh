#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

rtl_dir="ip/compute unit"
cache_dir="ip/cache"
mailbox_dir="ip/mailbox"
mailbox_pkg="ip/mailbox/mailbox_pkg.sv"
rt_dir="ip/rt_accel"
rt_pkg="$rt_dir/rt_pkg.sv"

# Collect RTL (exclude package files, compiled separately for deterministic order)
rtl_files=()
for f in "$rtl_dir"/*.sv; do
  [[ "$f" == "$rtl_dir/isa_pkg.sv" ]] && continue
  # Skip old RT files in compute unit (now in ip/rt_accel)
  [[ "$f" == "$rtl_dir/rt_pkg.sv"          ]] && continue
  [[ "$f" == "$rtl_dir/ray_intersect_aabb.sv" ]] && continue
  [[ "$f" == "$rtl_dir/ray_intersect_tri.sv"  ]] && continue
  [[ "$f" == "$rtl_dir/bvh_traversal_engine.sv" ]] && continue
  [[ "$f" == "$rtl_dir/ray_tracing_unit.sv"     ]] && continue
  rtl_files+=("$f")
done

for f in "$mailbox_dir"/*.sv; do
  [[ "$f" == "$mailbox_dir/mailbox_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

for f in "$cache_dir"/*.sv; do
  rtl_files+=("$f")
done

# Add RT accelerator files
rtl_files+=("$rt_dir/fxp_reciprocal.sv")
rtl_files+=("$rt_dir/ray_intersect_aabb.sv")
rtl_files+=("$rt_dir/ray_intersect_tri.sv")
rtl_files+=("$rt_dir/bvh_traversal_engine.sv")
rtl_files+=("$rt_dir/ray_tracing_unit.sv")

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  --timing \
  --trace \
  --trace-structs \
  --binary \
  -Iip/mailbox -Iip/cache -Iip/rt_accel \
  --top-module cluster_rt_tb \
  "$mailbox_pkg" \
  "$rtl_dir/isa_pkg.sv" \
  "$rt_pkg" \
  "${rtl_files[@]}" \
  testbench/cluster_rt_tb.sv \
  -o Vcluster_rt_tb

echo "=== Build OK — running cluster_rt_tb ==="
./obj_dir/Vcluster_rt_tb "$@"
