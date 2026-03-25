#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

rt_dir="ip/rt_accel"
mailbox_dir="ip/mailbox"

# RT accelerator files (package first, then modules)
rt_files=(
  "$mailbox_dir/mailbox_pkg.sv"
  "$rt_dir/rt_pkg.sv"
  "$rt_dir/fxp_reciprocal.sv"
  "$rt_dir/ray_intersect_aabb.sv"
  "$rt_dir/ray_intersect_tri.sv"
  "$rt_dir/bvh_traversal_engine.sv"
  "$rt_dir/ray_tracing_unit.sv"
)

echo "=== Building RT Accelerator TB ==="
verilator \
  -j 0 \
  -Wall -Wno-fatal \
  --timing \
  --trace \
  --trace-structs \
  --binary \
  -I"$rt_dir" -I"$mailbox_dir" \
  --top-module rt_accel_tb \
  "${rt_files[@]}" \
  testbench/rt_accel_tb.sv \
  -o Vrt_accel_tb

echo "=== Build OK — running rt_accel_tb ==="
./obj_dir/Vrt_accel_tb "$@"
