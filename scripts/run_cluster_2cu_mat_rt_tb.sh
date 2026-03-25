#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

rtl_dir="ip/compute unit"
cache_dir="ip/cache"
mailbox_dir="ip/mailbox"
rt_dir="ip/rt_accel"
mat_dir="ip/matrix_unit"

# Packages first (order matters)
pkg_files=(
  "$mailbox_dir/mailbox_pkg.sv"
  "$rtl_dir/isa_pkg.sv"
  "$rt_dir/rt_pkg.sv"
  "$mat_dir/mat_pkg.sv"
)

# Collect RTL
rtl_files=()
for f in "$rtl_dir"/*.sv; do
  case "$f" in
    *isa_pkg.sv|*cluster_top.sv|*ray_*|*bvh_*|*rt_pkg*|*fxp_reciprocal*) continue ;;
  esac
  rtl_files+=("$f")
done

# New slot-based cluster
rtl_files+=("$rtl_dir/cluster_top_v2.sv")

for f in "$mailbox_dir"/*.sv; do
  [[ "$f" == "$mailbox_dir/mailbox_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

for f in "$cache_dir"/*.sv; do
  rtl_files+=("$f")
done

for f in "$rt_dir"/*.sv; do
  [[ "$f" == "$rt_dir/rt_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

for f in "$mat_dir"/*.sv; do
  [[ "$f" == "$mat_dir/mat_pkg.sv" ]] && continue
  rtl_files+=("$f")
done

echo "=== Compiling 2CU+MAT+RT Cluster Testbench ==="

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  --timing \
  --trace \
  --trace-structs \
  --binary \
  "-Iip/mailbox" "-Iip/cache" "-Iip/rt_accel" "-Iip/matrix_unit" \
  --top-module cluster_2cu_mat_rt_tb \
  "${pkg_files[@]}" \
  "${rtl_files[@]}" \
  testbench/cluster_2cu_mat_rt_tb.sv \
  -o Vcluster_2cu_mat_rt_tb

echo "=== Running ==="
./obj_dir/Vcluster_2cu_mat_rt_tb "$@"
