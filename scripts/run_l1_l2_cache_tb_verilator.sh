#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TB="$ROOT_DIR/testbench/l1_l2_cache_tb.sv"
L1="$ROOT_DIR/ip/cache/l1_data_cache.sv"
L2="$ROOT_DIR/ip/cache/l2_data_cache.sv"
TEX="$ROOT_DIR/ip/compute unit/texture_cache.sv"
MB_PKG="$ROOT_DIR/ip/mailbox/mailbox_pkg.sv"
OBJ_DIR="$ROOT_DIR/obj_dir_l1_l2_cache"

mkdir -p "$OBJ_DIR"

verilator -Wall --Wno-fatal --binary \
  -j 0 \
  --timing \
  --top-module l1_l2_cache_tb \
  -Mdir "$OBJ_DIR" \
  "$MB_PKG" "$TB" "$L1" "$L2" "$TEX"

"$OBJ_DIR"/Vl1_l2_cache_tb
