#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

rtl_dir="ip/interface/uart"
rtl_files=("$rtl_dir/uart_sat.sv")

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  -sv \
  --trace --trace-structs \
  --binary \
  --top-module uart_sat_tb \
  "${rtl_files[@]}" \
  testbench/uart_sat_tb.sv \
  -o Vuart_sat_tb

./obj_dir/Vuart_sat_tb "$@"
