#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

rtl_dir="ip/interface/spi"

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  -sv \
  --trace --trace-structs \
  --binary \
  --top-module spi_sat_tb \
  "$rtl_dir/spi_sat.sv" \
  testbench/spi_sat_tb.sv \
  -o Vspi_sat_tb

./obj_dir/Vspi_sat_tb "$@"
