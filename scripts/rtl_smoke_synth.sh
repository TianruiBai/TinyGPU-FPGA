#!/usr/bin/env bash
set -euo pipefail

if ! command -v yosys >/dev/null 2>&1; then
  echo "ERROR: yosys not found. Install yosys to run smoke synthesis." >&2
  exit 2
fi

# Collect tracked RTL sources (adjust patterns if your repo layout differs)
SV_FILES=$(git ls-files 'rtl/*.sv' 'ip/**/*.sv' || true)
if [ -z "$SV_FILES" ]; then
  echo "No RTL files found for smoke synthesis." && exit 0
fi

# Run a general smoke synth (no target-specific back-end)
yosys -p "read_verilog -sv $SV_FILES; synth; opt; stat"

echo "Yosys smoke synthesis completed successfully."