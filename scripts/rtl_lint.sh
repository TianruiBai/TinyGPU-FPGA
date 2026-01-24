#!/usr/bin/env bash
set -euo pipefail

if ! command -v verilator >/dev/null 2>&1; then
  echo "ERROR: verilator not found. Install verilator to run lint." >&2
  exit 2
fi

SV_FILES=$(git ls-files "*.sv" "*.v")
if [ -z "$SV_FILES" ]; then
  echo "No SystemVerilog files found to lint." && exit 0
fi

echo "Running Verilator lint on tracked SV files..."
verilator --lint-only -sv $SV_FILES

echo "Verilator lint completed successfully."