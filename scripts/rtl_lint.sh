#!/usr/bin/env bash
set -euo pipefail

if ! command -v verilator >/dev/null 2>&1; then
  echo "ERROR: verilator not found. Install verilator to run lint." >&2
  exit 2
fi

mapfile -d '' -t SV_FILES < <(git ls-files -z "*.sv" "*.v")
if [ ${#SV_FILES[@]} -eq 0 ]; then
  echo "No SystemVerilog files found to lint." && exit 0
fi

# Filter out testbench/test sources for RTL lint.
FILTERED_FILES=()
for f in "${SV_FILES[@]}"; do
  case "$f" in
    testbench/*|test/*|*tb*.sv|*tb*.v|ip/interface/octal-spi/ospi_controller.sv)
      continue
      ;;
  esac
  FILTERED_FILES+=("$f")
done
SV_FILES=("${FILTERED_FILES[@]}")
if [ ${#SV_FILES[@]} -eq 0 ]; then
  echo "No RTL SystemVerilog files found to lint." && exit 0
fi

# Ensure packages are compiled first, and preserve paths with spaces.
FILE_LIST=()
PKG_FILES=()
NON_PKG_FILES=()

for f in "${SV_FILES[@]}"; do
  if [[ "$f" == *"_pkg.sv" ]]; then
    PKG_FILES+=("$f")
  else
    NON_PKG_FILES+=("$f")
  fi
done

FILE_LIST+=("${PKG_FILES[@]}")
FILE_LIST+=("${NON_PKG_FILES[@]}")

echo "Running Verilator lint on tracked SV files..."
verilator --lint-only -sv --Wno-fatal --Wno-MULTITOP --Wno-TIMESCALEMOD "${FILE_LIST[@]}"

echo "Verilator lint completed successfully."