#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

mailbox_dir="ip/mailbox"
mailbox_pkg="$mailbox_dir/mailbox_pkg.sv"

rtl_files=()
for f in "$mailbox_dir"/*.sv; do
  [[ "$f" == "$mailbox_pkg" ]] && continue
  rtl_files+=("$f")
done

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  -sv \
  --trace --trace-structs \
  --binary \
  --top-module mailbox_tb \
  "$mailbox_pkg" \
  "${rtl_files[@]}" \
  testbench/mailbox_tb.sv \
  -o Vmailbox_tb

./obj_dir/Vmailbox_tb "$@"
