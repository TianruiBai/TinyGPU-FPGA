#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

mailbox_dir="ip/mailbox"
mailbox_pkg="$mailbox_dir/mailbox_pkg.sv"

rtl_files=("$mailbox_pkg")
for f in "$mailbox_dir"/*.sv; do
  if [[ "$f" != "$mailbox_pkg" ]]; then
    rtl_files+=("$f")
  fi
done
rtl_files+=("ip/interface/spi/spi_sat.sv" "ip/interface/spi/spi_mailboxfabric.sv")

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  -sv \
  --trace --trace-structs \
  --binary \
  --top-module spi_mailboxfabric_tb \
  "${rtl_files[@]}" \
  testbench/spi_mailboxfabric_tb.sv \
  -o Vspi_mailboxfabric_tb

./obj_dir/Vspi_mailboxfabric_tb "$@"
