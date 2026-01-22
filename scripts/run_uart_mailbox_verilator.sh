#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

mailbox_dir="ip/mailbox"
mailbox_pkg="$mailbox_dir/mailbox_pkg.sv"

# Build mailbox package and RTL first (package must be available to dependent modules)
rtl_files=("$mailbox_pkg")
# add all mailbox RTL (except package which is already listed)
for f in "$mailbox_dir"/*.sv; do
  if [[ "$f" != "$mailbox_pkg" ]]; then
    rtl_files+=("$f")
  fi
done
# then add the uart wrapper
rtl_files+=("ip/interface/uart/uart_mailboxfabric.sv")

verilator \
  -j 0 \
  -Wall -Wno-fatal \
  -sv \
  --trace --trace-structs \
  --binary \
  --top-module uart_mailboxfabric_tb \
  "${rtl_files[@]}" \
  testbench/uart_mailboxfabric_tb.sv \
  -o Vuart_mailboxfabric_tb

./obj_dir/Vuart_mailboxfabric_tb "$@"
