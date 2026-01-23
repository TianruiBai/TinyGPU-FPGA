## This module is a generic UART SAT‑IP ✅

This directory contains two variants of a simple 8-bit UART peripheral:

- `uart_sat.sv` — Direct-port SAT‑IP (lightweight registers / direct TX/RX handshake).
- `uart_mailboxfabric.sv` — MailboxFabric Leaf (AXI4‑Lite Full‑Duplex) variant that integrates with the project's MailboxFabric interconnect.

Features
- Configurable baud (parameterizable via `BAUD_DIV`).
- 1‑byte TX / 1‑byte RX with small FIFOs.
- Optional CTS/RTS flow control (pins exposed as ports).

Direct SAT interface

- Ports: `clk`, `rst_n`, `tx_data[7:0]`, `tx_valid`, `tx_ready`, `rx_data[7:0]`, `rx_rdy`, `clr_rdy`.
- Simple behavior: software writes bytes to `tx_data` + `tx_valid`; peripheral accepts when `tx_ready` is high. Received bytes assert `rx_rdy` until software clears with `clr_rdy`.

MailboxFabric variant (recommended for system integration)

This variant acts as a Mailbox **Leaf / Endpoint** and exposes a formal CSR map (word‑addressable 32‑bit CSRs). Addressing note: CSR index N corresponds to byte offset `0x04 * N` from the endpoint base address (e.g. CSR0 = +0x00, CSR1 = +0x04, CSR2 = +0x08, ...).

- CSR Map (offsets shown as endpoint‑relative)
  - **0x00 (CSR 0)** — TX data (write)
    - Write: `s_wdata[7:0]` enqueued into the UART TX FIFO.
    - If the TX FIFO is full the endpoint will backpressure the write (writes are blocked until space available).
  - **0x04 (CSR 1)** — RX data (read‑only, passive)
    - Read: `s_rdata[7:0]` returns the last received byte.
    - Passive read: reading CSR1 does **not** clear the RX valid flag; software must clear RX valid explicitly (see CSR4).
  - **0x08 (CSR 2)** — Default RX destination (write/read)
    - Write/Read: `cfg_dest[15:0]` — mailbox destination address used for outgoing mailbox TX messages when a UART RX byte is captured.
  - **0x0C (CSR 3)** — Baud divisor (write/read)
    - Write/Read: 32‑bit `cfg_baud_div` (runtime BAUD_DIV override).
  - **0x10 (CSR 4)** — Status / Control (read/write)
    - Read bits (s_rdata):
      - `[1]` TX FIFO full (1 = full)
      - `[0]` RX valid (1 = unread RX byte present)
    - Write bits:
      - Writing `1` to `[0]` clears the RX valid flag.

- Behavior / Mailbox message format
  - Sending TX: mailbox writes to **CSR0 (+0x00)** enqueue a byte for UART TX.
  - Receiving RX: when a byte is received on `UART_RX` the byte is captured into CSR1 (passive readable) and **RX valid** is set; it remains set until software clears it (CSR4 write bit[0]=1).
  - Outbound mailbox TX: each received byte is also emitted as a mailbox TX message (opcode = `OPC_DATA`, `eop` = 1) with `tx_data = {24'h0, rx_byte}`, `tx_dest = cfg_dest`. The mailbox endpoint computes tags/parity using the configured `SRC_ID`.

Files
- `uart_sat.sv` — direct‑port UART SAT‑IP (no mailbox). Useful for small standalone integrations and simple smoke testing.
- `uart_mailboxfabric.sv` — mailbox leaf wrapper + UART core. Use this for cluster/system integration with `ip/mailbox`.

Notes
- MailboxFabric access is strongly‑ordered and non‑cacheable — ensure LSU bypasses caches for mailbox addresses.
- The mailbox endpoint handles parity and tags according to `docs/docs_mailbox_interconnect_spec.md`.
- CSR0 writes are backpressured when internal TX FIFO is full (write will not complete until there is room).