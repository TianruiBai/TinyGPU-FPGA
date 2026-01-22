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

This variant acts as a Mailbox **Leaf / Endpoint** and instantiates the shared `mailbox_endpoint` implementation in `ip/mailbox/`.

- Addressing & CSRs
  - Mailbox CSR index `0` is the **data channel** (writes enqueue a TX to the device or reads pop RX):
    - Writes to CSR `0` (from a remote master) are treated as bytes to transmit out the UART TX line.
    - Reads from CSR `0` pop the RX FIFO (returns `0xDEADBEEF` if empty).
  - CSR `1` — status/control (reserved; implemented by the endpoint: includes simple status bits like RX/TX empty, mute flags).
  - CSR `2` — **default TX destination** (16-bit value): when the UART receives a byte from the serial line, the Mailbox TX message will be sent to this destination by default.
  - CSR `3` — **baud divisor** (32-bit): optional software-writable divisor to change `BAUD_DIV` at runtime (if implemented by the integration wrapper).

- Behavior
  - Incoming mailbox writes (CSR `0`) are pushed into UART TX FIFO and transmitted serially.
  - Incoming serial RX bytes are packaged as mailbox TX messages (opcode = DATA, eop = 1) and sent to the `DEFAULT_DEST` configured in CSR `2`.
  - Level IRQ is asserted while the endpoint's RX FIFO (mailbox inbound) is non-empty (handled by `mailbox_endpoint`).

Files
- `uart_sat.sv` — direct-port UART SAT‑IP (no mailbox). Useful for small standalone integrations and simple smoke testing.
- `uart_mailboxfabric.sv` — mailbox leaf wrapper + simple UART core. Use this for cluster/system integration with `ip/mailbox`.

Examples
- To send a byte to the UART from software: write the byte to the mailbox address for the UART endpoint with CSR index `0` (a single-word write). The UART will transmit it on the serial line.
- When a byte arrives on `UART_RX`, the peripheral will send a Mailbox message (CSR `0` semantics) to `CSR 2`'s destination; software at that destination can read and react.

Notes
- MailboxFabric access is strongly-ordered and non-cacheable — ensure LSU bypasses caches for mailbox addresses.
- The mailbox endpoint handles parity and tags according to `docs/docs_mailbox_interconnect_spec.md`.

If you'd like, I can add a simple unit testbench that exercises TX from mailbox writes and verifies UART serial output (simulated) and that RX bytes generate mailbox TX messages.