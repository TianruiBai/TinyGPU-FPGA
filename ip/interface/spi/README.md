## SPI SAT‑IP (Direct + MailboxFabric) ✅

This directory contains two SPI SAT‑IP variants:

- `spi_sat.sv` — direct interface SPI master
- `spi_mailboxfabric.sv` — MailboxFabric leaf wrapper (AXI4‑Lite full‑duplex)

### Features
- Configurable TX/RX length: **1–8 bytes**
- Multiple chip‑selects (CS) via `CS_NUM`
- Configurable SCLK divider via `SCLK_DIV`
- SPI mode 0 (CPOL=0, CPHA=0)

---

## Direct SAT Interface (`spi_sat.sv`)

**Ports**
- `clk`, `rst_n`
- `cmd[TX_LEN*8-1:0]` — TX payload
- `resp[RX_LEN*8-1:0]` — RX payload
- `trmt` — start transfer (pulse)
- `rx_rdy` — response valid
- `clr_rdy` — clear `rx_rdy`

**SPI Pins**
- `SPI_SCLK`, `SPI_MOSI`, `SPI_MISO`
- `SPI_CS[CS_NUM-1:0]` (active‑low)

**Behavior**
- `trmt` launches a transfer and clears `rx_rdy`
- `rx_rdy` asserts after the transfer completes
- `resp` holds the last RX payload until `clr_rdy`

---

## MailboxFabric Variant (`spi_mailboxfabric.sv`)

This variant wraps `spi_sat.sv` behind the MailboxFabric endpoint (`mailbox_endpoint`).

### CSR Map (Mailbox Endpoint)
Addresses: CSR index N → endpoint offset `0x04 * N` (bytes). The following offsets are endpoint‑relative words.

- **0x00 (CSR 0)** — TX data low (write)
  - `s_wdata[31:0]` (write low 32 bits of the TX payload)
- **0x04 (CSR 1)** — TX data high (write)
  - `s_wdata[31:0]` (write high 32 bits of the TX payload for >32‑bit transfers)
- **0x08 (CSR 2)** — RX data low (read‑only, passive)
  - `s_rdata[31:0]` returns the low 32 bits of the last SPI response (passive read, does not auto‑clear)
- **0x0C (CSR 3)** — RX data high (read‑only, passive)
  - `s_rdata[31:0]` returns the high 32 bits when response > 32 bits
- **0x10 (CSR 4)** — Control / Status (read/write)
  - Write fields:
    - `[7:0]`   = `tx_bits`   (number of TX bits to transmit; typically bytes*8)
    - `[15:8]`  = `rx_bits`   (number of RX bits expected; typically bytes*8)
    - `[16]`    = `start`     (write‑1 to initiate transfer when not busy)
    - `[18]`    = `clear_done` (write‑1 to clear the internal done flag)
  - Read fields (status returned in `s_rdata`):
    - `[7:0]`   = `tx_bits` (mirror)
    - `[15:8]`  = `rx_bits` (mirror)
    - `[16]`    = `busy` (read‑only)
    - `[17]`    = `done` (read‑only)
- **0x14 (CSR 5)** — Default response destination (`cfg_dest[15:0]`) (write/read)

### Response Path / Mailbox behavior
- The SPI core captures the RX payload (width = `RX_LEN * 8` bits) into an internal response register when a transfer completes and sets a `done` flag.
- The wrapper places the response into the passive RX CSRs (CSR2/CSR3) so software can read it later; reading CSR2/CSR3 does **not** clear the `done` flag.
- On completion the wrapper also emits one or two mailbox TX beats to `cfg_dest` (opcode = `OPC_DATA`):
  - If response width ≤ 32 bits the wrapper sends a single beat containing the lower 32 bits of the response.
  - If response width > 32 bits the wrapper sends two beats: low 32 bits first, then high 32 bits (final beat marked `eop=1`).
- To clear the `done` flag after software has consumed the response, write CSR4 with `clear_done` (bit 18) set.

Notes
- Control writes to CSR4 with `start` set will be ignored if the core is busy.
- TX payload is loaded from CSR0/CSR1 (low first, then high) before starting a transfer.
- Addressing: from a software viewpoint, if your leaf endpoint base address is `BASE`, then
  - `BASE + 0x00` → CSR0 (TX low), `BASE + 0x04` → CSR1 (TX high), `BASE + 0x08` → CSR2 (RX low), etc.

Examples
1. Set response destination: write `0x00001234` to offset `0x14` (CSR5).
2. Load TX payload: write low word to `0x00` (CSR0) and high word to `0x04` (CSR1) as needed.
3. Start transfer: write `tx_bits`, `rx_bits` into `0x10` (CSR4) and set `start=1` (bit 16).
4. Poll/Read response: read `0x10` (CSR4) to check `done` (bit 17) and then read `0x08`/`0x0C` (CSR2/CSR3) for payload; clear done by writing CSR4 bit 18.


---

## Example (Mailbox)

1. **Set response destination** (CSR5):
	- Write `0x00001234` to mailbox address `{Cluster, Endpoint, CSR=5}`.
2. **Load TX data** (CSR0/CSR1):
	- Write TX payload to CSR0 (low) and CSR1 (high).
3. **Start transfer** (CSR4):
	- Write `tx_bits`, `rx_bits`, and set `start=1`.
4. **Read response**:
	- Poll `done` in CSR4 and read CSR2/CSR3 (passive).
	- Or receive mailbox response at destination `0x1234`.

---

## Verilator Testbenches

- `testbench/spi_sat_tb.sv`
- `testbench/spi_mailboxfabric_tb.sv`

Run scripts:
- `scripts/run_spi_sat_verilator.sh`
- `scripts/run_spi_mailbox_verilator.sh`