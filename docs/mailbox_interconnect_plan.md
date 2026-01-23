# AXI‑MailboxFabric Implementation Plan

This plan outlines the RTL deliverables and integration steps for the full **AXI‑MailboxFabric** network: Center, Switch, and Leaf (Endpoint). Goals: low-latency sync, deterministic flow control, and simple timing closure on FPGA.

## 1. Mailbox Center

**Scope:** Root router connecting system/MCU and up to 4 cluster switches, plus a direct high-priority (HP) node.

**Interfaces:**
- 4× Downlinks: Center → Switch (full-duplex **AXI‑MailboxFabric** links: `valid/ready/data` + separate `dest_id` sideband, baseline profile).
- 1× Uplink per downlink (full-duplex **AXI‑MailboxFabric** link).

> **Note:** The **AXI‑MailboxFabric** is the *control/status* plane and uses an **AXI‑Stream hybrid** look-ahead routing model (valid/ready/data with `dest_id` sideband). The **main system memory bus** (VRAM/data path) remains **AXI4** (see `docs/memory_map.md`). Mailbox accesses are 32-bit word oriented and strongly-ordered/non-cacheable; software should address mailbox CSR words using the CSR index. Stores and loads to the mailbox address window are captured by the LSU and converted into mailbox flits (writes produce fire-and-forget flits; reads pop RX via CSR). Note that byte-enable (`wstrb`) semantics are not part of the wire protocol.
- 1× HP port: direct endpoint (e.g., MCU or interrupt concentrator) with highest priority arbitration.

**Functionality:**
- Decode `dest_id[15:8]` (Cluster) and route to one of 4 switches; `dest_id==0` routes to HP/MCU.
- Broadcast handling: `dest_cluster==0xFF` replicates to all switches + HP (if enabled).
- Priority: HP input can preempt RR arbitration for latency-critical IRQ/doorbell traffic.
- Parity check (if enabled) per ingress; drop + count on error.
- Backpressure: aggregate `ready` across broadcast fanout; stall source when any child is full.

**Microarchitecture:**
- Per-port ingress FIFO (≥4 recommended, ≥2 absolute minimum) and egress FIFO (≥4) to absorb fanout and multi-initiator bursts while keeping logic small.
- Arbiter: Weighted RR with HP override; latency class (`tag_prio`) preempts but guarantees BE service (at least 1 BE grant every 4 grants). Use `tag_hops` (saturating, increment per hop) as tiebreaker to drain older/further packets first.
- Optional register slices on data and ack/ready for timing.

**Verification (Center):**
- Directed: unicast to each switch, HP path, parity error drop, broadcast fanout with one stalled child.
- Random: mixed unicast/broadcast with HP bursts and multi-initiator contention to the same remote cluster; ensure fairness, BE minimum service, and no drops.

## 2. Mailbox Switch (4×1)

**Scope:** One uplink to Center, four downlinks to Leaves.

**Interfaces:**
- 1× Uplink (full-duplex **AXI‑MailboxFabric** link) to Center.
- 4× Downlinks (full-duplex **AXI‑MailboxFabric** links) to Leaves.

**Functionality:**
- Local vs upstream routing via `dest_cluster` compare; if equal, send to `dest_local`; else forward upstream.
- Broadcasts: `dest_cluster==0xFF` forward upstream and replicate to all locals.
- Route-lock on bursts until `eop` seen to keep bursts contiguous.
- Parity check (if enabled) and error counter.

**Microarchitecture:**
- Per-output FIFO ≥2 (locals, uplink). Ingress skid buffer optional.
- Per-output weighted RR: latency class first with BE minimum service (1 in 4); use `tag_hops` (saturating) to favor long-traveled/older packets as tiebreaker. Route-lock still enforced on bursts.
- Ready/ack generation per output; avoid deep combinational cones (register if needed).

**Verification (Switch):**
- Directed: local unicast (port0→port1), upstream (port2→uplink), broadcast with one stalled local.
- Stress: random traffic across all 5 inputs with bursts and mixed prio; include multi-initiator contention where several CUs target the same remote cluster to confirm fairness/no starvation.

## 2b. Mailbox Switch (2×1 lightweight)

**Scope:** Variant switch for small clusters: one uplink to Center, two downlinks to Leaves.

**Interfaces:**
- 1× Uplink (full-duplex **AXI‑MailboxFabric** link) to Center.
- 2× Downlinks (full-duplex **AXI‑MailboxFabric** links) to Leaves.

**Functionality:**
- Same routing rules as 4×1: local vs upstream by `dest_cluster`; broadcast replicates to both locals and uplink when `dest_cluster==0xFF`.
- Route-lock on bursts until `eop` to keep bursts contiguous.
- Parity check optional; error counter.

**Microarchitecture:**
- Per-output FIFO ≥2 (locals and uplink).  
- Arbiter: weighted RR with BE minimum service (1 in 4); `tag_prio` for latency, `tag_hops` as age/distance tiebreaker.  
- Ready/ack generation per output; register if timing requires.

**Verification (Switch 2×1):**
- Directed: local unicast (down0→down1), upstream (down1→uplink), broadcast with one stalled downlink.
- Stress: random traffic across 3 inputs with mixed prio; include both locals targeting the same remote cluster to confirm fairness/no starvation.

## 3. Mailbox Leaf (Endpoint)

**Scope:** Leaf module instantiated in processor/peripheral tiles; connects to Switch via mailbox bus and exposes CSR/IRQ interface to the core.

**Interfaces:**
- Mailbox **AXI‑MailboxFabric** full-duplex link (Valid/Ready/Data + `dest_id` sideband) to Switch (Compact allowed for area: can drop `tag_prio/tag_parity`).
- Core side: LSU sideband for TX/RX (produces/consumes flits), CSR for RX/STATUS, IRQ line.

**Addressing (refined for CSR subspace):**
- `awaddr/araddr[15:8]` = Cluster ID
- `awaddr/araddr[7:4]` = Endpoint ID (supports up to 16 endpoints per cluster)
- `awaddr/araddr[3:0]` = Endpoint CSR index (16-word space per endpoint)
	- CSR index 0 is the data path (RX ring pop on read; TX enqueue on write)
	- CSR indices 1–15 are user-defined/control/status words; reads return the register contents directly (no pop)

**Functionality:**
- TX: Detect stores to mailbox window; the LSU builds a mailbox flit (32-bit payload + header) and enqueues it into the TX FIFO with a separate `dest_id` sideband. For data sends, software uses CSR index 0; mailbox transfers are 32-bit word oriented (no byte-enable on-wire). Stall the core on TX FIFO full or when downstream backpressure holds `tx_ready` low.
- RX: Switch forwards flits to Leaf over the stream link; Leaf accepts when `rx_ready` is asserted. When RX FIFO is not empty, IRQ_MAILBOX is asserted; reads via CSR index 0 pop the RX FIFO.
- Reads (pop semantics): Loads to CSR index 0 pop the RX FIFO. If RX is empty, return 32'hDEADBEEF and do not stall the fabric.
- Control CSRs (via CSR indices 1–15 at the targeted endpoint ID): bits include mute_tx/mute_rx, clear RX/TX FIFO, flood-mute, mute broadcast, and clear-all outputs. Switch control remains local (LocalID 0xFD), center control at Cluster 0x00/LocalID 0xFC.
- Optional ACK/NACK opcode handling in CSR for reliability flows.
- Error counter for parity (if enabled) and invalid dest responses (if surfaced).

**LSU Modifications:**
- Address decode for mailbox region; bypass data cache; mark as strongly ordered, non-cacheable.
- For stores: on a write to 0x7000_xxxx, convert the store into a mailbox flit (32-bit payload + header) and present it to the TX port (`tx_valid` + `tx_data` + `tx_dest_id`) and hold until `tx_ready` is asserted. Stall the LSU/TXN when TX FIFO is full or `tx_ready` is low.
- For loads: reads to CSR index 0 pop the RX FIFO via the CSR interface. Consume `CSR_MAILBOX_RX` result; if empty, read returns 32'hDEADBEEF.


**Verification (Leaf):**
- Directed: TX stall on full, RX IRQ level behavior, CSR pop ordering, ACK/NACK roundtrip (if used).
- Integration: end-to-end unicast and broadcast with Switch testbench.

## 4. Integration & Staging

1. Implement Leaf (with LSU hooks) + unit test.
2. Implement Switch 4×1; verify local/upstream/broadcast paths.
3. Implement Center; verify 4-switch fanout and HP port.
4. Integrate in cluster: instantiate Switch and connect 4 Leaves; hook to Center.
5. System-level sims: randomized traffic, broadcast under congestion, HP preemption, parity error injection.

## 5. Deliverables

- RTL: `mailbox_center.sv`, `mailbox_switch_4x1.sv`, `mailbox_endpoint.sv`, LSU modifications, shared bus interface typedef/package.
- Testbenches: unit benches for each block; top-level system smoke with randomized traffic.
- Docs: updated spec (this plan plus **AXI‑MailboxFabric** protocol description), API/RTL headers (`mailbox_pkg`, `mailbox_link_if`), CSR map for mailbox endpoint, and verification checklist.
