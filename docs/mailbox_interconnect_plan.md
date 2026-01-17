# Mailbox Interconnect Implementation Plan

This plan outlines the RTL deliverables and integration steps for the full mailbox network: Center, Switch, and Leaf (Endpoint). Goals: low-latency sync, deterministic flow control, and simple timing closure on FPGA.

## 1. Mailbox Center

**Scope:** Root router connecting system/MCU and up to 4 cluster switches, plus a direct high-priority (HP) node.

**Interfaces:**
- 4× Downlinks: Center → Switch (full-duplex mailbox Wishbone-Lite write channels, baseline profile).
- 1× Uplink per downlink (full-duplex Wishbone-Lite).
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
- 1× Uplink (full-duplex Wishbone-Lite) to Center.
- 4× Downlinks (full-duplex Wishbone-Lite) to Leaves.

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
- 1× Uplink (full-duplex Wishbone-Lite) to Center.
- 2× Downlinks (full-duplex Wishbone-Lite) to Leaves.

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
- Mailbox Wishbone-Lite full-duplex to Switch (Compact allowed for area: can drop `tag_prio/tag_parity`).
- Core side: LSU sideband for TX, CSR for RX/STATUS, IRQ line.

**Functionality:**
- TX: Detect stores to 0x7000_xxxx in LSU; drive Wishbone-Lite master (write-only) with `wb_adr=dest_id`, `wb_dat_w=data`, tags for src/prio/eop/opcode; stall LSU if FIFO or downstream `ack` is backpressured.
- RX: Switch drives Wishbone-Lite master toward Leaf to deliver inbound messages; Leaf slaves accept into RX FIFO; level IRQ when not empty; CSR read pops.
- Optional ACK/NACK opcode handling in CSR for reliability flows.
- Error counter for parity (if enabled) and invalid dest responses (if surfaced).

**LSU Modifications:**
- Address decode for mailbox region; bypass data cache; mark as strongly ordered, non-cacheable.
- Drive Wishbone-Lite master signals: `cyc/stb/we=1`, `adr=dest_id`, `dat_w=data`, `sel=4'hF`, plus tags (src_id/eop/prio/opcode/parity). Stall LSU on TX FIFO full or outstanding master not acked.

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
- Docs: updated spec (this plan plus bus spec), CSR map for mailbox endpoint, and verification checklist.
