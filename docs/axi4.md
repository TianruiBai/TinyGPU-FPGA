# AXI4 Integration Notes (Normative)

This document captures the project-wide normative expectations for using
**AXI4** as the main system memory bus carrying VRAM and bulk data traffic.

## Role
- **AXI4 (full)**: high-bandwidth VRAM/data path carrying bursts from CUs, ROP, TEX,
  display scanout, and host DMA engines.
- **AXI4-Lite**: recommended for small MMIO/CSR windows (doorbells, control registers)
  where burst semantics and multiple outstanding beats are unnecessary.

## Recommended properties
- **Data width:** baseline 64-bit; allow wider (128/256) per-platform.
- **Burst size:** masters should align and coalesce to the bus beat size for efficiency;
  the memory subsystem may enforce a minimum burst granularity (e.g., 32B/64B).
- **ID/QoS:** support multiple outstanding IDs and the `QOS` field so clients can
  express priority; display reads must have a high-priority QoS to avoid underflow.
- **Attributes:** use `CACHE`/`PROT`/`LOCK` as appropriate. Mark mailbox/MMIO as
  non-cacheable/strongly-ordered.

## Ordering & MEMBAR
- `MEMBAR` semantics (architectural): drain write buffers and ensure prior stores
  are visible to other agents before completing.
- Practically, implement `MEMBAR` by waiting for all outstanding AXI write
  responses (`B` channel) and ensuring read-after-write visibility where required.

## LSU / Master behaviour
- LSU and DMA engines must translate CU/global requests to AXI transactions and
  preserve semantic attributes (cacheable vs non-cacheable, ordered vs unordered).
- For combined/merged stores, coalesce into the fewest compatible AXI bursts while
  preserving ordering.
- Atomics should use the AXI atomics or a serialized exclusive/lock/compare-exchange
  mechanism at the VRAM arbiter, preserving atomicity across all masters.

## Integration notes
- Mailbox/control plane now uses **AXI4-Lite (write-only)** for familiarity; mailbox addresses
  are strongly-ordered/non-cacheable. Address decoding must route mailbox
  accesses to the control plane and not to the AXI burst fabric unless a bridge
  is present that enforces ordering.
- The memory arbiter must implement fairness (e.g., weighted RR) and ensure
  display reads are protected against starvation.

## Verification checklist
- Burst semantics: large vector/texture bursts correctly translated/merged to AXI bursts.
- MEMBAR: prior writes complete and are visible after barrier completion.
- QoS: display priority prevents underflow under mixed workloads.
- Ordered MMIO/mailbox: no reordering across strong-ordered accesses; doorbells observed in order.

---

See also: `docs/memory_map.md`, `docs/microarchitecture.md`, `docs/system_contracts.md` for region-level and microarchitectural interaction details.