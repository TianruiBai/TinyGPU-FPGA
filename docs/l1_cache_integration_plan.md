# L1 Cache Integration Plan

## Scope
Integrate L1 instruction and data caches into `compute_unit_top`, split LSU responsibilities, and expose appropriate memory interfaces (L2/AXI and framebuffer write path). This document tracks design intent and staged implementation steps.

## Current State (before changes)
- Single `lsu` instance handles scalar/vector/atomic + gfx stores, and arbitrates texture/descriptor refills via `miu_arbiter` to a 32-bit global port.
- Fetch is fixed-latency (no ready) and consumes a 64-bit instruction bundle directly from `inst_rdata`.
- No L1 caches are instantiated; top exposes a simple global data port.

## Target Architecture
- **L1 Instruction Cache**: `l1_inst_cache` sits in front of fetch. Fetch becomes req/ready/resp; fetch stalls on cache miss. I-cache miss path drives external `inst_miss_req_*` to L2/OSPI.
- **L1 Data Cache**: `l1_data_cache` with three LSU ports:
  - `lsu0`: pipeline slot0 loads/stores/atomics (scalar+vector).
  - `lsu1`: pipeline slot1 loads/stores/atomics (scalar+vector).
  - `lsu_gfx`: graphics/ROP stores and texture/descriptor refill path.
- **LSU Split**:
  - `lsu_core` (or two instances) handles pipeline LSU flows, mailbox, atomics, and local-memory access. Issues through L1 D-cache ports instead of direct MIU.
  - `lsu_gfx` handles write-only gfx/ROP stores (and optional texture/descriptor refills if bypassing cache).
- **External Interfaces**:
  - L2/AXI data port for D-cache `mem_req_*` / `mem_resp_*`.
  - AXI4 framebuffer write channel (can share L2 or be a separate master) for gfx direct-to-framebuffer writes.
  - Instruction miss port from I-cache to L2/OSPI.

## Staged Implementation
1) **Fetch + I-Cache**
   - Add req/ready/resp handshake to `fetch_unit`.
   - Instantiate `l1_inst_cache` in `compute_unit_top`; connect miss port to new top-level instruction miss interface.
   - Update TBs to model I-cache miss/ready.

2) **LSU Split + D-Cache Plumb**
   - Factor existing `lsu` into `lsu_core` (slot0/slot1 capable) and `lsu_gfx` (write-only).
   - Remove `miu_arbiter` coupling; LSU issues to L1 D-cache ports.
   - Keep mailbox/atomic/local memory behaviors in `lsu_core`.

3) **Top-Level Data Path + AXI**
   - Instantiate `l1_data_cache` and connect three LSU ports.
   - Add L2/AXI-facing data interface ports on `compute_unit_top` for D-cache `mem_req_*` / `mem_resp_*`.
   - Add AXI4 write channel for framebuffer (either dedicated or muxed through cache/L2).

4) **Testbench Updates**
   - Adjust compute-unit TBs to drive I-cache miss interface and D-cache memory/AXI responses.
   - Add scenarios for dual-issue LSU access, gfx framebuffer writes, and cache misses/refills.

## Interface Sketches
- **Instruction path**: fetch issues `ic_req_valid/addr`, waits for `ic_req_ready`; cache returns `ic_resp_valid/data`. Miss channel: `ic_miss_req_valid/addr` -> external memory; refill via `ic_miss_resp_valid/data`.
- **Data path**: per L1 D-cache port: `req_valid/type/addr/wdata/wstrb/is_vector/id` with ready/resp back; shared `mem_req_valid/rw/addr/size/qos/id/wdata/wstrb` to L2; `mem_resp_valid/data/id` back.
- **Framebuffer AXI**: AW/W/B (write-only) channel for gfx/ROP stores; optional AXI ID/QoS fields kept simple.

## Open Choices
- Whether to route gfx writes through D-cache or keep a dedicated AXI path; initial plan is dedicated AXI for framebuffer, D-cache for other gfx data.
- Exact ID width and QoS for L2/AXI ports; propose 8-bit IDs matching cache config.
- How much of the existing `miu_arbiter` logic is reused (likely minimal; may delete).

## Next Actions
- Implement Stage 1 (fetch + I-cache), then Stage 2 (LSU split + D-cache) in sequence, with TB adjustments after each stage.
- Revisit timing/area after synthesis once caches are in place.
