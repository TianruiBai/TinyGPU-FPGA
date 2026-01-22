# Matrix Computing Core (MCC) — Architecture & Implementation Plan

**Status:** Recommendation: RISC‑V (RV32I) + Custom Matrix Extension; Mailbox used as Control Plane (job dispatch / IRQ) while L1/L2 + AXI handle the Data Plane.

---

## 1. Goals & Scope 🎯
- Provide a compact, high-throughput Matrix Computing Core (MCC) optimized for block (tile) matrix operations (matmul, conv-like kernels). 
- Integrate with the existing L1 cache (write-back) and the MailboxFabric for control and signalling.
- Support both direct memory streaming (via cache / AXI) and low-latency control via AXI4-Lite mailbox.

---

## 2. Strategic Decision: ISA & Control Flow ✅
**Recommendation:** Use a small RISC‑V scalar core (RV32I base) extended with a compact set of custom Matrix instructions.

**Rationale:**
- RISC‑V provides standard instruction fetch from an I-cache (fits current L1 design), mature toolchain, and flexible control.
- Matrix work handled by a dedicated Matrix Unit (MXU) invoked via custom opcodes (e.g., MATMUL, MLOAD, MSTORE). 
- Mailbox is Control Plane only: job descriptors, synchronization, IRQs — not instruction source.

**Mailbox role:** job dispatch, completion/heartbeat, light-weight command/ack.

---

## 3. High-level Architecture 🏗️

Components (tile):
- Scalar RISC‑V core (5-stage pipeline) — control, loop counters, pointer math.
- Matrix Unit (MXU) — systolic array or SIMD tile engine (configurable tile size: e.g., 8×8 or 16×16).
- Split Load/Store Unit (LSU) — routes addresses to cached data plane or mailbox uncached plane.
- Mailbox Endpoint (AXI4-Lite) — TX/RX FIFOs, IRQ output to scalar core.
- L1 D-cache & shared L2 (AXI) — data streaming and coherence.

Memory map (example):
- 0x7000_0000 - 0x7000_FFFF : Mailbox region (AXI4-Lite, strongly ordered, non-cacheable)
- 0x8000_0000 - ...        : Main DRAM region (AXI4 Full, cacheable)

AXI config recommendations:
- AXI4 Full for data path: 128-bit or 256-bit width depending on bus budget (128-bit is a good trade-off)
- AXI4-Lite for mailbox: 32-bit, low-latency control

---

## 4. Detailed Design Notes 🔧

### 4.1 Matrix Unit (MXU)
- Interface: custom instruction and scalar register file / tile registers.
- Tile registers: large local tiles (e.g., 16×16 elements) to minimize traffic.
- Microarchitecture options:
  - Systolic array (good for streaming & energy efficiency)
  - Vector/SIMD lanes (simpler to implement)
- Precision: support 16/32-bit fixed/fp formats (parameterizable)

### 4.2 Split LSU
- Decode top address bits to select path: mailbox region vs cached region.
- Cached loads/stores: follow L1 D-cache path (use existing cache protocol, write-back enabled).
- Mailbox loads/stores (0x7000_XXXX): map to mailbox endpoint (AXI4-Lite);
  - Writes -> push to TX FIFO; if FIFO full, LSU must stall the core (backpressure)
  - Reads -> pop from RX FIFO; if empty, return a sentinel (0xDEADBEEF) and do not block
- Ensure Mailbox accesses are strongly ordered and non-cacheable.

### 4.3 Mailbox Endpoint (HW semantics)
- TX FIFO (core -> MCU): writes push data; produce IRQ to MCU on non-empty; backpressure when full.
- RX FIFO (MCU -> core): MCU writes push inbound messages; core reads pop; read-empty returns sentinel.
- IRQ lines into the scalar core: `IRQ_MAILBOX` (machine-level) wired to `mip`/`mie`.
- Job Descriptor layout (recommended):
  - DWORD[0]: Job flags + opcode
  - DWORD[1]: kernel entry pointer (PC) or kernel ID
  - DWORD[2]: data pointer
  - DWORD[3]: dims/flags

### 4.4 L1 cache interaction
- Use Write-back cache (existing design). When flushing for writeback, ensure flush is triggered when cache is idle or latch the flush request; see testbench behavior (pulse while idle) to reliably start `flush_active`.
- Use WB buffer (FIFO) for dirty evictions and flush scanner that pushes lines to WB FIFO.
- Ensure `LSU` respects mailbox region as non-cachable (do not rely on cache to forward mailbox data)

---

## 5. Matrix ISA & Software ABI 🧾

Core instruction set (custom extension opcodes) — examples:
- MCFG imm: configure MXU parameters (tile dims, precision)
- MLOAD tX, (rs1): loads tile from memory into tile register
- MSTORE tX, (rs1): stores tile register back to memory
- MMUL tdst, t0, t1: tdst = t0 * t1 (tile multiply/accumulate)
- MWAIT: wait on MXU completion (optional fence)

Software / Driver Model:
1. MCU writes Job Descriptor into mailbox TX.
2. Core IRQ wakes (WFI semantics) and reads Job Descriptor.
3. Core sets up data and invokes `MMUL`/`MLOAD` sequences.
4. On completion writes DONE to mailbox and optionally issues IRQ to MCU.

Job descriptor example (struct):
- u32  flags
- u32  kernel_pc
- u32  src_ptr
- u32  dst_ptr
- u32  m, n, k

---

## 6. Implementation Roadmap (Phased) 🛠️

Phase 0 — Project setup (1-2 weeks)
- Create repo skeleton: `ip/matrix_unit.sv`, `ip/mailbox_endpoint.sv`, `cores/mcc_wrapper.sv`, bare-metal example kernel (`sw/matrix_test.S`).
- Add RTL interface docs & memory map.

Phase 1 — Basic MXU + ISA (3-4 weeks)
- Implement simple MXU (tile load/store, int multiply-accumulate).
- Add custom instruction decode to scalar core (trap/CSR or custom op).
- Create a minimal software driver + assembly kernels.
- Unit test: small matmul (4×4, 8×8) in simulation.

Phase 2 — Mailbox Integration + LSU routing (2-3 weeks)
- Implement `mailbox_endpoint.sv` (AXI4-Lite, FIFO semantics, IRQ).
- Add LSU routing logic and memory map decode (0x7000.. for mailbox, 0x8000.. for DRAM).
- Add testbench that validates mailbox interrupts and FIFO backpressure.

Phase 3 — L1/L2 integration & performance tuning (3-4 weeks)
- Integrate MXU with L1 (streaming loads via cache) and test high‑bandwidth matmul.
- Add prefetcher/stride predictor in L1 (optional) for streaming performance.
- Tune AXI widths, outstanding transactions, and tile sizes.
- Add stress tests (multiple concurrent cores, eviction & flush paths).

Phase 4 — Verification, profiling, and SW stacks (2-3 weeks)
- Create directed and random tests for correctness and corner cases (mailbox races, flushes, WB buffer overflow). 
- Add performance microbenchmarks and SW APIs (mailbox driver, kernel launcher).

---

## 7. Verification & Test Plan ✅
- Unit tests: verify MXU correctness on small matrices.
- Integration tests: full flow from MCU → Mailbox → Core → MXU → Mailbox DONE.
- Cache/Flush tests: exercise WB pipeline and the `cfg_flush` flow. (Use `testbench/l1_cache_tb.sv` as a model — ensure flush pulses when cache is idle.)
- Stress tests: many tile loads to force evictions and WB buffer behavior; use randomized addresses with varied strides.
- Performance tests: measure sustained GB/s and GFLOPS per core; tune tile sizes & AXI width.

---

## 8. Risks & Mitigations ⚠️
- WB buffer overflow under pathological eviction patterns → Mitigate by backpressure on cache fill or slow-down policy; assert/fail in simulation to catch design bugs.
- Mailbox ordering/semantics leading to races → Strongly order mailbox region and return sentinel values on empty reads to avoid blocking.
- Poor streaming performance → Add hardware prefetcher or a simple software DMA descriptor to batch transfers.

---

## 9. Deliverables & Next Steps (Immediate) ✅
- Add RTL skeletons: `ip/mailbox_endpoint.sv`, `ip/matrix_unit.sv`, `cores/mcc_wrapper.sv`.
- Add TBs: mailbox unit tests + matrix functional tests; extend `testbench/l1_cache_tb.sv` with a scenario exercising mailbox-triggered MXU runs.
- Document memory map & Job Descriptor format in `docs/` (this file).

Quick testbench note (practical): in the current `testbench/l1_cache_tb.sv` keep `req_type` set back to LOAD before the hit test (line after LSU1 store test). This prevents an accidental STORE from turning the simple load-hit into a miss that blocks progress.

---

If you want, I can:
- Generate RTL skeletons (mailbox, MXU, wrapper) and minimal SW launcher examples, or
- Open a PR with the `docs/matrix_core_plan.md` file and a short testbench patch to set `req_type=0` before the simple load-hit test.

What would you like me to do next? 🔁
