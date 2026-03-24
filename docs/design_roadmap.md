# TinyGPGPU — Design Roadmap & Implementation Plan 🚀

This document converts your 7‑point proposal into a structured roadmap with rationale, design notes, priorities, verification steps, and a suggested timeline so we can execute the plan incrementally and measure improvements.

---

## Executive summary ✅
- Short term (high impact): add a per‑CU **L1 data cache** and extend the **MIU/arbiter** with QoS/IDs. These immediately reduce stalls in the graphics pipeline and vector workloads.
- Mid term: refine microarchitecture (dual‑issue expansion, 3 LSUs, 4R/3W register files), add robust writeback/commit logic and banking to remain FPGA friendly.
- Long term: migrate the control path to **RISC‑V + graphics extensions**, implement **cluster** mode with a shared L2, and add mailbox‑instruction primitives for fast intra‑cluster comms.

---

## 1) L1 Data Cache for per‑CU latency reduction 🔧
**Why:** reduces load latency and memory traffic, directly improving vector and ROP/texturing throughput.

**Design notes:**
- Start with private per‑CU L1 (write‑through, MSHRs, PLRU, small config defaults — see `docs/l1_cache_spec.md`).
- Expose `mem_req_qos`/`id` on L1→MIU interface.

**Files to add/update:** `ip/cache/l1_data_cache.sv`, `lsu.sv`, `miu_arbiter.sv`, `texture_cache.sv`, `testbench/l1_cache_tb.sv`.

**Acceptance:** L1 reduces measured memory stalls by >30% on texture streaming microbench.

---

## 2) Switch to RISC‑V + custom graphics extension ⚖️
**Why:** leverage LLVM, toolchain and ecosystem; enables OpenCL/POCL-style portability later.

> **📋 Detailed plan:** See [`docs/riscv_isa_migration_plan.md`](riscv_isa_migration_plan.md) for the comprehensive migration plan covering opcode mapping, Xgpu custom extension design, RTL file change list, testbench migration strategy, and phased implementation roadmap.

**Summary of phased approach:**
- Phase 1: **ISA Package + Decoder** — Rewrite `isa_pkg.sv` with RV32IMA_Zicsr standard opcodes + Xgpu custom slots (`custom-0` through `custom-2`). Rewrite `decoder.sv` for standard RV32 opcode routing. *(Control MCU already done — `ip/control_proc/mcu_core.sv` implements RV32IMA_Zicsr)*
- Phase 2: **Execution Unit Updates** — Update `alu_scalar.sv`, `branch_unit.sv`, `lsu.sv`, `csr_file.sv`, `compute_unit_top.sv`; add AUIPC, JALR (dedicated opcode), LBU/LHU, standard FENCE.
- Phase 3: **Custom Extension Opcodes** — Move FP16 to `custom-0` (0x0B), vector ALU to `custom-1` (0x2B), vector mem/atomics to `custom-2` (0x5B), graphics/texture to `custom-0`. Standard AMO replaces custom scalar atomics.
- Phase 4: **Testbench Migration** — Update all ~25 testbench files with new opcode constants; run full regression.
- Phase 5: **Documentation + Toolchain** — GNU as + `.insn` for Xgpu; riscv-tests compliance suite; rewrite ISA docs.
- Phase 6: **Optimization** — Optional C extension, F extension upgrade, shared ISA package with control processor.

**Files/areas:** `ip/compute unit/isa_pkg.sv` (rewrite), `ip/compute unit/decoder.sv` (major), all execution units, ~25 testbenches, ~15 documentation files. See migration plan for complete file list.

---

## 3) Microarchitecture: dual‑issue enhancement & 3 LSUs 🔁
**Goal:** expand slot semantics to support more load/store/ALU combinations without structural stalls.

**Proposal:**
- Slot0: support scalar (branch/load/store) and vector (load/store).
- Slot1: support scalar & vector ALU and load/store.
- Provide **3 LSUs**: Scalar LSU, Vector LSU, ROP/Texture LSU. Each LSU maintains `empty/busy` status exposed to the issue unit and `QOS` to MIU.
- Pipeline will have 2 scalar ALUs + 1 vector ALU (configurable to 2 vector ALUs if FPGA budget allows).

**Implementation points:** update `compute_unit_top.sv` issue logic, `scoreboard.sv`, `lsu.sv`, and `scalar_wb_arb_pending2.sv`.

**Verification:** microbenchmarks for dual‑issue throughput (independent ALU/VLOAD pairs) and stress tests for simultaneous LSU usage.

---

## 4) Register file: 4R/3W + enhanced writeback commit ➕
**Why:** enables wider issue without sacrificing correctness.

**FPGA-friendly practical options:**
- Banked register files or time‑multiplexed ports rather than fully-ported multi‑ported SRAM.
- Implement writeback commit pipeline and 3‑entry writeback buffers consistent with `scalar_wb_arb_pending2.sv` semantics.

**Files:** `regfile_scalar.sv`, `regfile_vector.sv`, `scalar_wb_arb_pending2.sv`.

---

## 5) Cluster design & sizing (1/2/4/6/8) 🧩
**Topology:** MailboxFabric as control plane + dedicated low‑latency fabric (AXI‑Stream / ring / crossbar) for fast data exchange.

**Options:** start with non‑coherent cluster (software-managed fences) and later add a directory-based coherence for L2.

**Files/areas:** `mailbox/*`, `miu_arbiter.sv` (cluster port), `docs/memory_map.md` and cluster provisioning scripts.

---

## 6) Shared L2 for cluster scaling 🏗️
**Why:** amortize bandwidth and provide larger capacity; L2 will provide coherence / per-CU miss resolution.

**Design:** initial L2: inclusive cache with simple directory or per‑line owner bits; L2 serviced by a higher-level MIU.

**Timing/Risks:** L2 coherence adds complexity; phase this to measure performance first with non‑coherent software-managed flow.

---

## 7) MailboxFabric enhancement & mailbox instructions ⚡
**Goal:** make intra‑cluster messaging low latency and expressive for coordination (doorbells, small DMA descriptors, fast notify).

**Design options:**
- Add custom RISC‑V CSR ops and minimal instruction encodings (fast store-to-mailbox, doorbell, wait_for_mailbox) or provide dedicated mailbox port in the ISA.
- Provide hardware paths for message copy (small FIFO/stream) that bypass global MIU for short messages.

**Files:** `ip/mailbox/*`, `control_proc/*`, `docs/mailbox_interconnect_spec.md`.

---

## Prioritization & tentative timeline ⏱️
1. Phase A (0–8 weeks): L1 cache prototyping (WT + MSHRs), MIU QoS update, and add microbench tests. (High ROI)
2. Phase B (8–20 weeks): microarch changes (3 LSUs, dual‑issue slots update), regfile banking, metrics collection. (Medium effort)
3. Phase C (3–9 months): cluster + shared L2 design + mailbox ISA, RISC‑V control MCU integration. (Strategic)

---

## Verification & metrics 📊
- Add counters & perf events in `compute_unit_top.sv` for: stall cycles, L1 hit/miss, LSU utilization, dual‑issue success rate.
- Define acceptance criteria for each milestone (e.g., L1 must reduce memory stall cycles by X% on texture striping benchmark).

---

## Risks & mitigation ⚠️
- FPGA resource limits: prototype with conservative cache sizes and banked regfiles; iterate based on synthesis.  
- Complexity of coherence: use phased approach with non‑coherent first.  
- Toolchain burden (RISC‑V): start with control MCU to reduce scope and enable incremental migration.

---

## Next actions (pick one) ▶️
- Implement Phase A **L1 cache** (I can continue building the functional RTL + unit tests). ✅
- Or implement **MIU/arbiter QoS/IDs** to prioritize ROP/texture traffic and measure impact (quick win). ✅

Tell me which action to take and I’ll create a small task list and PR draft to get started.

---

*Document created by GitHub Copilot — Raptor mini (Preview).*