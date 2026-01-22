# Cluster‑Shared L2 Data Cache — Design Principles & Plan

## 1. Goals
- Provide a **shared, cluster‑level L2** cache that reduces inter‑core memory traffic and improves miss latency for 2/4‑core clusters.
- Keep coherence **simple and deterministic** by enforcing a **single cluster‑wide policy**: either **write‑through (WT)** or **write‑back (WB)** for *all* L1s in the cluster (no mixing).
- Support standard **AXI4** for access to system memory and optional **MailboxFabric** control/status for cluster management.

---

## 2. Key Constraints (from plan)
1. **Size configurable 16–128KB** depending on cluster size.
2. **Optimized miss/refill logic** for 2/4‑core clusters.
3. **AXI4** system bus access.
4. **QoS + priority** for refill arbitration.
5. **Consistency with L1** (WT/WB policy alignment).
6. **L1↔L2 testbench** for correctness and data consistency.
7. **Cluster‑wide single policy** (WT or WB, no mixing).

---

## 3. Architecture Overview
### 3.1 Set‑Associative Shared L2
- **4‑way set‑associative** cache (default). 
- Parameters:
  - `L2_SIZE_BYTES` ∈ [16KB, 128KB]
  - `LINE_BYTES` (default 64B)
  - `ASSOCIATIVITY` = 4
- Derived:
  - `NUM_SETS = L2_SIZE_BYTES / (LINE_BYTES * ASSOCIATIVITY)`

### 3.2 Multi‑port L1 Interfaces
- `NUM_PORTS` corresponds to L1 count (2 or 4 typical).
- Each port uses the same **mem_req‑style interface** as L1→MIU, allowing easy drop‑in.

### 3.3 Internal Cluster Partitioning (Way Partitioning)
To reduce inter‑core thrashing:
- **Way partitioning** splits associativity across L1 ports.
- Example:
  - 2‑core cluster, 4‑way L2 → each core gets 2 ways per set.
  - 4‑core cluster, 4‑way L2 → each core gets 1 way per set.
- If `ASSOCIATIVITY % NUM_PORTS != 0`, partitioning is disabled and all ways are shared.

**Benefits:**
- Higher QoS predictability under multi‑core contention.
- Limits eviction interference between cores.

---

## 4. Write Policy & L1 Coherence
### 4.1 Cluster‑Wide Policy
- **WT cluster**: All L1 caches are WT. L2 updates on stores and forwards the full line to memory.
- **WB cluster**: All L1 caches are WB. L2 collects dirty lines and writes back on eviction.

**No mixing allowed**: a cluster must select one policy via the shared `WRITEBACK` parameter.

### 4.2 L1–L2 Consistency Rules
- L1 misses always fetch from L2.
- L2 refills from memory are **serialized per request** (single outstanding in baseline).
- On WT store:
  - L2 updates its line and issues **write‑through** to memory.
- On WB store:
  - L2 updates its line and sets **dirty bit**.
  - Writeback to memory happens on **eviction**.

**Consistency expectation:**
- WT cluster: all cores observe new data quickly because writes go to memory immediately.
- WB cluster: data visibility across L1s is guaranteed by software fences or explicit flush/invalidate of L1s and L2 (planned extension).

---

## 5. QoS + Priority Fill Logic
- Arbitration chooses the **highest QoS** request among L1s.
- Round‑robin fairness among requests of equal QoS.
- Priority influences **miss/fill** scheduling so latency‑critical L1s can preempt bandwidth.

---

## 6. AXI4 Memory Interface
- L2 acts as a **standard AXI4 master** with burst transfers for line fills and evictions.
- `LINE_BEATS = LINE_BYTES / (AXI_DATA_BITS/8)`
- Read path: `AR → R` beats into a refill buffer.
- Write path: `AW → W → B` with full‑line writebacks.

---

## 7. Optional MailboxFabric Control
The L2 cache exposes a lightweight **MailboxFabric (AXI‑Lite)** control/status interface:
- **CONTROL**: enable/disable, invalidate.
- **STATUS**: hits, misses, evictions, refills, writebacks.

This interface allows the control MCU to monitor cache performance or trigger invalidation without using the system memory bus.

---

## 8. Verification Plan (L1↔L2 Testbench)
**Baseline testbench:** `testbench/l1_l2_cache_tb.sv`
- Two L1 instances → L2 → AXI memory model.
- Tests for:
  1. L1 miss → L2 miss → memory refill.
  2. L1 store (WT) → L2 write‑through → memory updated.
  3. Multi‑core contention and partitioned ways.
  4. WB path (if enabled): dirty eviction and writeback.

**Future tests:**
- Stress with random addresses to validate partitioning fairness.
- QoS preemption under mixed request loads.
- WB flush + invalidate correctness.

---

## 9. Roadmap Extensions
- Add **multi‑MSHR** support in L2 to allow multiple outstanding misses.
- Implement **explicit flush/invalidate** sequences for WB clusters.
- Add **directory/L1 ownership tracking** for stronger WB consistency.
- Extend MailboxFabric CSR map with per‑core QoS control and partition overrides.

---

## 10. Summary
This L2 design achieves:
- **Shared capacity** with controlled interference (partitioning).
- **Standard AXI4** integration.
- **QoS‑aware arbitration** for multi‑core balance.
- **Single‑policy WT/WB** consistency across the cluster.
- A clear path to richer coherence mechanisms later.
