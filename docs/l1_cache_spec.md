# Per‑CU L1 Data Cache — Specification & Test Plan 🔧

## Goal
Provide a small, low-latency per‑Compute‑Unit (per‑CU) L1 data cache to reduce memory access latency for the graphics pipeline (ROP/texture) and vector workloads while keeping coherence and implementation complexity low for initial prototyping.

This document specifies a concrete default design (parameters, interface), verification plan (unit tests) and an incremental roadmap for enhancements.

---

## High‑level design choices
- Local private L1 per compute unit (no hardware coherence initially). Simpler, lowest latency. Software-visible fences/membar will be used for ordering between CUs. (Longer term: shared L2 + directory.)
- Non-blocking read path with a small number of MSHRs (configurable, default 4) so the CU can issue other independent loads while a miss is outstanding.
- Write policy: configurable **Write‑Through (WT)** or **Write‑Back (WB)** via the `WRITEBACK` module parameter (default = WT). WT simplifies correctness (stores are forwarded to memory immediately) but can increase bandwidth; WB reduces write bandwidth by holding dirty lines and writing back on eviction/flush, but requires dirty bits, writeback buffering and flush/eviction logic. Phase A defaults to WT, but `WRITEBACK=1` enables a write‑back mode and a flush mechanism (`cfg_flush`) to force writebacks; full WB will later be extended with a writeback buffer and MSHR/store coalescing for performance.
- Replacement: pseudo‑LRU (PLRU) per set (FPGA friendly).
- Line size and size defaults tuned for small FPGA-friendly caches: defaults shown in Parameters.

---

## Default parameters (configurable generics)
- `L1_ENABLED = 1` (allow disable)
- `L1_SIZE_BYTES = 4096` (4 KiB)
- `LINE_BYTES = 64` (64B line)
- `ASSOCIATIVITY = 4` (4‑way)
- `NUM_MSHR = 4` (non‑blocking miss slots)
- `WB_DEPTH = 8` (write buffer depth, used for write‑through staging)
- `AXI_DATA_BITS = 64` (downstream MIU AXI width; adapter will serialize/merge for larger beats)
- `MAX_RDATA_WIDTH = 128` (max load width supported returned to the CU; vector loads up to 128b)

Notes: these are conservative defaults; can scale up to 8KB/16KB depending on FPGA resources.

---

## Block size (line size) evaluation 📏
Choosing an appropriate cache block size (LINE_BYTES) is a key tradeoff for both graphics and compute workloads. Below is a concise evaluation, numbers for common choices, and concrete recommendations for TinyGPGPU.

Key tradeoffs:
- Larger lines (e.g., 64–128B) improve spatial locality (good for texture streaming and ROP write coalescing) and make external bursts more efficient, but increase miss refill cost (more beats) and can waste bandwidth/space for fine-grained scalar writes.
- Smaller lines (e.g., 16–32B) reduce wasted bandwidth on small accesses and reduce refill time, but increase tag storage overhead and typically have higher miss rates for streaming/2D spatial patterns.
- Miss latency interacts with `NUM_MSHR`: larger lines keep MSHRs occupied longer, so increase `NUM_MSHR` when using larger LINE_BYTES.

Numeric examples (fixed `L1_SIZE_BYTES = 4 KiB`, `ASSOCIATIVITY = 4`):
- LINE_BYTES = 16B: 256 lines, 64 sets (4‑way), offset=4 bits, index=6 bits, tag=22 bits. Refill beats @ AXI64 = 16B/8B = 2 beats.
- LINE_BYTES = 32B: 128 lines, 32 sets, offset=5 bits, index=5 bits, tag=22 bits. Refill beats = 4.
- LINE_BYTES = 64B (default): 64 lines, 16 sets, offset=6 bits, index=4 bits, tag=22 bits. Refill beats = 8.
- LINE_BYTES = 128B: 32 lines, 8 sets, offset=7 bits, index=3 bits, tag=22 bits. Refill beats = 16.

Practical implications:
- Vector workloads (VEC_WORDS=4, 4×32b = 16B): a 64B line holds 4 vector words in one line, allowing several contiguous vectors or scalar words to hit within the same line. This makes 64B a good balance for vector-heavy code.
- Texture sampling (bilinear/tri-linear): typical sampling fetches a small neighborhood of texels; 64B is usually sufficient, but streaming high-resolution textures can benefit from 128B lines or prefetching. A dedicated texture cache tuned for line/stripe prefetch is recommended (`texture_cache.sv`).
- ROP (framebuffer) stores are often write-bursty and benefit from wider coalescing; 64–128B lines improve write coalescing and reduce external transactions.
- Refill latency & MSHR sizing: since refill beats scale with LINE_BYTES, increase `NUM_MSHR` when using 64B→128B lines. For example, if memory round-trip + refill is ~50–100 cycles and refill transfers take ~8 cycles for 64B, using `NUM_MSHR = 4` may be modest; `NUM_MSHR = 8` is safer to hide latency for multiple outstanding misses.

FPGA considerations:
- Tag RAM footprint scales with number of lines; for the fixed total L1 size tag bits (22 in these examples) remain constant per line count but total tag memory grows with number of lines.
- BRAM width/porting: selecting LINE_BYTES that align well with BRAM word widths simplifies implementation and can avoid complex packing. 64B lines map cleanly to common block widths when using 64–128b BRAM ports plus simple aggregation.

Recommendation (initial default):
- **L1 LINE_BYTES = 64B**: balanced for vector (16B vectors) and texture locality, reasonable refill costs, and good match to AXI64 burst patterns. Keep `NUM_MSHR` configurable; consider `NUM_MSHR = 6..8` as a pragmatic default if resources permit.
- **Texture cache**: keep as a separate IP (`texture_cache.sv`) and consider **64B or 128B** lines depending on target resolution and streaming behavior; make this configurable.

Suggested experiments (microbench plan):
1. Microbench A (Vector memory streaming): stream vector loads/stores across contiguous memory and measure L1 miss rate and stall cycles for LINE_BYTES ∈ {16,32,64,128} with `NUM_MSHR` ∈ {4,8}.
2. Microbench B (Texture sampling): bilinear texture sample kernel over a big texture (random & tiled access patterns) measuring miss rate and bandwidth for texture cache line sizes {64,128} and L1 sizes {4KB,8KB}.
3. Microbench C (ROP/stores): stress framebuffer writes with random and tiled writes to observe write coalescing and inverted performance with write-through policy.

Based on the tests pick final LINE_BYTES and `NUM_MSHR` for Phase A implementation and iterate. The code already exposes `LINE_BYTES` and `NUM_MSHR` as parameters so running these experiments is straightforward.

---

## Interface (CU ↔ L1)
The L1 cache exposes **three LSU ports** to match the proposed microarchitecture: `LSU0`, `LSU1`, and `LSU_TEX`. Each LSU port has an independent request/response handshake and backpressure. The cache supports vector coalesced responses: a parameter `VEC_WORDS` (default 4) controls how many words are returned atomically for vector loads (so `resp_data` is `MAX_RDATA_WIDTH * VEC_WORDS` bits when configured).

Per‑port signals (example names in `ip/cache/l1_data_cache.sv`):
- Inputs (per port)
  - `<lsu>_req_valid` : request valid
  - `<lsu>_req_type` : enum {LOAD, STORE, ATOMIC}
  - `<lsu>_req_addr[31:0]`
  - `<lsu>_req_wdata[MAX_RDATA_WIDTH-1:0]` (for STORE)
  - `<lsu>_req_wstrb[LINE_BYTES/8-1:0]` (byte enables)
  - `<lsu>_req_id[N-1:0]` (optional in-order id for tracing)
  - **`<lsu>_req_is_vector`** : boolean, when set the request is a vector request that operates on `VEC_WORDS` words (default 4 → 128 bits). Vector requests coalesce/return `VEC_WORDS` words in a single atomic response.
  - **`<lsu>_req_vec_wmask[VEC_WORDS-1:0]`** : per-vector-word mask for writes (LSB covers lowest word). When doing partial vector writes this mask indicates which vector lanes are valid.

Vector/Scalar write semantics:
- **Scalar write (32-bit)**: set `<lsu>_req_is_vector=0` and set `<lsu>_req_vec_wmask` such that the target word lane is marked valid (e.g., `4'b0001` for lane 0). The `lsu` should supply the 32-bit scalar payload in the corresponding lane of `<lsu>_req_wdata`.
- **Vector write (128-bit)**: set `<lsu>_req_is_vector=1` and set `<lsu>_req_vec_wmask` (typically `all ones` for full vector). The `lsu` provides `VEC_WORDS` words packed in `<lsu>_req_wdata` in lane order.
- **Addressing & alignment**: vector requests must be naturally aligned to a `VEC_WORDS * MAX_RDATA_WIDTH` boundary (default: 16 bytes for 4×32-bit lanes). Misaligned vector writes should be split by the LSU before issuing to the L1.

Note: `LSU_TEX` is treated separately by the dedicated `texture_cache.sv` IP; texture accesses typically operate at scalar granularity or tile lines (LSU_TEX `resp_data` is `MAX_RDATA_WIDTH` bits). The L1 ports (`LSU0`, `LSU1`) and the texture cache have different data paths and should be integrated via the MIU/arbiter. See `ip/compute unit/texture_cache.sv` for texture cache behaviors and refill semantics.
- Outputs (per port)
  - `<lsu>_req_ready` : backpressure hint
  - `<lsu>_resp_valid` / `<lsu>_resp_data[(MAX_RDATA_WIDTH*VEC_WORDS)-1:0]` / `<lsu>_resp_id`
  - `<lsu>_resp_err` (optional)

- Control
  - `cfg_flush` (flush cache lines)
  - `cfg_invalidate` (invalidate all)
  - perf counters: `cnt_hits`, `cnt_misses`, `cnt_cycles_busy`

---

## Interface (L1 ↔ MIU / Memory)
- `mem_req_valid`/`mem_req_addr`/`mem_req_size`/`mem_req_qos`/`mem_req_id`
- `mem_req_ready`
- `mem_resp_valid`/`mem_resp_data`/`mem_resp_id`

`mem_req_qos` and `mem_req_id` are strongly recommended so the `miu_arbiter.sv` can route and prioritize ROP/texture/scalar traffic.

---

## Miss handling & MSHRs
- On a miss, allocate an MSHR and issue a `mem_req` to the MIU. Support up to `NUM_MSHR` outstanding misses. Subsequent loads to the same line should be merged into the same MSHR (coalescing).
- Once a refill returns, update tags + data and complete all merged requests.

---

## Forwarding, loads after stores
- Implement store-to-load forwarding inside the L1 for the write buffer region to ensure load-after-store ordering within the CU.
- For simplicity with WT policy, store updates are immediately forwarded to memory but still buffered locally to satisfy subsequent loads.

---

## Barriers & Ordering
- Provide `MEMBAR` semantics: when software issues a barrier, drain write buffer and optionally stall new loads until outstanding stores complete. `lsu.sv` must provide MEMBAR hooks.

---

## Performance counters and debug
- Counters: `hits`, `misses`, `refills`, `evictions`, `writebacks` (if later needed), `avg_miss_lat`, `cycles_stalled`.
- Trace/log mode to dump miss/evict events to `stderr` in simulation.

---

## Verification / Unit Test Plan (Verilator)
Create `testbench/l1_cache_tb.sv` to validate the following cases:
1. Simple load hit: preload a line in the model; load should return in 1 cycle (or defined latency).
2. Load miss then refill: load to uncached address -> `mem_req` observed; `mem_resp` returns after latency; data returned to CU; counters update.
3. Multiple outstanding misses: issue more than 1 independent load, confirm MSHR allocation and coalescing if to same line.
4. Store + load forwarding: store to address then load to overlapping bytes should get forwarded data.
5. Eviction policy: fill more than cache capacity and confirm LRU/PLRU eviction occurs.
6. MEMBAR semantics: enqueue a mix of stores, execute a barrier, confirm draining behavior.

Test harness will mock `mem` (backing memory with configurable latency) and assert expected signals and counters.

---

## Integration notes
- `lsu.sv` should consult the L1: if `L1_ENABLED`, route requests to L1 first; L1 will decide whether to serve or forward to MIU.
- `miu_arbiter.sv` should accept `mem_req_qos` and `mem_req_id` fields so arbiter can prioritize ROP/texture vs scalar traffic.
- `texture_cache.sv` can remain as specialized sampler cache or be merged with L1 for scalar/vector access depending on perf results.

---

## Roadmap & Enhancements
1. Initial prototyping: write‑through L1 + 4 MSHRs, 4KB default — unit test and measure impact. (Phase A)
2. Optional: switch to write‑back + writeback buffer + software invalidation or directory for multi-CU coherence. (Phase B)
3. Tune parameters (size/assoc/lines) by area vs perf on target FPGA and measure using microbenchmarks.
4. Replace PLRU with near‑LRU (if resources permit) and add prefetch heuristics for texture streaming.

---

## Files added as part of Phase A
- `docs/l1_cache_spec.md` (this file)
- `ip/cache/l1_data_cache.sv` (skeleton RTL & parameters)
- `testbench/l1_cache_tb.sv` (Verilator-friendly unit tests)

---

If you want, I can implement the initial L1 skeleton RTL plus the unit tests and run a Verilator simulation in this workspace to collect baseline hit/miss counters. Let me know if you want the cache defaults changed (e.g., 8KB or 32B lines) to match any hardware constraints.
