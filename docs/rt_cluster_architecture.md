# TinyGPU Hardware Ray-Tracing & Cluster Architecture

## Overview

This document describes the hardware ray-tracing pipeline, SIMT/SIMD hybrid
compute unit enhancements, and multi-CU cluster architecture added to TinyGPU.

---

## 1. Hardware Ray-Tracing Pipeline

### 1.1 Arithmetic: Q16.16 Fixed-Point

All RT spatial computations use **Q16.16 signed fixed-point** (32 bits) instead
of FP32. This trades dynamic range for FPGA-friendly DSP48 multiply inference
and deterministic timing. Defined in `rt_pkg.sv`:

| Constant      | Value           | Purpose              |
|--------------|-----------------|----------------------|
| `FXP_FRAC`   | 16              | Fractional bits      |
| `FXP_ONE`    | `32'h0001_0000` | 1.0                  |
| `FXP_EPSILON`| `32'h0000_0010` | ~0.00024 tolerance   |
| `FXP_MAX`    | `32'h7FFF_FFFF` | +32767.999           |
| `FXP_MIN`    | `32'h8000_0000` | -32768.0             |

### 1.2 Data Structures

**BVH Node (32 bytes = half cache line):**
```
Word 0:  type (0=internal, 1=leaf) | child/tri_offset | tri_count
Words 1-6:  AABB min_x/y/z, max_x/y/z (Q16.16 each)
Word 7:  padding
```

**Triangle (48 bytes = 12 words):**
```
Words 0-2:   v0 (x, y, z)
Words 3-5:   v1 (x, y, z)
Words 6-8:   v2 (x, y, z)
Words 9-11:  triangle_id, material_id, padding
```

### 1.3 Ray-AABB Intersection (`ray_intersect_aabb.sv`)

6-stage pipelined slab method:

| Stage | Operation                        |
|-------|----------------------------------|
| S0    | diff = bound - origin (6 subs)   |
| S1    | prod = diff × inv_dir (6 DSP48)  |
| S2    | Truncate 64→32 (Q16.16)          |
| S3    | Swap per axis so t0 ≤ t1         |
| S4    | Accumulate X,Y: max(t0), min(t1) |
| S5    | Accumulate Z + hit test           |

Backpressure: `pipe_stall = valid[5] && !out_ready`.

### 1.4 Ray-Triangle Intersection (`ray_intersect_tri.sv`)

12-stage pipelined Möller-Trumbore algorithm:

| Stage | Operation                               |
|-------|-----------------------------------------|
| S0    | Edge vectors e1, e2, s = origin - v0    |
| S1-2  | h = cross(dir, e2)                      |
| S3-4  | det = dot(e1, h), dot(s, h)             |
| S5    | Reciprocal 1/det (seed iteration)       |
| S6    | u = f × dot(s, h), early reject         |
| S7-8  | q = cross(s, e1)                        |
| S9-10 | dot(dir, q), dot(e2, q)                 |
| S11   | Final v, t computation + hit test       |

Degenerate check: `|det| < EPSILON` → miss.

### 1.5 BVH Traversal Engine (`bvh_traversal_engine.sv`)

Autonomous depth-first BVH walker with 12-state FSM:

```
IDLE → FETCH_NODE → WAIT_NODE → TEST_AABB → WAIT_AABB
  ↓
PROCESS_HIT → FETCH_TRI → WAIT_TRI → TEST_TRI → WAIT_TRI_RES
  ↓
POP_STACK → DONE
```

- **Stack depth**: 24 entries (supports trees up to depth 24)
- **Memory interface**: 64-bit beats, 4 beats/node, 6 beats/triangle
- **Closest-hit tracking**: `t_max` tightens on each triangle hit
- **Performance counters**: nodes_visited, tris_tested

### 1.6 Ray Tracing Unit (`ray_tracing_unit.sv`)

Memory-mapped register file (32 words) wrapping the BVH traversal engine:

| Register     | Address | Direction | Description                |
|-------------|---------|-----------|----------------------------|
| RTU_CTRL    | 0x00    | W         | Bit 0: start               |
| RTU_STATUS  | 0x01    | R         | Bit 0: busy, Bit 1: done   |
| RAY_OX-OZ   | 0x04-06 | W         | Ray origin (Q16.16)        |
| RAY_DX-DZ   | 0x07-09 | W         | Ray direction               |
| RAY_TMIN    | 0x0A    | W         | Min t                       |
| RAY_TMAX    | 0x0B    | W         | Max t                       |
| BVH_ROOT    | 0x0C    | W         | Root node address           |
| HIT_T       | 0x10    | R         | Hit distance                |
| HIT_U/V     | 0x11-12 | R         | Barycentrics                |
| HIT_TRI_ID  | 0x13    | R         | Triangle ID                 |
| HIT_VALID   | 0x14    | R         | 1 if hit found              |
| PERF0-1     | 0x18-19 | R         | Nodes visited, tris tested  |
| INV_DX-DZ   | 0x1A-1C | W         | Precomputed 1/dir           |

---

## 2. SIMT/SIMD Hybrid Enhancements

### 2.1 Warp Scheduler (`warp_scheduler.sv`)

Multi-warp hardware thread scheduler enabling SIMT-style latency hiding:

- **N_WARPS** parameter (default 4, degenerates to passthrough at 1)
- Per-warp states: `FREE → READY → RUNNING → STALLED`
- Round-robin selection with priority
- Yield reasons: cache miss, RT pending, barrier, membar, resource, voluntary
- Wake interface: external signal unblocks stalled warps

### 2.2 Reservation Station (`reservation_station.sv`)

Out-of-order issue staging:

- **RS_DEPTH**: 8 entries
- **N_ISSUE_PORTS**: 2 (dual-issue)
- Writeback wakeup snooping (2 broadcast ports, CAM match)
- Oldest-ready issue policy (no double-select)
- Per-warp flush support
- FU types: ALU (0), LSU (1), FP (2), VEC (3), GFX (4), RT (5)

These modules provide the infrastructure for future integration into the
compute unit pipeline's decode → execute path.

---

## 3. Multi-CU Cluster (`cluster_top.sv`)

### 3.1 Parameters

| Parameter         | Default | Description                      |
|------------------|---------|----------------------------------|
| `NUM_CUS`        | 2       | 2 or 4 compute units             |
| `CLUSTER_ID`     | 0x01    | 8-bit cluster identifier         |
| `RT_ENABLE`      | 1       | Instantiate shared RTU           |
| `MAILBOX_ENABLE` | 1       | Enable mailbox fabric            |
| `GFX_ENABLE`     | 1       | Enable graphics pipeline in CUs  |
| `L2_SIZE_BYTES`  | 16384   | Shared L2 cache size             |
| `L2_LINE_BYTES`  | 64      | L2 cache line size               |
| `L2_ASSOC`       | 4       | L2 associativity                 |
| `L2_WRITEBACK`   | 0       | 0=write-through, 1=write-back    |

### 3.2 Block Diagram

```
┌──────────────────────────────────────────────┐
│                 cluster_top                   │
│                                              │
│  ┌──────┐  ┌──────┐  ┌──────┐  ┌──────┐     │
│  │ CU 0 │  │ CU 1 │  │ CU 2 │  │ CU 3 │     │
│  └──┬───┘  └──┬───┘  └──┬───┘  └──┬───┘     │
│     │ D$       │ D$       │ D$       │ D$     │
│  ┌──┴──────────┴──────────┴──────────┴──┐    │
│  │        Shared L2 Data Cache          │    │
│  │   (NUM_CUS+1 ports if RT_ENABLE)     │    │
│  └──────────────┬───────────────────────┘    │
│                 │ AXI4                        │
│  ┌──────────────┴──┐   ┌────────────────┐    │
│  │  RTU (shared)   │   │  Mailbox Fabric│    │
│  │  ray_tracing_   │   │  2CU: xconnect │    │
│  │  unit           │   │  4CU: ring     │    │
│  └─────────────────┘   └────────────────┘    │
│                                              │
│  ┌────────────┐  ┌──────────────────────┐    │
│  │ I-Cache    │  │  Framebuffer AXI     │    │
│  │ Miss Arb   │  │  Arbiter (RR)        │    │
│  │ (RR, 1-out)│  │  AW+W+B FSM         │    │
│  └─────┬──────┘  └──────────┬───────────┘    │
└────────┼────────────────────┼────────────────┘
         ↓                    ↓
    I-cache miss         FB AXI master
    (to system)          (to VRAM)
```

### 3.3 Shared Resources

1. **L2 Data Cache**: Multi-port (one per CU + one for RTU). Way partitioning
   isolates CU working sets. QoS-aware arbitration.

2. **Ray Tracing Unit**: Single shared instance. Gets its own L2 port
   (`RTU_L2_PORT = NUM_CUS`). Memory-mapped register access from host.

3. **I-Cache Miss Arbiter**: Round-robin, one outstanding miss at a time.
   Routes response back to the requesting CU.

4. **Framebuffer AXI Arbiter**: Round-robin with full AW→W→B handshake FSM.
   Ensures atomic write transactions per CU.

5. **Mailbox Fabric**:
   - 2-CU mode: Direct cross-connect (CU0↔CU1)
   - 4-CU mode: Ring topology (CU[i]→CU[(i+1)%4])

### 3.4 CU Configuration

Each CU receives:
- `CORE_ID = 0..NUM_CUS-1` (readable via CSR 0xC00)
- `MAILBOX_SRC_ID = {CLUSTER_ID[3:0], CU_index[3:0]}`

### 3.5 Reconfigurability

Feature enables allow FPGA-optimized configurations:

| Configuration        | RT | MB | GFX | Use Case              |
|---------------------|----|----|-----|-----------------------|
| Full GPU            | ✓  | ✓  | ✓   | Ray-tracing + raster  |
| Compute-only        | ✗  | ✓  | ✗   | GPGPU workloads       |
| Graphics-only       | ✗  | ✓  | ✓   | Rasterization only    |
| RT accelerator      | ✓  | ✗  | ✗   | Dedicated RT engine   |

---

## 4. File Inventory

| File                          | Lines | Description                    |
|-------------------------------|-------|--------------------------------|
| `ip/compute unit/rt_pkg.sv`              | ~120 | RT types, constants, reg map  |
| `ip/compute unit/ray_intersect_aabb.sv`  | ~230 | 6-stage AABB intersection     |
| `ip/compute unit/ray_intersect_tri.sv`   | ~350 | 12-stage triangle intersection|
| `ip/compute unit/bvh_traversal_engine.sv`| ~400 | BVH traversal FSM             |
| `ip/compute unit/ray_tracing_unit.sv`    | ~200 | RTU top with registers        |
| `ip/compute unit/warp_scheduler.sv`      | ~200 | SIMT warp scheduler           |
| `ip/compute unit/reservation_station.sv` | ~250 | Issue staging station          |
| `ip/compute unit/cluster_top.sv`         | ~700 | Multi-CU cluster integration  |
| `testbench/cluster_rt_tb.sv`             | ~560 | Cluster testbench             |
| `scripts/run_cluster_rt_verilator.sh`    | ~45  | Verilator build script        |

---

## 5. Build & Test

```bash
# Compile and run cluster testbench (WSL with Verilator 5.036)
bash scripts/run_cluster_rt_verilator.sh

# Output: cluster_frame.ppm (64×64 pixel fill, 2-CU alternating rows)
```

The testbench instantiates a 2-CU cluster with RT enabled. Each CU fills
alternate framebuffer rows (CU0=red, CU1=blue) to verify shared L2 and
I-cache arbitration.

---

## 6. Integration Roadmap

1. **Warp scheduler integration**: Insert into compute_unit_top between
   fetch and decode, multiplexing PC and register file access per warp.

2. **Reservation station integration**: Place between decode and execute,
   replacing current scoreboard stall logic with dynamic scheduling.

3. **RTU software driver**: Memory-mapped CSR writes from CU software to
   program rays and poll results (currently via external register interface).

4. **Full mailbox fabric**: Replace simplified cross-connect/ring with
   mailbox_switch_2x1/4x1 using stream-to-AXI bridges at CU endpoints.

5. **4-CU validation**: Test with NUM_CUS=4 configuration and verify
   ring mailbox, 5-port L2, and quad-CU arbitration correctness.
