# **Mailbox Interconnect Specification**

## **1\. Concept Overview**

The **Mailbox Interconnect** is a dedicated, low-latency "Network-on-Chip" (NoC) designed for high-speed synchronization and small data transfers between Processing Elements (PEs) and the Control MCU.

Architecture: Hierarchical Tree Topology

To optimize for physical layout and timing closure (FPGA/ASIC), the network is split into three tiers:

1. **Mailbox Center (Root):** The main backbone connecting high-level subsystems (MCU, Memory, Clusters). It manages traffic *between* clusters.  
2. **Mailbox Switch (Regional):** Aggregates traffic for a local cluster of cores (e.g., "Compute Cluster Switch"). **Crucially, it handles intra-cluster traffic locally without involving the Center.**  
3. **Mailbox Endpoint (Leaf):** The FIFO interface embedded directly into each Core.

**Key Features:**

* **Zero-Copy:** Data moves directly from Register File $\\rightarrow$ Network $\\rightarrow$ Register File.  
* **Local Routing:** Traffic between cores in the same cluster (e.g., CU0 $\\to$ CU1) never consumes backbone bandwidth.  
* **Hardware Flow Control:** Backpressure propagates from Endpoint $\\to$ Switch $\\to$ Center $\\to$ Sender.  
* **Burst-Capable:** The protocol supports streaming multiple words to the same destination efficiently.

## **2\. Addressing & Protocol**

The system leverages the **MPU's "Mailbox Mesh" region (0x7000\_XXXX)** to map network destinations.

### **2.1 Hierarchical & Aggregate Addressing (with CSR index)**

Destination is 16 bits carrying Cluster, Endpoint, and a 4-bit CSR index. Split into **Cluster ID \[15:8\]**, **Endpoint ID \[7:4\]** (up to 16 endpoints per cluster), and **CSR index \[3:0\]** (16 words per endpoint for data/CSRs). CSR index 0 is the data channel (RX ring pop on read, enqueue on write); indices 1–15 are user/control/status registers that read directly (no pop).

**Address Format (byte aligned):** 0x7000\_{ClusterID\[7:0\]}{EndpointID\[3:0\]}{CsrIdx\[3:0\]}

| Cluster ID | Endpoint ID | Target |
| :---- | :---- | :---- |
| 0x00 | 0x0 | **MCU (Control Processor)** |
| 0x00 | 0x1 | **Memory Controller** (DMA IRQ) |
| 0x00 | 0x2 | **Display Engine** (VSYNC IRQ) |
| 0x01 | 0x0 \- 0x7 | **Compute Cluster** (CUs 0-7) |
| 0x02 | 0x0 \- 0x7 | **Matrix Cluster** (MXUs 0-7) |
| ... | ... | Future clusters/cores up to 256×16 endpoints |
| **Multicast** |  |  |
| 0x01 | 0xF | **Cluster 1 Broadcast** (All CUs in Cluster 1) |
| 0x02 | 0xF | **Cluster 2 Broadcast** (All MXUs in Cluster 2) |
| 0xFF | 0x0 | **Group 0 Broadcast** (Endpoint 0 of every Cluster) |
| 0xFF | 0xF | **Global Broadcast** (All Nodes) |

*Example:* Writing to 0x7000_0102 targets Cluster 0x01 (Compute), Endpoint 0x1, CSR index 0x2. Writing to 0x7000_01F0 targets **ALL** endpoints in Cluster 1 (broadcast, CSR index 0).

### **2.2 Packet Format**

Goal: keep width friendly to FPGA BRAM/URAM slices and reserve space for control/QoS.

* **Baseline (44-bit flit, 8-bit SrcID):** \[31:0\] Data, \[39:32\] SrcID, \[40\] EOP, \[41\] Prio (0=best-effort, 1=latency), \[42\] Parity, \[43\] Valid.  
* **Compact (42-bit flit):** Drop Prio+Parity if area/timing are tight.

Sideband (per transfer, not necessarily stored in flit): 4-bit `Opcode` from LSU/CSR (`DATA`, `IRQ`, `ACK`, `NACK`, `RSV`).

Handshake: `Valid`/`Ready` on every hop. `Ready` reflects downstream FIFO space; backpressure propagates.

### **2.3 Interconnect Bus Specification (Mailbox over AXI4-Lite, read + write full-duplex)**

Scope: point-to-point links between Endpoint↔Switch and Switch↔Center implemented as **full-duplex AXI4-Lite read + write** channels (two unidirectional write paths and two unidirectional read paths). Loads and stores to the mailbox address window are redirected into the mailbox network. All links are synchronous to the fabric clock; use CDC FIFOs if crossing domains.

**Channel model:**

* TX direction: source is AXI-Lite master issuing writes; sink is AXI-Lite slave.  
* RX direction: sink becomes master to return messages back (mirrored interface).  
* Read direction (pop semantics): source is AXI-Lite master issuing reads on `ar*`; sink returns data on `r*` with a single-beat response. Reads pop the target RX FIFO; if empty, return 32'hDEADBEEF. One outstanding read per ingress (except CSR reads) to keep the bus passive.  
* AW and W must be asserted together; B is the acceptance response. No bursting beyond single-beat writes (route-lock preserves multi-beat mailbox packets using `tag_eop`). Reads are single-beat and aligned to the mailbox word size.

**Signals per direction (TX or RX):**

* `clk`, `rst_n` — Shared clock/reset.  
* `awvalid/awready`, `awaddr[15:0]` — Address phase carrying Cluster/Endpoint/CSR index.  
* `wvalid/wready`, `wdata[31:0]`, `wstrb[3:0]` — Data phase. `wstrb` typically 4'hF.  
* `bvalid/bready` — Write response (OK only). Used as acceptance pulse; no error encoding.  
* `tag_src_id[7:0]`, `tag_eop`, `tag_prio`, `tag_parity`, `tag_opcode[3:0]`, `tag_hops[3:0]` — Mailbox sideband aligned with AW/W. Parity is even over `wdata|tag_src_id|tag_eop|tag_prio` when enabled.

**Signals per direction (read path AR/R):**

* `arvalid/arready`, `araddr[15:0]` — Address phase carrying Cluster/Endpoint/CSR index.  
* `rvalid/rready`, `rdata[31:0]`, `rresp` (always OKAY) — Data return (DEADBEEF if RX empty).  
* Sideband tags (optional on read return): implementations may echo the request tag or return implicit status; baseline keeps tags on write path and treats reads as single-word pulls of status/mailbox payload or control CSR fields.

**Handshake & timing:**

* Transfer occurs when `awvalid && wvalid && awready && wready`; `bvalid` must pulse for the same beat and deassert once `bready` is seen. Reads occur when `arvalid && arready` then complete on `rvalid && rready`; only one outstanding read per ingress (except CSR reads).  
* Slaves gate `awready/wready` (and `arready` for reads) to reflect FIFO space and broadcast fanout readiness.  
* Masters hold `awvalid/wvalid` until handshake; `bready` is typically tied high. Masters hold `arvalid` until `arready`; `rready` is typically tied high unless SW stalls consumption.  
* `tag_eop` marks last beat of a mailbox burst; route-lock holds from first beat until the beat after `eop` is accepted.  
* Reset: deassert `awvalid/wvalid`; `bvalid` must be 0 during reset.

**Error handling:**

* Parity error at slave: drop flit, increment error counter, optional IRQ.  
* Unsupported `opcode`: treat as DATA.  
* AXI-Lite BRESP is always OKAY; drops are surfaced via counters only.

**Transfer ordering and atomicity:**

* In-order per AXI-Lite write and read channel. Bursts are contiguous due to route-lock keyed by `tag_eop`.  
* Broadcast replication preserves order per child; if any child stalls, upstream master sees withheld `awready/wready` (and therefore no `bvalid`).

**QoS and distance-aware arbitration:**

* Priority tiers: latency class (`tag_prio`=1) is served ahead of best-effort, but cannot starve best-effort; recommend weighted RR with at least 1 BE grant per 4 total grants.  
* Distance/age: use `tag_hops` (saturating increment per hop) as a tiebreaker to favor older/further-traveled packets. If `tag_hops` not implemented, approximate distance by port class (local vs upstream).  
* Suggested arb order: (1) latency class, higher `tag_hops` first, RR among equals; (2) best-effort, higher `tag_hops` first, RR among equals; enforce BE minimum service (e.g., grant BE after at most 3 consecutive latency grants).  
* `tag_hops` optional; if absent, treat as zero and rely on `tag_prio` and BE minimum service.

**Multi-initiator (cross-cluster) handling:**

* Every ingress (each Leaf TX) has its own FIFO; Switch/Center arbiters choose among ingress FIFOs per output.  
* Route-lock is per destination output; bursts from one CU hold the output until `tag_eop` completes, but other destinations can still progress through other outputs.  
* Admission control: if an output is busy, upstream masters see withheld `ack`; no drops. Recommended ingress FIFO depth ≥4 (≥2 minimum) to absorb multi-initiator bursts while keeping logic small.  
* Fairness: weighted RR across ingress FIFOs with the QoS/distance rules above prevents any single CU from starving others when multiple CUs target the same remote cluster.  
* For broadcasts from many CUs, the Center aggregates backpressure; consider limiting outstanding broadcasts per CU in software if workloads are highly bursty.

**Physical/CDC guidance:**

* Single clock per link; add async FIFO if domains differ (keep AXI-Lite master on source side of FIFO).  
* Optional skid buffer or register slice on `awready/wready/bvalid` for timing in Switch↔Center paths.  
* Limit ready fanout by per-port FIFOs; avoid combinational backpressure trees.

**Endpoint↔Switch (minimal profile):**

* May omit `tag_prio` and `tag_parity` (Compact mode).  
* Endpoint masters issue writes for TX and reads for pop-style mailbox loads; Switch masters the reverse direction.  
* Byte enables fixed to 4'hF; reads are single-beat pulls; RX empty returns 32'hDEADBEEF.

**Switch↔Center (robust profile):**

* Keep `tag_prio` and `tag_parity` enabled; carry `tag_opcode` for diagnostics/ACK.  
* Per-child FIFO ≥4 to mask broadcast fanout; optional register slice on `awready/wready/bvalid`.

## **3\. Hardware Implementation**

### **3.1 Routing Logic (The "Smart Switch")**

Each **Mailbox Switch** contains a simple routing table logic to keep traffic local.

Inputs: Packet from Core $i$ (Source).

Destination: Extracted from the write address (provided by LSU sideband).

**Algorithm:**

* if (Dest\_Cluster \== My\_Cluster\_ID) begin  
*     // Local Shortcut  
*     Target\_Port \= Dest\_Local\_ID;   
*     Send\_To\_Neighbor(Target\_Port, Packet);  
* end else if (Dest\_Cluster \== 0xF) begin  
*     // Global Broadcast (Send Up AND Local)  
*     Send\_To\_Center(Packet);  
*     Broadcast\_Locally(Packet);  
* end else begin  
*     // Upstream  
*     Send\_To\_Center(Packet);  
* end

Burst Support:

If a core executes a burst store (or multiple stores to the same address), the Switch locks the route until EOP is seen. This prevents packet interleaving from different sources to the same destination, ensuring message atomicity.

**Flow Control & Arbitration**

* Per-port buffering: ≥2 flits per output (local0-3, upstream) to absorb arbitration jitter.  
* Handshake: Valid/Ready per hop. Ready deasserts when the target FIFO is full; backpressure propagates to sources.  
* Arbiter: Round-robin per output; route-lock bypasses the arbiter until `EOP` to keep bursts contiguous.  
* Optional head-of-line relief: If `Prio==1`, allow single-beat preemption after a programmable wait to avoid starvation.

### **3.2 Mailbox Center (Root Router)**

Connects the "System Cluster" (MCU) to the "Worker Clusters".

* **Logic:**  
  * If Dest.Cluster \== 0: Route to MCU/Peripherals.  
  * If Dest.Cluster \== 1: Route to Compute Cluster Switch.  
  * If Dest.Cluster \== 2: Route to Matrix Cluster Switch.  
  * If Dest.Cluster \== 0xF: Broadcast to ALL connected Switches.

**Broadcast semantics:** Replicate at the Center, then again at each Switch. Delivery is flow-controlled: if any child is not ready, backpressure stalls the source; hardware does not drop or timeout. No duplicate suppression at endpoints.

### **3.3 Mailbox Endpoint (Leaf)**

Integrated into compute\_unit\_top.sv.

1. **TX Path (LSU Hook):**  
   * LSU detects write to 0x7000\_XXXX.  
   * Pushes Data \+ DestID into **TX FIFO** (Depth 8).  
   * Stalls core if FIFO full.  
2. **RX Path (CSR Hook):**  
   * Incoming messages fill **RX FIFO** (Depth 8).  
   * Asserts IRQ_MAILBOX if not empty.  
   * Read via CSR\_MAILBOX\_RX (0xF08).

**Endpoint rules:**

* IRQ is level-triggered until RX FIFO drains.  
* If RX FIFO is full, Ready deasserts to the Switch; packets are stalled, not dropped.  
* Ordering is preserved per source/destination; route-lock keeps bursts contiguous.  
* Mark 0x7000_xxxx as strongly-ordered, non-cacheable to avoid store merging/reordering.  
* Invalid DestID: drop and increment an error counter; optional local IRQ for diagnostics.  
* Optional ACK/NACK via Opcode for software that needs reliability.
* Reads pop RX FIFO; empty returns 32'hDEADBEEF.

## **4\. Usage Examples (Software)**

### **4.1 Local Synchronization (CU0 $\\to$ CU1)**

Efficient barrier without disturbing the MCU. Traffic stays inside the Compute Cluster Switch.

* \# Core 1.0 (CU0) signals Core 1.1 (CU1)  
* LI   s1, 0xSYNC\_MSG  
* LI   s2, 0x70001100   \# Cluster 1, Local 1  
* SW   s1, 0(s2)        \# Routed locally by Switch  
*   
* 

### **4.2 Job Completion (CU $\\to$ MCU)**

* \# Core 1.X finishes  
* LI   s1, 0xDONE  
* LI   s2, 0x70000000   \# Cluster 0 (System), Local 0 (MCU)  
* SW   s1, 0(s2)        \# Routed Up to Center \-\> MCU  
*   
* 

### **4.3 Cluster Broadcast (MCU $\\to$ All CUs)**

* \# Wake up all CUs in Cluster 1  
* LI   s1, 0xWAKEUP  
* LI   s2, 0x70001F00   \# Cluster 1, Local F (Broadcast)  
* SW   s1, 0(s2)

## **5\. Integration Roadmap**

### **Phase 1: Endpoint Design**

* Implement mailbox\_endpoint.sv: The FIFO wrapper for the LSU.
* Expose TX Ready/Valid to the Switch; expose RX level interrupt and status (depth, overflow flag) in CSR.

### **Phase 2: Switch Design**

* Implement mailbox\_switch\_4x1.sv: 4 Local Ports \+ 1 Upstream Port.  
* Verify local routing (Port 0 $\to$ Port 1) and upstream routing.  
* Add per-output RR arbiter, per-output FIFO (≥2), and enforce route-lock on bursts.

### **Phase 3: Center Design**

* Implement mailbox\_center.sv: Connects MCU, Peripherals, and Cluster Switches.
* Add broadcast replication and backpressure gating across children; optional parity check if enabled.

### **Phase 4: Cluster Integration**

* Instantiate mailbox\_switch inside compute\_cluster.sv.  
* Connect it to the 4 CUs.  
* Directed tests: unicast, broadcast, route-lock bursts, RX overflow, invalid DestID, high-prio preemption.

### **Phase 5: Verification & QoS**

* Randomized traffic (mixed unicast/broadcast) under congestion; ensure in-order per flow and no drops.  
* Latency/throughput sweeps with and without `Prio==1`.  
* Fault injection: parity flip (if enabled), force RX full to confirm backpressure.  
* Software tests: barriers, wakeups, completion interrupts, and ACK/NACK flows.

