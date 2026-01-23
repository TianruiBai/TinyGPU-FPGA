# **AXI‑MailboxFabric Specification**

## **1\. Concept Overview**

The **AXI‑MailboxFabric** is a dedicated, low-latency "Network-on-Chip" (NoC) designed for high-speed synchronization and small data transfers between Processing Elements (PEs) and the Control MCU.

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

## **2\. AXI-MailboxFabric Protocol (AXI-Stream hybrid)**

The interconnect has been redesigned into the **AXI‑MailboxFabric**: a hybrid that uses the physical simplicity of **AXI-Stream** (Valid/Ready/Data) while enforcing the routing semantics of **AXI-Lite** (explicit Destination/CSR index). The goal is low-latency, single-cycle hops with minimal control overhead.

### **2.1 Hierarchical & Aggregate Addressing (with CSR index)**

Destination remains 16 bits carrying Cluster, Endpoint, and a 4-bit CSR index. Split into **Cluster ID \[15:8\]**, **Endpoint ID \[7:4\]** (up to 16 endpoints per cluster), and **CSR index \[3:0\]** (16 32-bit words per endpoint for data/CSRs). CSR index 0 is the data channel (RX ring pop on read, enqueue on write); indices 1–15 are user/control/status registers that read directly (no pop).

**Address Format (word-aligned, 32-bit words):** 0x7000\_{ClusterID\[7:0\]}{EndpointID\[3:0\]}{CsrIdx\[3:0\]}

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

*Note:* We removed byte-addressable subword write support — mailbox transfers are 32-bit word oriented. Software should address 32-bit words using the CSR index; subword operations must be performed by software reading/modifying the 32-bit CSR or using masked payload conventions.

*Example:* Writing to 0x7000_0448 targets Cluster 0x01 (Compute), Endpoint 0x1, CSR index 0x2 (32-bit word).

---

### **2.2 Packet Structure (mailbox_packet_t / mailbox_flit_t)**

Packets are "fire-and-forget" and carry their routing fields and metadata with the flit. This allows single-beat transactions and removes a separate Address Phase.

```systemverilog
package mailbox_pkg;
  localparam int DATA_WIDTH = 32;
  localparam int NODE_ID_WIDTH = 16; // 256 x 16 clusters / endpoints

  typedef struct packed {
    logic [NODE_ID_WIDTH-1:0] src_id;   // Who sent this? (For reply)
    logic [3:0]               opcode;   // DATA, IRQ, ACK, ERROR
    logic [1:0]               prio;     // 0=Low, 3=Critical/NMI
    logic                     eop;      // End of Packet (for multi-flit msgs)
    logic                     debug;    // 1 = Trace this packet
  } mailbox_header_t;

  typedef struct packed {
    mailbox_header_t          hdr;
    logic [DATA_WIDTH-1:0]    payload;
  } mailbox_flit_t;

endpackage : mailbox_pkg
```

Key points:

* All routing metadata required to deliver and interpret the packet travels with the flit.  
* `opcode` encodes DATA/IRQ/ACK/ERROR semantics.  
* `prio` is 2-bit to allow several prioritized classes (including a critical NMI class).  
* Multi-flit messages are supported (EOP marks the last flit).

---

### **2.3 Link Interface (mailbox_link_if)**

Each unidirectional link is an AXI-Stream-like interface optimized for look-ahead routing: the `dest_id` is carried *separately* and is valid when `valid` is asserted.

```systemverilog
interface mailbox_link_if (input logic clk, input logic rst_n);
  import mailbox_pkg::*;

  logic                     valid;    // "I have data"
  logic                     ready;    // "I can accept data" (Backpressure)

  mailbox_flit_t            data;     // Payload + Header
  logic [NODE_ID_WIDTH-1:0] dest_id;  // Look-Ahead Routing ID

  modport source (
    output valid, data, dest_id,
    input  ready
  );

  modport sink (
    input  valid, data, dest_id,
    output ready
  );

  modport monitor (
    input valid, ready, data, dest_id
  );
endinterface
```

Design choice: `dest_id` separate from `data` enables the Switch to perform look-ahead routing and set crossbars/selects before the full flit is latched — saving a clock cycle.

---

### **2.4 Dual-Role Endpoint (TX + RX)**

Endpoints are dual-role: they expose one TX (egress) port and one RX (ingress) port for full-duplex operation. If SystemVerilog `interface`s are not convenient, the raw signal list follows the same direction rules (see `mailbox_endpoint` port list in the spec).

Essentials:

* TX (Source): `tx_valid`, `tx_ready`, `tx_data`, `tx_dest_id`, `tx_src_id`, `tx_opcode`, `tx_prio`.
* RX (Sink): `rx_valid`, `rx_ready`, `rx_data`, `rx_dest_id`, `rx_src_id`, `rx_opcode`.
* IRQ: `irq_new_msg` asserted when RX FIFO is non-empty.

This separation allows the core to send while simultaneously receiving messages.

---

### **2.5 Flow Control & the "Must-Sink" Rule**

Handshake is standard: transaction completes when `valid && ready` on the hop. Backpressure is expressed by deasserting `ready`.

Crucially, to avoid network-wide deadlock when a core stops servicing its mailbox, *endpoints must implement a "Must-Sink" policy.* In short: an endpoint's `rx_ready` is not merely `!rx_fifo_full`. If the core ignores the mailbox for an extended period, the endpoint should (configurably) drop or accept-and-discard packets and raise an error/IRQ rather than permanently stall the fabric.

Example behaviour (conceptual):

```systemverilog
assign rx_ready = !rx_fifo_full; // standard

// Safety valve: after N cycles of rx_fifo_full while rx_valid==1, raise an error
logic [7:0] stall_counter;
always_ff @(posedge clk) begin
  if (rx_valid && rx_fifo_full)
    stall_counter <= stall_counter + 1;
  else
    stall_counter <= 0;

  if (stall_counter == 8'hFF) begin
    error_irq <= 1'b1; // notify software
    // optional: accept and drop new flits to avoid backpressure
  end
end
```

This prevents an unresponsive core from causing systemic deadlock.

---

### **2.6 Channel Semantics & Operational Notes**

* Fire-and-Forget: There is no separate write-response (`BVALID`) channel. Once `tx_ready` is asserted on the handshake cycle, the write is accepted. Use `ACK` opcode if software-level confirmation is required.  
* Look-Ahead Routing: `dest_id` is valid together with `valid` so switches can steer the packet immediately.  
* No byte-enable semantics: mailbox traffic is word-oriented (32-bit). Partial updates must be handled in software.  
* Broadcast/Multicast: same semantics as before (Cluster/Group/Global). The Center replicates and each switch performs local replication; delivery is flow-controlled (no hardware drops).  
* Priority: `prio` field is used by switches/center for QoS.  


<!-- End of new protocol section -->


## **3\. Hardware Implementation**

### **3.1 Routing Logic (The "Smart Switch")**

Each **Mailbox Switch** contains a simple routing table logic to keep traffic local.

Inputs: Packet from Core $i$ (Source).

Destination: supplied as a separate `dest_id` sideband from the LSU (look-ahead routing) or derived from a write to 0x7000_xxxx; `dest_id` is valid together with `valid` so switches can steer packets immediately.

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
   * LSU detects write to 0x7000\_XXXX and instantiates a flit with a separate `dest_id` sideband for look-ahead routing.  
   * Pushes Data \+ DestID into **TX FIFO** (Depth 8).  
   * Stalls core if FIFO full.  
2. **RX Path (CSR Hook):**  
   * Incoming messages fill **RX FIFO** (Depth 8).  
   * Asserts IRQ_MAILBOX if not empty.  
   * Read via CSR\_MAILBOX\_RX (0xF08).

**Endpoint rules:**

* IRQ is level-triggered until RX FIFO drains.  
* If RX FIFO is full, `rx_ready` should be deasserted to apply backpressure. Additionally, endpoints SHOULD implement the safety-valve policy (see Section 2.5) to optionally accept-and-discard or drop packets after a configurable stall threshold, preventing network-wide deadlocks.  
* Ordering is preserved per source/destination; route-lock keeps bursts contiguous.  
* Mark 0x7000_xxxx as strongly-ordered, non-cacheable to avoid store merging/reordering.  
* Invalid DestID: drop and increment an error counter; optional local IRQ for diagnostics.  
* Optional ACK/NACK via Opcode for software that needs reliability.
* Reads via `CSR_MAILBOX_RX` (0xF08) pop the RX FIFO; if empty, the read returns 32'hDEADBEEF.

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

