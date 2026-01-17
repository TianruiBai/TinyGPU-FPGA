# **Scalable Heterogeneous Architecture Plan**

> **Scope note:** This document describes a **multi-core roadmap**. The current RTL in this repo centers on a single compute unit (`ip/compute unit/compute_unit_top.sv`) with a local/global LSU split. Components such as the Wishbone XBAR, L2 cache, and heterogeneous cores are **planned** unless explicitly marked **as-built** below.

## **1\. System Architecture Overview**

The system evolves into a **Heterogeneous Compute Cluster** inspired by the **IBM Cell Broadband Engine**. It features a central **Control Processor (PPE-equivalent)** managing a fabric of specialized **Processing Elements (SPE-equivalents)**.

### **1.1 Top-Level Block Diagram**

graph TD  
    %% External Interfaces  
    Host\[ESP32-S3 Host\] \<==\>|OSPI/QSPI| Ingress\[Ingress Bridge\]  
    PSRAM\[External PSRAM 2MB-1GB\] \<==\>|Octal SPI| VRAM\_Ctrl\[VRAM Controller\]  
    Monitor\[Display\] \<==|HDMI/HUB75| DisplayEng\[Display Engine\]

    %% Internal Bus / Crossbar  
    subgraph "FPGA Fabric (Heterogeneous System Interconnect)"  
        Ingress \--\>|CMD FIFO| MCU\[RISC-V Control Core (PPE)\]  
          
        %% The Central Interconnect (Wishbone Crossbar)  
        XBAR((System Wishbone Crossbar))  
          
        %% Shared Last-Level Cache  
        L2Cache\[System L2 Cache\]  
          
        %% Masters connecting to XBAR  
        MCU \<==\> XBAR  
        Ingress \==\>|Fast DMA| XBAR  
          
        %% The Heterogeneous Processing Cluster  
        subgraph "Processing Cluster (SPEs)"  
            %% General Purpose  
            CU0\[Compute Unit 0\]  
            CU1\[Compute Unit 1\]  
              
            %% AI / Math Acceleration  
            MXU0\[Matrix/Tensor Core 0\]  
              
            %% Graphics Acceleration  
            RTU0\[Ray-Tracing Unit 0\]  
              
            %% Cluster-level aggregation  
            CU0 \--\> ClusterArb  
            CU1 \--\> ClusterArb  
            MXU0 \--\> ClusterArb  
            RTU0 \--\> ClusterArb  
            ClusterArb\[Cluster L1 Arbiter\]  
              
            %% Fast Control Path (Bypasses Wishbone for CSRs)  
            FastCtrl\[Control-Path Bus\]  
            MCU \==\> FastCtrl  
            FastCtrl \-.-\> CU0  
            FastCtrl \-.-\> CU1  
            FastCtrl \-.-\> MXU0  
            FastCtrl \-.-\> RTU0  
        end  
          
        ClusterArb \<==\> XBAR  
          
        %% Slaves connecting to XBAR  
        XBAR \<==\> L2Cache  
        L2Cache \<==\> VRAM\_Ctrl  
          
        %% Broadcast Channels (Per Core Type)  
        XBAR \==\>|Broadcast: General| CU0 & CU1  
        XBAR \==\>|Broadcast: Matrix| MXU0  
        XBAR \==\>|Reg Access| CSR\[System CSRs\]  
    end  
      
    DisplayEng \<==\> XBAR

  **As-built today (RTL):** one compute unit with a **local/global LSU split**. Local memory is selected when `addr[31]==0`, global when `addr[31]==1`. Multi-core interconnect, XBAR, and L2 cache are **future work**.

## **2\. Memory Map & Address Space (Refined)**

The physical address space is 32-bit. It distinguishes between **Unified VRAM** (Data), **Core-Type Specific IMEM** (Code), and **Fast-Path CSRs**.

| Range Start | Range End | Size | Target | Access Behavior |
| :---- | :---- | :---- | :---- | :---- |
| 0x0000\_0000 | 0x0000\_7FFF | 32 KB | **MCU TCM** | Private to RISC-V Control Core *(planned)*. |
| 0x0000\_8000 | 0x0000\_FFFF | 32 KB | **Local BRAM / Scratch** | Default local region *(as-built CU selects local when addr[31]==0)*. |
| 0x1000\_0000 | 0x1000\_FFFF | 64 KB | **System CSRs** | **Fast Path Access.** R/W by Host/MCU. Read-Only by Cores *(planned)*. |
| 0x2000\_0000 | 0x5FFF\_FFFF | 2 MB \- 1 GB | **VRAM (PSRAM)** | Shared Data. Cached by System L2. Configurable Size *(planned)*. |
| **Broadcast Windows** |  |  |  |  |
| 0x6000\_0000 | 0x6000\_FFFF | 64 KB | **Unified CU IMEM** | Broadcasts to all **Standard CUs** *(planned)*. |
| 0x6100\_0000 | 0x6100\_FFFF | 64 KB | **Matrix Core IMEM** | Broadcasts to all **Matrix/Tensor Cores** *(planned)*. |
| 0x6200\_0000 | 0x6200\_FFFF | 64 KB | **RT Core Config** | Broadcasts config/tables to all **RT Units** *(planned)*. |

## **3\. Scalable Core Microarchitecture**

To support heterogeneity, the "Compute Unit" interface is standardized, but the internal implementation varies by **Core Type**.

### **3.1 Standard Interface (All Cores)**

Every processing element (Standard CU, Matrix Core, RT Core) exposes:

1. **Memory Master:** For fetching data/instructions from VRAM/L2 (Wishbone or AXI-lite style).  
2. **Control Slave:** For Fast-Path CSR access (Start, Stop, Status).  
3. **Broadcast Slave:** For receiving instructions/config.  
4. **Core ID / Type ID:** Hardwired identification.

**As-built CU today:** exposes a single global memory request/response interface plus a local BRAM path (no system XBAR integration yet).

### **3.2 Core Types**

#### **Type A: Standard Compute Unit (CU)**

* **Role:** General Purpose, Shader Execution, Scalar Logic.  
* **ISA:** Custom TinyGPU RISC-V variant with Vector Extensions.  
* **Memory:** Private IMEM (SRAM).  
* **Usage:** Runs the main logic, control flow, and general shaders.

#### **Type B: Matrix/Tensor Core (MXU)**

* **Role:** Dense Math acceleration (AI/ML).  
* **Architecture:** Systolic Array (e.g., 8x8 FP16/INT8) or SIMD Tensor Unit.  
* **ISA:** Specialized VLIW or Command-Queue driven (e.g., MATMUL, CONV2D).  
* **Memory:** Large internal scratchpad (Weight Buffer, Accumulator).  
* **Usage:** The MCU or Standard CU dispatches "Matrix Jobs" to this core.

#### **Type C: Ray-Tracing Unit (RTU)**

* **Role:** Fixed-function acceleration for 3D graphics.  
* **Architecture:** Intersection Engine (Ray-Box, Ray-Triangle).  
* **Interface:** Memory-Mapped.  
* **Operation:**  
  1. Host/CU writes Ray and BVH\_Pointer to RTU registers.  
  2. RTU traverses BVH in VRAM (via L2 Cache).  
  3. RTU returns Hit/Miss and Barycentrics.

## **4\. System Interconnect & Memory Subsystem**

### **4.1 Wishbone Crossbar (Scalable)**

The interconnect uses a **Hierarchical Wishbone** design.

* **L1 Cluster Bus:** Aggregates traffic from CUs, MXUs, and RTUs locally.  
* **L2 System Bus:** Connects Cluster, MCU, and Display to Memory.

**Planned QoS policy:** Display reads highest priority, then ROP writes, then scalar/vector LSU, then TEX/command. Ensure no starvation via weighted round-robin.

### **4.2 Type-Aware Broadcast Router**

The interconnect logic is expanded to route writes based on destination address:

* Write to 0x6000\_xxxx $\\rightarrow$ we\_i on all **Type A (Standard)** cores.  
* Write to 0x6100\_xxxx $\\rightarrow$ we\_i on all **Type B (Matrix)** cores.  
* Write to 0x6200\_xxxx $\\rightarrow$ we\_i on all **Type C (RT)** cores.

**Note:** broadcast windows must be **write-only** to cores; reads should return from a debug mirror (core0) or be undefined.

### **4.3 System L2 Cache**

Essential for the RT Core (which does irregular memory reads for BVH traversal) and Matrix Core (which streams large weight tensors).

* **Size:** 128KB \- 256KB.  
* **Line Size:** 64 Bytes (Matches PSRAM burst).  
* **Associativity:** 4-way.

## **5\. RISC-V Control Processor Role (The PPE)**

The MCU acts as the "Traffic Cop" for this heterogeneous city.

### **5.1 Discovery**

At boot, the MCU iterates through the **Control-Path Bus** to discover the hardware topology.

* Reads 0x1000 \+ (CoreID \* 0x10) \+ Offset 0: **CORE\_TYPE** Register.  
* Builds a resource table (e.g., "Cores 0-3 are CUs, Core 4 is Matrix, Core 5 is RT").

**Contract:** CORE\_TYPE should encode {class, revision, capabilities} so firmware can feature-detect FP/VDOT/VCROSS support.

### **5.2 Task Scheduling**

* **Graphics Task:** Assigned to Standard CUs.  
* **AI Inference:** MCU loads weights to VRAM, signal Matrix Core to process.  
* **Hybrid Rendering:**  
  * Standard CU calculates ray generation.  
  * Standard CU offloads intersection test to RTU (via shared memory queue or direct interconnect if tightly coupled).

## **6\. MPU (Memory Protection Unit)**

Protection is now type-aware:

* **Standard CUs:** Allowed executable access to 0x6000\_xxxx.  
* **Matrix Cores:** Allowed executable access to 0x6100\_xxxx.  
* **Global:** All cores denied write access to any 0x6... region (Code).

**As-built note:** MPU is not present in current RTL; enforce in the system interconnect or MCU firmware until hardware exists.

### **6.1 CU-local MPU (iMPU + dMPU)**
- **iMPU (fetch_unit):** validates PC against Broadcast IMEM or VRAM execute window. Invalid fetch injects `TRAP` and sets `FAULT_EXEC`.
- **dMPU (lsu):** validates loads/stores. Invalid access squashes the request and sets `FAULT_LOAD/FAULT_STORE`.
- **Kernel write-protect:** `MPU_KREGION_BASE..LIMIT` is read/execute only.

### **6.2 Multi-CU CSR integration**
- **Global MPU CSRs** are broadcast to all CUs via the control-path bus (`MPU_KREGION_BASE/LIMIT`, `MPU_CTRL`).
- **Per-CU fault CSRs** are exposed in a banked MMIO window (`CU_FAULT_STATUS/ADDR/PC`).
- **Interrupt aggregation:** any CU fault raises `INT_STATUS.MPU_FAULT`.

See [docs/system_contracts.md](docs/system_contracts.md) for the normative register map.

## **7\. Implementation Roadmap**

### **Phase 1: The Foundation**

* Implement **Standard CU** and **System Interconnect** (Wishbone \+ L2 \+ Broadcast).  
* Verify "One Instruction Write, Multiple Cores Execute" flow.
* Add **CU-local** IMEM window + broadcast write path with simple test (same program on 2 CUs).

### **Phase 2: Matrix Expansion**

* Design **Matrix Core (MXU)** with a simplified instruction set (Load/Store/MAC).  
* Map MXU IMEM to 0x6100\_xxxx.  
* Verify parallel execution of General Logic (CU) and Matrix Math (MXU).
* Define **job queue** ABI: descriptor format, base/stride, and completion fence.

### **Phase 3: Ray Tracing Future**

* Design **RT Unit** as a slave peripheral first (Memory mapped intersection tester).  
* Later upgrade to a Bus Master that can traverse linked lists (BVH) in VRAM independently.
* Define **BVH memory layout** and cacheability rules (L2 vs uncached).