# Memory Map & Address Space

The TinyGPU architecture designed for the Infineon S80KS5123 (64MB Octal xSPI PSRAM).

## 1. System Physical Address (Core View)
This is the unified 32-bit address map visible to the RISC-V MCU and the Compute Units.

**RTL note (as-built):** the compute unit LSU selects **local memory** when `addr[31]==0` and **global memory** when `addr[31]==1`. The current testbenches use `0x8000_0000+` for global and `0x0000_0000+` for local.

| Range Start | Range End | Size | Description |
| :--- | :--- | :--- | :--- |
| `0x0000_0000` | `0x0000_7FFF` | 32 KB | **MCU Internal RAM (ITCM/DTCM)** <br> Firmware Code + Stack *(planned)* |
| `0x0000_8000` | `0x0000_FFFF` | 32 KB | **Local BRAM / Scratch** *(as-built default local region)* |
| `0x1000_0000` | `0x1000_0FFF` | 4 KB  | **Hardware Registers (CSRs)** *(planned)* |
| `0x1000_1000` | `0x1000_1FFF` | 4 KB  | **Ingress Mailbox (FIFO)** *(planned)* |
| `0x8000_0000` | `0x8FFF_FFFF` | 256 MB | **Global/VRAM window** *(RTL selects global when addr[31]==1; size depends on integration)* |
| `0x3000_0000` | `0x3000_FFFF` | 64 KB | **Compute Unit Instruction RAM (CU_IMEM)** *(planned)* |

---

## 2. Compute Unit Customization (Accelerator Mode)
To support both standard graphics and custom acceleration, the Compute Units (CUs) support a **Dual-Mode Instruction Fetch**:

### Mode A: GPU Mode (VRAM Fetch)
- **Behavior:** CUs behave like standard GPU cores.
- **Fetch Path:** Instructions are fetched from **VRAM** (`0x2000_0000`) via the L1/L2 Cache.
- **Use Case:** Large shader programs, typical rendering pipelines.

### Mode B: Accelerator Mode (TCM Fetch)
- **Behavior:** CUs execute from a dedicated, high-speed On-Chip SRAM (Tightly Coupled Memory).
- **Fetch Path:** Instructions are fetched from **CU_IMEM** (`0x3000_0000`).
- **Use Case:** High-performance custom algorithms (FFT, Matrix Mul) requiring deterministic latency and high bandwidth instruction access.

### Accessing CU_IMEM
The `0x3000_0000` range is special.
- **Writes:** Broadcast to **ALL** Compute Units. (Writing code here updates the program for the entire cluster simultaneously).
- **Reads:** Return data from CU #0 (Debugging only).
- **Host Access:** The Host (ESP32) can write to this region using the standard `WRITE_MEM` command, targeting address `0x3000_xxxx`.

---

## 3. Register Bank (CSRs) - Mapped at `0x1000_0000`

Accessed by MCU (LW/SW) or Host (REG_WRITE packet).

| Offset | R/W | Name | Description |
| :--- | :--- | :--- | :--- |
| `0x000` | R/W | `GPU_STATUS` | [0]: Idle, [1]: FIFO Full, [2]: IRQ Pending |
| `0x004` | R/W | `GPU_CONTROL`| [0]: Reset, [1]: Enable Display, [2]: Enable CUs |
| `0x008` | R/W | `FENCE_VAL`  | Current completed fence ID |
| `0x100` | R/W | `DISP_ADDR`  | Framebuffer VRAM Start Address |
| `0x104` | R/W | `DISP_RES`   | [31:16] Height, [15:0] Width |
| `0x108` | R/W | `DISP_CONFIG`| **[1:0]: Mode (0=Off, 1=HUB75, 2=HDMI)** <br> [3:2]: HUB75 Chain Len (0=1, 3=4 panels) |
| `0x200` | R/W | `CU_CONTROL` | **[0]: Mode (0=GPU/VRAM, 1=Accel/IMEM)** <br> [1]: Single Step, [2]: Pause |
| `0x204` | R/W | `CU_PC_START`| Initial Program Counter (Virtual Address) |
| `0x208` | R/W | `CU_MASK`    | Active Compute Units bitmap (default 0xF) |

---

## 4. Host Aperture (Host View)
The ESP32 sees the FPGA as an OSPI Slave device.

**Concept:** To access the massive VRAM, the Host writes to a specific OSPI command to select an operation mode:
- **Mode A (Command):** Writes go to the `Ingress Mailbox`.
- **Mode B (VRAM Direct):** Writes go to `VRAM` (offset 0 -> `0x2000_0000` + Address).
