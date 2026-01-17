# Control Firmware (RISC-V)

The RISC-V firmware is the "Kernel" of the GPU. It runs a tight loop reading the Command FIFO and configuring the hardware accelerators.

## Responsibilities

1.  **Command Execution (Slow Path):**
    -   Reads 32-bit packets from `Ingress Mailbox`.
    -   Decodes Opcode (`BIND`, `DRAW`, `DISPATCH`).
    -   **BIND:** Updates internal shadow registers or loads state from VRAM to CSRs.
    -   **DRAW/DISPATCH:** Trigger the hardware "Work Distributor" to launch jobs to Compute Units.
    -   **FENCE:** Updates the Fence Counter when the pipeline is empty.

2.  **Resource Management:**
    -   Manages VRAM allocation tables (if dynamic).
    -   Validates addresses to ensure security (Command Buffer must be inside Safety Window).

3.  **Error Handling (Watchdog):**
    -   detects if a Compute Unit hangs (e.g., infinite loop shader).
    -   Resets the CU and signals error to Host.

## Main Loop Pseudo-code

```c
void main() {
    hw_init();
    
    while(1) {
        // 1. Fetch Command
        if (FIFO_EMPTY) {
            wait_for_interrupt();
            continue;
        }
        uint32_t header = FIFO_POP();
        uint8_t opcode = header >> 24;
        
        // 2. Decode & Execute
        switch(opcode) {
            case OP_REG_WRITE:
                uint32_t val = FIFO_POP();
                CSR_WRITE(header & 0xFFFFFF, val);
                break;
                
            case OP_BIND_PIPE:
                load_pipeline_state(header & 0xFFFFFF);
                break;
                
            case OP_DRAW:
                uint32_t count = header & 0xFFFFFF;
                uint32_t first = FIFO_POP();
                hw_kick_draw(current_pipeline, count, first);
                break;
                
            case OP_FENCE:
                while(hw_is_busy()); // Wait for HW
                CSR_WRITE(REG_FENCE_VAL, header & 0xFFFFFF);
                break;
        }
    }
}
```

## Boot Sequence
1.  **Reset:** MCU starts from Boot ROM.
2.  **Hardware Init:** 
    -   Configures specific PSRAM registers (Latency Code, Burst Length).
    -   Performs VRAM Training/Calibration if necessary.
3.  **Shadow Load:** Checks OSPI "Shadow Region" (or I2C EEPROM) for user firmware.
4.  **Init:** Sets up Display Timing to default 640x480.
5.  **Ready:** 
    -   Sets `GPU_STATUS` bit [0] (Ready). 
    -   **Important:** Host must poll this bit before attempting "Fast Path" VRAM writes.

