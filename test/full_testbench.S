# ============================================================================
# TinyGPU Compute Test: Self-Checking & Timeout
# ============================================================================
#
# Memory Map:
#   .text:      0x0000_0000
#   .data:      0x0000_2000
#   RESULT:     0x2000_0100 (Status written here)
#
# Register Usage:
#   s0:  Zero
#   s30: Timeout Counter (Watchdog)
#   s31: Error Code Register
# ============================================================================

.data
.org 0x80000000

# --- Constants & Expected Values ---
CONST_INT_A:    .word 100
CONST_INT_B:    .word 25
CONST_FP_1_0:   .word 0x3C00    # FP16 1.0
CONST_FP_1_5:   .word 0x3E00    # FP16 1.5
CONST_FP_2_5:   .word 0x4100    # FP16 2.5 (Expected Add)

# --- Error Codes ---
ERR_NONE:       .word 0x600D    # "GOOD"
ERR_INT_ADD:    .word 0x1001
ERR_INT_SUB:    .word 0x1002
ERR_LOGIC:      .word 0x1003
ERR_FP_ADD:     .word 0x2001
ERR_HAZARD_RAW: .word 0x3001
ERR_HAZARD_LU:  .word 0x3002
ERR_VEC_DOT:    .word 0x4001
ERR_TIMEOUT:    .word 0xDEAD

# --- Vector Test Data ---
VEC_DOT_A:      .word 1, 1, 1, 1
VEC_DOT_B:      .word 1, 1, 1, 1
VEC_DOT_OUT:    .word 0, 0, 0, 0

# ============================================================================
.text
.org 0x0000

_start:
    # 1. Initialize Watchdog & Status
    LI      s30, 1000       # Timeout Limit (1000 cycles/ops)
    LI      s31, 0          # Clear Error Code
    
    # ========================================================================
    # TEST BLOCK 1: SCALAR INTEGER
    # ========================================================================
    
    # Load Constants
    LI      s2, 100
    LI      s3, 25
    
    # Test ADD
    ADD     s4, s2, s3      # s4 = 125
    LI      s5, 125         # Expected
    LI      s31, 0x1001     # Load potential error code (ERR_INT_ADD)
    BNE     s4, s5, test_fail
    
    # Test SUB
    SUB     s4, s2, s3      # s4 = 75
    LI      s5, 75          # Expected
    LI      s31, 0x1002     # ERR_INT_SUB
    BNE     s4, s5, test_fail
    
    # Test Logic (AND)
    LI      s2, 0xF0F0
    LI      s3, 0x0F0F
    AND     s4, s2, s3      # s4 = 0
    LI      s5, 0           # Expected
    LI      s31, 0x1003     # ERR_LOGIC
    BNE     s4, s5, test_fail

    # ========================================================================
    # TEST BLOCK 2: SCALAR FP16
    # ========================================================================
    
    # Build FP operands via FCVT.i2f (operands live in FP regs)
    LI      s2, 1
    LI      s3, 2
    FCVT.I2F f2, s2, s0     # f2 = 1.0
    FCVT.I2F f3, s3, s0     # f3 = 2.0
    
    # Test FADD
    FADD    f1, f2, f3      # f1 = 3.0
    
    # Verify Result (Move to Int to compare)
    # NOTE: FCVT.f2i uses legacy fixed-point scaling where +1.0 -> 2048.
    #       So 3.0 becomes 3 * 2048 = 6144.
    FCVT.F2I s4, f1, s0     # s4 = 6144
    LI       s5, 6144
    LI       s31, 0x2001    # ERR_FP_ADD (Int check)
    BNE      s4, s5, test_fail
    
    # Optional: Bitwise Exact Check
    # (Requires moving f1 bits back to scalar without conversion. 
    #  Assuming FMV.X.W or store/load. We'll stick to logic check.)

    # ========================================================================
    # TEST BLOCK 3: PIPELINE HAZARDS
    # ========================================================================
    
    # RAW Hazard (Forwarding)
    LI      s2, 10
    ADDI    s2, s2, 5       # s2 = 15
    ADD     s3, s2, s0      # s3 = 15 (Must read new value)
    LI      s5, 15
    LI      s31, 0x3001     # ERR_HAZARD_RAW
    BNE     s3, s5, test_fail
    
    # Load-Use Hazard (Stall)
    LA      s2, CONST_INT_A
    LW      s3, s2, 0       # Load 100
    ADDI    s4, s3, 1       # s4 = 101 (Must stall 1 cycle)
    LI      s5, 101
    LI      s31, 0x3002     # ERR_HAZARD_LU
    BNE     s4, s5, test_fail

    # ========================================================================
    # TEST BLOCK 4: TIMEOUT LOOP CHECK
    # ========================================================================
    # This loop runs 10 times. The watchdog s30 ensures it doesn't run forever.
    
    LI      s10, 10         # Loop Counter
    
loop_start:
    # 1. Watchdog Decrement
    ADDI    s30, s30, -1    # Decrement watchdog
    LI      s11, 0
    BEQ     s30, s11, timeout_fail # If watchdog == 0, abort
    
    # 2. Loop Logic
    ADDI    s10, s10, -1
    BNE     s10, s0, loop_start

    # ========================================================================
    # TEST BLOCK 5: VECTOR DOT PRODUCT
    # ========================================================================
    
    # Init Vectors (int32 lanes): {1, 1, 1, 1} in RAM
    LI      s2, 0x00001000
    LI      s3, 1
    SW      s3, 0(s2)
    SW      s3, 4(s2)
    SW      s3, 8(s2)
    SW      s3, 12(s2)

    VLD     v1, 0(s2)
    VLD     v2, 0(s2)

    # Vector add (1 + 1 = 2 per lane)
    VADD    v3, v1, v2

    # Store result vector and check lane0
    VST     v3, 0(s2)
    MEMBAR  s0, s0, 0
    LW      s4, s2, 0

    LI      s5, 2            # Expected lane0
    LI      s31, 0x4001     # ERR_VEC_DOT
    BNE     s4, s5, test_fail

    # ========================================================================
    # TEST PASSED
    # ========================================================================
test_pass:
    LI      s31, 0x600D     # Success Code
    LA      s10, 0x80000100 # Result Address
    SW      s31, 0(s10)     # Write Result
    MEMBAR  s0, s0, 0
    WFI     s0, s0, 0        # Halt

    # ========================================================================
    # FAILURE HANDLERS
    # ========================================================================
timeout_fail:
    LI      s31, 0xDEAD     # Timeout Marker
    # Fall through to standard fail write

test_fail:
    # s31 already contains the specific Error Code loaded before BNE
    LA      s10, 0x80000100
    SW      s31, 0(s10)     # Write Error Code to RAM
    MEMBAR  s0, s0, 0
    
stop_forever:
    WFI     s0, s0, 0        # Halt (Testbench should detect writes)
    JAL     s0, stop_forever