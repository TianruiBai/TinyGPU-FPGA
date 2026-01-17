# ============================================================================
# TinyGPU Comprehensive System Test (v2)
# ============================================================================
# Features:
# 1. Pipeline Hazards (RAW, Load-Use)
# 2. Control Flow (Loops, Jumps)
# 3. Vector Math (FP16 Matrix Multiplication for 3D Rotation)
# 4. Graphics (Software T&L + Hardware Rasterization)
#    [cite_start]- Purple/Black Checkerboard Cube (12 Triangles) [cite: 5]
#    [cite_start]- Utah Teapot (60 Triangles) [cite: 6]
#
# Memory Map:
#   .text:      0x0000_0000
#   .data:      0x0000_2000
#   VRAM:       0x2000_0000
# ============================================================================

.data
.org 0x2000

# --- Constants ---
CONST_SCREEN_W: .word 128
CONST_SCREEN_H: .word 128
CONST_PURPLE:   .word 0xFF800080
CONST_BLACK:    .word 0xFF000000
CONST_SCALE:    .word 0x42200000  # FP32 40.0 (Scale factor for models)

# --- Descriptors ---
DESC_RSTATE:
    .word 0x20100000    # FB_BASE
    .word 512           # STRIDE (128*4)
    .word 0             # FORMAT (ARGB8888)
    .word 0x00800080    # WH (128x128)
    .word 0             # DEPTH_BASE (None)
    .word 0, 0, 0
    .word 0xFF202020    # CONST_COLOR (Dark Gray Background)
    .word 0             # SAMPLER_HANDLE
    .word 0             # SCISSOR_XY
    .word 0x00800080    # SCISSOR_WH
    .word 0x00000003    # FLAGS (Scissor | Texture)
    .zero 16

DESC_RRECT:
    .word 0, 0, 128, 128    # Full Screen
    .word 0xFF333333        # Dark Grey
    .word 0x0               # Flags
    .word 0, 0

# --- Texture (Checkerboard) ---
TEX_CHECKER_BASE:
    .zero 1024          # 16x16 * 4 bytes

# --- 3D Models ---
# The testbench defines scale S=40. Vertices are integers in range +/-40.
# We store them here as FP16 for the compute unit to process.

# [cite_start]1. CUBE (8 Vertices) [cite: 110]
# X, Y, Z (FP16)
MODEL_CUBE_VERTS:
    .half 0xD500, 0xD500, 0xD500 # V0: -40, -40, -40
    .half 0x5500, 0xD500, 0xD500 # V1:  40, -40, -40
    .half 0x5500, 0x5500, 0xD500 # V2:  40,  40, -40
    .half 0xD500, 0x5500, 0xD500 # V3: -40,  40, -40
    .half 0xD500, 0xD500, 0x5500 # V4: -40, -40,  40
    .half 0x5500, 0xD500, 0x5500 # V5:  40, -40,  40
    .half 0x5500, 0x5500, 0x5500 # V6:  40,  40,  40
    .half 0xD500, 0x5500, 0x5500 # V7: -40,  40,  40

# [cite_start]1. CUBE INDICES (12 Triangles) [cite: 119]
MODEL_CUBE_INDICES:
    # Z+ Face
    .word 4, 5, 6,  4, 6, 7
    # Z- Face
    .word 0, 2, 1,  0, 3, 2
    # X- Face
    .word 0, 4, 7,  0, 7, 3
    # X+ Face
    .word 1, 2, 6,  1, 6, 5
    # Y+ Face
    .word 3, 7, 6,  3, 6, 2
    # Y- Face
    .word 0, 1, 5,  0, 5, 4
    .word -1 # End

# [cite_start]2. TEAPOT (32 Vertices) [cite: 162]
# Generated based on profile radius function in TB.
# Rings=5, Segs=6. Top/Bottom points included.
MODEL_TEA_VERTS:
    # Ring 0 (Y = -1.0 -> -40)
    .half 0x5100, 0xD500, 0x0000 # R0S0
    .half 0x4D00, 0xD500, 0x2A00 # R0S1
    .half 0x2A00, 0xD500, 0x4D00 # R0S2
    .half 0xD600, 0xD500, 0x4D00 # R0S3
    .half 0xB300, 0xD500, 0x2A00 # R0S4
    .half 0xAF00, 0xD500, 0x0000 # R0S5

    # Ring 1 (Y = -0.5 -> -20)
    .half 0x5500, 0xCD00, 0x0000 # R1S0
    .half 0x5100, 0xCD00, 0x2C00 # R1S1
    .half 0x2C00, 0xCD00, 0x5100 # R1S2
    .half 0xD400, 0xCD00, 0x5100 # R1S3
    .half 0xAF00, 0xCD00, 0x2C00 # R1S4
    .half 0xAB00, 0xCD00, 0x0000 # R1S5

    # Ring 2 (Y = 0.0 -> 0)
    .half 0x5600, 0x0000, 0x0000 # R2S0
    .half 0x5200, 0x0000, 0x2D00 # R2S1
    .half 0x2D00, 0x0000, 0x5200 # R2S2
    .half 0xD300, 0x0000, 0x5200 # R2S3
    .half 0xAE00, 0x0000, 0x2D00 # R2S4
    .half 0xAA00, 0x0000, 0x0000 # R2S5

    # Ring 3 (Y = 0.5 -> 20)
    .half 0x4E00, 0x4D00, 0x0000 # R3S0
    .half 0x4A00, 0x4D00, 0x2900 # R3S1
    .half 0x2900, 0x4D00, 0x4A00 # R3S2
    .half 0xD700, 0x4D00, 0x4A00 # R3S3
    .half 0xB600, 0x4D00, 0x2900 # R3S4
    .half 0xB200, 0x4D00, 0x0000 # R3S5

    # Ring 4 (Y = 1.0 -> 40)
    .half 0x3C00, 0x5500, 0x0000 # R4S0
    .half 0x3800, 0x5500, 0x2000 # R4S1
    .half 0x2000, 0x5500, 0x3800 # R4S2
    .half 0xE000, 0x5500, 0x3800 # R4S3
    .half 0xC800, 0x5500, 0x2000 # R4S4
    .half 0xC400, 0x5500, 0x0000 # R4S5

    # Caps
    .half 0x0000, 0xD500, 0x0000 # Bottom (30)
    .half 0x0000, 0x5500, 0x0000 # Top (31)

# [cite_start]2. TEAPOT INDICES (60 Triangles) [cite: 178]
# Pattern: Two triangles per quad segment (6 segs * 4 rings = 24 quads = 48 tris)
# Plus 6 tris for bottom cap, 6 tris for top cap.
MODEL_TEA_INDICES:
    # Ring 0-1
    .word 0, 6, 7, 0, 7, 1,   1, 7, 8, 1, 8, 2
    .word 2, 8, 9, 2, 9, 3,   3, 9, 10, 3, 10, 4
    .word 4, 10, 11, 4, 11, 5, 5, 11, 6, 5, 6, 0
    # Ring 1-2
    .word 6, 12, 13, 6, 13, 7,  7, 13, 14, 7, 14, 8
    .word 8, 14, 15, 8, 15, 9,  9, 15, 16, 9, 16, 10
    .word 10, 16, 17, 10, 17, 11, 11, 17, 12, 11, 12, 6
    # Ring 2-3
    .word 12, 18, 19, 12, 19, 13, 13, 19, 20, 13, 20, 14
    .word 14, 20, 21, 14, 21, 15, 15, 21, 22, 15, 22, 16
    .word 16, 22, 23, 16, 23, 17, 17, 23, 18, 17, 18, 12
    # Ring 3-4
    .word 18, 24, 25, 18, 25, 19, 19, 25, 26, 19, 26, 20
    .word 20, 26, 27, 20, 27, 21, 21, 27, 28, 21, 28, 22
    .word 22, 28, 29, 22, 29, 23, 23, 29, 24, 23, 24, 18
    # Bottom Cap
    .word 30, 1, 0, 30, 2, 1, 30, 3, 2, 30, 4, 3, 30, 5, 4, 30, 0, 5
    # Top Cap
    .word 31, 24, 25, 31, 25, 26, 31, 26, 27, 31, 27, 28, 31, 28, 29, 31, 29, 24
    .word -1 # End

# Scratchpad
SCRATCH_RSETUP: .zero 96

# ============================================================================
.text
.org 0x0000

_start:
    # ------------------------------------------------------------------------
    # 1. Pipeline & ALU Test
    # ------------------------------------------------------------------------
    # Hazards
    LI      s2, 10
    ADDI    s2, s2, 5       # s2 = 15
    ADD     s3, s2, s2      # RAW Hazard Check (s3 should be 30)
    
    LA      s4, CONST_SCREEN_W
    LW      s5, 0(s4)       # Load 128
    ADDI    s6, s5, 1       # Load-Use Stall Check (s6 should be 129)
    
    # Branch
    LI      s10, 5
branch_loop:
    ADDI    s10, s10, -1
    LI      s0, 0
    BNE     s10, s0, branch_loop # Loop 5 times

    # ------------------------------------------------------------------------
    # 2. Graphics Init
    # ------------------------------------------------------------------------
    # Generate Texture (Checkerboard)
    LA      s1, TEX_CHECKER_BASE
    LA      s2, CONST_PURPLE
    LW      s2, 0(s2)
    LA      s3, CONST_BLACK
    LW      s3, 0(s3)
    LI      s4, 256         # 16x16
    LI      s5, 0
tex_loop:
    ANDI    s6, s5, 1       # x bit
    ASR     s7, s5, 4       # y index >> 4
    ANDI    s7, s7, 1       # y bit
    XOR     s6, s6, s7
    BEQ     s6, s0, write_purple
    SW      s3, 0(s1)
    JAL     s0, next_pix
write_purple:
    SW      s2, 0(s1)
next_pix:
    ADDI    s1, s1, 4
    ADDI    s5, s5, 1
    BNE     s5, s4, tex_loop
    MEMBAR

    # Load Render State
    LA      s1, DESC_RSTATE
    [cite_start]RSTATE  s1 [cite: 92]
    
    # Clear Screen
    LA      s1, DESC_RRECT
    [cite_start]RRECT   s1 [cite: 96]

    # ------------------------------------------------------------------------
    # 3. Geometry Pass
    # ------------------------------------------------------------------------
    # Setup Rotation Matrix (Y-Axis)
    # Cos(45) ~ 0.707 (0x39A8 FP16)
    LI      s4, 0x39A8
    MOV     f0, s4          # Cos
    MOV     f1, s4          # Sin
    FNEG    f2, f1          # -Sin
    
    # Setup Viewport Scale (64.0)
    LI      s3, 64
    FCVT.I2F f10, s3

    # Draw Cube
    LA      s10, MODEL_CUBE_VERTS
    LA      s11, MODEL_CUBE_INDICES
    JAL     s31, draw_mesh

    # Draw Teapot
    LA      s10, MODEL_TEA_VERTS
    LA      s11, MODEL_TEA_INDICES
    JAL     s31, draw_mesh

    MEMBAR
    WFI

# ============================================================================
# Subroutine: draw_mesh
# s10: Verts, s11: Indices, f0/f1/f2: Rot Matrix, f10: Scale
# ============================================================================
draw_mesh:
mesh_loop:
    LW      s20, 0(s11)     # Index 0
    LI      s29, -1
    BEQ     s20, s29, mesh_done

    LW      s21, 4(s11)     # Index 1
    LW      s22, 8(s11)     # Index 2
    
    # Setup RSETUP Buffer
    LA      s27, SCRATCH_RSETUP
    
    # Process V0
    JAL     s30, process_vert
    SW      s24, 0(s27)     # X
    SW      s25, 4(s27)     # Y
    LI      s9, 0
    SW      s9,  8(s27)     # Z
    LI      s9, 0x3F800000
    SW      s9,  12(s27)    # W
    SW      s0,  16(s27)    # U
    SW      s0,  20(s27)    # V
    LI      s9, 0xFFFFFFFF
    SW      s9,  24(s27)    # Color

    # Process V1
    MOV     s20, s21        # Move Index 1 to arg
    JAL     s30, process_vert
    SW      s24, 32(s27)
    SW      s25, 36(s27)
    LI      s9, 0
    SW      s9,  40(s27)
    LI      s9, 0x3F800000
    SW      s9,  44(s27)
    SW      s0,  48(s27)
    SW      s0,  52(s27)
    LI      s9, 0xFFFFFFFF
    SW      s9,  56(s27)

    # Process V2
    MOV     s20, s22        # Move Index 2 to arg
    JAL     s30, process_vert
    SW      s24, 64(s27)
    SW      s25, 68(s27)
    LI      s9, 0
    SW      s9,  72(s27)
    LI      s9, 0x3F800000
    SW      s9,  76(s27)
    SW      s0,  80(s27)
    SW      s0,  84(s27)
    LI      s9, 0xFFFFFFFF
    SW      s9,  88(s27)

    # Issue Draw
    [cite_start]RSETUP  s27 [cite: 172]
    LI      s28, 1          # Textured
    RDRAW   s28

    ADDI    s11, s11, 12    # Next Triangle
    JAL     s0, mesh_loop

mesh_done:
    JALR    s0, s31, 0

# Helper: Transform s20(Index) -> s24(X), s25(Y)
process_vert:
    # Addr = Base + Index*6
    LI      s28, 6
    MUL     s23, s20, s28
    ADD     s23, s23, s10
    
    LH      s24, 0(s23)     # Load X
    LH      s25, 2(s23)     # Load Y
    LH      s26, 4(s23)     # Load Z
    
    # Rotate Y
    MOV     f4, s24
    MOV     f5, s25
    MOV     f6, s26
    
    FMUL    f7, f4, f0      # x*cos
    FMA     f7, f6, f2      # + z*-sin
    
    FMUL    f8, f4, f1      # x*sin
    FMA     f8, f6, f0      # + z*cos
    
    # Project
    FMUL    f7, f7, f10     # * Scale
    FADD    f7, f7, f10     # + Offset
    FMUL    f5, f5, f10     # * Scale
    FADD    f5, f5, f10     # + Offset
    
    FCVT.F2I s24, f7
    FCVT.F2I s25, f5
    JALR    s0, s30, 0