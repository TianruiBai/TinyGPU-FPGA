package isa_pkg;
    // RISC-V RV32IMA_Zicsr base + V (vector) + Xgpu custom extension opcodes.
    // Migrated from custom TinyGPU encoding to standard RISC-V opcode map.
    // Vector pipeline uses standard RVV encoding (OP-V, LOAD-FP, STORE-FP).
    // See docs/riscv_isa_migration_plan.md for rationale and full mapping.
    typedef enum logic [6:0] {
        // ------- RV32I base integer ISA -------
        OP_LUI      = 7'b0110111,  // 0x37 U-type
        OP_AUIPC    = 7'b0010111,  // 0x17 U-type
        OP_JAL      = 7'b1101111,  // 0x6F J-type
        OP_JALR     = 7'b1100111,  // 0x67 I-type
        OP_BRANCH   = 7'b1100011,  // 0x63 B-type
        OP_LOAD     = 7'b0000011,  // 0x03 I-type scalar load
        OP_STORE    = 7'b0100011,  // 0x23 S-type scalar store
        OP_IMM      = 7'b0010011,  // 0x13 I-type ALU
        OP_REG      = 7'b0110011,  // 0x33 R-type ALU
        OP_FENCE    = 7'b0001111,  // 0x0F MISC-MEM
        OP_SYSTEM   = 7'b1110011,  // 0x73 CSR/ECALL/EBREAK
        // ------- RV32A atomic extension -------
        OP_AMO      = 7'b0101111,  // 0x2F R-type atomics
        // ------- RVV vector extension -------
        OP_V        = 7'b1010111,  // 0x57 OP-V: vector arithmetic + vsetvl{i}
        OP_VL       = 7'b0000111,  // 0x07 LOAD-FP: vector loads (vle8/16/32 etc.)
        OP_VS       = 7'b0100111,  // 0x27 STORE-FP: vector stores (vse8/16/32 etc.)
        // ------- Xgpu custom extension -------
        OP_CUSTOM0  = 7'b0001011,  // 0x0B — FP16 scalar + Graphics macros + Texture
        OP_CUSTOM1  = 7'b0101011,  // 0x2B — Custom vector ops (VDOT/VCROSS/VSWIZ/VPACK/VRCP/VRSQRT/VUNPACK)
        OP_CUSTOM2  = 7'b1011011   // 0x5B — Vector Atomics (VATOM)
    } opcode_t;

    // ------- RVV funct3 operand-type categories (for OP_V = 0x57) -------
    localparam logic [2:0] RVV_OPIVV = 3'b000;  // integer vector-vector
    localparam logic [2:0] RVV_OPFVV = 3'b001;  // FP vector-vector
    localparam logic [2:0] RVV_OPMVV = 3'b010;  // mask/int vector-vector (vmul, etc.)
    localparam logic [2:0] RVV_OPIVI = 3'b011;  // integer vector-immediate
    localparam logic [2:0] RVV_OPIVX = 3'b100;  // integer vector-scalar
    localparam logic [2:0] RVV_OPFVF = 3'b101;  // FP vector-scalar
    localparam logic [2:0] RVV_OPMVX = 3'b110;  // mask/int vector-scalar
    localparam logic [2:0] RVV_OPCFG = 3'b111;  // vsetvl/vsetvli/vsetivli

    // ------- RVV funct6 operation codes (under OP_V) -------
    // Integer/FP arithmetic (shared funct6, distinguished by funct3 category)
    localparam logic [5:0] RVV_VADD    = 6'b000000;  // vadd / vfadd
    localparam logic [5:0] RVV_VSUB    = 6'b000010;  // vsub / vfsub
    localparam logic [5:0] RVV_VRSUB   = 6'b000011;  // vrsub (OPIVX/OPIVI only)
    localparam logic [5:0] RVV_VMINU   = 6'b000100;  // vminu / vfmin
    localparam logic [5:0] RVV_VMIN    = 6'b000101;  // vmin (signed)
    localparam logic [5:0] RVV_VMAXU   = 6'b000110;  // vmaxu / vfmax
    localparam logic [5:0] RVV_VMAX    = 6'b000111;  // vmax (signed)
    localparam logic [5:0] RVV_VAND    = 6'b001001;  // vand
    localparam logic [5:0] RVV_VOR     = 6'b001010;  // vor
    localparam logic [5:0] RVV_VXOR    = 6'b001011;  // vxor
    localparam logic [5:0] RVV_VMERGE  = 6'b010111;  // vmerge / vmv
    localparam logic [5:0] RVV_VMSEQ   = 6'b011000;  // vmseq / vmfeq
    localparam logic [5:0] RVV_VMSNE   = 6'b011001;  // vmsne / vmfle
    localparam logic [5:0] RVV_VMSLTU  = 6'b011010;  // vmsltu
    localparam logic [5:0] RVV_VMSLT   = 6'b011011;  // vmslt / vmflt
    localparam logic [5:0] RVV_VMSLEU  = 6'b011100;  // vmsleu / vmfne
    localparam logic [5:0] RVV_VMSLE   = 6'b011101;  // vmsle
    localparam logic [5:0] RVV_VMSGTU  = 6'b011110;  // vmsgtu (OPIVX/OPIVI only)
    localparam logic [5:0] RVV_VMSGT   = 6'b011111;  // vmsgt  (OPIVX/OPIVI only)
    localparam logic [5:0] RVV_VSLL    = 6'b100101;  // vsll (OPIVV) / vmul (OPMVV)
    localparam logic [5:0] RVV_VMUL    = 6'b100101;  // vmul (OPMVV/OPMVX — same code, diff funct3)
    localparam logic [5:0] RVV_VSRL    = 6'b101000;  // vsrl
    localparam logic [5:0] RVV_VSRA    = 6'b101001;  // vsra
    localparam logic [5:0] RVV_VFMUL   = 6'b100100;  // vfmul (OPFVV/OPFVF)
    localparam logic [5:0] RVV_VFDIV   = 6'b100000;  // vfdiv (OPFVV/OPFVF)

    // ------- RVV vector load/store width encoding (inst[14:12]) -------
    localparam logic [2:0] RVV_VEW8   = 3'b000;  // 8-bit element
    localparam logic [2:0] RVV_VEW16  = 3'b101;  // 16-bit element
    localparam logic [2:0] RVV_VEW32  = 3'b110;  // 32-bit element
    localparam logic [2:0] RVV_VEW64  = 3'b111;  // 64-bit element (not supported)

    // ------- RVV vtype CSR bit fields -------
    // vtype[2:0] = vlmul, vtype[5:3] = vsew, vtype[6] = vta, vtype[7] = vma
    localparam int VLEN = 128;
    localparam int VLENB = VLEN / 8;  // 16

    // ------- RVV vector CSR addresses -------
    localparam logic [11:0] CSR_VSTART = 12'h008;
    localparam logic [11:0] CSR_VXSAT  = 12'h009;
    localparam logic [11:0] CSR_VXRM   = 12'h00A;
    localparam logic [11:0] CSR_VCSR   = 12'h00F;
    localparam logic [11:0] CSR_VL     = 12'hC20;
    localparam logic [11:0] CSR_VTYPE  = 12'hC21;
    localparam logic [11:0] CSR_VLENB  = 12'hC22;

    // Xgpu custom-0 sub-class discriminators (funct7 values under OP_CUSTOM0)
    localparam logic [6:0] C0_FP16_ALU = 7'h08;  // funct7 for FP16 ALU ops
    localparam logic [6:0] C0_FP16_SFU = 7'h09;  // funct7 for FP16 SFU (FRECIP/FRSQRT)
    localparam logic [6:0] C0_GFX      = 7'h00;  // funct7 for graphics macro-ops
    localparam logic [6:0] C0_TEX      = 7'h42;  // funct7 for texture sampling

    // Xgpu custom-2 sub-class discriminators (funct3 values under OP_CUSTOM2)
    // VATOM uses S-type (funct3[2]=1)
    // Vector atomics: funct3[2] = 1 (e.g. 3'b100 = VATOM.ADD)

    // ------- Internal ALU element-type codes (for alu_vector type_sel port) -------
    localparam logic [2:0] TYPE_I32  = 3'b000;
    localparam logic [2:0] TYPE_I16  = 3'b001;
    localparam logic [2:0] TYPE_I8   = 3'b010;
    localparam logic [2:0] TYPE_FP32 = 3'b011;
    localparam logic [2:0] TYPE_FP16 = 3'b100;
    localparam logic [2:0] TYPE_FP8  = 3'b101;

    typedef logic [4:0] reg_idx_t;

    typedef enum logic [1:0] {
        CLASS_SCALAR = 2'b00,
        CLASS_FP     = 2'b01,
        CLASS_VEC    = 2'b10
    } reg_class_e;

    typedef struct packed {
        logic        is_valid;
        logic        is_scalar_int;
        logic        is_scalar_fp;
        logic        is_vector;
        logic        vm_enable; // vector predication bit (inst[25])
        logic        is_system;
        logic        is_tex;
        logic        is_gfx; // non-texture graphics macro-ops (RSTATE/RSETUP/RDRAW/GDRAW)
        logic        is_load;
        logic        is_store;
        logic        is_branch;
        logic        is_jalr;   // JALR instruction (register-indirect jump-and-link)
        logic        is_auipc;  // AUIPC instruction (PC + upper immediate)
        logic        is_lui;
        logic        is_fence;  // FENCE / memory barrier
        logic        is_atomic;
        logic        is_vsetvl; // vsetvli / vsetivli / vsetvl (vector config)
        logic        is_rvv;    // standard RVV instruction (OP_V/OP_VL/OP_VS), not custom
        logic        uses_rs1;
        logic        uses_rs2;
        logic        uses_rd;
        logic        rd_is_vec;
        logic        rd_is_fp;
        reg_class_e  rs1_class;
        reg_class_e  rs2_class;
        reg_class_e  rd_class;
        reg_idx_t    rs1;
        reg_idx_t    rs2;
        reg_idx_t    rd;
        logic [2:0]  funct3;
        logic [6:0]  funct7;
        logic [2:0]  type_sel;  // ALU element type: TYPE_I32/I16/I8/FP32/FP16/FP8
        logic [31:0] imm;
    } decode_ctrl_t;
endpackage
