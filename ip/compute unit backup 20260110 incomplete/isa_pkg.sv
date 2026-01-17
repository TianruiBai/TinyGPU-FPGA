package isa_pkg;
    // Opcodes per doc/isa.md (scalar, vector, texture, atomics)
    typedef enum logic [6:0] {
        OP_INT      = 7'b0001011,
        OP_LOAD     = 7'b0001100,
        OP_STORE    = 7'b0001101,
        OP_BRANCH   = 7'b0001110,
        OP_SYSTEM   = 7'b0001111,
        OP_VEC_ALU  = 7'b0101111,
        OP_VLD      = 7'b0010001,
        OP_VST      = 7'b0010010,
        OP_TEX      = 7'b0010011,
        OP_ATOM_SC  = 7'b0010100,
        OP_ATOM_V   = 7'b0010101
    } opcode_t;

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
        logic        is_atomic;
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
        logic [31:0] imm;
    } decode_ctrl_t;
endpackage
