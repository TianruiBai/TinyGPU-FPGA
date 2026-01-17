module decoder (
    input  logic [31:0] inst,
    output isa_pkg::decode_ctrl_t ctrl
);
    import isa_pkg::*;

    logic [6:0] opcode;
    logic [2:0] funct3;
    logic [6:0] funct7;
    reg_idx_t   rd;
    reg_idx_t   rs1;
    reg_idx_t   rs2;

    logic       is_int_rtype;

    logic [31:0] imm_i;
    logic [31:0] imm_s;
    logic [31:0] imm_b;
    logic [31:0] imm_j;
    logic [31:0] imm_u;

    always_comb begin
        opcode = inst[6:0];
        rd     = inst[11:7];
        funct3 = inst[14:12];
        rs1    = inst[19:15];
        rs2    = inst[24:20];
        funct7 = inst[31:25];

        imm_i = {{20{inst[31]}}, inst[31:20]};
        imm_s = {{20{inst[31]}}, inst[31:25], inst[11:7]};
        imm_b = {{19{inst[31]}}, inst[31], inst[7], inst[30:25], inst[11:8], 1'b0};
        imm_j = {{11{inst[31]}}, inst[31], inst[19:12], inst[20], inst[30:21], 1'b0};
        imm_u = {inst[31:12], 12'b0};

        ctrl = '0;
        ctrl.is_valid     = 1'b1;
        // OP_JAL has no funct3 field; force funct3=011 so downstream logic treats it as JAL.
        ctrl.funct3       = (opcode == OP_JAL) ? 3'b011 : funct3;
        // For OP_INT_IMM, inst[31:25] are immediate bits, not a true funct7.
        // Only shift-immediates use these bits (SLLI/SRLI/SRAI via funct3=001/101).
        // Mask funct7 for other I-type ALU ops so execute stage doesn't mis-decode MUL/SUB.
        ctrl.funct7       = (opcode == OP_INT_IMM && !(funct3 == 3'b001 || funct3 == 3'b101)) ? 7'b0000000 : funct7;
        ctrl.rs1          = rs1;
        ctrl.rs2          = rs2;
        ctrl.rd           = rd;
        ctrl.is_system    = (opcode == OP_SYSTEM);
        ctrl.is_lui       = (opcode == OP_LUI);
        ctrl.is_scalar_fp = (opcode == OP_INT) && ((funct7 == 7'b0001000) || (funct7 == 7'b0001001));
        // Treat SYSTEM as scalar for scoreboard/hazard purposes
        ctrl.is_scalar_int= (opcode == OP_INT) || (opcode == OP_INT_IMM) || (opcode == OP_SYSTEM) || (opcode == OP_LUI);
        ctrl.is_vector    = (opcode == OP_VEC_ALU) || (opcode == OP_VLD) || (opcode == OP_VST) || (opcode == OP_TEX) || (opcode == OP_ATOM_V);
        ctrl.vm_enable    = ctrl.is_vector ? inst[25] : 1'b0;
        ctrl.is_tex       = (opcode == OP_TEX);

        // Opcode 0010100 is shared by scalar atomics and raster/geometry macro-ops.
        // ISA docs state they are distinguished by funct7/funct3.
        // Treat macro-ops as funct7==0 with funct3 in the raster/geom ranges.
        // (imm12 is "usually 0"; low imm bits may still carry small flags.)
        ctrl.is_gfx       = (opcode == OP_ATOM_SC)
             && (funct7 == 7'b0000000)
             && ((funct3 <= 3'b011) || (funct3 >= 3'b100 && funct3 <= 3'b110));

        ctrl.is_load      = (opcode == OP_LOAD) || (opcode == OP_VLD);
        ctrl.is_store     = (opcode == OP_STORE) || (opcode == OP_VST);
        ctrl.is_branch    = (opcode == OP_BRANCH) || (opcode == OP_JAL);
        ctrl.is_atomic    = ((opcode == OP_ATOM_SC) && !ctrl.is_gfx) || (opcode == OP_ATOM_V);
        // CSRRW/CSRRS need rs1; MEMBAR/WFI do not.
        // JAL does not use rs1; JALR/conditional branches do.
        // LUI uses no source registers.
        if (opcode == OP_LUI) begin
            ctrl.uses_rs1 = 1'b0;
        end else if (opcode == OP_JAL) begin
            ctrl.uses_rs1 = 1'b0;
        end else if (opcode == OP_SYSTEM) begin
            ctrl.uses_rs1 = (funct3 == 3'b001) || (funct3 == 3'b010);
        end else if (opcode == OP_BRANCH) begin
            ctrl.uses_rs1 = (funct3 != 3'b011); // JAL
        end else begin
            ctrl.uses_rs1 = 1'b1;
        end

                // Disambiguation note:
                // - OP_INT is treated as R-type (register-register)
                // - OP_INT_IMM is treated as I-type (register-immediate)
                is_int_rtype = (opcode == OP_INT);

                ctrl.uses_rs2     = (opcode == OP_LUI) ? 1'b0 :
                                    (opcode == OP_JAL) ? 1'b0 :
                                    (opcode == OP_INT_IMM) ? 1'b0 :
                                    (opcode == OP_INT) ? 1'b1 :
                                                         (((opcode == OP_BRANCH) && (funct3 != 3'b010) && (funct3 != 3'b011))
                                                     || (opcode == OP_STORE)
                                                     || (opcode == OP_VST)
                                                     || (opcode == OP_VEC_ALU)
                                                     || (opcode == OP_TEX)
                                                     || (opcode == OP_ATOM_V)
                                                     || ((opcode == OP_ATOM_SC) && !ctrl.is_gfx));

                // Scalar FP ops are encoded with OP_INT but are not part of the integer R/I heuristic above.
                // Ensure rs2 hazards are tracked for 2-operand FP ops (and FMA).
                // Conversions use rs1 only.
                if (ctrl.is_scalar_fp && (funct3 != 3'b110) && (funct3 != 3'b111)) begin
                    ctrl.uses_rs2 = 1'b1;
                end
                ctrl.uses_rd      = ((opcode != OP_BRANCH) || (funct3 == 3'b010) || (funct3 == 3'b011))
                                                 && (opcode != OP_STORE)
                                                 && (opcode != OP_VST)
                                                 && !(opcode == OP_SYSTEM && funct3 == 3'b000);
        ctrl.rd_is_vec    = ctrl.is_vector; // TEX also writes vector rd
        ctrl.rd_is_fp     = ctrl.is_scalar_fp && !(funct7 == 7'b0001000 && funct3 == 3'b111); // FCVT.f2i writes scalar

        if (ctrl.is_gfx) begin
            // Macro-ops do not write rd; treat as scalar class for hazard-free tracking
            ctrl.uses_rd   = 1'b0;
            ctrl.rd_is_vec = 1'b0;
            ctrl.rd_is_fp  = 1'b0;
        end

        // VCMP writes scalar mask (integer types only).
        // FP types may use funct6=000011 for other ops (e.g. FMAX-style).
        if (opcode == OP_VEC_ALU && funct7[6:1] == 6'b000011
            && (funct3 == 3'b000 || funct3 == 3'b001 || funct3 == 3'b010)) begin
            ctrl.rd_is_vec = 1'b0;
            ctrl.rd_is_fp  = 1'b0;
        end

        // Operand classes (override per-op)
        ctrl.rs1_class = ctrl.is_vector ? CLASS_VEC : (ctrl.is_scalar_fp ? CLASS_FP : CLASS_SCALAR);
        ctrl.rs2_class = ctrl.is_vector ? CLASS_VEC : (ctrl.is_scalar_fp ? CLASS_FP : CLASS_SCALAR);
        ctrl.rd_class  = ctrl.rd_is_vec ? CLASS_VEC : (ctrl.rd_is_fp ? CLASS_FP : CLASS_SCALAR);

        // Texture: rs1 is a coord vector, rs2 is a scalar sampler handle (index or descriptor pointer)
        if (opcode == OP_TEX) begin
            ctrl.rs1_class = CLASS_VEC;
            ctrl.rs2_class = CLASS_SCALAR;
            ctrl.rd_class  = CLASS_VEC;
        end

        // Vector ALU per-op operand class/arity overrides.
        // Note: the current microarchitecture always reads scalar rs1/rs2 in parallel with vector rs1/rs2.
        // Some vector ops intentionally use the scalar rs2 value as an immediate/mask/payload.
        if (opcode == OP_VEC_ALU) begin
            unique case (funct7[6:1])
                6'b000111: begin
                    // VSWIZ: rs1 is vector, rs2 is scalar selector mask; src_b is ignored.
                    ctrl.rs2_class = CLASS_SCALAR;
                end
                6'b001101, // VRCP
                6'b001110: begin // VRSQRT
                    // Unary SFU ops: only rs1 is consumed.
                    ctrl.uses_rs2 = 1'b0;
                end
                6'b001111: begin
                    // VUNPACK: payload comes from scalar rs2 (ARGB8888); rs1 unused.
                    ctrl.uses_rs1  = 1'b0;
                    ctrl.rs2_class = CLASS_SCALAR;
                end
                6'b001000: begin
                    // VPACK: produces a scalar packed result.
                    ctrl.uses_rs2  = 1'b0;
                    ctrl.rd_is_vec = 1'b0;
                    ctrl.rd_is_fp  = 1'b0;
                    ctrl.rd_class  = CLASS_SCALAR;
                end
                default: ;
            endcase
        end

        if (ctrl.is_scalar_fp && funct7 == 7'b0001000) begin
            case (funct3)
                3'b110: begin // FCVT i2f: scalar src -> fp dest
                    ctrl.rs1_class = CLASS_SCALAR;
                    ctrl.rd_class  = CLASS_FP;
                end
                3'b111: begin // FCVT f2i: fp src -> scalar dest
                    ctrl.rs1_class = CLASS_FP;
                    ctrl.rd_class  = CLASS_SCALAR;
                end
                default: ;
            endcase
        end

        if (opcode == OP_VEC_ALU && funct7[6:1] == 6'b000011
            && (funct3 == 3'b000 || funct3 == 3'b001 || funct3 == 3'b010)) begin
            ctrl.rd_class = CLASS_SCALAR;
        end

        unique case (opcode)
            OP_BRANCH: begin
                // Conditional branches use B-type.
                // JAL/JALR use I-type immediates in this implementation to avoid overlapping funct3 with the immediate field.
                if (funct3 == 3'b011)      ctrl.imm = imm_i; // JAL (pc + imm12)
                else if (funct3 == 3'b010) ctrl.imm = imm_i; // JALR (rs1 + imm12)
                else                       ctrl.imm = imm_b; // BEQ/BNE/...
            end
            OP_JAL:      ctrl.imm = imm_j; // JAL (pc + imm[20|10:1|11|19:12] << 1)
            OP_STORE:    ctrl.imm = imm_s;
            OP_VST:      ctrl.imm = imm_s;
            OP_ATOM_V:   ctrl.imm = imm_s; // vector atomics use S-type to carry rs2 operand and offset
            OP_LUI:      ctrl.imm = imm_u;
            default:     ctrl.imm = imm_i;
        endcase
    end
endmodule
