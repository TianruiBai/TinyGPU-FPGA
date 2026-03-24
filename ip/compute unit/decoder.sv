module decoder (
    input  logic [31:0] inst,
    output isa_pkg::decode_ctrl_t ctrl
);

    import isa_pkg::*;

    logic [6:0] opcode;
    logic [2:0] funct3;
    logic [6:0] funct7;
    logic [5:0] funct6; // inst[31:26] — used by Xgpu custom-1/custom-2
    reg_idx_t   rd;
    reg_idx_t   rs1;
    reg_idx_t   rs2;

    logic       is_int_rtype;

    // Xgpu custom-0 sub-class helpers
    logic       is_c0_fp16;
    logic       is_c0_gfx;
    logic       is_c0_tex;
    // Xgpu custom-2 sub-class helper (vector atomics only)
    logic       is_c2_vatom;
    // RVV standard vector helpers
    logic       is_rvv_arith;  // OP_V non-config vector arithmetic
    logic       is_rvv_vload;  // OP_VL vector load
    logic       is_rvv_vstore; // OP_VS vector store
    logic       is_rvv_cfg;    // OP_V with funct3=OPCFG (vsetvl/vsetvli/vsetivli)
    logic       is_fp_cat;     // RVV FP operand category (OPFVV or OPFVF)

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
        funct6 = inst[31:26];

        imm_i = {{20{inst[31]}}, inst[31:20]};
        imm_s = {{20{inst[31]}}, inst[31:25], inst[11:7]};
        imm_b = {{19{inst[31]}}, inst[31], inst[7], inst[30:25], inst[11:8], 1'b0};
        imm_j = {{11{inst[31]}}, inst[31], inst[19:12], inst[20], inst[30:21], 1'b0};
        imm_u = {inst[31:12], 12'b0};

        // Xgpu custom-0 sub-classification (FP16 / Graphics / Texture under OP_CUSTOM0)
        is_c0_fp16 = (opcode == OP_CUSTOM0) && ((funct7 == C0_FP16_ALU) || (funct7 == C0_FP16_SFU));
        is_c0_gfx  = (opcode == OP_CUSTOM0) && (funct7 == C0_GFX)
                      && ((funct3 <= 3'b011) || (funct3 >= 3'b100 && funct3 <= 3'b110));
        is_c0_tex  = (opcode == OP_CUSTOM0) && (funct7 == C0_TEX);

        // Xgpu custom-2 sub-classification (vector atomics only)
        is_c2_vatom = (opcode == OP_CUSTOM2) && funct3[2]; // funct3[2]=1 → vector atomics

        // RVV standard vector sub-classification
        is_rvv_cfg    = (opcode == OP_V) && (funct3 == RVV_OPCFG);
        is_rvv_arith  = (opcode == OP_V) && (funct3 != RVV_OPCFG);
        is_rvv_vload  = (opcode == OP_VL);
        is_rvv_vstore = (opcode == OP_VS);
        is_fp_cat     = (funct3 == RVV_OPFVV) || (funct3 == RVV_OPFVF);

        ctrl = '0;
        ctrl.is_valid     = 1'b1;
        // OP_JAL has no funct3 field; force funct3=011 so downstream logic treats it as JAL.
        ctrl.funct3       = (opcode == OP_JAL) ? 3'b011 : funct3;
        // For OP_IMM, inst[31:25] are immediate bits, not a true funct7.
        // Only shift-immediates use these bits (SLLI/SRLI/SRAI via funct3=001/101).
        // Mask funct7 for other I-type ALU ops so execute stage doesn't mis-decode MUL/SUB.
        ctrl.funct7       = (opcode == OP_IMM && !(funct3 == 3'b001 || funct3 == 3'b101)) ? 7'b0000000 : funct7;
        ctrl.rs1          = rs1;
        ctrl.rs2          = rs2;
        ctrl.rd           = rd;
        ctrl.is_system    = (opcode == OP_SYSTEM);
        ctrl.is_lui       = (opcode == OP_LUI);
        ctrl.is_auipc     = (opcode == OP_AUIPC);
        ctrl.is_jalr      = (opcode == OP_JALR);
        ctrl.is_fence     = (opcode == OP_FENCE);

        // FP16 scalar: now under OP_CUSTOM0 with same funct7 discriminators
        ctrl.is_scalar_fp = is_c0_fp16;

        // Scalar integer pipeline: RV32I base + system + LUI/AUIPC/JALR/FENCE + AMO + vsetvl
        ctrl.is_scalar_int= (opcode == OP_REG) || (opcode == OP_IMM) || (opcode == OP_SYSTEM)
                          || (opcode == OP_LUI) || (opcode == OP_AUIPC) || (opcode == OP_JALR)
                          || (opcode == OP_FENCE) || (opcode == OP_AMO) || is_rvv_cfg;

        // Vector pipeline: CUSTOM1 (custom vec ALU) + RVV arith/VLD/VST + TEX under CUSTOM0
        ctrl.is_vector    = (opcode == OP_CUSTOM1) || is_rvv_arith || is_rvv_vload || is_rvv_vstore || is_c0_tex;
        ctrl.vm_enable    = ctrl.is_vector ? inst[25] : 1'b0;
        ctrl.is_tex       = is_c0_tex;
        ctrl.is_rvv       = is_rvv_arith || is_rvv_vload || is_rvv_vstore || is_rvv_cfg;
        ctrl.is_vsetvl    = is_rvv_cfg;

        // Graphics macro-ops: now under OP_CUSTOM0 with funct7=C0_GFX
        ctrl.is_gfx       = is_c0_gfx;

        // Load/Store classification
        ctrl.is_load      = (opcode == OP_LOAD) || is_rvv_vload;
        ctrl.is_store     = (opcode == OP_STORE) || is_rvv_vstore;

        // Branch/Jump: conditional branches + JAL (both PC-relative)
        // JALR is handled separately via is_jalr
        ctrl.is_branch    = (opcode == OP_BRANCH) || (opcode == OP_JAL);

        // Atomics: scalar AMO (RV32A) + vector atomics (Xgpu CUSTOM2)
        ctrl.is_atomic    = (opcode == OP_AMO) || is_c2_vatom;

        // --- uses_rs1 ---
        if (opcode == OP_LUI || opcode == OP_AUIPC) begin
            ctrl.uses_rs1 = 1'b0;
        end else if (opcode == OP_JAL) begin
            ctrl.uses_rs1 = 1'b0;
        end else if (opcode == OP_FENCE) begin
            ctrl.uses_rs1 = 1'b0;
        end else if (opcode == OP_SYSTEM) begin
            // CSRRW/CSRRS/CSRRC(funct3=001/010/011) use rs1; ECALL/EBREAK/WFI(000) do not
            ctrl.uses_rs1 = (funct3 == 3'b001) || (funct3 == 3'b010) || (funct3 == 3'b011);
        end else begin
            ctrl.uses_rs1 = 1'b1;
        end

                // Disambiguation note:
                // - OP_REG is R-type (register-register)
                // - OP_IMM is I-type (register-immediate)
                is_int_rtype = (opcode == OP_REG);

                ctrl.uses_rs2     = (opcode == OP_LUI) ? 1'b0 :
                                    (opcode == OP_AUIPC) ? 1'b0 :
                                    (opcode == OP_JAL) ? 1'b0 :
                                    (opcode == OP_JALR) ? 1'b0 :
                                    (opcode == OP_IMM) ? 1'b0 :
                                    (opcode == OP_REG) ? 1'b1 :
                                    (opcode == OP_FENCE) ? 1'b0 :
                                                         ((opcode == OP_BRANCH)
                                                     || (opcode == OP_STORE)
                                                     || (opcode == OP_CUSTOM1)
                                                     || is_c0_tex
                                                     || is_c2_vatom
                                                     || (opcode == OP_AMO));

                // Scalar FP ops are encoded with OP_CUSTOM0 (funct7=08/09).
                // Ensure rs2 hazards are tracked for 2-operand FP ops (and FMA).
                // Conversions use rs1 only.
                if (ctrl.is_scalar_fp && (funct3 != 3'b110) && (funct3 != 3'b111)) begin
                    ctrl.uses_rs2 = 1'b1;
                end

                ctrl.uses_rd      = (opcode != OP_BRANCH)
                                                 && (opcode != OP_STORE)
                                                 && !is_rvv_vstore
                                                 && (opcode != OP_FENCE)
                                                 && !(opcode == OP_SYSTEM && funct3 == 3'b000);
        ctrl.rd_is_vec    = ctrl.is_vector; // TEX also writes vector rd
        ctrl.rd_is_fp     = ctrl.is_scalar_fp && !(funct7 == C0_FP16_ALU && funct3 == 3'b111); // FCVT.f2i writes scalar

        if (ctrl.is_gfx) begin
            // Macro-ops do not write rd; treat as scalar class for hazard-free tracking
            ctrl.uses_rd   = 1'b0;
            ctrl.rd_is_vec = 1'b0;
            ctrl.rd_is_fp  = 1'b0;
        end

        // VCMP writes scalar mask (integer types only).
        // FP types may use funct6=000011 for other ops (e.g. FMAX-style).
        if (opcode == OP_CUSTOM1 && funct7[6:1] == 6'b000011
            && (funct3 == 3'b000 || funct3 == 3'b001 || funct3 == 3'b010)) begin
            ctrl.rd_is_vec = 1'b0;
            ctrl.rd_is_fp  = 1'b0;
        end

        // Operand classes (override per-op)
        ctrl.rs1_class = ctrl.is_vector ? CLASS_VEC : (ctrl.is_scalar_fp ? CLASS_FP : CLASS_SCALAR);
        ctrl.rs2_class = ctrl.is_vector ? CLASS_VEC : (ctrl.is_scalar_fp ? CLASS_FP : CLASS_SCALAR);
        ctrl.rd_class  = ctrl.rd_is_vec ? CLASS_VEC : (ctrl.rd_is_fp ? CLASS_FP : CLASS_SCALAR);

        // Texture: rs1 is a coord vector, rs2 is a scalar sampler handle (index or descriptor pointer)
        if (is_c0_tex) begin
            ctrl.rs1_class = CLASS_VEC;
            ctrl.rs2_class = CLASS_SCALAR;
            ctrl.rd_class  = CLASS_VEC;
        end

        // Vector ALU per-op operand class/arity overrides (now under OP_CUSTOM1).
        if (opcode == OP_CUSTOM1) begin
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

        if (ctrl.is_scalar_fp && funct7 == C0_FP16_ALU) begin
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

        if (opcode == OP_CUSTOM1 && funct7[6:1] == 6'b000011
            && (funct3 == 3'b000 || funct3 == 3'b001 || funct3 == 3'b010)) begin
            ctrl.rd_class = CLASS_SCALAR;
        end

        // --- type_sel: element type for vector ALU ---
        // Custom ops (OP_CUSTOM1): type_sel = funct3 (backward compatible encoding)
        if (opcode == OP_CUSTOM1) begin
            ctrl.type_sel = funct3;
        end
        // RVV ops: set FP/INT placeholder; compute_unit_top overrides width from vtype CSR
        if (is_rvv_arith) begin
            ctrl.type_sel = is_fp_cat ? TYPE_FP32 : TYPE_I32;
        end else if (is_rvv_vload || is_rvv_vstore) begin
            ctrl.type_sel = TYPE_I32;
        end

        // --- RVV vector arithmetic: funct6 translation + operand class overrides ---
        if (is_rvv_arith) begin
            logic [5:0] rvv_f6;
            logic is_opmvv, is_opmvx;
            rvv_f6   = inst[31:26];
            is_opmvv = (funct3 == RVV_OPMVV);
            is_opmvx = (funct3 == RVV_OPMVX);

            // Operand classes based on RVV funct3 category
            case (funct3)
                RVV_OPIVV, RVV_OPFVV, RVV_OPMVV: begin
                    ctrl.rs1_class = CLASS_VEC;   // vs1
                    ctrl.rs2_class = CLASS_VEC;   // vs2
                end
                RVV_OPIVX, RVV_OPFVF, RVV_OPMVX: begin
                    ctrl.rs1_class = CLASS_SCALAR; // scalar rs1
                    ctrl.rs2_class = CLASS_VEC;    // vs2
                    ctrl.uses_rs2  = 1'b1;
                end
                RVV_OPIVI: begin
                    ctrl.rs1_class = CLASS_SCALAR; // simm5 (immediate)
                    ctrl.rs2_class = CLASS_VEC;    // vs2
                    ctrl.uses_rs1  = 1'b0;         // immediate, not register
                    ctrl.uses_rs2  = 1'b1;
                end
                default: begin
                    ctrl.rs1_class = CLASS_VEC;
                    ctrl.rs2_class = CLASS_VEC;
                end
            endcase
            ctrl.rd_class  = CLASS_VEC;
            ctrl.rd_is_vec = 1'b1;

            // Translate RVV funct6 → internal ALU {funct7, funct3}
            // Internal funct6 = ctrl.funct7[6:1], sub-op = ctrl.funct3
            ctrl.funct7 = {6'b000000, 1'b0}; // default: VADD
            ctrl.funct3 = 3'b000;
            case (rvv_f6)
                RVV_VADD:   begin ctrl.funct7 = {6'b000000, 1'b0}; ctrl.funct3 = 3'b000; end
                RVV_VSUB:   begin ctrl.funct7 = {6'b000001, 1'b0}; ctrl.funct3 = 3'b000; end
                RVV_VRSUB:  begin ctrl.funct7 = {6'b000001, 1'b0}; ctrl.funct3 = 3'b001; end // reverse sub (b-a)
                RVV_VMINU:  begin ctrl.funct7 = {6'b000010, 1'b0}; ctrl.funct3 = 3'b000; end
                RVV_VMIN:   begin ctrl.funct7 = {6'b000010, 1'b0}; ctrl.funct3 = 3'b010; end
                RVV_VMAXU: begin
                    if (is_fp_cat) begin
                        ctrl.funct7 = {6'b000010, 1'b0}; ctrl.funct3 = 3'b001; // vfmax → same as unsigned max, FP override picks it up
                    end else begin
                        ctrl.funct7 = {6'b000010, 1'b0}; ctrl.funct3 = 3'b001; // vmaxu
                    end
                end
                RVV_VMAX:   begin ctrl.funct7 = {6'b000010, 1'b0}; ctrl.funct3 = 3'b011; end
                RVV_VAND:   begin ctrl.funct7 = {6'b001010, 1'b0}; ctrl.funct3 = 3'b000; end
                RVV_VOR:    begin ctrl.funct7 = {6'b001011, 1'b0}; ctrl.funct3 = 3'b000; end
                RVV_VXOR:   begin ctrl.funct7 = {6'b001001, 1'b0}; ctrl.funct3 = 3'b000; end
                RVV_VMERGE: begin ctrl.funct7 = {6'b000110, 1'b0}; ctrl.funct3 = 3'b000; end
                RVV_VMSEQ:  begin ctrl.funct7 = {6'b000011, 1'b0}; ctrl.funct3 = 3'b000; end // eq
                RVV_VMSNE:  begin ctrl.funct7 = {6'b000011, 1'b0}; ctrl.funct3 = 3'b011; end // ne
                RVV_VMSLTU: begin ctrl.funct7 = {6'b000011, 1'b0}; ctrl.funct3 = 3'b010; end // unsigned lt
                RVV_VMSLT:  begin ctrl.funct7 = {6'b000011, 1'b0}; ctrl.funct3 = 3'b001; end // signed lt
                RVV_VMSLEU: begin ctrl.funct7 = {6'b000011, 1'b0}; ctrl.funct3 = 3'b110; end // unsigned le
                RVV_VMSLE:  begin ctrl.funct7 = {6'b000011, 1'b0}; ctrl.funct3 = 3'b101; end // signed le
                RVV_VMSGTU: begin ctrl.funct7 = {6'b000011, 1'b0}; ctrl.funct3 = 3'b100; end // unsigned gt
                RVV_VMSGT:  begin ctrl.funct7 = {6'b000011, 1'b0}; ctrl.funct3 = 3'b111; end // signed gt
                RVV_VSLL: begin
                    if (is_opmvv || is_opmvx) begin // vmul (same funct6, OPMVV/OPMVX)
                        ctrl.funct7 = {6'b001100, 1'b0}; ctrl.funct3 = 3'b000;
                    end else begin // vsll (OPIVV/OPIVX)
                        ctrl.funct7 = {6'b000000, 1'b0}; ctrl.funct3 = 3'b001;
                    end
                end
                RVV_VSRL:   begin ctrl.funct7 = {6'b000000, 1'b0}; ctrl.funct3 = 3'b101; end
                RVV_VSRA:   begin ctrl.funct7 = {6'b000000, 1'b0}; ctrl.funct3 = 3'b101; end
                RVV_VFMUL:  begin ctrl.funct7 = {6'b001100, 1'b0}; ctrl.funct3 = 3'b000; end
                RVV_VFDIV:  begin ctrl.funct7 = {6'b001100, 1'b0}; ctrl.funct3 = 3'b001; end // FP division
                default:     begin ctrl.funct7 = {6'b000000, 1'b0}; ctrl.funct3 = 3'b000; end
            endcase

            // CMP ops write scalar mask, not vector
            if (rvv_f6 == RVV_VMSEQ  || rvv_f6 == RVV_VMSNE  ||
                rvv_f6 == RVV_VMSLTU || rvv_f6 == RVV_VMSLT  ||
                rvv_f6 == RVV_VMSLEU || rvv_f6 == RVV_VMSLE  ||
                rvv_f6 == RVV_VMSGTU || rvv_f6 == RVV_VMSGT) begin
                ctrl.rd_is_vec = 1'b0;
                ctrl.rd_class  = CLASS_SCALAR;
            end
        end

        // --- RVV vector loads (OP_VL = LOAD-FP 0x07) ---
        if (is_rvv_vload) begin
            ctrl.rs1_class = CLASS_SCALAR; // base address
            ctrl.rd_class  = CLASS_VEC;
            ctrl.rd_is_vec = 1'b1;
            ctrl.uses_rs1  = 1'b1;
            ctrl.uses_rs2  = 1'b0; // lumop for unit-stride
            ctrl.uses_rd   = 1'b1;
        end

        // --- RVV vector stores (OP_VS = STORE-FP 0x27) ---
        if (is_rvv_vstore) begin
            ctrl.rs1_class = CLASS_SCALAR; // base address
            ctrl.rs2       = rd;           // vs3 (data source) is in inst[11:7]
            ctrl.rs2_class = CLASS_VEC;
            ctrl.rd        = 5'd0;
            ctrl.uses_rs1  = 1'b1;
            ctrl.uses_rs2  = 1'b1;
            ctrl.uses_rd   = 1'b0;
            ctrl.rd_is_vec = 1'b0;
        end

        // --- vsetvl/vsetvli/vsetivli (OP_V with funct3=OPCFG) ---
        if (is_rvv_cfg) begin
            ctrl.is_vector = 1'b0; // not a vector pipeline op
            ctrl.rd_is_vec = 1'b0;
            ctrl.rd_class  = CLASS_SCALAR;
            ctrl.rs1_class = CLASS_SCALAR;
            ctrl.rs2_class = CLASS_SCALAR;
            ctrl.uses_rd   = 1'b1;
            if (inst[31] == 1'b0) begin
                // vsetvli: zimm[10:0] = inst[30:20]
                ctrl.uses_rs1 = 1'b1;
                ctrl.uses_rs2 = 1'b0;
            end else if (inst[30] == 1'b1) begin
                // vsetivli: zimm[9:0] = inst[29:20], uimm[4:0] = inst[19:15]
                ctrl.uses_rs1 = 1'b0;
                ctrl.uses_rs2 = 1'b0;
            end else begin
                // vsetvl: rs1 and rs2 are registers
                ctrl.uses_rs1 = 1'b1;
                ctrl.uses_rs2 = 1'b1;
            end
        end

        // --- Immediate selection ---
        unique case (opcode)
            OP_BRANCH:  ctrl.imm = imm_b;  // B-type for conditional branches
            OP_JAL:     ctrl.imm = imm_j;  // J-type for JAL
            OP_JALR:    ctrl.imm = imm_i;  // I-type for JALR
            OP_STORE:   ctrl.imm = imm_s;  // S-type for scalar stores
            OP_LUI:     ctrl.imm = imm_u;  // U-type for LUI
            OP_AUIPC:   ctrl.imm = imm_u;  // U-type for AUIPC
            OP_V: begin
                // vsetvli/vsetivli encode zimm in inst[30:20] or inst[29:20]
                if (is_rvv_cfg) begin
                    if (inst[31] == 1'b0)
                        ctrl.imm = {21'b0, inst[30:20]}; // vsetvli
                    else if (inst[30] == 1'b1)
                        ctrl.imm = {22'b0, inst[29:20]}; // vsetivli
                    else
                        ctrl.imm = 32'h0; // vsetvl (register-register)
                end else begin
                    ctrl.imm = 32'h0; // vector arithmetic — no immediate
                end
            end
            OP_VL:      ctrl.imm = 32'h0;  // RVV vector load — address in rs1
            OP_VS:      ctrl.imm = 32'h0;  // RVV vector store — address in rs1
            OP_CUSTOM2: begin
                // Vector atomics: S-type immediate
                if (is_c2_vatom) ctrl.imm = imm_s;
                else             ctrl.imm = 32'h0;
            end
            default:    ctrl.imm = imm_i;  // I-type default (OP_LOAD, OP_IMM, OP_SYSTEM, etc.)
        endcase
    end
endmodule
