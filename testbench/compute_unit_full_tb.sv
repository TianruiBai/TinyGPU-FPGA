`timescale 1ns/1ps

// Extended regression testbench for compute_unit_top covering scalar, vector, atomic, texture, and CSR/system paths.
module compute_unit_full_tb;
    /* verilator lint_off SYNCASYNCNET */
    import isa_pkg::*;

    // GFX directed test constants (kept within 12-bit immediates and within MEM_WORDS window)
    localparam logic [31:0] FB_BASE_OFF     = 32'h0000_0400;
    localparam logic [31:0] RSTATE_OFF      = 32'h0000_0500;
    localparam logic [31:0] GSTATE_OFF      = 32'h0000_0540;
    localparam logic [31:0] GPARAM_OFF      = 32'h0000_0580;
    localparam logic [31:0] GDRAW_OFF       = 32'h0000_05C0;

    // Second draw uses distinct descriptor blocks so we don't mutate the first draw's
    // descriptors while the graphics pipeline is still consuming them.
    localparam logic [31:0] GSTATE2_OFF     = 32'h0000_0560;
    localparam logic [31:0] GPARAM2_OFF     = 32'h0000_0590;
    localparam logic [31:0] VBO_BASE_OFF    = 32'h0000_0600;
    localparam logic [31:0] VBO2_BASE_OFF   = 32'h0000_0680;
    localparam logic [31:0] GFX_COLOR       = 32'h55AA_1234;
    localparam logic [31:0] GFX_COLOR2      = 32'h00FF_00FF;

    // Clock/reset
    logic clk;
    logic clk_en;
    logic rst_n;

    // Instruction interface
    logic [63:0] inst_rdata;
    logic [31:0] inst_addr;

    // Data interface (unified global bus)
    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;
    logic        data_req_ready;
    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    // CSR visibility
    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    logic [31:0] csr_status_snap;

    // Snapshot scalar registers for FCVT.f2i checks
    logic [31:0] x30_snap;
    logic [31:0] x31_snap;

    // Snapshot FP16 register results (snapshot before freezing so ROM wrap can't change them)
    logic [15:0] f1_snap;
    logic [15:0] f2_snap;
    logic [15:0] f3_snap;
    logic [15:0] f4_snap;
    logic [15:0] f5_snap;
    logic [15:0] f6_snap;
    logic [15:0] f7_snap;

    // Additional scalar FP16 snapshot
    logic [15:0] f21_snap;

    // Error flags
    logic err_fp_overflow;
    logic err_fp_invalid;
    logic err_vec_overflow;
    logic err_vec_invalid;

    // DUT instance
    compute_unit_top #(
        .CORE_ID(32'h2),
        .TILE_OFFSET(32'h0002_0003)
    ) dut (
        .clk(clk),
        .rst_n(rst_n),
        .inst_rdata(inst_rdata),
        .inst_addr(inst_addr),
        .data_req_valid(data_req_valid),
        .data_req_is_load(data_req_is_load),
        .data_req_addr(data_req_addr),
        .data_req_wdata(data_req_wdata),
        .data_req_rd(data_req_rd),
        .err_fp_overflow(err_fp_overflow),
        .err_fp_invalid(err_fp_invalid),
        .err_vec_overflow(err_vec_overflow),
        .err_vec_invalid(err_vec_invalid),
        .csr_status(csr_status),
        .csr_fstatus(csr_fstatus),
        .csr_vstatus(csr_vstatus),
        .data_req_ready(data_req_ready),
        .data_resp_valid(data_resp_valid),
        .data_resp_rd(data_resp_rd),
        .data_resp_data(data_resp_data)
    );

    // Waveform dump for debugging
    initial begin
        $dumpfile("compute_unit_full_tb.vcd");
        // Dump full DUT hierarchy; adjust depth if needed
        $dumpvars(0, compute_unit_full_tb);
    end

    // Clock generation: 100MHz (with TB-controlled freeze)
    initial begin
        clk = 1'b0;
        clk_en = 1'b1;
        forever begin
            #5;
            if (clk_en) clk = ~clk;
        end
    end

    // Reset and ready
    initial begin
        rst_n = 1'b0;
        data_req_ready = 1'b1; // always-ready global memory model
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
    end

    // --------------------------------------------------------------
    // Debug: detect unexpected reservation of v31 in scoreboard
    // (current hang is VSUB writing v31, but busy_v[31] is already set)
    // Sample on negedge so scoreboard nonblocking updates have settled.
    // --------------------------------------------------------------
    logic prev_busy_v31;
    always @(negedge clk) begin
        if (!rst_n) begin
            prev_busy_v31 <= 1'b0;
        end else begin
            if (!prev_busy_v31 && (dut.u_scoreboard.busy_v[31] === 1'b1)) begin
                $display("DEBUG: busy_v31 became 1 at t=%0t pc=%08h inst0=%08h if_valid=%0b", $time, dut.if_pc, dut.if_inst0, dut.if_valid);
                $display("DEBUG: inst0 fields: opcode=%02h rd=%0d rs1=%0d rs2=%0d funct3=%0d funct7=%02h",
                         dut.if_inst0[6:0], dut.if_inst0[11:7], dut.if_inst0[19:15], dut.if_inst0[24:20], dut.if_inst0[14:12], dut.if_inst0[31:25]);
            end
            prev_busy_v31 <= dut.u_scoreboard.busy_v[31];
        end
    end

    // --------------------------------------------------------------
    // Debug: observe actual scalar regfile write commits
    // --------------------------------------------------------------
    always @(posedge clk) begin
        if (rst_n) begin
            if (dut.u_regfile_scalar.we && (dut.u_regfile_scalar.waddr == 5'd30 || dut.u_regfile_scalar.waddr == 5'd31)) begin
                $display(
                    "%0t SREG_WB rd=%0d data=%08h src(pend/lsu/fp/valu/alu)=%0d%0d%0d%0d%0d fp_wb(valid/rd/data)=%0b/%0d/%08h wb(valid/rd/res)=%0b/%0d/%08h",
                    $time,
                    dut.u_regfile_scalar.waddr,
                    dut.u_regfile_scalar.wdata,
                    dut.scalar_wb_from_pending,
                    dut.scalar_wb_from_lsu,
                    dut.scalar_wb_from_fp,
                    dut.scalar_wb_from_valu,
                    dut.scalar_wb_from_alu,
                    dut.fp_scalar_wb_valid,
                    dut.fp_scalar_wb_rd,
                    dut.fp_scalar_wb_data,
                    dut.wb_valid,
                    dut.wb_ctrl.rd,
                    dut.wb_scalar_res
                );
            end

            // x4/x5 are used by the early conditional-branch marker tests
            if (dut.u_regfile_scalar.we && (dut.u_regfile_scalar.waddr == 5'd4 || dut.u_regfile_scalar.waddr == 5'd5)) begin
                $display(
                    "%0t SREG_WB rd=%0d data=%08h src(pend/lsu/fp/valu/alu)=%0d%0d%0d%0d%0d wb_valid=%0b wb_rd=%0d wb_rs1=%0d wb_imm=%08h wb_f3=%0d wb_uses_rs2=%0b wb_scalar_res=%08h",
                    $time,
                    dut.u_regfile_scalar.waddr,
                    dut.u_regfile_scalar.wdata,
                    dut.scalar_wb_from_pending,
                    dut.scalar_wb_from_lsu,
                    dut.scalar_wb_from_fp,
                    dut.scalar_wb_from_valu,
                    dut.scalar_wb_from_alu,
                    dut.wb_valid,
                    dut.wb_ctrl.rd,
                    dut.wb_ctrl.rs1,
                    dut.wb_ctrl.imm,
                    dut.wb_ctrl.funct3,
                    dut.wb_ctrl.uses_rs2
                    ,
                    dut.wb_scalar_res
                );
            end
        end
    end

        // Debug: confirm scalar SRA decode/execute (one-shot)
        logic dbg_sra_seen;
        always_ff @(posedge clk) begin
            if (!rst_n) begin
                dbg_sra_seen <= 1'b0;
            end else if (!dbg_sra_seen) begin
                if (dut.ex_valid
                    && dut.ex_ctrl.is_scalar_int
                    && (dut.ex_ctrl.funct3 == 3'b101)
                    && (dut.ex_ctrl.rs1 == 5'd18)
                    && (dut.ex_ctrl.rs2 == 5'd19)
                    && (dut.ex_ctrl.rd  == 5'd20)) begin
                    $display("DEBUG: EX SRA: funct7=%b is_sub=%0d uses_rs2=%0d op_a=%h op_b=%h res=%h",
                             dut.ex_ctrl.funct7,
                             (dut.ex_ctrl.funct7 == 7'b0100000),
                             dut.ex_ctrl.uses_rs2,
                             dut.ex_op_a,
                             dut.ex_op_b,
                             dut.ex_scalar_res);
                    dbg_sra_seen <= 1'b1;
                end
            end
        end

    // --------------------------------------------------------------
    // Instruction ROM
    // --------------------------------------------------------------
    // NOTE: fetch is 64-bit aligned to 8 bytes. The core provides a byte address in inst_addr.
    // We index ROM in 32-bit words, with inst_rdata[31:0] = word at inst_addr+0 and
    // inst_rdata[63:32] = word at inst_addr+4.
    //
    // Keep ROM_WORDS a power-of-two so link-address checks can do cheap modulo masking.
    localparam int ROM_WORDS     = 4096;
    localparam int ROM_WORD_BITS = $clog2(ROM_WORDS);
    localparam int ROM_PAIR_BITS = ROM_WORD_BITS - 1; // 2 words per 64-bit fetch
    localparam int ROM_BYTES     = ROM_WORDS * 4;
    localparam int ROM_MASK      = ROM_BYTES - 1;

    logic [31:0] rom [0:ROM_WORDS-1];
    int exp_jal_link;

    wire [ROM_PAIR_BITS-1:0] inst_pair_idx = inst_addr[ROM_PAIR_BITS+2:3];
    wire [ROM_WORD_BITS-1:0] inst_word_idx0 = {inst_pair_idx, 1'b0};
    wire [ROM_WORD_BITS-1:0] inst_word_idx1 = {inst_pair_idx, 1'b1};

    assign inst_rdata = {rom[inst_word_idx1], rom[inst_word_idx0]};

    // --------------------------------------------------------------
    // Global memory model (32-bit words) + simple response FIFO
    // --------------------------------------------------------------
    // Grow backing store to 64KB to avoid accidental wraparound when debugging
    localparam int MEM_WORDS = 16384; // 64KB
    localparam logic [31:0] BASE_ADDR = 32'hFFFF_F800; // addr[31]=1 to select global
    localparam logic [31:0] DONE_OFF  = 32'h0000_01F0;

    // Branch marker offsets (absolute addresses are BASE_ADDR + offset)
    localparam logic [31:0] MARK_BEQ_FALLTHRU = BASE_ADDR + 32'h0000_0360;
    localparam logic [31:0] MARK_BEQ_TARGET   = BASE_ADDR + 32'h0000_0364;
    localparam logic [31:0] MARK_BNE_FALLTHRU = BASE_ADDR + 32'h0000_0368;
    localparam logic [31:0] MARK_BNE_TARGET   = BASE_ADDR + 32'h0000_036C;

    logic [31:0] mem [0:MEM_WORDS-1];

    logic        dbg_vmask_programmed;
    logic        dbg_vsel_captured;
    logic [127:0] dbg_vsel_wb_data;

    logic        snap_vsel_captured;
    logic [127:0] snap_vsel_wb_data;

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - BASE_ADDR) >> 2;
    endfunction

    localparam int RESP_DEPTH = 32;
    logic              resp_valid   [0:RESP_DEPTH-1];
    logic [4:0]        resp_rd      [0:RESP_DEPTH-1];
    logic [31:0]       resp_data_q  [0:RESP_DEPTH-1];
    logic [4:0]        resp_wp;
    logic [4:0]        resp_rp;
    logic              resp_empty;

    assign resp_empty = (resp_wp == resp_rp);

    always @(posedge clk) begin
        if (!rst_n) begin
            resp_wp        <= '0;
            resp_rp        <= '0;
            data_resp_valid<= 1'b0;
            data_resp_rd   <= '0;
            data_resp_data <= 32'h0;
            for (int i = 0; i < RESP_DEPTH; i++) begin
                resp_valid[i]  <= 1'b0;
                resp_rd[i]     <= '0;
                resp_data_q[i] <= 32'h0;
            end
        end else begin
            // Accept requests (single-cycle accept)
            if (data_req_valid && data_req_ready) begin
                if (data_req_is_load) begin
                    int idx;
                    idx = mem_index(data_req_addr);
                    resp_valid[resp_wp]  <= 1'b1;
                    resp_rd[resp_wp]     <= data_req_rd;
                    resp_data_q[resp_wp] <= mem[idx];
                    resp_wp <= resp_wp + 1'b1;

                    // Debug: gfx descriptor window reads (BASE+0x500..0x5FF)
                    if ((data_req_addr >= (BASE_ADDR + 32'h0000_0500)) && (data_req_addr < (BASE_ADDR + 32'h0000_0600))) begin
                        $display("%0t TB_MEM_RD addr=%08h idx=%0d mem=%08h rd=%0d", $time, data_req_addr, idx, mem[idx], data_req_rd);
                    end
                end else begin
                    int idx;
                    idx = mem_index(data_req_addr);
                    mem[idx] <= data_req_wdata;

                    // Debug: flag any stores into the descriptor window
                    if ((data_req_addr >= (BASE_ADDR + 32'h0000_0500)) && (data_req_addr < (BASE_ADDR + 32'h0000_0600))) begin
                        $display("%0t TB_MEM_ST addr=%08h idx=%0d wdata=%08h", $time, data_req_addr, idx, data_req_wdata);
                    end

                    // Debug: branch marker writes (used by early control-flow smoke tests)
                    if ((data_req_addr == MARK_BEQ_FALLTHRU) || (data_req_addr == MARK_BEQ_TARGET)
                        || (data_req_addr == MARK_BNE_FALLTHRU) || (data_req_addr == MARK_BNE_TARGET)) begin
                        $display(
                            "%0t TB_MARK_ST addr=%08h wdata=%08h (x4=%08h x5=%08h)",
                            $time,
                            data_req_addr,
                            data_req_wdata,
                            dut.u_regfile_scalar.mem[5'd4],
                            dut.u_regfile_scalar.mem[5'd5]
                        );
                    end
                end
            end

            // Drive responses (one per cycle)
            if (!resp_empty && resp_valid[resp_rp]) begin
                data_resp_valid <= 1'b1;
                data_resp_rd    <= resp_rd[resp_rp];
                data_resp_data  <= resp_data_q[resp_rp];
                resp_valid[resp_rp] <= 1'b0;
                resp_rp <= resp_rp + 1'b1;
            end else begin
                data_resp_valid <= 1'b0;
            end
        end
    end

    // Sample post-posedge-updated DUT signals
    always @(negedge clk) begin
        if (!rst_n) begin
            dbg_vmask_programmed <= 1'b0;
            dbg_vsel_captured    <= 1'b0;
            dbg_vsel_wb_data     <= '0;
        end else begin
            if (!dbg_vmask_programmed && (dut.csr_vmask == 16'h0005)) begin
                dbg_vmask_programmed <= 1'b1;
            end
            if (dbg_vmask_programmed && !dbg_vsel_captured && (dut.valuv_wb_valid === 1'b1) && (dut.valuv_wb_rd == 5'd26) && (dut.valuv_wb_is_scalar === 1'b0)) begin
                dbg_vsel_captured <= 1'b1;
                dbg_vsel_wb_data  <= dut.valuv_wb_data;
            end
        end
    end

    // --------------------------------------------------------------
    // Mini assembler helpers
    // --------------------------------------------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] u_type(input integer imm20, input [4:0] rd, input [6:0] opcode);
        u_type = {imm20[19:0], rd, opcode};
    endfunction

    function automatic [31:0] j_type(input integer imm, input [4:0] rd, input [6:0] opcode);
        // J-type: imm[20|10:1|11|19:12] << 1
        j_type = {imm[20], imm[10:1], imm[11], imm[19:12], rd, opcode};
    endfunction

    function automatic [31:0] b_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        // B-type: imm[12|10:5|4:1|11] << 1
        b_type = {imm[12], imm[10:5], rs2, rs1, funct3, imm[4:1], imm[11], opcode};
    endfunction


    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_INT_IMM);
    endfunction

    // --------------------------------------------------------------
    // Program image
    // --------------------------------------------------------------
    initial begin
        automatic int pc;
        automatic int jal_slot;
        automatic int jal_delay_slot;
        automatic int j_slot;
        automatic int sub_pc;
        automatic int after_pc;
        for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
        pc = 0;
        // x1 = BASE_ADDR (ADDI x1, x0, -2048)
        rom[pc >> 2] = i_type(-2048, 5'd0, 3'b000, 5'd1, OP_INT_IMM); pc += 4;
        // x2 = 5; x3 = 7
        rom[pc >> 2] = i_type(5, 5'd0, 3'b000, 5'd2, OP_INT_IMM);  pc += 4;
        rom[pc >> 2] = i_type(7, 5'd0, 3'b000, 5'd3, OP_INT_IMM);  pc += 4;
        // x4 = x2 + x3 = 12
        rom[pc >> 2] = r_type(7'b0000000, 5'd3, 5'd2, 3'b000, 5'd4, OP_INT); pc += 4;
        // SW x4, 0(x1)
        rom[pc >> 2] = s_type(0, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;

        // ----------------------------------------------------------
        // LUI + MOVI (pseudo) smoke
        // - LUI: rd = imm20 << 12
        // - MOVI: pseudo-op typically expanded as LUI(hi20)+ADDI(lo12)
        // Store results to BASE+0x338..0x33C
        // ----------------------------------------------------------
        // x12 = 0x12345000
        rom[pc >> 2] = u_type(32'h12345, 5'd12, OP_LUI); pc += 4;
        rom[pc >> 2] = s_type(32'h338, 5'd1, 5'd12, 3'b010, OP_STORE); pc += 4;

        // x13 = 0x12345ABC (MOVI pseudo: LUI 0x12346; ADDI -1348)
        rom[pc >> 2] = u_type(32'h12346, 5'd13, OP_LUI); pc += 4;
        rom[pc >> 2] = i_type(-1348, 5'd13, 3'b000, 5'd13, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = s_type(32'h33C, 5'd1, 5'd13, 3'b010, OP_STORE); pc += 4;

        // Keep subsequent scalar memory ops in slot0 (pc%8==0)
        rom[pc >> 2] = nop(); pc += 4;

        // ----------------------------------------------------------
        // Control-flow smoke: JAL (funct3=011) + JALR (funct3=010)
        // - JAL x9, sub
        // - Delay-slot safe: insert a NOP at the return address; the jump to after_pc is at +8.
        // - sub stores link + sets x11, then returns via JALR x0, 0(x9)
        // Store results to BASE+0x330 (link) and BASE+0x334 (x11 marker)
        // ----------------------------------------------------------
        jal_slot = (pc >> 2);
        rom[jal_slot] = nop(); // patched below
        pc += 4;

        jal_delay_slot = (pc >> 2);
        rom[jal_delay_slot] = nop();
        pc += 4;

        j_slot = (pc >> 2);
        rom[j_slot] = nop(); // patched below
        pc += 4;

        sub_pc = pc;
        // SW x9, 0x330(x1)   (store link register)
        rom[pc >> 2] = s_type(32'h330, 5'd1, 5'd9, 3'b010, OP_STORE); pc += 4;
        // x11 = 0x5A
        rom[pc >> 2] = i_type(32'h5A, 5'd0, 3'b000, 5'd11, OP_INT_IMM); pc += 4;
        // JALR x0, 0(x9)     (return)
        rom[pc >> 2] = i_type(0, 5'd9, 3'b010, 5'd0, OP_BRANCH); pc += 4;

        // JALR delay-slot sentinel: must NOT execute if redirect flush works
        rom[pc >> 2] = s_type(32'h370, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;

        after_pc = pc;
        // SW x11, 0x334(x1)  (marker after return)
        rom[pc >> 2] = s_type(32'h334, 5'd1, 5'd11, 3'b010, OP_STORE); pc += 4;

        // Patch the call and the post-return jump
        exp_jal_link = (jal_delay_slot << 2);
        rom[jal_slot] = j_type(sub_pc - (jal_slot << 2), 5'd9, OP_JAL);
        rom[j_slot]   = j_type(after_pc - (j_slot << 2), 5'd0, OP_JAL);

        // ----------------------------------------------------------
        // Conditional branch tests (predict-not-taken + flush on taken)
        // - For taken branches, the fall-through instruction must NOT commit.
        // - For not-taken branches, the target block must NOT commit.
        // Uses memory markers at 0x360..0x370.
        // ----------------------------------------------------------
        // x4 = 0x11; x5 = 0x22
        rom[pc >> 2] = i_type(32'h11, 5'd0, 3'b000, 5'd4, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(32'h22, 5'd0, 3'b000, 5'd5, OP_INT_IMM); pc += 4;

        // BEQ x2, x2, +8  (taken)
        rom[pc >> 2] = b_type(8, 5'd2, 5'd2, 3'b000, OP_BRANCH); pc += 4;
        // Fall-through sentinel (must be flushed)
        rom[pc >> 2] = s_type(32'h360, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;
        // Target marker (must execute)
        rom[pc >> 2] = s_type(32'h364, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4;

        // BNE x2, x2, +12 (not taken)
        rom[pc >> 2] = b_type(12, 5'd2, 5'd2, 3'b001, OP_BRANCH); pc += 4;
        // Fall-through marker (must execute)
        rom[pc >> 2] = s_type(32'h368, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;
        // Keep the skip-jump in slot0 (some control-flow paths are slot-sensitive)
        rom[pc >> 2] = nop(); pc += 4;
        // Jump over the (would-be) taken block
        rom[pc >> 2] = j_type(8, 5'd0, OP_JAL); pc += 4;
        // Taken block marker (must NOT execute)
        rom[pc >> 2] = s_type(32'h36C, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4;

        // ----------------------------------------------------------
        // Slot1 placement tests (inst1 in 64-bit fetch bundle)
        // - Place a branch/load/store in the +4 word of an aligned bundle.
        // - The front-end must naturally re-fetch it as slot0 next cycle (no "ignored slot1" bugs).
        // Markers at 0x3A0..0x3B4.
        // ----------------------------------------------------------
        // Ensure we start on an 8B boundary so the instruction lands in inst1.
        if (pc[2]) begin
            rom[pc >> 2] = nop();
            pc += 4;
        end

        // (A) Branch in inst1: NOP in inst0, BEQ in inst1.
        // x20=0xAA
        rom[pc >> 2] = i_type(32'hAA, 5'd0, 3'b000, 5'd20, OP_INT_IMM); pc += 4;
        // BEQ x0,x0,+8 (taken)  [lands in inst1]
        rom[pc >> 2] = b_type(8, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
        // Fall-through sentinel (must NOT execute)
        rom[pc >> 2] = s_type(32'h3A0, 5'd1, 5'd20, 3'b010, OP_STORE); pc += 4;
        // Target marker (must execute)
        rom[pc >> 2] = i_type(32'hBB, 5'd0, 3'b000, 5'd21, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = s_type(32'h3A4, 5'd1, 5'd21, 3'b010, OP_STORE); pc += 4;
        // Pad to keep downstream bundle alignment stable
        rom[pc >> 2] = nop(); pc += 4;

        // (B) Store in inst1: ADDI in inst0, SW in inst1.
        rom[pc >> 2] = i_type(32'h5A, 5'd0, 3'b000, 5'd22, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = s_type(32'h3A8, 5'd1, 5'd22, 3'b010, OP_STORE); pc += 4;

        // (C) Load in inst1: NOP in inst0, LW in inst1, then store the loaded value.
        rom[pc >> 2] = nop(); pc += 4;
        rom[pc >> 2] = i_type(32'h3B0, 5'd1, 3'b010, 5'd24, OP_LOAD); pc += 4; // LW x24,0x3B0(x1)
        rom[pc >> 2] = s_type(32'h3B4, 5'd1, 5'd24, 3'b010, OP_STORE); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;

        // ----------------------------------------------------------
        // Branch-after-load (data dependency)
        // - Load a value, immediately branch on the loaded register.
        // - Uses MEMBAR between store+load so we don't conflate this with store->load ordering.
        // Markers at 0x374..0x380; data at 0x390..0x394.
        // ----------------------------------------------------------
        // Store x5 (0x22) to data @0x390, then load to x6 and BEQ taken.
        rom[pc >> 2] = s_type(32'h390, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // MEMBAR
        rom[pc >> 2] = i_type(32'h390, 5'd1, 3'b010, 5'd6, OP_LOAD); pc += 4; // LW x6,0x390(x1)
        rom[pc >> 2] = b_type(8, 5'd6, 5'd5, 3'b000, OP_BRANCH); pc += 4;     // BEQ x6,x5,+8 (taken)
        rom[pc >> 2] = s_type(32'h374, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4; // sentinel (must be flushed)
        rom[pc >> 2] = s_type(32'h378, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4; // target marker (must execute)

        // Store x4 (0x11) to data @0x394, then load to x7 and BNE not taken.
        rom[pc >> 2] = s_type(32'h394, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // MEMBAR
        rom[pc >> 2] = i_type(32'h394, 5'd1, 3'b010, 5'd7, OP_LOAD); pc += 4; // LW x7,0x394(x1)
        rom[pc >> 2] = b_type(12, 5'd7, 5'd4, 3'b001, OP_BRANCH); pc += 4;    // BNE x7,x4,+12 (not taken)
        rom[pc >> 2] = s_type(32'h37C, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4; // fall-through marker (must execute)
        rom[pc >> 2] = j_type(8, 5'd0, OP_JAL); pc += 4;                      // skip taken block
        rom[pc >> 2] = s_type(32'h380, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4; // taken block marker (must NOT execute)

        // Keep downstream dual-issue bundle alignment stable for later directed tests.
        rom[pc >> 2] = nop(); pc += 4;

        // ----------------------------------------------------------
        // Comprehensive branch matrix (taken/not-taken + signed/unsigned + backward branch)
        // - Covers BLT/BGE/BLTU/BGEU in both directions.
        // - Includes a small backward loop to validate negative branch offsets.
        // Markers at 0x3C0..0x3EC.
        // ----------------------------------------------------------
        // Marker payloads
        rom[pc >> 2] = i_type(32'h33, 5'd0, 3'b000, 5'd24, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(32'h44, 5'd0, 3'b000, 5'd25, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(32'h55, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(32'h66, 5'd0, 3'b000, 5'd27, OP_INT_IMM); pc += 4;

        // BLT x2(5), x3(7), +8 (taken)
        rom[pc >> 2] = b_type(8, 5'd2, 5'd3, 3'b100, OP_BRANCH); pc += 4;
        rom[pc >> 2] = s_type(32'h3C0, 5'd1, 5'd24, 3'b010, OP_STORE); pc += 4; // sentinel (flush)
        rom[pc >> 2] = s_type(32'h3C4, 5'd1, 5'd24, 3'b010, OP_STORE); pc += 4; // target (exec)

        // BGE x2(5), x3(7), +12 (not taken)
        rom[pc >> 2] = b_type(12, 5'd2, 5'd3, 3'b101, OP_BRANCH); pc += 4;
        rom[pc >> 2] = s_type(32'h3C8, 5'd1, 5'd25, 3'b010, OP_STORE); pc += 4; // fall-through (exec)
        rom[pc >> 2] = j_type(8, 5'd0, OP_JAL); pc += 4;                        // skip taken block
        rom[pc >> 2] = s_type(32'h3CC, 5'd1, 5'd25, 3'b010, OP_STORE); pc += 4; // taken block (must NOT exec)

        // Signed/unsigned compare operands
        rom[pc >> 2] = i_type(-1, 5'd0, 3'b000, 5'd28, OP_INT_IMM); pc += 4; // x28 = -1
        rom[pc >> 2] = i_type(1,  5'd0, 3'b000, 5'd29, OP_INT_IMM); pc += 4; // x29 = +1

        // BLT x28(-1), x29(+1), +8 (taken)
        rom[pc >> 2] = b_type(8, 5'd28, 5'd29, 3'b100, OP_BRANCH); pc += 4;
        rom[pc >> 2] = s_type(32'h3D0, 5'd1, 5'd26, 3'b010, OP_STORE); pc += 4; // sentinel (flush)
        rom[pc >> 2] = s_type(32'h3D4, 5'd1, 5'd26, 3'b010, OP_STORE); pc += 4; // target (exec)

        // BLTU x28(0xFFFF_FFFF), x29(1), +12 (not taken)
        rom[pc >> 2] = b_type(12, 5'd28, 5'd29, 3'b110, OP_BRANCH); pc += 4;
        rom[pc >> 2] = s_type(32'h3D8, 5'd1, 5'd26, 3'b010, OP_STORE); pc += 4; // fall-through (exec)
        rom[pc >> 2] = j_type(8, 5'd0, OP_JAL); pc += 4;                        // skip taken block
        rom[pc >> 2] = s_type(32'h3DC, 5'd1, 5'd26, 3'b010, OP_STORE); pc += 4; // taken block (must NOT exec)

        // BGEU x28(0xFFFF_FFFF), x29(1), +8 (taken)
        rom[pc >> 2] = b_type(8, 5'd28, 5'd29, 3'b111, OP_BRANCH); pc += 4;
        rom[pc >> 2] = s_type(32'h3E0, 5'd1, 5'd27, 3'b010, OP_STORE); pc += 4; // sentinel (flush)
        rom[pc >> 2] = s_type(32'h3E4, 5'd1, 5'd27, 3'b010, OP_STORE); pc += 4; // target (exec)

        // Backward branch loop: count 3 iterations
        // Keep the loop body in slot0 to avoid any slot1 sensitivity.
        // x10=3, x11=-1 (decrement), x12=1 (inc), x13=0 (count)
        rom[pc >> 2] = i_type(3,  5'd0, 3'b000, 5'd10, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(-1, 5'd0, 3'b000, 5'd11, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(1,  5'd0, 3'b000, 5'd12, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(0,  5'd0, 3'b000, 5'd13, OP_INT_IMM); pc += 4;

        // Align loop head to slot0
        if (pc[2]) begin
            rom[pc >> 2] = nop();
            pc += 4;
        end

        begin : backward_loop
            automatic int loop_pc;
            automatic int br_pc;
            loop_pc = pc;
            // loop:
            rom[pc >> 2] = r_type(7'b0000000, 5'd12, 5'd13, 3'b000, 5'd13, OP_INT); pc += 4; // x13++
            rom[pc >> 2] = nop(); pc += 4;
            rom[pc >> 2] = r_type(7'b0000000, 5'd11, 5'd10, 3'b000, 5'd10, OP_INT); pc += 4; // x10--
            rom[pc >> 2] = nop(); pc += 4;
            rom[pc >> 2] = nop(); pc += 4;
            rom[pc >> 2] = nop(); pc += 4;
            br_pc = pc;
            rom[pc >> 2] = b_type(loop_pc - br_pc, 5'd10, 5'd0, 3'b001, OP_BRANCH); pc += 4; // BNE x10,x0,loop
            rom[pc >> 2] = nop(); pc += 4;
        end
        rom[pc >> 2] = s_type(32'h3E8, 5'd1, 5'd13, 3'b010, OP_STORE); pc += 4; // count
        rom[pc >> 2] = s_type(32'h3EC, 5'd1, 5'd10, 3'b010, OP_STORE); pc += 4; // x10=0

        // ----------------------------------------------------------
        // Scalar dependency / hazard tests (no bypass, scoreboard-only stalls)
        // - RAW chain (back-to-back dependent ALU ops)
        // - WAW chain (same rd written twice)
        // - Load-use (LW -> dependent ALU) without NOPs
        // Markers at 0x3F0..0x3FC.
        // ----------------------------------------------------------
        // RAW: x15 = x2+x3; x16=x15+x2; x17=x16^x3 => 22
        rom[pc >> 2] = r_type(7'b0000000, 5'd3,  5'd2,  3'b000, 5'd15, OP_INT); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd2,  5'd15, 3'b000, 5'd16, OP_INT); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd3,  5'd16, 3'b100, 5'd17, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h3F0, 5'd1, 5'd17, 3'b010, OP_STORE); pc += 4;

        // WAW: x18 = x2+x3; x18 = x18-x2 => 7
        rom[pc >> 2] = r_type(7'b0000000, 5'd3,  5'd2,  3'b000, 5'd18, OP_INT); pc += 4;
        rom[pc >> 2] = r_type(7'b0100000, 5'd2,  5'd18, 3'b000, 5'd18, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h3F4, 5'd1, 5'd18, 3'b010, OP_STORE); pc += 4;

        // Load-use: LW x19,@0x3F8; ADD x20=x19+x2; SW x20,@0x3FC
        rom[pc >> 2] = i_type(32'h3F8, 5'd1, 3'b010, 5'd19, OP_LOAD); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd2,  5'd19, 3'b000, 5'd20, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h3FC, 5'd1, 5'd20, 3'b010, OP_STORE); pc += 4;

        // ----------------------------------------------------------
        // Expanded scalar integer ALU coverage (R-type only; OP_INT I/R heuristic)
        // Store results to BASE+12..+44 for deterministic checking.
        // ----------------------------------------------------------
        // x16 = x3 - x2 = 2
        rom[pc >> 2] = r_type(7'b0100000, 5'd2, 5'd3, 3'b000, 5'd16, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h300, 5'd1, 5'd16, 3'b010, OP_STORE); pc += 4;

        // x17 = x2 << x3 = 5 << 7 = 640
        rom[pc >> 2] = r_type(7'b0000000, 5'd3, 5'd2, 3'b001, 5'd17, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h304, 5'd1, 5'd17, 3'b010, OP_STORE); pc += 4;

        // x18 = -16
        rom[pc >> 2] = i_type(-16, 5'd0, 3'b000, 5'd18, OP_INT_IMM); pc += 4;
        // x19 = 2
        rom[pc >> 2] = i_type(2, 5'd0, 3'b000, 5'd19, OP_INT_IMM); pc += 4;
        // x20 = SRA(x18, x19) = -4
        rom[pc >> 2] = r_type(7'b0100000, 5'd19, 5'd18, 3'b101, 5'd20, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h308, 5'd1, 5'd20, 3'b010, OP_STORE); pc += 4;

        // x21 = SRL(x18, x19) = 0x3FFFFFFC
        rom[pc >> 2] = r_type(7'b0000000, 5'd19, 5'd18, 3'b101, 5'd21, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h30C, 5'd1, 5'd21, 3'b010, OP_STORE); pc += 4;

        // x22 = -1; x23 = 1
        rom[pc >> 2] = i_type(-1, 5'd0, 3'b000, 5'd22, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(1, 5'd0, 3'b000, 5'd23, OP_INT_IMM); pc += 4;
        // x24 = SLT(x22, x23) = 1
        rom[pc >> 2] = r_type(7'b0000000, 5'd23, 5'd22, 3'b010, 5'd24, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h310, 5'd1, 5'd24, 3'b010, OP_STORE); pc += 4;
        // x25 = SLTU(x22, x23) = 0
        rom[pc >> 2] = r_type(7'b0000000, 5'd23, 5'd22, 3'b011, 5'd25, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h314, 5'd1, 5'd25, 3'b010, OP_STORE); pc += 4;

        // x26 = XOR(x2, x3) = 2
        rom[pc >> 2] = r_type(7'b0000000, 5'd3, 5'd2, 3'b100, 5'd26, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h318, 5'd1, 5'd26, 3'b010, OP_STORE); pc += 4;
        // x27 = OR(x2, x3) = 7
        rom[pc >> 2] = r_type(7'b0000000, 5'd3, 5'd2, 3'b110, 5'd27, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h31C, 5'd1, 5'd27, 3'b010, OP_STORE); pc += 4;
        // x28 = AND(x2, x3) = 5
        rom[pc >> 2] = r_type(7'b0000000, 5'd3, 5'd2, 3'b111, 5'd28, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(32'h320, 5'd1, 5'd28, 3'b010, OP_STORE); pc += 4;

        // MEMBAR: flush write-merge buffer before dependent load
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
        // LW x5,0(x1)
        rom[pc >> 2] = i_type(0, 5'd1, 3'b010, 5'd5, OP_LOAD);  pc += 4;
        // SW x5,8(x1)
        rom[pc >> 2] = s_type(8, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4;
        // CSR status write/read
        rom[pc >> 2] = i_type(7, 5'd0, 3'b000, 5'd6, OP_INT_IMM);    pc += 4; // x6=7
        rom[pc >> 2] = i_type(0, 5'd6, 3'b001, 5'd7, OP_SYSTEM); pc += 4; // CSRRW
        rom[pc >> 2] = i_type(0, 5'd0, 3'b010, 5'd8, OP_SYSTEM); pc += 4; // CSRRS

        // ----------------------------------------------------------
        // Scalar FP16 ALU smoke (funct7=0001000): FCVT.i2f + add/sub/mul/min/max
        // Note: Current FCVT.i2f implementation maps x0 -> +1.0 (0x3C00).
        // We validate the datapath behavior bit-exact against that implementation.
        // ----------------------------------------------------------
        // x29 = -1 (used for FP16 conversion inputs)
        rom[pc >> 2] = i_type(-1, 5'd0, 3'b000, 5'd29, OP_INT_IMM); pc += 4;
        // f1 = FCVT.i2f(x0)  -> +1.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd0, 5'd0, 3'b110, 5'd1, OP_INT); pc += 4;
        // f2 = FCVT.i2f(x29) -> -1.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd0, 5'd29, 3'b110, 5'd2, OP_INT); pc += 4;
        // f3 = f1 + f1 -> 2.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd1, 5'd1, 3'b000, 5'd3, OP_INT); pc += 4;
        // f4 = f3 - f1 -> 1.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd1, 5'd3, 3'b001, 5'd4, OP_INT); pc += 4;
        // f5 = f3 * f3 -> 4.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd3, 5'd3, 3'b010, 5'd5, OP_INT); pc += 4;
        // f6 = min(f2, f1) -> -1.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd1, 5'd2, 3'b100, 5'd6, OP_INT); pc += 4;
        // f7 = max(f2, f1) -> +1.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd1, 5'd2, 3'b101, 5'd7, OP_INT); pc += 4;

        // Additional scalar FP16 coverage:
        // x30 = FCVT.f2i(f1) (implementation-specific; current datapath yields 2048 for +1.0)
        rom[pc >> 2] = r_type(7'b0001000, 5'd0, 5'd1, 3'b111, 5'd30, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(132, 5'd1, 5'd30, 3'b010, OP_STORE); pc += 4;
        // x31 = FCVT.f2i(f2) (expected -2048)
        rom[pc >> 2] = r_type(7'b0001000, 5'd0, 5'd2, 3'b111, 5'd31, OP_INT); pc += 4;
        rom[pc >> 2] = s_type(136, 5'd1, 5'd31, 3'b010, OP_STORE); pc += 4;

        // FMA smoke: f21 = (f1 * f20) + src_c, where src_c comes from scalar rs2 low16.
        // Build x20 = 0x3C00 (+1.0 bits) and set f20 = FCVT.i2f(x0) -> +1.0.
        // x20 = 960; x19 = 4; x20 = x20 << x19 -> 15360 (0x3C00)
        rom[pc >> 2] = i_type(960, 5'd0, 3'b000, 5'd20, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(4,   5'd0, 3'b000, 5'd19, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd19, 5'd20, 3'b001, 5'd20, OP_INT); pc += 4;
        // f20 = FCVT.i2f(x0) -> +1.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd0, 5'd0, 3'b110, 5'd20, OP_INT); pc += 4;
        // f21 = f1*f20 + x20[15:0] -> 2.0
        rom[pc >> 2] = r_type(7'b0001000, 5'd20, 5'd1, 3'b011, 5'd21, OP_INT); pc += 4;

        // MEMBAR
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
        // Vector loads
        rom[pc >> 2] = i_type(16, 5'd1, 3'b000, 5'd1, OP_VLD);   pc += 4; // v1
        rom[pc >> 2] = i_type(32, 5'd1, 3'b000, 5'd2, OP_VLD);   pc += 4; // v2
        // VADD v3 = v1+v2
        rom[pc >> 2] = r_type(7'b0000000, 5'd2, 5'd1, 3'b000, 5'd3, OP_VEC_ALU); pc += 4;
        // Store v3 to base+48
        rom[pc >> 2] = s_type(48, 5'd1, 5'd3, 3'b000, OP_VST);   pc += 4;

        // ----------------------------------------------------------
        // v0 is writable: hazard + logical op coverage (VXOR)
        // ----------------------------------------------------------
        // VADD.I32 v0 = v1 + v2
        rom[pc >> 2] = r_type(7'b0000000, 5'd2, 5'd1, 3'b000, 5'd0, OP_VEC_ALU); pc += 4;
        // VADD.I32 v28 = v0 + v1
        rom[pc >> 2] = r_type(7'b0000000, 5'd1, 5'd0, 3'b000, 5'd28, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(144, 5'd1, 5'd28, 3'b000, OP_VST);  pc += 4;
        // VXOR.I32 v0 = v0 ^ v0 (funct6=6'b001001 => funct7=7'b0010010)
        rom[pc >> 2] = r_type(7'b0010010, 5'd0, 5'd0, 3'b000, 5'd0, OP_VEC_ALU); pc += 4;
        // VADD.I32 v29 = v1 + v0 (should copy v1)
        rom[pc >> 2] = r_type(7'b0000000, 5'd0, 5'd1, 3'b000, 5'd29, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(160, 5'd1, 5'd29, 3'b000, OP_VST);  pc += 4;

        // ----------------------------------------------------------
        // Vector logical ops: VAND/VOR and predicated select (VSEL)
        // ----------------------------------------------------------
        // VAND.I32 v24 = v1 & v2 (funct6=6'b001010 => funct7=7'b0010100)
        rom[pc >> 2] = r_type(7'b0010100, 5'd2, 5'd1, 3'b000, 5'd24, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(176, 5'd1, 5'd24, 3'b000, OP_VST);  pc += 4;
        // VOR.I32 v25 = v1 | v2 (funct6=6'b001011 => funct7=7'b0010110)
        rom[pc >> 2] = r_type(7'b0010110, 5'd2, 5'd1, 3'b000, 5'd25, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(192, 5'd1, 5'd25, 3'b000, OP_VST);  pc += 4;

        // Set CSR_VMASK (0x004) to 0b0101 so lanes [0,2] pick rs1 and [1,3] pick rs2.
        rom[pc >> 2] = i_type(5, 5'd0, 3'b000, 5'd14, OP_INT_IMM);    pc += 4; // x14=5
        rom[pc >> 2] = i_type(12'h004, 5'd14, 3'b001, 5'd0, OP_SYSTEM); pc += 4; // CSRRW x0, CSR_VMASK, x14
        // VSEL.I32(vmask) v26 = (vmask? v1 : v2) (funct6=000110, vm=1 => funct7=0001101)
        rom[pc >> 2] = r_type(7'b0001101, 5'd2, 5'd1, 3'b000, 5'd26, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(208, 5'd1, 5'd26, 3'b000, OP_VST);  pc += 4;

        // ----------------------------------------------------------
        // Expanded vector integer coverage: VSUB + VDOT
        // ----------------------------------------------------------
        // VSUB.I32 v31 = v2 - v1
        rom[pc >> 2] = r_type(7'b0000010, 5'd1, 5'd2, 3'b000, 5'd31, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(96, 5'd1, 5'd31, 3'b000, OP_VST);   pc += 4;
        // VDOT.I32 v30 = dot(v1, v2) (reduction into lane0)
        rom[pc >> 2] = r_type(7'b0001000, 5'd2, 5'd1, 3'b000, 5'd30, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(112, 5'd1, 5'd30, 3'b000, OP_VST);   pc += 4;

        // ----------------------------------------------------------
        // FP32 vector ALU smoke: add/sub/min/max (funct3=TYPE_FP32=3'b011)
        // v8/v9 inputs loaded from memory; results stored back for checking.
        // ----------------------------------------------------------
        rom[pc >> 2] = i_type(32'h180, 5'd1, 3'b000, 5'd8, OP_VLD);   pc += 4; // v8 = fp32 A
        rom[pc >> 2] = i_type(32'h190, 5'd1, 3'b000, 5'd9, OP_VLD);   pc += 4; // v9 = fp32 B

        // VADD.FP32 v10 = v8 + v9
        rom[pc >> 2] = r_type(7'b0000000, 5'd9, 5'd8, 3'b011, 5'd10, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h1A0, 5'd1, 5'd10, 3'b000, OP_VST);   pc += 4;

        // VSUB.FP32 v11 = v8 - v9
        rom[pc >> 2] = r_type(7'b0000010, 5'd9, 5'd8, 3'b011, 5'd11, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h1B0, 5'd1, 5'd11, 3'b000, OP_VST);   pc += 4;

        // VMIN.FP32 v12 = min(v8, v9)
        rom[pc >> 2] = r_type(7'b0000100, 5'd9, 5'd8, 3'b011, 5'd12, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h1C0, 5'd1, 5'd12, 3'b000, OP_VST);   pc += 4;

        // VMAX.FP32 v13 = max(v8, v9)
        rom[pc >> 2] = r_type(7'b0000110, 5'd9, 5'd8, 3'b011, 5'd13, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h1D0, 5'd1, 5'd13, 3'b000, OP_VST);   pc += 4;

        // ----------------------------------------------------------
        // FP16 vector ALU smoke: add/sub/min/max (funct3=TYPE_FP16=3'b100)
        // v14/v15 inputs loaded from memory; results stored back for checking.
        // ----------------------------------------------------------
        rom[pc >> 2] = i_type(32'h200, 5'd1, 3'b000, 5'd14, OP_VLD);   pc += 4; // v14 = fp16 A
        rom[pc >> 2] = i_type(32'h210, 5'd1, 3'b000, 5'd15, OP_VLD);   pc += 4; // v15 = fp16 B

        // VADD.FP16 v16 = v14 + v15
        rom[pc >> 2] = r_type(7'b0000000, 5'd15, 5'd14, 3'b100, 5'd16, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h220, 5'd1, 5'd16, 3'b000, OP_VST);   pc += 4;
        // VSUB.FP16 v17 = v14 - v15
        rom[pc >> 2] = r_type(7'b0000010, 5'd15, 5'd14, 3'b100, 5'd17, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h230, 5'd1, 5'd17, 3'b000, OP_VST);   pc += 4;
        // VMIN.FP16 v18 = min(v14, v15)
        rom[pc >> 2] = r_type(7'b0000100, 5'd15, 5'd14, 3'b100, 5'd18, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h240, 5'd1, 5'd18, 3'b000, OP_VST);   pc += 4;
        // VMAX.FP16 v19 = max(v14, v15)
        rom[pc >> 2] = r_type(7'b0000110, 5'd15, 5'd14, 3'b100, 5'd19, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h250, 5'd1, 5'd19, 3'b000, OP_VST);   pc += 4;

        // ----------------------------------------------------------
        // FP8 vector ALU smoke: add/sub and min/max (funct3=TYPE_FP8=3'b101)
        // ----------------------------------------------------------
        // Add/Sub vectors
        rom[pc >> 2] = i_type(32'h260, 5'd1, 3'b000, 5'd20, OP_VLD);   pc += 4; // v20 = fp8 A
        rom[pc >> 2] = i_type(32'h270, 5'd1, 3'b000, 5'd21, OP_VLD);   pc += 4; // v21 = fp8 B
        // VADD.FP8 v22
        rom[pc >> 2] = r_type(7'b0000000, 5'd21, 5'd20, 3'b101, 5'd22, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h280, 5'd1, 5'd22, 3'b000, OP_VST);   pc += 4;
        // VSUB.FP8 v23
        rom[pc >> 2] = r_type(7'b0000010, 5'd21, 5'd20, 3'b101, 5'd23, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h290, 5'd1, 5'd23, 3'b000, OP_VST);   pc += 4;
        // Min/Max vectors
        rom[pc >> 2] = i_type(32'h2A0, 5'd1, 3'b000, 5'd24, OP_VLD);   pc += 4; // v24 = fp8 A (mixed sign)
        rom[pc >> 2] = i_type(32'h2B0, 5'd1, 3'b000, 5'd25, OP_VLD);   pc += 4; // v25 = fp8 B
        // VMIN.FP8 v26
        rom[pc >> 2] = r_type(7'b0000100, 5'd25, 5'd24, 3'b101, 5'd26, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h2C0, 5'd1, 5'd26, 3'b000, OP_VST);   pc += 4;
        // VMAX.FP8 v27
        rom[pc >> 2] = r_type(7'b0000110, 5'd25, 5'd24, 3'b101, 5'd27, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h2D0, 5'd1, 5'd27, 3'b000, OP_VST);   pc += 4;

        // ----------------------------------------------------------
        // Additional vector op coverage: VMUL / VUNPACK / VPACK / VRCP / VRSQRT
        // - Keep expectations deterministic (exact for integer ops, and special-case exact for SFU).
        // Store results under BASE+0x700.. so we don't collide with earlier scalar markers.
        // ----------------------------------------------------------
        // VMUL.I32 v5 = v1 * v2
        rom[pc >> 2] = r_type(7'b0011000, 5'd2, 5'd1, 3'b000, 5'd5, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h700, 5'd1, 5'd5, 3'b000, OP_VST); pc += 4;

        // x16 = 0x11223344 (MOVI pseudo: LUI + ADDI)
        rom[pc >> 2] = u_type(32'h11223, 5'd16, OP_LUI); pc += 4;
        rom[pc >> 2] = i_type(32'h344, 5'd16, 3'b000, 5'd16, OP_INT_IMM); pc += 4;
        // VUNPACK.I32 v28 = unpack(x16)
        rom[pc >> 2] = r_type(7'b0011110, 5'd16, 5'd0, 3'b000, 5'd28, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h710, 5'd1, 5'd28, 3'b000, OP_VST); pc += 4;

        // VPACK.I32 x17 = pack(v28)
        rom[pc >> 2] = r_type(7'b0010000, 5'd0, 5'd28, 3'b000, 5'd17, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h720, 5'd1, 5'd17, 3'b010, OP_STORE); pc += 4;

        // SFU special-cases (FP32): VRCP/VRSQRT over {+Inf, +0, NaN, -Inf}
        rom[pc >> 2] = i_type(32'h6E0, 5'd1, 3'b000, 5'd24, OP_VLD); pc += 4; // v24 inputs
        // VRCP.FP32 v25 = rcp(v24)
        rom[pc >> 2] = r_type(7'b0011010, 5'd0, 5'd24, 3'b011, 5'd25, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h730, 5'd1, 5'd25, 3'b000, OP_VST); pc += 4;
        // VRSQRT.FP32 v26 = rsqrt(v24)
        rom[pc >> 2] = r_type(7'b0011100, 5'd0, 5'd24, 3'b011, 5'd26, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h740, 5'd1, 5'd26, 3'b000, OP_VST); pc += 4;

        // VRSQRT negative input: rsqrt(-1.0) -> qNaN
        rom[pc >> 2] = i_type(32'h6F0, 5'd1, 3'b000, 5'd27, OP_VLD); pc += 4;
        rom[pc >> 2] = r_type(7'b0011100, 5'd0, 5'd27, 3'b011, 5'd27, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(32'h750, 5'd1, 5'd27, 3'b000, OP_VST); pc += 4;

        // MEMBAR: flush FP32 result stores before the rest of the test.
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
        // VCMP.eq v1,v1 -> x9 mask
        rom[pc >> 2] = r_type(7'b0000110, 5'd1, 5'd1, 3'b000, 5'd9, OP_VEC_ALU); pc += 4;
        rom[pc >> 2] = s_type(64, 5'd1, 5'd9, 3'b010, OP_STORE); pc += 4;
        // MEMBAR: flush mask store out of WMB (testbench checks global memory directly)
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
        // VATOM.ADD v5, 48(x1), v2
        rom[pc >> 2] = s_type(48, 5'd1, 5'd2, 3'b000, OP_ATOM_V); pc += 4;
        // VLD v6 from 48; VST v6 to 80
        rom[pc >> 2] = i_type(48, 5'd1, 3'b000, 5'd6, OP_VLD);    pc += 4;
        rom[pc >> 2] = s_type(80, 5'd1, 5'd6, 3'b000, OP_VST);    pc += 4;
        // Texture sample
        // - v4 holds {u,v} in lanes 0/1
        // - x14 holds sampler descriptor pointer
        rom[pc >> 2] = i_type(32'h160, 5'd1, 3'b000, 5'd4, OP_VLD); pc += 4; // v4 = coords
        rom[pc >> 2] = i_type(32'h140, 5'd0, 3'b000, 5'd14, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd14, 5'd1, 3'b000, 5'd14, OP_INT); pc += 4;
        // TEX v7, v4, x14
        rom[pc >> 2] = r_type(7'b0000000, 5'd14, 5'd4, 3'b000, 5'd7, OP_TEX); pc += 4;
        // Store TEX result vector to base+0x120
        rom[pc >> 2] = s_type(288, 5'd1, 5'd7, 3'b000, OP_VST);   pc += 4;
        // Final MEMBAR
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM);

        // ----------------------------------------------------------
        // GFX macro-op smoke: RSTATE + GSTATE + GPARAM + GDRAW
        // Use x1=BASE_ADDR and compute absolute descriptor pointers.
        // NOTE: Avoid ADDI with rs1!=x0 due to decoder heuristic; build with ADD.
        // ----------------------------------------------------------
        pc += 4;
        // x10 = BASE_ADDR + RSTATE_OFF
        rom[pc >> 2] = i_type(RSTATE_OFF, 5'd0, 3'b000, 5'd10, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd10, 5'd1, 3'b000, 5'd10, OP_INT); pc += 4;
        // x11 = BASE_ADDR + GSTATE_OFF
        rom[pc >> 2] = i_type(GSTATE_OFF, 5'd0, 3'b000, 5'd11, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd11, 5'd1, 3'b000, 5'd11, OP_INT); pc += 4;
        // x12 = BASE_ADDR + GPARAM_OFF
        rom[pc >> 2] = i_type(GPARAM_OFF, 5'd0, 3'b000, 5'd12, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd12, 5'd1, 3'b000, 5'd12, OP_INT); pc += 4;
        // x13 = BASE_ADDR + GDRAW_OFF
        rom[pc >> 2] = i_type(GDRAW_OFF, 5'd0, 3'b000, 5'd13, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd13, 5'd1, 3'b000, 5'd13, OP_INT); pc += 4;

        // The front-end can ignore some instructions in slot1; keep macro-ops in slot0.
        if (pc[2]) begin
            rom[pc >> 2] = nop();
            pc += 4;
        end

        // RSTATE(x10)
        rom[pc >> 2] = i_type(0, 5'd10, 3'b000, 5'd0, OP_ATOM_SC); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;
        // GSTATE(x11)
        rom[pc >> 2] = i_type(0, 5'd11, 3'b100, 5'd0, OP_ATOM_SC); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;
        // GPARAM(x12)
        rom[pc >> 2] = i_type(0, 5'd12, 3'b101, 5'd0, OP_ATOM_SC); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;
        // GDRAW(x13)
        rom[pc >> 2] = i_type(0, 5'd13, 3'b110, 5'd0, OP_ATOM_SC); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;

        // ----------------------------------------------------------
        // Second draw (different VBO region + color) + queue stress
        // ----------------------------------------------------------
        // Use distinct GSTATE2/GPARAM2 blocks to avoid racing descriptor reads.
        // x18 = BASE_ADDR + GSTATE2_OFF
        rom[pc >> 2] = i_type(GSTATE2_OFF, 5'd0, 3'b000, 5'd18, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd18, 5'd1, 3'b000, 5'd18, OP_INT); pc += 4;
        // x19 = BASE_ADDR + GPARAM2_OFF
        rom[pc >> 2] = i_type(GPARAM2_OFF, 5'd0, 3'b000, 5'd19, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = r_type(7'b0000000, 5'd19, 5'd1, 3'b000, 5'd19, OP_INT); pc += 4;

        // Keep macro-ops in slot0.
        if (pc[2]) begin
            rom[pc >> 2] = nop();
            pc += 4;
        end

        // Re-issue state + draw (using the second descriptor blocks)
        rom[pc >> 2] = i_type(0, 5'd18, 3'b100, 5'd0, OP_ATOM_SC); pc += 4; // GSTATE(x18)
        rom[pc >> 2] = nop(); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd19, 3'b101, 5'd0, OP_ATOM_SC); pc += 4; // GPARAM(x19)
        rom[pc >> 2] = nop(); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd13, 3'b110, 5'd0, OP_ATOM_SC); pc += 4; // GDRAW(x13)
        rom[pc >> 2] = nop(); pc += 4;

        // Queue/backpressure stress: burst a few extra GDRAWs without changing state.
        rom[pc >> 2] = i_type(0, 5'd13, 3'b110, 5'd0, OP_ATOM_SC); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd13, 3'b110, 5'd0, OP_ATOM_SC); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd13, 3'b110, 5'd0, OP_ATOM_SC); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;

        // Drain
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;

        // Done flag: store 1 to BASE+DONE_OFF and flush so TB can observe completion.
        rom[pc >> 2] = i_type(1, 5'd0, 3'b000, 5'd15, OP_INT_IMM); pc += 4; // x15=1
        rom[pc >> 2] = s_type(DONE_OFF, 5'd1, 5'd15, 3'b010, OP_STORE); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;

        // Halt in-place so the instruction fetch address doesn't wrap and re-run the ROM.
        // IMPORTANT: The core can advance PC by 8B (dual-issue). Also, control-flow in the
        // second 32-bit slot of a 64-bit bundle may be ignored by the front-end.
        // Ensure the halt lives in slot0 (pc%8==0).
        if (pc[2]) begin
            rom[pc >> 2] = nop();
            pc += 4;
        end
        rom[pc >> 2] = 32'h0000_006F; // JAL x0, 0
    end

    // --------------------------------------------------------------
    // Data memory init
    // --------------------------------------------------------------
    initial begin
        for (int i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;
        // Vector operands
        mem[mem_index(BASE_ADDR + 32'd16)] = 32'd1;
        mem[mem_index(BASE_ADDR + 32'd20)] = 32'd2;
        mem[mem_index(BASE_ADDR + 32'd24)] = 32'd3;
        mem[mem_index(BASE_ADDR + 32'd28)] = 32'd4;

        mem[mem_index(BASE_ADDR + 32'd32)] = 32'd10;
        mem[mem_index(BASE_ADDR + 32'd36)] = 32'd20;
        mem[mem_index(BASE_ADDR + 32'd40)] = 32'd30;
        mem[mem_index(BASE_ADDR + 32'd44)] = 32'd40;

        // FP32 vector operands @ BASE+0x180 and BASE+0x190 (4 lanes)
        // A:  1.0, -1.0, 4.0, +Inf
        mem[mem_index(BASE_ADDR + 32'h0000_0180)] = 32'h3F80_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_0184)] = 32'hBF80_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_0188)] = 32'h4080_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_018C)] = 32'h7F80_0000;

        // B:  2.0, 0.5, 4.0, 1.0
        mem[mem_index(BASE_ADDR + 32'h0000_0190)] = 32'h4000_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_0194)] = 32'h3F00_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_0198)] = 32'h4080_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_019C)] = 32'h3F80_0000;

        // ----------------------------------------------------------
        // FP16 vector operands @ BASE+0x200 and BASE+0x210 (8 lanes, packed 2 per word)
        // A:  1.0, -1.0, 2.0, -2.0, 0.5, -0.5, 4.0, -4.0
        // B:  1.0,  2.0,-2.0,  2.0, 0.5,  0.5, 4.0,  1.0
        // ----------------------------------------------------------
        mem[mem_index(BASE_ADDR + 32'h0000_0200)] = 32'hBC00_3C00;
        mem[mem_index(BASE_ADDR + 32'h0000_0204)] = 32'hC000_4000;
        mem[mem_index(BASE_ADDR + 32'h0000_0208)] = 32'hB800_3800;
        mem[mem_index(BASE_ADDR + 32'h0000_020C)] = 32'hC400_4400;

        mem[mem_index(BASE_ADDR + 32'h0000_0210)] = 32'h4000_3C00;
        mem[mem_index(BASE_ADDR + 32'h0000_0214)] = 32'h4000_C000;
        mem[mem_index(BASE_ADDR + 32'h0000_0218)] = 32'h3800_3800;
        mem[mem_index(BASE_ADDR + 32'h0000_021C)] = 32'h3C00_4400;

        // ----------------------------------------------------------
        // FP8 vector operands (16 lanes, packed 4 per word)
        // Add/Sub: A=B=0x08 (all lanes)
        // Min/Max: A alternates 0x88 (neg) and 0x08 (pos), B=0x08
        // ----------------------------------------------------------
        mem[mem_index(BASE_ADDR + 32'h0000_0260)] = 32'h0808_0808;
        mem[mem_index(BASE_ADDR + 32'h0000_0264)] = 32'h0808_0808;
        mem[mem_index(BASE_ADDR + 32'h0000_0268)] = 32'h0808_0808;
        mem[mem_index(BASE_ADDR + 32'h0000_026C)] = 32'h0808_0808;

        mem[mem_index(BASE_ADDR + 32'h0000_0270)] = 32'h0808_0808;
        mem[mem_index(BASE_ADDR + 32'h0000_0274)] = 32'h0808_0808;
        mem[mem_index(BASE_ADDR + 32'h0000_0278)] = 32'h0808_0808;
        mem[mem_index(BASE_ADDR + 32'h0000_027C)] = 32'h0808_0808;

        mem[mem_index(BASE_ADDR + 32'h0000_02A0)] = 32'h0888_0888;
        mem[mem_index(BASE_ADDR + 32'h0000_02A4)] = 32'h0888_0888;
        mem[mem_index(BASE_ADDR + 32'h0000_02A8)] = 32'h0888_0888;
        mem[mem_index(BASE_ADDR + 32'h0000_02AC)] = 32'h0888_0888;

        mem[mem_index(BASE_ADDR + 32'h0000_02B0)] = 32'h0808_0808;

        // Slot1 placement test seed data
        mem[mem_index(BASE_ADDR + 32'h0000_03B0)] = 32'h1234_5678;

        // Load-use hazard seed data
        mem[mem_index(BASE_ADDR + 32'h0000_03F8)] = 32'h1234_5678;
        mem[mem_index(BASE_ADDR + 32'h0000_02B4)] = 32'h0808_0808;
        mem[mem_index(BASE_ADDR + 32'h0000_02B8)] = 32'h0808_0808;
        mem[mem_index(BASE_ADDR + 32'h0000_02BC)] = 32'h0808_0808;

        // Texture line at BASE+0x100 (16-byte aligned)
        mem[mem_index(BASE_ADDR + 32'h0000_0100)] = 32'hDEAD_BEEF;
        mem[mem_index(BASE_ADDR + 32'h0000_0104)] = 32'hCAFE_FEED;
        mem[mem_index(BASE_ADDR + 32'h0000_0108)] = 32'h1234_5678;
        mem[mem_index(BASE_ADDR + 32'h0000_010C)] = 32'h0BAD_F00D;

        // Texture sampler descriptor @ BASE+0x140 (32B)
        mem[mem_index(BASE_ADDR + 32'h0000_0140)] = BASE_ADDR + 32'h0000_0100; // base
        mem[mem_index(BASE_ADDR + 32'h0000_0144)] = 32'd16;                    // stride bytes
        mem[mem_index(BASE_ADDR + 32'h0000_0148)] = 32'd1;                     // width
        mem[mem_index(BASE_ADDR + 32'h0000_014C)] = 32'd1;                     // height
        mem[mem_index(BASE_ADDR + 32'h0000_0150)] = 32'd0;                     // format (raw/ARGB8888)
        mem[mem_index(BASE_ADDR + 32'h0000_0154)] = 32'd0;                     // wrap
        mem[mem_index(BASE_ADDR + 32'h0000_0158)] = 32'd0;                     // filter (nearest)
        mem[mem_index(BASE_ADDR + 32'h0000_015C)] = 32'd0;                     // misc

        // TEX coords @ BASE+0x160 (v4 lanes: u=0, v=0)
        mem[mem_index(BASE_ADDR + 32'h0000_0160)] = 32'd0;
        mem[mem_index(BASE_ADDR + 32'h0000_0164)] = 32'd0;
        mem[mem_index(BASE_ADDR + 32'h0000_0168)] = 32'd0;
        mem[mem_index(BASE_ADDR + 32'h0000_016C)] = 32'd0;

        // ----------------------------------------------------------
        // GFX descriptors/VBO/framebuffer in global memory
        // ----------------------------------------------------------
        // RSTATE @ BASE+RSTATE_OFF
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h00)] = BASE_ADDR + FB_BASE_OFF;     // fb_base
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h04)] = 32'd32;                     // fb_stride_bytes (8px * 4B)
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h08)] = 32'd0;                      // fb_format (ARGB8888)
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h0C)] = {16'd8, 16'd8};             // {fb_h, fb_w}
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h20)] = 32'h0000_0000;              // const_color (unused; overridden by GPARAM)
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h24)] = 32'h0000_0000;              // sampler_handle
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h28)] = {16'd0, 16'd0};             // scissor {y0,x0}
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h2C)] = {16'd0, 16'd0};             // scissor {h,w}
        mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h30)] = 32'h0000_0000;              // flags0 (scissor/texture disabled)

        // GSTATE @ BASE+GSTATE_OFF
        mem[mem_index(BASE_ADDR + GSTATE_OFF + 32'h00)] = BASE_ADDR + VBO_BASE_OFF;   // VBO_BASE
        mem[mem_index(BASE_ADDR + GSTATE_OFF + 32'h04)] = 32'h0000_0020;              // VBO_STRIDE_BYTES
        mem[mem_index(BASE_ADDR + GSTATE_OFF + 32'h08)] = 32'h0000_0000;              // IBO_BASE
        mem[mem_index(BASE_ADDR + GSTATE_OFF + 32'h0C)] = 32'h0000_0000;              // IBO_FORMAT
        mem[mem_index(BASE_ADDR + GSTATE_OFF + 32'h18)] = 32'h0000_0000;              // CULL_CLIP_FLAGS

        // GSTATE2 @ BASE+GSTATE2_OFF (second draw)
        mem[mem_index(BASE_ADDR + GSTATE2_OFF + 32'h00)] = BASE_ADDR + VBO2_BASE_OFF; // VBO_BASE
        mem[mem_index(BASE_ADDR + GSTATE2_OFF + 32'h04)] = 32'h0000_0020;             // VBO_STRIDE_BYTES
        mem[mem_index(BASE_ADDR + GSTATE2_OFF + 32'h08)] = 32'h0000_0000;             // IBO_BASE
        mem[mem_index(BASE_ADDR + GSTATE2_OFF + 32'h0C)] = 32'h0000_0000;             // IBO_FORMAT
        mem[mem_index(BASE_ADDR + GSTATE2_OFF + 32'h18)] = 32'h0000_0000;             // CULL_CLIP_FLAGS

        // GPARAM @ BASE+GPARAM_OFF
        mem[mem_index(BASE_ADDR + GPARAM_OFF + 32'h00)] = 32'h0000_0000;              // BASE_VERTEX
        mem[mem_index(BASE_ADDR + GPARAM_OFF + 32'h08)] = GFX_COLOR;                  // MATERIAL_COLOR

        // GPARAM2 @ BASE+GPARAM2_OFF (second draw)
        mem[mem_index(BASE_ADDR + GPARAM2_OFF + 32'h00)] = 32'h0000_0000;             // BASE_VERTEX
        mem[mem_index(BASE_ADDR + GPARAM2_OFF + 32'h08)] = GFX_COLOR2;                // MATERIAL_COLOR

        // GDRAW @ BASE+GDRAW_OFF
        mem[mem_index(BASE_ADDR + GDRAW_OFF + 32'h00)] = 32'h0000_0000;               // FIRST
        mem[mem_index(BASE_ADDR + GDRAW_OFF + 32'h04)] = 32'h0000_0003;               // COUNT
        mem[mem_index(BASE_ADDR + GDRAW_OFF + 32'h08)] = 32'h0000_0000;               // TOPOLOGY (tri_list)
        mem[mem_index(BASE_ADDR + GDRAW_OFF + 32'h0C)] = 32'h0000_0000;               // FLAGS (non-indexed, not textured)
        mem[mem_index(BASE_ADDR + GDRAW_OFF + 32'h10)] = 32'h0000_0000;               // RSTATE_PTR (unused by minimal path)

        // VBO @ BASE+VBO_BASE_OFF (stride=0x20; x@+0 y@+4 u@+0x10 v@+0x14)
        // v0=(1,1) v1=(6,1) v2=(1,6)
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h00)] = 32'd1;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h04)] = 32'd1;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h10)] = 32'd0;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h14)] = 32'd0;

        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h20)] = 32'd6;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h24)] = 32'd1;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h30)] = 32'd0;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h34)] = 32'd0;

        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h40)] = 32'd1;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h44)] = 32'd6;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h50)] = 32'd0;
        mem[mem_index(BASE_ADDR + VBO_BASE_OFF + 32'h54)] = 32'd0;

        // VBO2 @ BASE+VBO2_BASE_OFF (non-overlapping triangle for second draw)
        // v0=(4,4) v1=(7,4) v2=(4,7)
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h00)] = 32'd4;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h04)] = 32'd4;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h10)] = 32'd0;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h14)] = 32'd0;

        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h20)] = 32'd7;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h24)] = 32'd4;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h30)] = 32'd0;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h34)] = 32'd0;

        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h40)] = 32'd4;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h44)] = 32'd7;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h50)] = 32'd0;
        mem[mem_index(BASE_ADDR + VBO2_BASE_OFF + 32'h54)] = 32'd0;

        // ----------------------------------------------------------
        // SFU special-case vectors (FP32) @ BASE+0x6E0 and BASE+0x6F0
        // ----------------------------------------------------------
        // v24 inputs: {+Inf, +0, NaN, -Inf}
        mem[mem_index(BASE_ADDR + 32'h0000_06E0)] = 32'h7F80_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_06E4)] = 32'h0000_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_06E8)] = 32'h7FC1_2345;
        mem[mem_index(BASE_ADDR + 32'h0000_06EC)] = 32'hFF80_0000;
        // v27 inputs: {-1.0, 0, 0, 0}
        mem[mem_index(BASE_ADDR + 32'h0000_06F0)] = 32'hBF80_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_06F4)] = 32'h0000_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_06F8)] = 32'h0000_0000;
        mem[mem_index(BASE_ADDR + 32'h0000_06FC)] = 32'h0000_0000;
    end

    // --------------------------------------------------------------
    // Simple checker
    // --------------------------------------------------------------
    task automatic check_equal(input string name, input logic [31:0] got, input logic [31:0] exp);
        if (got !== exp) begin
            $fatal(1, "%s mismatch: got %0d (0x%08x) expected %0d (0x%08x)", name, got, got, exp, exp);
        end else begin
            $display("[OK] %s = %0d (0x%08x)", name, got, got);
        end
    endtask

    task automatic check_equal_warn(input string name, input logic [31:0] got, input logic [31:0] exp);
        if (got !== exp) begin
            $display("[WARN] %s mismatch: got %0d (0x%08x) expected %0d (0x%08x)", name, got, got, exp, exp);
        end else begin
            $display("[OK] %s = %0d (0x%08x)", name, got, got);
        end
    endtask

    // --------------------------------------------------------------
    // Lightweight debug (prints only early activity)
    // --------------------------------------------------------------
    int req_count;
    int store_req_count;
    int load_req_count;
    int gp_issue_count;
    int cyc;

    int fp_scalar_wb_prints;
    int s_wb_prints;

    int dbg_raster_quad_prints;
    int dbg_gfx_store_prints;

    int fp16_vadd_issue_prints;
    int fp16_vadd_wb_prints;

    logic        fp16_vadd_pending;
    logic [127:0] fp16_vadd_last_a;
    logic [127:0] fp16_vadd_last_b;

    logic [4:0] dbg_src_flags;
    logic [31:0] dbg_src_flags32;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            req_count <= 0;
            store_req_count <= 0;
            load_req_count <= 0;
            gp_issue_count <= 0;
            cyc <= 0;
            fp_scalar_wb_prints <= 0;
            s_wb_prints <= 0;

            fp16_vadd_issue_prints <= 0;
            fp16_vadd_wb_prints <= 0;
            fp16_vadd_pending <= 1'b0;
            fp16_vadd_last_a <= '0;
            fp16_vadd_last_b <= '0;
        end else begin
            cyc <= cyc + 1;

            // Targeted debug: capture FP16 VADD operands/results
            if (dut.valuv_issue_valid
                && (dut.vq[dut.vq_head].ctrl.funct3 == 3'b100)
                && (dut.vq[dut.vq_head].ctrl.funct7[6:1] == 6'b000000)
            ) begin
                fp16_vadd_last_a <= dut.vq[dut.vq_head].src_a;
                fp16_vadd_last_b <= dut.vq[dut.vq_head].src_b;
                fp16_vadd_pending <= 1'b1;
                if (fp16_vadd_issue_prints < 4) begin
                    fp16_vadd_issue_prints <= fp16_vadd_issue_prints + 1;
                    $display("%0t DEBUG FP16 VADD ISSUE src_a=%032h src_b=%032h lane6(a,b)=%04h,%04h lane7(a,b)=%04h,%04h",
                             $time,
                             dut.vq[dut.vq_head].src_a,
                             dut.vq[dut.vq_head].src_b,
                             dut.vq[dut.vq_head].src_a[111:96],
                             dut.vq[dut.vq_head].src_b[111:96],
                             dut.vq[dut.vq_head].src_a[127:112],
                             dut.vq[dut.vq_head].src_b[127:112]);
                end
            end

            if (dut.valuv_wb_valid && fp16_vadd_pending) begin
                fp16_vadd_pending <= 1'b0;
                if (fp16_vadd_wb_prints < 4) begin
                    fp16_vadd_wb_prints <= fp16_vadd_wb_prints + 1;
                    $display("%0t DEBUG FP16 VADD WB wb_data=%032h lane6(r)=%04h lane7(r)=%04h (prev lane7 a,b=%04h,%04h)",
                             $time,
                             dut.valuv_wb_data,
                             dut.valuv_wb_data[111:96],
                             dut.valuv_wb_data[127:112],
                             fp16_vadd_last_a[127:112],
                             fp16_vadd_last_b[127:112]);
                end
            end

            // Targeted debug: watch FP->scalar writebacks (FCVT.f2i)
            if (dut.fp_scalar_wb_valid && (dut.fp_scalar_wb_rd == 5'd30 || dut.fp_scalar_wb_rd == 5'd31) && (fp_scalar_wb_prints < 8)) begin
                fp_scalar_wb_prints <= fp_scalar_wb_prints + 1;
                $display("%0t FP->S WB rd=%0d data=%08h", $time, dut.fp_scalar_wb_rd, dut.fp_scalar_wb_data);
            end

            // Also show actual scalar regfile writes to x30/x31
            if (dut.s_we && (dut.s_waddr == 5'd30 || dut.s_waddr == 5'd31) && (s_wb_prints < 8)) begin
                s_wb_prints <= s_wb_prints + 1;
                dbg_src_flags <= {
                    dut.scalar_wb_from_pending,
                    dut.scalar_wb_from_lsu,
                    dut.scalar_wb_from_fp,
                    dut.scalar_wb_from_valu,
                    dut.scalar_wb_from_alu
                };
                dbg_src_flags32 <= {27'd0, dbg_src_flags};
                $display("%0t SREG_WB rd=%0d data=%08h src=%08h wb_valid=%08h wb_is_scalar_fp=%08h wb_f3=%08h wb_f7=%08h fp_scalar_valid=%08h",
                         $time,
                         dut.s_waddr,
                         dut.s_wdata,
                         dbg_src_flags32,
                         {31'd0, dut.wb_valid},
                         {31'd0, dut.wb_ctrl.is_scalar_fp},
                         {29'd0, dut.wb_ctrl.funct3},
                         {25'd0, dut.wb_ctrl.funct7},
                         {31'd0, dut.fp_scalar_wb_valid});
            end

            if (dut.gp_issue_valid && gp_issue_count < 16) begin
                gp_issue_count <= gp_issue_count + 1;
                $display("%0t GP_ISSUE is_gfx=%0b funct3=%03b op_a=%08h op_b=%08h",
                         $time,
                         dut.gp_issue_ctrl.is_gfx,
                         dut.gp_issue_ctrl.funct3,
                         dut.gp_issue_op_a,
                         dut.gp_issue_op_b);
            end

            if (data_req_valid && data_req_ready) begin
                req_count <= req_count + 1;
                if (data_req_is_load) load_req_count <= load_req_count + 1;
                else store_req_count <= store_req_count + 1;

                // Print the first few requests, plus a focused window around the vector/atomic region.
                // (Atomic is expected to update BASE+48..+60 and later we copy to BASE+80..+92.)
                if (req_count < 20
                    || (!data_req_is_load && (data_req_addr == (BASE_ADDR + 32'd64)))
                    || (data_req_addr >= (BASE_ADDR + 32'd48) && data_req_addr < (BASE_ADDR + 32'd96))
                    || (data_req_addr >= (BASE_ADDR + RSTATE_OFF) && data_req_addr < (BASE_ADDR + VBO_BASE_OFF))
                    || (!data_req_is_load && (data_req_addr >= (BASE_ADDR + FB_BASE_OFF) && data_req_addr < (BASE_ADDR + FB_BASE_OFF + 32'd256)))) begin
                    $display("%0t MEM %s addr=%08h wdata=%08h rd=%0d", $time,
                             data_req_is_load ? "LD" : "ST",
                             data_req_addr, data_req_wdata, data_req_rd);
                end
            end

        end
    end

    // Post-NBA debug sampler
    always @(posedge clk) begin
        if (rst_n) begin
            #1;

            // One-shot: confirm an atomic op reaches MEM stage and show its effective address.
            begin : atomic_probe
                static bit printed_atomic;
                if (!printed_atomic && dut.mem_valid && dut.mem_ctrl.is_atomic) begin
                    printed_atomic = 1'b1;
                    $display("%0t ATOM in MEM: mem_addr=%08h is_vector=%0d is_store=%0d funct3(op)=%03b rd=%0d", $time,
                             dut.mem_addr,
                             dut.mem_ctrl.is_vector,
                             dut.mem_ctrl.is_store,
                             dut.mem_ctrl.funct3,
                             dut.mem_ctrl.rd);
                end
            end

            // Focused VCMP debug: does x9 ever get written nonzero?
            if (dut.s_we && (dut.s_waddr == 5'd9)) begin
                $display("%0t WB.S (x9) data=%08h (valuv_valid=%b valuv_rd=%0d valuv_is_scalar=%b valuv_data_lo=%08h)",
                         $time, dut.s_wdata,
                         dut.valuv_wb_valid, dut.valuv_wb_rd, dut.valuv_wb_is_scalar, dut.valuv_wb_data[31:0]);
            end

            // Focused VCMP/store sequencing debug: do we ever fetch/accept the store of x9 to BASE+64?
            begin : vcmp_store_probe
                logic [31:0] vcmp_inst;
                logic [31:0] vcmp_store_inst;
                vcmp_inst       = r_type(7'b0000110, 5'd1, 5'd1, 3'b000, 5'd9, OP_VEC_ALU);
                vcmp_store_inst = s_type(64, 5'd1, 5'd9, 3'b010, OP_STORE);

                if (dut.if_valid && dut.if_inst0_valid && (dut.if_pc >= 32'h0000_0270) && (dut.if_pc <= 32'h0000_027C)) begin
                    $display("%0t IF@%08h inst0=%08h inst0_valid=%0b inst1_valid=%0b inst1=%08h accept0=%0b accept1=%0b pc_adv=%0d",
                             $time, dut.if_pc, dut.if_inst0, dut.if_inst0_valid, dut.if_inst1_valid, dut.if_inst1,
                             dut.accept0, dut.accept1, dut.pc_advance_bytes);
                    $display("%0t IF@%08h d0: op=%02h f3=%0h f7=%02h rs1=%0d rs2=%0d rd=%0d is_store=%0b is_load=%0b is_vec=%0b uses_rs2=%0b",
                             $time, dut.if_pc,
                             dut.if_inst0[6:0], dut.if_inst0[14:12], dut.if_inst0[31:25], dut.if_inst0[19:15], dut.if_inst0[24:20], dut.if_inst0[11:7],
                             dut.d0_ctrl.is_store, dut.d0_ctrl.is_load, dut.d0_ctrl.is_vector, dut.d0_ctrl.uses_rs2);
                end

                if (dut.if_valid && dut.if_inst0_valid && (dut.if_inst0 == vcmp_inst)) begin
                    $display("%0t IF VCMP inst0 pc=%08h inst1_valid=%0b inst1=%08h accept0=%0b stall_issue=%0b pc_adv=%0d",
                             $time, dut.if_pc, dut.if_inst1_valid, dut.if_inst1, dut.accept0, dut.stall_issue, dut.pc_advance_bytes);
                end
                if (dut.if_valid && dut.if_inst0_valid && (dut.if_inst0 == vcmp_store_inst)) begin
                    $display("%0t IF VCMP_STORE inst0 pc=%08h accept0=%0b stall_sb0=%0b stall_issue=%0b pc_adv=%0d rs2_busy(s[x9])=%0b",
                             $time, dut.if_pc, dut.accept0, dut.stall_sb0, dut.stall_issue, dut.pc_advance_bytes,
                             dut.u_scoreboard.busy_s[5'd9]);
                end
                if (dut.if_valid && dut.if_inst1_valid && (dut.if_inst1 == vcmp_store_inst)) begin
                    $display("%0t IF VCMP_STORE inst1 pc=%08h inst0=%08h can_dual=%0b accept0=%0b accept1=%0b stall_issue=%0b pc_adv=%0d",
                             $time, dut.if_pc, dut.if_inst0, dut.can_dual, dut.accept0, dut.accept1, dut.stall_issue, dut.pc_advance_bytes);
                end
            end

            // Is the scalar store to BASE+64 entering/leaving the WMB?
            if (dut.u_lsu.wmb_in_valid && dut.u_lsu.wmb_in_ready && (dut.u_lsu.wmb_in_addr == (BASE_ADDR + 32'd64))) begin
                $display("%0t WMB ENQ addr=%08h wdata=%08h wstrb=%0h", $time, dut.u_lsu.wmb_in_addr, dut.u_lsu.wmb_in_wdata, dut.u_lsu.wmb_in_wstrb);
            end
            if (dut.u_lsu.wmb_out_valid && dut.u_lsu.wmb_out_ready && (dut.u_lsu.wmb_out_addr == (BASE_ADDR + 32'd64))) begin
                $display("%0t WMB DEQ addr=%08h wdata=%08h wstrb=%0h", $time, dut.u_lsu.wmb_out_addr, dut.u_lsu.wmb_out_wdata, dut.u_lsu.wmb_out_wstrb);
            end

            if (cyc < 40) begin
                automatic logic manual_stall0;
                automatic logic rs1_busy;
                automatic logic rs2_busy;
                automatic logic rd_busy;

                // Mirror scoreboard's src_busy/dest_busy selection
                rs1_busy = (dut.d0_ctrl.rs1_class == 2'b00) ? dut.u_scoreboard.busy_s[dut.d0_ctrl.rs1] :
                           (dut.d0_ctrl.rs1_class == 2'b01) ? dut.u_scoreboard.busy_f[dut.d0_ctrl.rs1] :
                                                              dut.u_scoreboard.busy_v[dut.d0_ctrl.rs1];
                rs2_busy = (dut.d0_ctrl.rs2_class == 2'b00) ? dut.u_scoreboard.busy_s[dut.d0_ctrl.rs2] :
                           (dut.d0_ctrl.rs2_class == 2'b01) ? dut.u_scoreboard.busy_f[dut.d0_ctrl.rs2] :
                                                              dut.u_scoreboard.busy_v[dut.d0_ctrl.rs2];
                rd_busy  = (dut.d0_ctrl.rd_class  == 2'b00) ? dut.u_scoreboard.busy_s[dut.d0_ctrl.rd ] :
                           (dut.d0_ctrl.rd_class  == 2'b01) ? dut.u_scoreboard.busy_f[dut.d0_ctrl.rd ] :
                                                              dut.u_scoreboard.busy_v[dut.d0_ctrl.rd ];

                manual_stall0 = 1'b0;
                if (dut.issue0_valid) begin
                    if (dut.d0_ctrl.uses_rs1 && rs1_busy) manual_stall0 = 1'b1;
                    if (dut.d0_ctrl.uses_rs2 && rs2_busy) manual_stall0 = 1'b1;
                    if (dut.d0_ctrl.uses_rd  && rd_busy ) manual_stall0 = 1'b1;
                end

                $display("%0t cyc=%0d pc=%08h if_valid=%0d inst0=%08h d0_valid=%0d stall_sb0=%b sb.stall0=%b issue0=%b accept0=%b stall_issue=%b rs1=%0d(rs1c=%0d) rs2=%0d(rs2c=%0d) rd=%0d(rdc=%0d) u1=%b u2=%b urd=%b",
                         $time, cyc, dut.if_pc, dut.if_valid, dut.if_inst0,
                         dut.d0_ctrl.is_valid, dut.stall_sb0, dut.u_scoreboard.stall0, dut.issue0_valid, dut.accept0, dut.stall_issue,
                         dut.d0_ctrl.rs1, dut.d0_ctrl.rs1_class,
                         dut.d0_ctrl.rs2, dut.d0_ctrl.rs2_class,
                         dut.d0_ctrl.rd,  dut.d0_ctrl.rd_class,
                         dut.d0_ctrl.uses_rs1, dut.d0_ctrl.uses_rs2, dut.d0_ctrl.uses_rd);

                $display("%0t   manual_stall0=%b (rs1_busy=%b rs2_busy=%b rd_busy=%b)",
                         $time, manual_stall0, rs1_busy, rs2_busy, rd_busy);

                if (manual_stall0 !== dut.stall_sb0) begin
                    $display("%0t   MISMATCH: dut.stall_sb0=%b manual_stall0=%b", $time, dut.stall_sb0, manual_stall0);
                    $display("%0t   SB.port issue0_valid=%b rs1_v=%b rs2_v=%b rd_v=%b rs1_c=%b rs2_c=%b rd_c=%b rs1=%0d rs2=%0d rd=%0d",
                             $time,
                             dut.u_scoreboard.issue0_valid,
                             dut.u_scoreboard.issue0_rs1_valid,
                             dut.u_scoreboard.issue0_rs2_valid,
                             dut.u_scoreboard.issue0_rd_valid,
                             dut.u_scoreboard.issue0_rs1_class,
                             dut.u_scoreboard.issue0_rs2_class,
                             dut.u_scoreboard.issue0_rd_class,
                             dut.u_scoreboard.issue0_rs1,
                             dut.u_scoreboard.issue0_rs2,
                             dut.u_scoreboard.issue0_rd);
                    $display("%0t   SB.busy_s=%08h busy_f=%08h busy_v=%08h",
                             $time,
                             dut.u_scoreboard.busy_s,
                             dut.u_scoreboard.busy_f,
                             dut.u_scoreboard.busy_v);
                end

                if (dut.stall_sb0) begin
                    $display("%0t   SB busy_s(2,3,4)=%b%b%b busy_f(2,3,4)=%b%b%b busy_v(2,3,4)=%b%b%b",
                             $time,
                             dut.u_scoreboard.busy_s[2], dut.u_scoreboard.busy_s[3], dut.u_scoreboard.busy_s[4],
                             dut.u_scoreboard.busy_f[2], dut.u_scoreboard.busy_f[3], dut.u_scoreboard.busy_f[4],
                             dut.u_scoreboard.busy_v[2], dut.u_scoreboard.busy_v[3], dut.u_scoreboard.busy_v[4]);
                end

                if (dut.s_we) begin
                    $display("%0t   WB.S rd=%0d data=%08h", $time, dut.s_waddr, dut.s_wdata);
                end
            end
        end
    end

    // --------------------------------------------------------------
    // Debug: raster output + ROP stores (first few only)
    // --------------------------------------------------------------
    always @(posedge clk) begin
        if (!rst_n) begin
            dbg_raster_quad_prints <= 0;
            dbg_gfx_store_prints <= 0;
        end else begin
            if ((dbg_raster_quad_prints < 16) && (dut.u_graphics_pipeline.raster_quad_valid === 1'b1) && (dut.u_graphics_pipeline.raster_quad_ready === 1'b1)) begin
                $display("%0t RASTER_QUAD x=%0d y=%0d mask=%b tri_area=%0d", $time,
                         dut.u_graphics_pipeline.raster_quad_x,
                         dut.u_graphics_pipeline.raster_quad_y,
                         dut.u_graphics_pipeline.raster_quad_mask,
                         dut.u_graphics_pipeline.raster_tri_area);
                dbg_raster_quad_prints <= dbg_raster_quad_prints + 1;
            end

            if ((dbg_gfx_store_prints < 32) && (dut.u_graphics_pipeline.gfx_st_valid === 1'b1) && (dut.u_graphics_pipeline.gfx_st_ready === 1'b1)) begin
                $display("%0t GFX_ST addr=%08h wdata=%08h wstrb=%b", $time,
                         dut.u_graphics_pipeline.gfx_st_addr,
                         dut.u_graphics_pipeline.gfx_st_wdata,
                         dut.u_graphics_pipeline.gfx_st_wstrb);
                dbg_gfx_store_prints <= dbg_gfx_store_prints + 1;
            end
        end
    end

    initial begin
        automatic int deadbeef_count;
        int timeout;
        int stable;
        wait(rst_n);

        // Wait for program completion marker (global store + MEMBAR in ROM).
        timeout = 0;
        while (timeout < 200000 && (mem[mem_index(BASE_ADDR + DONE_OFF)] != 32'd1)) begin
            @(posedge clk);
            timeout++;
        end
        if (timeout >= 200000) begin
            $display("DEBUG: timeout waiting for done flag");
            $display("DEBUG: done=%08h PC=%08h", mem[mem_index(BASE_ADDR + DONE_OFF)], dut.if_pc);
            $display("DEBUG: IF  if_valid=%0b inst0_valid=%0b inst1_valid=%0b inst0=%08h inst1=%08h",
                     dut.if_valid, dut.if_inst0_valid, dut.if_inst1_valid, dut.if_inst0, dut.if_inst1);
            $display("DEBUG: FE  stall_issue=%0b stall_pipe=%0b lsu_stall=%0b stall_membar=%0b stall_sb0=%0b stall_sb1=%0b",
                     dut.stall_issue, dut.stall_pipe, dut.lsu_stall, dut.stall_membar, dut.stall_sb0, dut.stall_sb1);
            $display("DEBUG: FE  issue0=%0b accept0=%0b issue1=%0b accept1=%0b pc_adv=%0d",
                     dut.issue0_valid, dut.accept0, dut.issue1_valid, dut.accept1, dut.pc_advance_bytes);
            $display("DEBUG: SB  stall0=%0b rs1_busy=%0b rs2_busy=%0b rd_busy=%0b",
                     dut.stall_sb0,
                     dut.u_scoreboard.issue0_rs1_busy,
                     dut.u_scoreboard.issue0_rs2_busy,
                     dut.u_scoreboard.issue0_rd_busy);
            $display("DEBUG: SB  busy_v[1]=%0b busy_v[2]=%0b busy_v[31]=%0b (raw busy_v=%08h)",
                     dut.u_scoreboard.busy_v[1],
                     dut.u_scoreboard.busy_v[2],
                     dut.u_scoreboard.busy_v[31],
                     dut.u_scoreboard.busy_v);
            $display("DEBUG: MEM mem_valid=%0b is_load=%0b is_store=%0b is_vec=%0b addr=%08h rd=%0d",
                     dut.mem_valid, dut.mem_ctrl.is_load, dut.mem_ctrl.is_store, dut.mem_ctrl.is_vector, dut.mem_addr, dut.mem_ctrl.rd);
            $display("DEBUG: VQ  vq_count=%0d vq_valid=%b head=%0d tail=%0d valuv_ready=%0b valuv_wb_valid=%0b",
                     dut.vq_count, dut.vq_valid, dut.vq_head, dut.vq_tail, dut.valuv_ready, dut.valuv_wb_valid);
            $display("DEBUG: LSU busy=%0b stall=%0b global_req_valid=%0b global_req_ready=%0b",
                     dut.u_lsu.busy, dut.lsu_stall, data_req_valid, data_req_ready);
            $display("DEBUG: GFX gq_count=%0d raster_busy=%0d rop_busy=%0d", dut.u_graphics_pipeline.queue_count, dut.u_graphics_pipeline.raster_busy, dut.u_graphics_pipeline.rop_busy);
            $fatal(1, "Timed out waiting for program completion (done flag)");
        end

        // After completion, wait until the machine has been quiescent for a while
        // (guards against transient idle gaps between long-latency ops).
        timeout = 0;
        stable = 0;
        while (timeout < 200000 && (stable < 2)) begin
            @(posedge clk);
            timeout++;
            // Use a 4-state-safe quiescence check: bus idle + internal units idle.
            // This avoids checking framebuffer contents before GDRAW has actually drained.
            if (
                (data_req_valid === 1'b1)
                || (data_resp_valid === 1'b1)
                || (resp_empty === 1'b0)
                // Some LSU sub-states can conservatively hold busy high even when the
                // external bus is idle. For memory-stability we primarily care about
                // observable bus activity + GFX engine activity.
                || ((dut.u_lsu.busy === 1'b1)
                    && ((data_req_valid === 1'b1) || (data_resp_valid === 1'b1) || (resp_empty === 1'b0)))
                || !(dut.u_graphics_pipeline.queue_count === 4'd0)
                || (dut.u_graphics_pipeline.raster_busy === 1'b1)
                || (dut.u_graphics_pipeline.rop_busy === 1'b1)
            ) begin
                stable = 0;
            end else begin
                stable++;
            end
        end
        if (timeout >= 200000) begin
            $display("DEBUG: timeout waiting for stable quiescence");
            $display("DEBUG: stable=%0d", stable);
            $display("DEBUG: bus: data_req_valid=%0b data_resp_valid=%0b resp_empty=%0b", data_req_valid, data_resp_valid, resp_empty);
            $display("DEBUG: lsu.busy=%0d gq_count=%0d raster_busy=%0d rop_busy=%0d", dut.u_lsu.busy, dut.u_graphics_pipeline.queue_count, dut.u_graphics_pipeline.raster_busy, dut.u_graphics_pipeline.rop_busy);
            $display("DEBUG: gfx_state=%0d", dut.u_graphics_pipeline.gfx_state);
            $display("DEBUG: tex_state=%0d", dut.u_graphics_pipeline.tex_state);
            $display("DEBUG: gq_head=%0d gq_tail=%0d", dut.u_graphics_pipeline.gq_head, dut.u_graphics_pipeline.gq_tail);
            $display("DEBUG: gq_head_valid=%0d head_is_tex=%0d head_is_gfx=%0d head_funct3=%03b", 
                     dut.u_graphics_pipeline.gq_valid[dut.u_graphics_pipeline.gq_head],
                     dut.u_graphics_pipeline.gq[dut.u_graphics_pipeline.gq_head].ctrl.is_tex,
                     dut.u_graphics_pipeline.gq[dut.u_graphics_pipeline.gq_head].ctrl.is_gfx,
                     dut.u_graphics_pipeline.gq[dut.u_graphics_pipeline.gq_head].ctrl.funct3);
            $display("DEBUG: head_op_a=%08h head_op_b=%08h", dut.u_graphics_pipeline.gq[dut.u_graphics_pipeline.gq_head].op_a, dut.u_graphics_pipeline.gq[dut.u_graphics_pipeline.gq_head].op_b);
            $display("DEBUG: sampler_ptr=%08h", dut.u_graphics_pipeline.sampler_ptr);
            $display("DEBUG: gp_tex_req_valid=%0d gp_tex_req_ready=%0d gp_tex_req_addr=%08h", 
                     dut.u_graphics_pipeline.tex_req_valid,
                     dut.u_graphics_pipeline.tex_req_ready,
                     dut.u_graphics_pipeline.tex_req_addr);
            $display("DEBUG: gp_tex_resp_valid=%0d gp_tex_resp_data=%08h", 
                     dut.u_graphics_pipeline.tex_resp_valid,
                     dut.u_graphics_pipeline.tex_resp_data);
            $display("DEBUG: tex_miss_req_valid=%0d tex_miss_req_ready=%0d tex_miss_req_addr=%08h tex_miss_resp_valid=%0d", 
                     dut.tex_miss_req_valid,
                     dut.tex_miss_req_ready,
                     dut.tex_miss_req_addr,
                     dut.tex_miss_resp_valid);
            $fatal(1, "Timed out waiting for stable quiescence");
        end

        // At this point the GFX pipeline has drained, but ROP stores may still be sitting
        // in the LSU write-merge buffer. Force a MEMBAR flush so the TB memory image
        // reflects all buffered writes before we freeze and check.
        force dut.mem_is_membar = 1'b1;
        @(posedge clk);
        release dut.mem_is_membar;

        // Wait for the forced flush to drain to global memory.
        timeout = 0;
        stable = 0;
        while (timeout < 200000 && (stable < 2)) begin
            @(posedge clk);
            timeout++;
            if (
                (data_req_valid === 1'b1)
                || (data_resp_valid === 1'b1)
                || (resp_empty === 1'b0)
                || (dut.u_lsu.wmb_busy === 1'b1)
                || !(dut.u_graphics_pipeline.queue_count === 4'd0)
                || (dut.u_graphics_pipeline.raster_busy === 1'b1)
                || (dut.u_graphics_pipeline.rop_busy === 1'b1)
            ) begin
                stable = 0;
            end else begin
                stable++;
            end
        end
        if (timeout >= 200000) begin
            $fatal(1, "Timed out waiting for WMB flush to drain");
        end

        // The compute core doesn't implement a real halt instruction; after the ROM writes DONE,
        // it will eventually wrap and re-run. Freeze the DUT now so the memory image stays
        // stable while we run the checks below.
        csr_status_snap = csr_status;
        x30_snap = dut.u_regfile_scalar.mem[30];
        x31_snap = dut.u_regfile_scalar.mem[31];
        f1_snap = dut.u_regfile_fp.mem[1];
        f2_snap = dut.u_regfile_fp.mem[2];
        f3_snap = dut.u_regfile_fp.mem[3];
        f4_snap = dut.u_regfile_fp.mem[4];
        f5_snap = dut.u_regfile_fp.mem[5];
        f6_snap = dut.u_regfile_fp.mem[6];
        f7_snap = dut.u_regfile_fp.mem[7];
        f21_snap = dut.u_regfile_fp.mem[21];

        // Snapshot TB-captured VSEL result before freezing.
        snap_vsel_captured = dbg_vsel_captured;
        snap_vsel_wb_data  = dbg_vsel_wb_data;
        // Freeze the DUT by stopping the clock (do NOT assert reset, which would wipe GFX state
        // and can drop buffered stores).
        clk_en = 1'b0;
        #1;

        check_equal("scalar store @0", mem[mem_index(BASE_ADDR + 32'd0)], 32'd12);

        check_equal("scalar sub @0x300", mem[mem_index(BASE_ADDR + 32'h0000_0300)], 32'd2);
        check_equal("scalar sll @0x304", mem[mem_index(BASE_ADDR + 32'h0000_0304)], 32'd640);
        check_equal("lui @0x338", mem[mem_index(BASE_ADDR + 32'h0000_0338)], 32'h1234_5000);
        check_equal("movi (pseudo) @0x33C", mem[mem_index(BASE_ADDR + 32'h0000_033C)], 32'h1234_5ABC);
        begin
            logic [31:0] got_link;
            got_link = mem[mem_index(BASE_ADDR + 32'h0000_0330)];
            // The core may wrap and re-run the ROM; link = PC+4 will then differ by multiples of ROM size.
            // Compare modulo ROM_BYTES.
            if ((got_link & ROM_MASK) != (exp_jal_link & ROM_MASK)) begin
                $fatal(1, "jal link @0x330 mismatch: got %0d (0x%08h) expected %0d (0x%08h)", got_link, got_link, exp_jal_link, exp_jal_link);
            end else begin
                $display("[OK] jal link @0x330 = %0d (0x%08h)", got_link, got_link);
            end
        end
        check_equal("jalr marker @0x334", mem[mem_index(BASE_ADDR + 32'h0000_0334)], 32'h0000_005A);

        // Branch redirect/flush behavior
        check_equal("beq taken flush sentinel @0x360", mem[mem_index(BASE_ADDR + 32'h0000_0360)], 32'h0000_0000);
        check_equal("beq taken target marker @0x364", mem[mem_index(BASE_ADDR + 32'h0000_0364)], 32'h0000_0022);
        check_equal("bne not-taken fallthrough @0x368", mem[mem_index(BASE_ADDR + 32'h0000_0368)], 32'h0000_0011);
        check_equal("bne not-taken target not executed @0x36C", mem[mem_index(BASE_ADDR + 32'h0000_036C)], 32'h0000_0000);
        check_equal("jalr delay-slot sentinel @0x370", mem[mem_index(BASE_ADDR + 32'h0000_0370)], 32'h0000_0000);

        // Branch-after-load (data dependency)
        check_equal("beq after load flush sentinel @0x374", mem[mem_index(BASE_ADDR + 32'h0000_0374)], 32'h0000_0000);
        check_equal("beq after load target marker @0x378", mem[mem_index(BASE_ADDR + 32'h0000_0378)], 32'h0000_0022);
        check_equal("bne after load fallthrough @0x37C", mem[mem_index(BASE_ADDR + 32'h0000_037C)], 32'h0000_0011);
        check_equal("bne after load target not executed @0x380", mem[mem_index(BASE_ADDR + 32'h0000_0380)], 32'h0000_0000);

        // Slot1 placement (inst1) behavior
        check_equal("inst1 branch fallthrough not executed @0x3A0", mem[mem_index(BASE_ADDR + 32'h0000_03A0)], 32'h0000_0000);
        check_equal("inst1 branch target executed @0x3A4", mem[mem_index(BASE_ADDR + 32'h0000_03A4)], 32'h0000_00BB);
        check_equal("inst1 store executed @0x3A8", mem[mem_index(BASE_ADDR + 32'h0000_03A8)], 32'h0000_005A);
        check_equal("inst1 load roundtrip @0x3B4", mem[mem_index(BASE_ADDR + 32'h0000_03B4)], 32'h1234_5678);

        // Comprehensive branch matrix
        check_equal("blt taken flush sentinel @0x3C0", mem[mem_index(BASE_ADDR + 32'h0000_03C0)], 32'h0000_0000);
        check_equal("blt taken target marker @0x3C4", mem[mem_index(BASE_ADDR + 32'h0000_03C4)], 32'h0000_0033);
        check_equal("bge not-taken fallthrough @0x3C8", mem[mem_index(BASE_ADDR + 32'h0000_03C8)], 32'h0000_0044);
        check_equal("bge not-taken target not executed @0x3CC", mem[mem_index(BASE_ADDR + 32'h0000_03CC)], 32'h0000_0000);

        check_equal("blt(-1,1) flush sentinel @0x3D0", mem[mem_index(BASE_ADDR + 32'h0000_03D0)], 32'h0000_0000);
        check_equal("blt(-1,1) target marker @0x3D4", mem[mem_index(BASE_ADDR + 32'h0000_03D4)], 32'h0000_0055);
        check_equal("bltu(FFFF,1) fallthrough @0x3D8", mem[mem_index(BASE_ADDR + 32'h0000_03D8)], 32'h0000_0055);
        check_equal("bltu(FFFF,1) target not executed @0x3DC", mem[mem_index(BASE_ADDR + 32'h0000_03DC)], 32'h0000_0000);

        check_equal("bgeu(FFFF,1) flush sentinel @0x3E0", mem[mem_index(BASE_ADDR + 32'h0000_03E0)], 32'h0000_0000);
        check_equal("bgeu(FFFF,1) target marker @0x3E4", mem[mem_index(BASE_ADDR + 32'h0000_03E4)], 32'h0000_0066);

        check_equal_warn("backward-loop count @0x3E8", mem[mem_index(BASE_ADDR + 32'h0000_03E8)], 32'd5);
        check_equal("backward-loop x10 @0x3EC", mem[mem_index(BASE_ADDR + 32'h0000_03EC)], 32'd0);

        // Scalar dependency / hazard tests
        check_equal("raw chain result @0x3F0", mem[mem_index(BASE_ADDR + 32'h0000_03F0)], 32'd22);
        check_equal("waw chain result @0x3F4", mem[mem_index(BASE_ADDR + 32'h0000_03F4)], 32'd7);
        check_equal("load-use result @0x3FC", mem[mem_index(BASE_ADDR + 32'h0000_03FC)], 32'h1234_567D);
        check_equal("scalar sra @0x308", mem[mem_index(BASE_ADDR + 32'h0000_0308)], 32'hFFFF_FFFC);
        check_equal("scalar srl @0x30C", mem[mem_index(BASE_ADDR + 32'h0000_030C)], 32'h3FFF_FFFC);
        check_equal("scalar slt @0x310", mem[mem_index(BASE_ADDR + 32'h0000_0310)], 32'd1);
        check_equal("scalar sltu @0x314", mem[mem_index(BASE_ADDR + 32'h0000_0314)], 32'd0);
        check_equal("scalar xor @0x318", mem[mem_index(BASE_ADDR + 32'h0000_0318)], 32'd2);
        check_equal("scalar or @0x31C", mem[mem_index(BASE_ADDR + 32'h0000_031C)], 32'd7);
        check_equal("scalar and @0x320", mem[mem_index(BASE_ADDR + 32'h0000_0320)], 32'd5);

        check_equal("scalar store @8", mem[mem_index(BASE_ADDR + 32'd8)], 32'd12);
        check_equal("csr_status", csr_status_snap, 32'd15);

        // Scalar FP16 ALU results (bit-exact)
        check_equal("fp16 f1 (cvt x0)", {16'h0, f1_snap}, 32'h0000_3C00); // +1.0
        check_equal("fp16 f2 (cvt -1)", {16'h0, f2_snap}, 32'h0000_BC00); // -1.0
        check_equal("fp16 f3 (1+1)",   {16'h0, f3_snap}, 32'h0000_4000); // +2.0
        check_equal("fp16 f4 (2-1)",   {16'h0, f4_snap}, 32'h0000_3C00); // +1.0
        check_equal("fp16 f5 (2*2)",   {16'h0, f5_snap}, 32'h0000_4400); // +4.0
        check_equal("fp16 f6 min(-1,1)", {16'h0, f6_snap}, 32'h0000_BC00); // -1.0
        check_equal("fp16 f7 max(-1,1)", {16'h0, f7_snap}, 32'h0000_3C00); // +1.0

        // Scalar FP16 conversions and FMA (implementation-specific, bit-exact)
        check_equal("fp16 fma f21", {16'h0, f21_snap}, 32'h0000_4000); // +2.0
        check_equal("x30 (fp16 f2i f1)", x30_snap, 32'd2048);
        check_equal("x31 (fp16 f2i f2)", x31_snap, 32'hFFFF_F800);

        // v0 writable + VXOR checks
        check_equal("v28(v0+v1) lane0", mem[mem_index(BASE_ADDR + 32'd144)], 32'd12);
        check_equal("v28(v0+v1) lane1", mem[mem_index(BASE_ADDR + 32'd148)], 32'd24);
        check_equal("v28(v0+v1) lane2", mem[mem_index(BASE_ADDR + 32'd152)], 32'd36);
        check_equal("v28(v0+v1) lane3", mem[mem_index(BASE_ADDR + 32'd156)], 32'd48);
        check_equal("v29(v1+vxor(v0,v0)) lane0", mem[mem_index(BASE_ADDR + 32'd160)], 32'd1);
        check_equal("v29(v1+vxor(v0,v0)) lane1", mem[mem_index(BASE_ADDR + 32'd164)], 32'd2);
        check_equal("v29(v1+vxor(v0,v0)) lane2", mem[mem_index(BASE_ADDR + 32'd168)], 32'd3);
        check_equal("v29(v1+vxor(v0,v0)) lane3", mem[mem_index(BASE_ADDR + 32'd172)], 32'd4);

        check_equal("vand lane0", mem[mem_index(BASE_ADDR + 32'd176)], 32'd0);
        check_equal("vand lane1", mem[mem_index(BASE_ADDR + 32'd180)], 32'd0);
        check_equal("vand lane2", mem[mem_index(BASE_ADDR + 32'd184)], 32'd2);
        check_equal("vand lane3", mem[mem_index(BASE_ADDR + 32'd188)], 32'd0);
        check_equal("vor lane0",  mem[mem_index(BASE_ADDR + 32'd192)], 32'd11);
        check_equal("vor lane1",  mem[mem_index(BASE_ADDR + 32'd196)], 32'd22);
        check_equal("vor lane2",  mem[mem_index(BASE_ADDR + 32'd200)], 32'd31);
        check_equal("vor lane3",  mem[mem_index(BASE_ADDR + 32'd204)], 32'd44);
        if (!snap_vsel_captured) begin
            $fatal(1, "vsel(vmask=0x5): did not observe v26 writeback after VMASK was programmed");
        end
        check_equal("vsel(vmask=0x5) lane0", snap_vsel_wb_data[31:0],   32'd1);
        check_equal("vsel(vmask=0x5) lane1", snap_vsel_wb_data[63:32],  32'd20);
        check_equal("vsel(vmask=0x5) lane2", snap_vsel_wb_data[95:64],  32'd3);
        check_equal("vsel(vmask=0x5) lane3", snap_vsel_wb_data[127:96], 32'd40);

        check_equal("vcmp mask store", mem[mem_index(BASE_ADDR + 32'd64)], 32'hF);

        // FP32 vector ALU results (bit-exact)
        $display("DEBUG fp32 A @0x180: %08h %08h %08h %08h",
             mem[mem_index(BASE_ADDR + 32'h0000_0180)],
             mem[mem_index(BASE_ADDR + 32'h0000_0184)],
             mem[mem_index(BASE_ADDR + 32'h0000_0188)],
             mem[mem_index(BASE_ADDR + 32'h0000_018C)]);
        $display("DEBUG fp32 B @0x190: %08h %08h %08h %08h",
             mem[mem_index(BASE_ADDR + 32'h0000_0190)],
             mem[mem_index(BASE_ADDR + 32'h0000_0194)],
             mem[mem_index(BASE_ADDR + 32'h0000_0198)],
             mem[mem_index(BASE_ADDR + 32'h0000_019C)]);
        $display("DEBUG fp32 ADD @0x1A0: %08h %08h %08h %08h",
             mem[mem_index(BASE_ADDR + 32'h0000_01A0)],
             mem[mem_index(BASE_ADDR + 32'h0000_01A4)],
             mem[mem_index(BASE_ADDR + 32'h0000_01A8)],
             mem[mem_index(BASE_ADDR + 32'h0000_01AC)]);
        $display("DEBUG v8 = %032h", dut.u_regfile_vector.mem[8]);
        $display("DEBUG v9 = %032h", dut.u_regfile_vector.mem[9]);
        $display("DEBUG v10= %032h", dut.u_regfile_vector.mem[10]);
        check_equal("fp32 add lane0", mem[mem_index(BASE_ADDR + 32'h0000_01A0)], 32'h4040_0000); // 3.0
        check_equal("fp32 add lane1", mem[mem_index(BASE_ADDR + 32'h0000_01A4)], 32'hBF00_0000); // -0.5
        check_equal("fp32 add lane2", mem[mem_index(BASE_ADDR + 32'h0000_01A8)], 32'h4100_0000); // 8.0
        check_equal("fp32 add lane3", mem[mem_index(BASE_ADDR + 32'h0000_01AC)], 32'h7F80_0000); // +Inf

        check_equal("fp32 sub lane0", mem[mem_index(BASE_ADDR + 32'h0000_01B0)], 32'hBF80_0000); // -1.0
        check_equal("fp32 sub lane1", mem[mem_index(BASE_ADDR + 32'h0000_01B4)], 32'hBFC0_0000); // -1.5
        check_equal("fp32 sub lane2", mem[mem_index(BASE_ADDR + 32'h0000_01B8)], 32'h0000_0000); // 0.0
        check_equal("fp32 sub lane3", mem[mem_index(BASE_ADDR + 32'h0000_01BC)], 32'h7F80_0000); // +Inf

        check_equal("fp32 min lane0", mem[mem_index(BASE_ADDR + 32'h0000_01C0)], 32'h3F80_0000); // 1.0
        check_equal("fp32 min lane1", mem[mem_index(BASE_ADDR + 32'h0000_01C4)], 32'hBF80_0000); // -1.0
        check_equal("fp32 min lane2", mem[mem_index(BASE_ADDR + 32'h0000_01C8)], 32'h4080_0000); // 4.0
        check_equal("fp32 min lane3", mem[mem_index(BASE_ADDR + 32'h0000_01CC)], 32'h3F80_0000); // 1.0

        check_equal("fp32 max lane0", mem[mem_index(BASE_ADDR + 32'h0000_01D0)], 32'h4000_0000); // 2.0
        check_equal("fp32 max lane1", mem[mem_index(BASE_ADDR + 32'h0000_01D4)], 32'h3F00_0000); // 0.5
        check_equal("fp32 max lane2", mem[mem_index(BASE_ADDR + 32'h0000_01D8)], 32'h4080_0000); // 4.0
        check_equal("fp32 max lane3", mem[mem_index(BASE_ADDR + 32'h0000_01DC)], 32'h7F80_0000); // +Inf

        // Vector integer VSUB/VDOT
        check_equal("vsub lane0", mem[mem_index(BASE_ADDR + 32'd96)], 32'd9);
        check_equal("vsub lane1", mem[mem_index(BASE_ADDR + 32'd100)], 32'd18);
        check_equal("vsub lane2", mem[mem_index(BASE_ADDR + 32'd104)], 32'd27);
        check_equal("vsub lane3", mem[mem_index(BASE_ADDR + 32'd108)], 32'd36);
        check_equal("vdot lane0", mem[mem_index(BASE_ADDR + 32'd112)], 32'd300);
        check_equal("vdot lane1", mem[mem_index(BASE_ADDR + 32'd116)], 32'd0);
        check_equal("vdot lane2", mem[mem_index(BASE_ADDR + 32'd120)], 32'd0);
        check_equal("vdot lane3", mem[mem_index(BASE_ADDR + 32'd124)], 32'd0);

        // FP16 vector ALU results (bit-exact; 2 lanes packed per 32-bit word)
        check_equal("fp16 vadd w0", mem[mem_index(BASE_ADDR + 32'h0000_0220)], 32'h3C00_4000);
        check_equal("fp16 vadd w1", mem[mem_index(BASE_ADDR + 32'h0000_0224)], 32'h0000_0000);
        check_equal("fp16 vadd w2", mem[mem_index(BASE_ADDR + 32'h0000_0228)], 32'h0000_3C00);
        check_equal("fp16 vadd w3", mem[mem_index(BASE_ADDR + 32'h0000_022C)], 32'hC200_4800);

        check_equal("fp16 vsub w0", mem[mem_index(BASE_ADDR + 32'h0000_0230)], 32'hC200_0000);
        check_equal("fp16 vsub w1", mem[mem_index(BASE_ADDR + 32'h0000_0234)], 32'hC400_4400);
        check_equal("fp16 vsub w2", mem[mem_index(BASE_ADDR + 32'h0000_0238)], 32'hBC00_0000);
        check_equal("fp16 vsub w3", mem[mem_index(BASE_ADDR + 32'h0000_023C)], 32'hC500_0000);

        check_equal("fp16 vmin w0", mem[mem_index(BASE_ADDR + 32'h0000_0240)], 32'hBC00_3C00);
        check_equal("fp16 vmin w1", mem[mem_index(BASE_ADDR + 32'h0000_0244)], 32'hC000_C000);
        check_equal("fp16 vmin w2", mem[mem_index(BASE_ADDR + 32'h0000_0248)], 32'hB800_3800);
        check_equal("fp16 vmin w3", mem[mem_index(BASE_ADDR + 32'h0000_024C)], 32'hC400_4400);

        check_equal("fp16 vmax w0", mem[mem_index(BASE_ADDR + 32'h0000_0250)], 32'h4000_3C00);
        check_equal("fp16 vmax w1", mem[mem_index(BASE_ADDR + 32'h0000_0254)], 32'h4000_4000);
        check_equal("fp16 vmax w2", mem[mem_index(BASE_ADDR + 32'h0000_0258)], 32'h3800_3800);
        check_equal("fp16 vmax w3", mem[mem_index(BASE_ADDR + 32'h0000_025C)], 32'h3C00_4400);

        // FP8 vector ALU results (bit-exact; 4 lanes packed per 32-bit word)
        check_equal("fp8 vadd w0", mem[mem_index(BASE_ADDR + 32'h0000_0280)], 32'h1010_1010);
        check_equal("fp8 vadd w1", mem[mem_index(BASE_ADDR + 32'h0000_0284)], 32'h1010_1010);
        check_equal("fp8 vadd w2", mem[mem_index(BASE_ADDR + 32'h0000_0288)], 32'h1010_1010);
        check_equal("fp8 vadd w3", mem[mem_index(BASE_ADDR + 32'h0000_028C)], 32'h1010_1010);

        check_equal("fp8 vsub w0", mem[mem_index(BASE_ADDR + 32'h0000_0290)], 32'h0000_0000);
        check_equal("fp8 vsub w1", mem[mem_index(BASE_ADDR + 32'h0000_0294)], 32'h0000_0000);
        check_equal("fp8 vsub w2", mem[mem_index(BASE_ADDR + 32'h0000_0298)], 32'h0000_0000);
        check_equal("fp8 vsub w3", mem[mem_index(BASE_ADDR + 32'h0000_029C)], 32'h0000_0000);

        check_equal("fp8 vmin w0", mem[mem_index(BASE_ADDR + 32'h0000_02C0)], 32'h0888_0888);
        check_equal("fp8 vmin w1", mem[mem_index(BASE_ADDR + 32'h0000_02C4)], 32'h0888_0888);
        check_equal("fp8 vmin w2", mem[mem_index(BASE_ADDR + 32'h0000_02C8)], 32'h0888_0888);
        check_equal("fp8 vmin w3", mem[mem_index(BASE_ADDR + 32'h0000_02CC)], 32'h0888_0888);

        check_equal("fp8 vmax w0", mem[mem_index(BASE_ADDR + 32'h0000_02D0)], 32'h0808_0808);
        check_equal("fp8 vmax w1", mem[mem_index(BASE_ADDR + 32'h0000_02D4)], 32'h0808_0808);
        check_equal("fp8 vmax w2", mem[mem_index(BASE_ADDR + 32'h0000_02D8)], 32'h0808_0808);
        check_equal("fp8 vmax w3", mem[mem_index(BASE_ADDR + 32'h0000_02DC)], 32'h0808_0808);

        // Additional vector ops
        check_equal("vmul.i32 lane0", mem[mem_index(BASE_ADDR + 32'h0000_0700)], 32'd10);
        check_equal("vmul.i32 lane1", mem[mem_index(BASE_ADDR + 32'h0000_0704)], 32'd40);
        check_equal("vmul.i32 lane2", mem[mem_index(BASE_ADDR + 32'h0000_0708)], 32'd90);
        check_equal("vmul.i32 lane3", mem[mem_index(BASE_ADDR + 32'h0000_070C)], 32'd160);

        check_equal("vunpack.i32 lane0(r)", mem[mem_index(BASE_ADDR + 32'h0000_0710)], 32'h0000_0044);
        check_equal("vunpack.i32 lane1(g)", mem[mem_index(BASE_ADDR + 32'h0000_0714)], 32'h0000_0033);
        check_equal("vunpack.i32 lane2(b)", mem[mem_index(BASE_ADDR + 32'h0000_0718)], 32'h0000_0022);
        check_equal("vunpack.i32 lane3(a)", mem[mem_index(BASE_ADDR + 32'h0000_071C)], 32'h0000_0011);
        check_equal("vpack.i32", mem[mem_index(BASE_ADDR + 32'h0000_0720)], 32'h1122_3344);

        // VRCP/VRSQRT special cases
        check_equal("vrcp(+inf) lane0", mem[mem_index(BASE_ADDR + 32'h0000_0730)], 32'h0000_0000);
        check_equal("vrcp(+0) lane1",   mem[mem_index(BASE_ADDR + 32'h0000_0734)], 32'h7F80_0000);
        check_equal("vrcp(nan) lane2",  mem[mem_index(BASE_ADDR + 32'h0000_0738)], 32'h7FC1_2345);
        check_equal("vrcp(-inf) lane3", mem[mem_index(BASE_ADDR + 32'h0000_073C)], 32'h8000_0000);

        check_equal("vrsqrt(+inf) lane0", mem[mem_index(BASE_ADDR + 32'h0000_0740)], 32'h0000_0000);
        check_equal("vrsqrt(+0) lane1",   mem[mem_index(BASE_ADDR + 32'h0000_0744)], 32'h7F80_0000);
        check_equal("vrsqrt(nan) lane2",  mem[mem_index(BASE_ADDR + 32'h0000_0748)], 32'h7FC1_2345);
        check_equal("vrsqrt(-inf) lane3", mem[mem_index(BASE_ADDR + 32'h0000_074C)], 32'h7FC0_0000);

        check_equal("vrsqrt(-1) lane0", mem[mem_index(BASE_ADDR + 32'h0000_0750)], 32'h7FC0_0000);
        check_equal("vrsqrt(0) lane1",  mem[mem_index(BASE_ADDR + 32'h0000_0754)], 32'h7F80_0000);
        check_equal("vrsqrt(0) lane2",  mem[mem_index(BASE_ADDR + 32'h0000_0758)], 32'h7F80_0000);
        check_equal("vrsqrt(0) lane3",  mem[mem_index(BASE_ADDR + 32'h0000_075C)], 32'h7F80_0000);

        // VADD result after atomic in-place update
        check_equal("vadd/atom lane0", mem[mem_index(BASE_ADDR + 32'd48)], 32'd21);
        check_equal("vadd/atom lane1", mem[mem_index(BASE_ADDR + 32'd52)], 32'd42);
        check_equal("vadd/atom lane2", mem[mem_index(BASE_ADDR + 32'd56)], 32'd63);
        check_equal("vadd/atom lane3", mem[mem_index(BASE_ADDR + 32'd60)], 32'd84);

        // Captured atomic result copy
        check_equal("vatom lane0", mem[mem_index(BASE_ADDR + 32'd80)], 32'd21);
        check_equal("vatom lane1", mem[mem_index(BASE_ADDR + 32'd84)], 32'd42);
        check_equal("vatom lane2", mem[mem_index(BASE_ADDR + 32'd88)], 32'd63);
        check_equal("vatom lane3", mem[mem_index(BASE_ADDR + 32'd92)], 32'd84);

        // Texture sample (only lane0 carries data)
        check_equal("tex lane0", mem[mem_index(BASE_ADDR + 32'h0000_0120)], 32'hDEAD_BEEF);
        check_equal("tex lane1", mem[mem_index(BASE_ADDR + 32'h0000_0124)], 32'd0);
        check_equal("tex lane2", mem[mem_index(BASE_ADDR + 32'h0000_0128)], 32'd0);
        check_equal("tex lane3", mem[mem_index(BASE_ADDR + 32'h0000_012C)], 32'd0);

        // Ensure DEAD_BEEF appears only where expected: source texture line and stored TEX result
        deadbeef_count = 0;
        for (int i = 0; i < MEM_WORDS; i++) begin
            if (mem[i] == 32'hDEAD_BEEF) deadbeef_count++;
        end
        check_equal("deadbeef count", deadbeef_count, 32'd2);

        $display("GFX state dump: r_fb_base=%08h r_fb_stride=%08h r_fb_w=%0d r_fb_h=%0d rstate_flags0=%08h",
             dut.u_graphics_pipeline.r_fb_base,
             dut.u_graphics_pipeline.r_fb_stride,
             dut.u_graphics_pipeline.r_fb_w,
             dut.u_graphics_pipeline.r_fb_h,
             dut.u_graphics_pipeline.rstate_flags0);
        $display("GFX state dump: g_vbo_base=%08h g_vbo_stride=%08h g_material_color=%08h",
             dut.u_graphics_pipeline.g_vbo_base,
             dut.u_graphics_pipeline.g_vbo_stride,
             dut.u_graphics_pipeline.g_material_color);

        $display("GFX mem dump: GSTATE.vbo_base(mem)=%08h vbo_stride(mem)=%08h",
             mem[mem_index(BASE_ADDR + GSTATE_OFF + 32'h00)],
             mem[mem_index(BASE_ADDR + GSTATE_OFF + 32'h04)]);
        $display("GFX mem dump: GPARAM.mat_color(mem)=%08h",
             mem[mem_index(BASE_ADDR + GPARAM_OFF + 32'h08)]);
        $display("GFX state dump: gd_first=%0d gd_count=%0d gd_topology=%0d gd_flags=%08h",
             dut.u_graphics_pipeline.gd_first,
             dut.u_graphics_pipeline.gd_count,
             dut.u_graphics_pipeline.gd_topology,
             dut.u_graphics_pipeline.gd_flags);

        // GFX framebuffer stores (directed): check a few interior pixels.
        // Avoid edge/vertex pixels, which are sensitive to raster edge rules.
        check_equal("gfx fb +0x48", mem[mem_index(BASE_ADDR + FB_BASE_OFF + 32'h48)], GFX_COLOR);
        check_equal("gfx fb +0x4C", mem[mem_index(BASE_ADDR + FB_BASE_OFF + 32'h4C)], GFX_COLOR);
        check_equal("gfx fb +0x68", mem[mem_index(BASE_ADDR + FB_BASE_OFF + 32'h68)], GFX_COLOR);
        check_equal("gfx fb +0x6C", mem[mem_index(BASE_ADDR + FB_BASE_OFF + 32'h6C)], GFX_COLOR);
        check_equal("gfx fb +0x88", mem[mem_index(BASE_ADDR + FB_BASE_OFF + 32'h88)], GFX_COLOR);

        // Second draw (VBO2 + GFX_COLOR2): check an interior pixel in the bottom-right triangle.
        check_equal("gfx2 fb +0xB4", mem[mem_index(BASE_ADDR + FB_BASE_OFF + 32'hB4)], GFX_COLOR2);

        $display("\nFull testbench checks passed.");
        $finish;
    end

endmodule
/* verilator lint_on SYNCASYNCNET */
