`timescale 1ns/1ps

// Multi-core software ray-tracing testbench
// - 2 compute units share memory, each renders alternate rows
// - True 3D ray-plane intersection for perspective checkerboard ground
// - 3-level gradient shadow on ground
// - Mirror sphere with specular + green diffuse sphere
// - Per-core DONE signaling via separate cache lines
module gfx_sw_rt_multicore_tb;
    import isa_pkg::*;

    // ---------------------------------------------------------------------
    // Tunables
    // ---------------------------------------------------------------------
    localparam int NUM_CORES = 2;
    localparam int W = 128;
    localparam int H = 128;
    localparam int FRAMES = 3;

    localparam logic [31:0] BASE_ADDR = 32'h8000_0000;

    localparam int FB_SIZE_BYTES = W * H * 4;
    localparam int FB_SIZE_WORDS = FB_SIZE_BYTES / 4;

    // Memory layout (offsets from BASE_ADDR)
    localparam logic [31:0] DONE_CU0_OFF    = 32'h0000_0100;  // cache-line aligned
    localparam logic [31:0] DONE_CU1_OFF    = 32'h0000_0140;  // next cache line
    localparam logic [31:0] SINCOS_OFF      = 32'h0000_2000;
    localparam logic [31:0] INVSQRT_OFF     = 32'h0000_3000;
    localparam logic [31:0] VEC_CONST_OFF   = 32'h0000_4000;
    localparam logic [31:0] PERSP_OFF       = 32'h0000_6000;  // perspective LUT (128 entries)
    localparam logic [31:0] FB_OFF          = 32'h0001_0000;

    // ---------------------------------------------------------------------
    // Clock / reset
    // ---------------------------------------------------------------------
    logic clk;
    logic rst_n;

    initial clk = 1'b0;
    always #1 clk = ~clk;

    initial begin
        rst_n = 1'b0;
        repeat (10) @(posedge clk);
        rst_n = 1'b1;
    end

    // ---------------------------------------------------------------------
    // Port declarations for CU0 and CU1
    // ---------------------------------------------------------------------
    import mailbox_pkg::*;

    // Macro-like generate for port bundles (per CU)
    `define DECL_CU_PORTS(N) \
        logic        inst_miss_req_valid_``N; \
        logic [31:0] inst_miss_req_addr_``N; \
        logic        inst_miss_req_ready_``N; \
        logic        inst_miss_resp_valid_``N; \
        logic [63:0] inst_miss_resp_data_``N; \
        logic        data_req_valid_``N; \
        logic        data_req_is_load_``N; \
        logic [31:0] data_req_addr_``N; \
        logic [31:0] data_req_wdata_``N; \
        logic [4:0]  data_req_rd_``N; \
        logic        data_req_ready_``N; \
        logic        data_resp_valid_``N; \
        logic [4:0]  data_resp_rd_``N; \
        logic [31:0] data_resp_data_``N; \
        logic        dcache_mem_req_valid_``N; \
        logic        dcache_mem_req_rw_``N; \
        logic [31:0] dcache_mem_req_addr_``N; \
        logic [7:0]  dcache_mem_req_size_``N; \
        logic [3:0]  dcache_mem_req_qos_``N; \
        logic [7:0]  dcache_mem_req_id_``N; \
        logic [511:0] dcache_mem_req_wdata_``N; \
        logic [7:0]  dcache_mem_req_wstrb_``N; \
        logic        dcache_mem_req_ready_``N; \
        logic        dcache_mem_resp_valid_``N; \
        logic [63:0] dcache_mem_resp_data_``N; \
        logic [7:0]  dcache_mem_resp_id_``N; \
        logic        fb_aw_valid_``N; \
        logic [31:0] fb_aw_addr_``N; \
        logic [7:0]  fb_aw_len_``N; \
        logic [2:0]  fb_aw_size_``N; \
        logic [1:0]  fb_aw_burst_``N; \
        logic        fb_aw_ready_``N; \
        logic [31:0] fb_w_data_``N; \
        logic [3:0]  fb_w_strb_``N; \
        logic        fb_w_last_``N; \
        logic        fb_w_valid_``N; \
        logic        fb_w_ready_``N; \
        logic        fb_b_valid_``N; \
        logic        fb_b_ready_``N; \
        mailbox_pkg::mailbox_flit_t mailbox_tx_data_``N; \
        mailbox_pkg::mailbox_flit_t mailbox_rx_data_``N; \
        logic        mailbox_tx_valid_``N; \
        logic        mailbox_tx_ready_``N; \
        logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_tx_dest_id_``N; \
        logic        mailbox_rx_valid_``N; \
        logic        mailbox_rx_ready_``N; \
        logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_rx_dest_id_``N; \
        logic        err_fp_overflow_``N; \
        logic        err_fp_invalid_``N; \
        logic        err_vec_overflow_``N; \
        logic        err_vec_invalid_``N; \
        logic [31:0] csr_status_``N; \
        logic [31:0] csr_fstatus_``N; \
        logic [31:0] csr_vstatus_``N;

    `DECL_CU_PORTS(0)
    `DECL_CU_PORTS(1)

    // ---------------------------------------------------------------------
    // CU instantiation
    // ---------------------------------------------------------------------
    `define INST_CU(N, CID) \
    compute_unit_top #(.CORE_ID(CID)) cu``N ( \
        .clk(clk), .rst_n(rst_n), \
        .inst_miss_req_valid(inst_miss_req_valid_``N), \
        .inst_miss_req_addr(inst_miss_req_addr_``N), \
        .inst_miss_req_ready(inst_miss_req_ready_``N), \
        .inst_miss_resp_valid(inst_miss_resp_valid_``N), \
        .inst_miss_resp_data(inst_miss_resp_data_``N), \
        .data_req_valid(data_req_valid_``N), \
        .data_req_is_load(data_req_is_load_``N), \
        .data_req_addr(data_req_addr_``N), \
        .data_req_wdata(data_req_wdata_``N), \
        .data_req_rd(data_req_rd_``N), \
        .data_req_ready(data_req_ready_``N), \
        .data_resp_valid(data_resp_valid_``N), \
        .data_resp_rd(data_resp_rd_``N), \
        .data_resp_data(data_resp_data_``N), \
        .err_fp_overflow(err_fp_overflow_``N), \
        .err_fp_invalid(err_fp_invalid_``N), \
        .err_vec_overflow(err_vec_overflow_``N), \
        .err_vec_invalid(err_vec_invalid_``N), \
        .csr_status(csr_status_``N), \
        .csr_fstatus(csr_fstatus_``N), \
        .csr_vstatus(csr_vstatus_``N), \
        .dcache_mem_req_valid(dcache_mem_req_valid_``N), \
        .dcache_mem_req_rw(dcache_mem_req_rw_``N), \
        .dcache_mem_req_addr(dcache_mem_req_addr_``N), \
        .dcache_mem_req_size(dcache_mem_req_size_``N), \
        .dcache_mem_req_qos(dcache_mem_req_qos_``N), \
        .dcache_mem_req_id(dcache_mem_req_id_``N), \
        .dcache_mem_req_wdata(dcache_mem_req_wdata_``N), \
        .dcache_mem_req_wstrb(dcache_mem_req_wstrb_``N), \
        .dcache_mem_req_ready(dcache_mem_req_ready_``N), \
        .dcache_mem_resp_valid(dcache_mem_resp_valid_``N), \
        .dcache_mem_resp_data(dcache_mem_resp_data_``N), \
        .dcache_mem_resp_id(dcache_mem_resp_id_``N), \
        .fb_aw_valid(fb_aw_valid_``N), \
        .fb_aw_addr(fb_aw_addr_``N), \
        .fb_aw_len(fb_aw_len_``N), \
        .fb_aw_size(fb_aw_size_``N), \
        .fb_aw_burst(fb_aw_burst_``N), \
        .fb_aw_ready(fb_aw_ready_``N), \
        .fb_w_data(fb_w_data_``N), \
        .fb_w_strb(fb_w_strb_``N), \
        .fb_w_last(fb_w_last_``N), \
        .fb_w_valid(fb_w_valid_``N), \
        .fb_w_ready(fb_w_ready_``N), \
        .fb_b_valid(fb_b_valid_``N), \
        .fb_b_ready(fb_b_ready_``N), \
        .mailbox_tx_valid(mailbox_tx_valid_``N), \
        .mailbox_tx_ready(mailbox_tx_ready_``N), \
        .mailbox_tx_data(mailbox_tx_data_``N), \
        .mailbox_tx_dest_id(mailbox_tx_dest_id_``N), \
        .mailbox_rx_valid(mailbox_rx_valid_``N), \
        .mailbox_rx_ready(mailbox_rx_ready_``N), \
        .mailbox_rx_data(mailbox_rx_data_``N), \
        .mailbox_rx_dest_id(mailbox_rx_dest_id_``N) \
    );

    `INST_CU(0, 32'h0)
    `INST_CU(1, 32'h1)

    // ---------------------------------------------------------------------
    // Tie-offs for unused ports (per CU)
    // ---------------------------------------------------------------------
    `define TIE_CU(N) \
        assign data_req_ready_``N  = 1'b1; \
        assign data_resp_valid_``N = 1'b0; \
        assign data_resp_rd_``N    = '0; \
        assign data_resp_data_``N  = 32'h0; \
        assign mailbox_tx_ready_``N   = 1'b1; \
        assign mailbox_rx_valid_``N   = 1'b0; \
        assign mailbox_rx_data_``N    = '0; \
        assign mailbox_rx_dest_id_``N = '0; \
        assign fb_aw_ready_``N = 1'b1; \
        assign fb_w_ready_``N  = 1'b1; \
        assign fb_b_valid_``N  = 1'b0; \
        assign dcache_mem_req_ready_``N = 1'b1; \
        assign inst_miss_req_ready_``N  = 1'b1;

    `TIE_CU(0)
    `TIE_CU(1)

    // ---------------------------------------------------------------------
    // Wave dump
    // ---------------------------------------------------------------------
    initial begin
        $dumpfile("gfx_sw_rt_multicore_tb.vcd");
        $dumpvars(0, gfx_sw_rt_multicore_tb);
    end

    // ---------------------------------------------------------------------
    // Mini assembler helpers
    // ---------------------------------------------------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1,
                                     input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3,
                                     input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] u_type(input integer imm, input [4:0] rd, input [6:0] opcode);
        u_type = {imm[31:12], rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2,
                                     input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] b_type(input integer imm, input [4:0] rs1, input [4:0] rs2,
                                     input [2:0] funct3, input [6:0] opcode);
        b_type = {imm[12], imm[10:5], rs2, rs1, funct3, imm[4:1], imm[11], opcode};
    endfunction

    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_IMM);
    endfunction

    function automatic [31:0] vsetvli_inst(input [4:0] rd, input [4:0] rs1, input [10:0] zimm);
        vsetvli_inst = {1'b0, zimm, rs1, 3'b111, rd, 7'b1010111};
    endfunction

    function automatic [31:0] rvv_vl(input [4:0] vd, input [4:0] rs1, input [2:0] width, input vm);
        rvv_vl = {3'b000, 1'b0, 2'b00, vm, 5'b00000, rs1, width, vd, 7'b0000111};
    endfunction

    // ---------------------------------------------------------------------
    // Instruction ROM (shared by both CUs)
    // ---------------------------------------------------------------------
    localparam int ROM_WORDS = 4096;
    logic [31:0] rom [0:ROM_WORDS-1];

    // ---------------------------------------------------------------------
    // Global memory model (shared by both CUs)
    // ---------------------------------------------------------------------
    localparam int MEM_WORDS = (FB_OFF + FB_SIZE_BYTES + 32'h0001_0000) >> 2;
    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - BASE_ADDR) >> 2;
    endfunction

    // D-cache parameters
    localparam int DCACHE_LINE_BYTES = 64;
    localparam int DCACHE_BEATS      = DCACHE_LINE_BYTES / 8;

    function automatic [63:0] dcache_read_beat(input logic [31:0] line_addr, input logic [2:0] beat);
        int base_word;
        begin
            dcache_read_beat = 64'h0;
            base_word = mem_index(line_addr) + (beat * 2);
            if ((base_word >= 0) && ((base_word + 1) < MEM_WORDS))
                dcache_read_beat = {mem[base_word + 1], mem[base_word + 0]};
        end
    endfunction

    task automatic dcache_write_line(
        input logic [31:0] line_addr,
        input logic [511:0] line_data,
        input logic [7:0]  line_wstrb
    );
        int base_word;
        begin
            base_word = mem_index(line_addr);
            for (int b = 0; b < DCACHE_BEATS; b++) begin
                if (line_wstrb[b]) begin
                    mem[base_word + (b * 2) + 0] = line_data[(b*64) +: 32];
                    mem[base_word + (b * 2) + 1] = line_data[(b*64) + 32 +: 32];
                end
            end
        end
    endtask

    // I-cache / D-cache state per CU
    `define DECL_CACHE_STATE(N) \
        logic        inst_pending_``N; \
        logic [31:0] inst_req_addr_q_``N; \
        logic        dcache_tx_active_``N; \
        logic        dcache_tx_rw_``N; \
        logic [31:0] dcache_tx_addr_``N; \
        logic [7:0]  dcache_tx_id_q_``N; \
        logic [2:0]  dcache_tx_beat_``N; \
        logic [511:0] dcache_tx_wdata_``N; \
        logic [7:0]  dcache_tx_wstrb_``N;

    `DECL_CACHE_STATE(0)
    `DECL_CACHE_STATE(1)

    // ---------------------------------------------------------------------
    // Memory initialization (LUTs + perspective table)
    // ---------------------------------------------------------------------
    task automatic init_memory();
        int i, ti, idx;
        real ang, s_q_r, c_q_r;
        int s_q, c_q;
        begin
            for (i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;

            // Sine/cosine LUT (Q1.15), 256 entries × 8 bytes
            for (ti = 0; ti < 256; ti++) begin
                ang = (6.283185307179586 * ti) / 256.0;
                s_q = $rtoi($sin(ang) * 32767.0);
                c_q = $rtoi($cos(ang) * 32767.0);
                if (s_q < -32768) s_q = -32768;
                if (s_q >  32767) s_q =  32767;
                if (c_q < -32768) c_q = -32768;
                if (c_q >  32767) c_q =  32767;
                mem[mem_index(BASE_ADDR + SINCOS_OFF + (ti * 8) + 0)] = $signed(s_q);
                mem[mem_index(BASE_ADDR + SINCOS_OFF + (ti * 8) + 4)] = $signed(c_q);
            end

            // Inverse sqrt LUT [0..1] in Q16.16, 1024 entries
            for (ti = 0; ti < 1024; ti++) begin
                real x_val, inv;
                x_val = (ti / 1023.0);
                if (x_val < 1e-6) inv = 0.0; else inv = 1.0 / $sqrt(x_val);
                idx = mem_index(BASE_ADDR + INVSQRT_OFF + (ti * 4));
                mem[idx] = $rtoi(inv * 65536.0);
            end

            // Vector constants for mirror tint: (0.80, 0.85, 0.95, 1.00)
            mem[mem_index(BASE_ADDR + VEC_CONST_OFF + 0)]  = 32'h3F4CCCCD;
            mem[mem_index(BASE_ADDR + VEC_CONST_OFF + 4)]  = 32'h3F59999A;
            mem[mem_index(BASE_ADDR + VEC_CONST_OFF + 8)]  = 32'h3F733333;
            mem[mem_index(BASE_ADDR + VEC_CONST_OFF + 12)] = 32'h3F800000;

            // Perspective LUT: for each row y, pz = 0.5 / |yndc(y)| in Q16.16
            // Camera at (0, 0, 0), ground at y = -0.5
            // yndc(y) = -(2*y - 127) * 642 (Q16.16 NDC, invH=642)
            for (ti = 0; ti < H; ti++) begin
                int yndc_raw;
                real yndc_real, t_val;
                yndc_raw = -(2 * ti - (H - 1)) * 642;
                yndc_real = yndc_raw / 65536.0;
                if (yndc_real >= 0.0) begin
                    t_val = 0.0;  // above horizon: no ground
                end else begin
                    t_val = -0.5 / yndc_real;
                    if (t_val > 16.0) t_val = 16.0;  // fog cap
                end
                idx = mem_index(BASE_ADDR + PERSP_OFF + (ti * 4));
                mem[idx] = $rtoi(t_val * 65536.0);
            end

            $display("TB: Memory initialized (sincos, invsqrt, persp, vec_const)");
        end
    endtask

    // ---------------------------------------------------------------------
    // Instruction ROM program
    // Multi-core aware: reads CORE_ID to determine row partition
    // True 3D ray-plane intersection for perspective ground checker
    // 3-level gradient shadow
    // ---------------------------------------------------------------------
    task automatic init_rom();
        int pc;
        // Branch source PCs
        int br_s1_pc, br_s2_pc, br_gnd_pc;
        int jmp_sky_pc;
        int br_fog_pc;
        int br_black_pc, br_hard_pc, br_med_pc;
        int jmp_noshadow_pc, jmp_red_med_pc, jmp_red_hard_pc, jmp_black_pc, jmp_fog_pc;
        int br_s1gnd_pc, jmp_s1sky_pc, br_s1chk_pc, jmp_s1dk_pc;
        int br_spec_pc, jmp_tint_pc;
        int br_s2lit_pc, jmp_s2lit_pc, jmp_s2dk_pc;
        int blt_x_pc, blt_y_pc, blt_frame_pc;
        // Branch target PCs
        int loop_frame_pc, loop_y_pc, loop_x_pc;
        int sphere1_pc, sphere2_pc;
        int ground_pc, fog_sky_pc;
        int ground_black_pc, red_hard_pc, red_med_pc;
        int s1_gnd_pc, s1_lt_pc, spec_pc, skip_spec_pc;
        int s2_dk_pc;
        int write_pc;
        int imm;
        begin
            for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
            pc = 0;

            // =============================================================
            // SETUP — persistent register constants
            // =============================================================
            // x1 = BASE_ADDR (also = 0x80000000 = INT32_MIN — dual purpose)
            rom[pc>>2] = u_type(BASE_ADDR, 5'd1, OP_LUI); pc += 4;
            // x2 = FB base
            rom[pc>>2] = u_type(BASE_ADDR + FB_OFF, 5'd2, OP_LUI); pc += 4;
            // x3 = invW, x4 = invH (Q16.16-style scale)
            rom[pc>>2] = i_type(642, 5'd0, 3'b000, 5'd3, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(642, 5'd0, 3'b000, 5'd4, OP_IMM); pc += 4;
            // x5 = ONE_FP (1.0 Q16.16) — also sphere_world_z for z=1.0
            rom[pc>>2] = u_type(32'h0001_0000, 5'd5, OP_LUI); pc += 4;
            // x17 = proj_r2_sphere1 = 0.36 Q16.16 = 0x5C29
            rom[pc>>2] = u_type(32'h0000_6000, 5'd17, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(-983, 5'd17, 3'b000, 5'd17, OP_IMM); pc += 4;
            // x19 = W, x20 = H
            rom[pc>>2] = i_type(W, 5'd0, 3'b000, 5'd19, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(H, 5'd0, 3'b000, 5'd20, OP_IMM); pc += 4;
            // x21 = sincos base, x22 = invsqrt base
            rom[pc>>2] = u_type(BASE_ADDR + SINCOS_OFF, 5'd21, OP_LUI); pc += 4;
            rom[pc>>2] = u_type(BASE_ADDR + INVSQRT_OFF, 5'd22, OP_LUI); pc += 4;
            // Load mirror tint vector into v2
            rom[pc>>2] = u_type(BASE_ADDR + VEC_CONST_OFF, 5'd7, OP_LUI); pc += 4;
            rom[pc>>2] = vsetvli_inst(5'd0, 5'd0, 11'h010); pc += 4;
            rom[pc>>2] = rvv_vl(5'd2, 5'd7, RVV_VEW32, 1'b1); pc += 4;
            // x13 = FRAMES
            rom[pc>>2] = i_type(FRAMES, 5'd0, 3'b000, 5'd13, OP_IMM); pc += 4;
            // x12 = CORE_ID (read from CSR 0xC00)
            rom[pc>>2] = i_type(12'hC00, 5'd0, 3'b010, 5'd12, OP_SYSTEM); pc += 4;
            // x9 = DONE_ADDR = BASE_ADDR + 0x100 + core_id * 64
            rom[pc>>2] = i_type(6, 5'd12, 3'b001, 5'd7, OP_IMM); pc += 4;  // SLLI x7, x12, 6
            rom[pc>>2] = i_type(256, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4; // ADDI x7, x7, 256
            rom[pc>>2] = r_type(7'b0000000, 5'd7, 5'd1, 3'b000, 5'd9, OP_REG); pc += 4; // ADD x9, x1, x7
            // x6 = frame counter = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd6, OP_IMM); pc += 4;

            // =============================================================
            // FRAME LOOP
            // =============================================================
            loop_frame_pc = pc;
            // angle_idx = frame & 0xFF → x7
            rom[pc>>2] = i_type(255, 5'd6, 3'b111, 5'd7, OP_IMM); pc += 4;
            // sincos ptr = base + idx*8
            rom[pc>>2] = i_type(3, 5'd7, 3'b001, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;
            // sin(angle) → x25 (temp)
            rom[pc>>2] = i_type(0, 5'd7, 3'b010, 5'd25, OP_LOAD); pc += 4;
            // sphere1_world_x = sin << 1 → x18
            rom[pc>>2] = i_type(1, 5'd25, 3'b001, 5'd18, OP_IMM); pc += 4;
            // proj_cx1 = sin (sphere at z=1.0 → projected cx = world_x / 1.0) → x16
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd25, 3'b000, 5'd16, OP_REG); pc += 4;
            // y = core_id (start at row 0 or 1)
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd12, 3'b000, 5'd8, OP_REG); pc += 4;

            // =============================================================
            // Y LOOP
            // =============================================================
            loop_y_pc = pc;
            // yndc = -(2*y - 127) * invH → x10
            rom[pc>>2] = i_type(1, 5'd8, 3'b001, 5'd7, OP_IMM); pc += 4;   // SLLI x7, x8, 1
            rom[pc>>2] = i_type(-127, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4; // ADDI x7, x7, -127
            rom[pc>>2] = r_type(7'b0000001, 5'd4, 5'd7, 3'b000, 5'd10, OP_REG); pc += 4; // MUL x10, x7, x4
            rom[pc>>2] = r_type(7'b0100000, 5'd10, 5'd0, 3'b000, 5'd10, OP_REG); pc += 4; // SUB x10, x0, x10
            // row_ptr = fb_base + (y << 9)
            rom[pc>>2] = i_type(9, 5'd8, 3'b001, 5'd23, OP_IMM); pc += 4; // SLLI x23, x8, 9
            rom[pc>>2] = r_type(7'b0000000, 5'd2, 5'd23, 3'b000, 5'd23, OP_REG); pc += 4; // ADD x23, x23, x2
            // Load perspective scale for this row: persp_scale → x15
            rom[pc>>2] = u_type(BASE_ADDR + PERSP_OFF, 5'd7, OP_LUI); pc += 4;  // x7 = PERSP_BASE
            rom[pc>>2] = i_type(2, 5'd8, 3'b001, 5'd25, OP_IMM); pc += 4;  // SLLI x25, x8, 2
            rom[pc>>2] = r_type(7'b0000000, 5'd25, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4; // ADD x7, x7, x25
            rom[pc>>2] = i_type(0, 5'd7, 3'b010, 5'd15, OP_LOAD); pc += 4; // LW x15, 0(x7)
            // x = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd11, OP_IMM); pc += 4;

            // =============================================================
            // X LOOP (inner pixel)
            // =============================================================
            loop_x_pc = pc;
            // xndc = invW * (2*x - 127) → x24
            rom[pc>>2] = i_type(1, 5'd11, 3'b001, 5'd24, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(-127, 5'd24, 3'b000, 5'd24, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd3, 5'd24, 3'b000, 5'd24, OP_REG); pc += 4;

            // ---- Sphere 1 test (projected circle) ----
            // dx = xndc - proj_cx1
            rom[pc>>2] = r_type(7'b0100000, 5'd16, 5'd24, 3'b000, 5'd25, OP_REG); pc += 4;
            // r2 = (dx>>8)² + (yndc>>8)²
            rom[pc>>2] = i_type(12'h408, 5'd25, 3'b101, 5'd26, OP_IMM); pc += 4; // SRAI x26, x25, 8
            rom[pc>>2] = i_type(12'h408, 5'd10, 3'b101, 5'd27, OP_IMM); pc += 4; // SRAI x27, x10, 8
            rom[pc>>2] = r_type(7'b0000001, 5'd26, 5'd26, 3'b000, 5'd26, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd27, 5'd27, 3'b000, 5'd27, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_REG); pc += 4;
            // BLT r2, r2_sphere1 → sphere1
            br_s1_pc = pc;
            rom[pc>>2] = b_type(0, 5'd26, 5'd17, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // ---- Sphere 2 test (projected circle) ----
            // s2: proj_cx = 0x9000 (0.5625), proj_cy = 0x8000 (0.5), proj_r2 = 0x1000 (0.0625)
            rom[pc>>2] = u_type(32'h0000_9000, 5'd27, OP_LUI); pc += 4;
            rom[pc>>2] = u_type(32'h0000_8000, 5'd28, OP_LUI); pc += 4;
            rom[pc>>2] = u_type(32'h0000_1000, 5'd29, OP_LUI); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd24, 3'b000, 5'd30, OP_REG); pc += 4; // dx2 = xndc - s2_cx
            rom[pc>>2] = r_type(7'b0100000, 5'd28, 5'd10, 3'b000, 5'd31, OP_REG); pc += 4; // dy2 = yndc - s2_cy
            rom[pc>>2] = i_type(12'h408, 5'd30, 3'b101, 5'd7, OP_IMM); pc += 4;  // SRAI dx2>>8
            rom[pc>>2] = i_type(12'h408, 5'd31, 3'b101, 5'd25, OP_IMM); pc += 4; // SRAI dy2>>8
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd25, 3'b000, 5'd25, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd25, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;
            // BLT r2_2, s2_r2 → sphere2
            br_s2_pc = pc;
            rom[pc>>2] = b_type(0, 5'd7, 5'd29, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // ---- Background ----
            // if yndc < 0 → ground
            br_gnd_pc = pc;
            rom[pc>>2] = b_type(0, 5'd10, 5'd0, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Sky: RGB(100,150,230) → ABGR 0xFFE69664
            rom[pc>>2] = u_type(32'hFFE6_9000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h664, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_sky_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // =============================================================
            // GROUND PATH — true 3D ray-plane intersection
            // =============================================================
            ground_pc = pc;

            // Fog check: if persp_scale >= MAX_T (8.0) → fog
            // MAX_T = ONE << 3 = x5 << 3
            rom[pc>>2] = i_type(3, 5'd5, 3'b001, 5'd28, OP_IMM); pc += 4;  // SLLI x28, x5, 3
            br_fog_pc = pc;
            rom[pc>>2] = b_type(0, 5'd15, 5'd28, 3'b101, OP_BRANCH); pc += 4; // BGE x15, x28 → fog
            rom[pc>>2] = nop(); pc += 4;

            // Ground hit point: px = (persp_scale >> 8) * (xndc >> 8) → x28
            rom[pc>>2] = i_type(12'h408, 5'd15, 3'b101, 5'd28, OP_IMM); pc += 4; // SRAI x28, x15, 8
            rom[pc>>2] = i_type(12'h408, 5'd24, 3'b101, 5'd29, OP_IMM); pc += 4; // SRAI x29, x24, 8
            rom[pc>>2] = r_type(7'b0000001, 5'd29, 5'd28, 3'b000, 5'd28, OP_REG); pc += 4; // MUL x28 = px

            // Checker: ((px >> 14) ^ (pz >> 14) + frame) & 1
            // pz = persp_scale = x15
            rom[pc>>2] = i_type(12'h40E, 5'd28, 3'b101, 5'd29, OP_IMM); pc += 4; // SRAI x29, x28, 14 = floor(4*px)
            rom[pc>>2] = i_type(12'h40E, 5'd15, 3'b101, 5'd30, OP_IMM); pc += 4; // SRAI x30, x15, 14 = floor(4*pz)
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd29, 3'b100, 5'd29, OP_REG); pc += 4; // XOR x29
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd29, 3'b000, 5'd29, OP_REG); pc += 4;  // ADD x29 += frame
            rom[pc>>2] = i_type(1, 5'd29, 3'b111, 5'd29, OP_IMM); pc += 4; // ANDI checker & 1

            // Shadow: sphere 1 projected on ground
            // sdx = px - sphere1_world_x (x18)
            rom[pc>>2] = r_type(7'b0100000, 5'd18, 5'd28, 3'b000, 5'd30, OP_REG); pc += 4; // SUB x30 = px - x18
            // sdz = pz - sphere_world_z = persp_scale - ONE (x15 - x5)
            rom[pc>>2] = r_type(7'b0100000, 5'd5, 5'd15, 3'b000, 5'd31, OP_REG); pc += 4; // SUB x31 = x15 - x5
            // sdist2 = (sdx>>8)² + (sdz>>8)²
            rom[pc>>2] = i_type(12'h408, 5'd30, 3'b101, 5'd30, OP_IMM); pc += 4; // SRAI x30 >>= 8
            rom[pc>>2] = i_type(12'h408, 5'd31, 3'b101, 5'd31, OP_IMM); pc += 4; // SRAI x31 >>= 8
            rom[pc>>2] = r_type(7'b0000001, 5'd30, 5'd30, 3'b000, 5'd30, OP_REG); pc += 4; // MUL
            rom[pc>>2] = r_type(7'b0000001, 5'd31, 5'd31, 3'b000, 5'd31, OP_REG); pc += 4; // MUL
            rom[pc>>2] = r_type(7'b0000000, 5'd31, 5'd30, 3'b000, 5'd30, OP_REG); pc += 4; // ADD x30 = sdist2

            // Color selection: checker → shadow level → color
            // if checker == 0 → black
            br_black_pc = pc;
            rom[pc>>2] = b_type(0, 5'd29, 5'd0, 3'b000, OP_BRANCH); pc += 4; // BEQ x29, x0 → black
            rom[pc>>2] = nop(); pc += 4;

            // Checker == 1 (red). Shadow levels using r2_sphere1 (x17) as base:
            // Level 0: sdist2 < r2_sphere → hard shadow
            br_hard_pc = pc;
            rom[pc>>2] = b_type(0, 5'd30, 5'd17, 3'b100, OP_BRANCH); pc += 4; // BLT x30, x17 → hard
            rom[pc>>2] = nop(); pc += 4;

            // Level 1: sdist2 < 2*r2_sphere → medium shadow
            rom[pc>>2] = i_type(1, 5'd17, 3'b001, 5'd31, OP_IMM); pc += 4; // SLLI x31, x17, 1
            br_med_pc = pc;
            rom[pc>>2] = b_type(0, 5'd30, 5'd31, 3'b100, OP_BRANCH); pc += 4; // BLT x30, x31 → med
            rom[pc>>2] = nop(); pc += 4;

            // No shadow: full red = 0xFF0000FF
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0FF, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_noshadow_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Medium shadow: RGB(160,0,0) = 0xFF0000A0
            red_med_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0A0, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_red_med_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Hard shadow: RGB(80,0,0) = 0xFF000050
            red_hard_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h050, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_red_hard_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Black square: 0xFF000000
            ground_black_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            jmp_black_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Fog (horizon fade): same as sky
            fog_sky_pc = pc;
            rom[pc>>2] = u_type(32'hFFE6_9000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h664, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_fog_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // =============================================================
            // SPHERE 1 PATH (mirror with specular)
            // =============================================================
            sphere1_pc = pc;
            // z2 = r2_sphere - r2 → x28
            rom[pc>>2] = r_type(7'b0100000, 5'd26, 5'd17, 3'b000, 5'd28, OP_REG); pc += 4;
            // invsqrt LUT: idx = z2>>6, word offset
            rom[pc>>2] = i_type(6, 5'd28, 3'b101, 5'd29, OP_IMM); pc += 4;  // SRLI z2>>6
            rom[pc>>2] = i_type(2, 5'd29, 3'b001, 5'd29, OP_IMM); pc += 4;  // SLLI *4
            rom[pc>>2] = r_type(7'b0000000, 5'd22, 5'd29, 3'b000, 5'd29, OP_REG); pc += 4; // ADD base
            rom[pc>>2] = i_type(0, 5'd29, 3'b010, 5'd30, OP_LOAD); pc += 4; // LW inv
            // z = (z2 * inv) >> 16
            rom[pc>>2] = r_type(7'b0000001, 5'd30, 5'd28, 3'b000, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd29, 3'b101, 5'd29, OP_IMM); pc += 4; // SRAI >>16
            // inv_radius = 427 (Q16.8, 1/0.6 ≈ 1.667)
            rom[pc>>2] = i_type(427, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // nx = (inv_r * dx) >> 8 → x14 (dx in x25)
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd25, 3'b000, 5'd14, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd14, 3'b101, 5'd14, OP_IMM); pc += 4;
            // ny = (inv_r * yndc) >> 8 → x24 (using x24 instead of x15 to preserve persp_scale)
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd10, 3'b000, 5'd24, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd24, 3'b101, 5'd24, OP_IMM); pc += 4;
            // nz = (inv_r * z) >> 8 → x29
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd29, 3'b000, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd29, 3'b101, 5'd29, OP_IMM); pc += 4;
            // tmp = 2*nz → x30
            rom[pc>>2] = i_type(1, 5'd29, 3'b001, 5'd30, OP_IMM); pc += 4;
            // r.x = -(2*nz*nx) >> 16 → x26
            rom[pc>>2] = r_type(7'b0000001, 5'd14, 5'd30, 3'b000, 5'd26, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd26, 3'b101, 5'd26, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd26, 5'd0, 3'b000, 5'd26, OP_REG); pc += 4;
            // r.y = -(2*nz*ny) >> 16 → x27
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd30, 3'b000, 5'd27, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd27, 3'b101, 5'd27, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd0, 3'b000, 5'd27, OP_REG); pc += 4;
            // r.z = one - (2*nz*nz) >> 16 → x30
            rom[pc>>2] = r_type(7'b0000001, 5'd29, 5'd30, 3'b000, 5'd30, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd30, 3'b101, 5'd30, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd30, 5'd5, 3'b000, 5'd30, OP_REG); pc += 4;

            // if r.y < 0 → reflected ground
            br_s1gnd_pc = pc;
            rom[pc>>2] = b_type(0, 5'd27, 5'd0, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Reflected sky
            rom[pc>>2] = u_type(32'hFFE6_9000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h664, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s1sky_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Reflected ground checker: (r.x>>14 ^ r.z>>14 + frame) & 1
            s1_gnd_pc = pc;
            rom[pc>>2] = i_type(12'h40E, 5'd26, 3'b101, 5'd28, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(12'h40E, 5'd30, 3'b101, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd29, 5'd28, 3'b100, 5'd28, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd28, 3'b000, 5'd28, OP_REG); pc += 4;
            rom[pc>>2] = i_type(1, 5'd28, 3'b111, 5'd28, OP_IMM); pc += 4;
            br_s1chk_pc = pc;
            rom[pc>>2] = b_type(0, 5'd28, 5'd0, 3'b001, OP_BRANCH); pc += 4; // BNE → light
            rom[pc>>2] = nop(); pc += 4;
            // Dark (black)
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s1dk_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // Light (red): 0xFF0000FF
            s1_lt_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0FF, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            // fall through to specular

            // ---- Specular check ----
            spec_pc = pc;
            // ny (in x24) >> 8 > threshold → white specular
            rom[pc>>2] = i_type(12'h408, 5'd24, 3'b101, 5'd28, OP_IMM); pc += 4; // SRAI x28, x24, 8
            rom[pc>>2] = i_type(156, 5'd0, 3'b000, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            br_spec_pc = pc;
            rom[pc>>2] = b_type(0, 5'd28, 5'd29, 3'b100, OP_BRANCH); pc += 4; // BLT → skip
            rom[pc>>2] = nop(); pc += 4;
            // Specular: white
            rom[pc>>2] = i_type(-1, 5'd0, 3'b000, 5'd31, OP_IMM); pc += 4;
            skip_spec_pc = pc;

            // ---- Mirror tint ----
            rom[pc>>2] = r_type(7'b0011110, 5'd31, 5'd0, 3'b011, 5'd1, OP_CUSTOM1); pc += 4; // VUNPACK
            rom[pc>>2] = r_type(7'b0011000, 5'd2, 5'd1, 3'b011, 5'd1, OP_CUSTOM1); pc += 4;  // VMUL
            rom[pc>>2] = r_type(7'b0010000, 5'd0, 5'd1, 3'b011, 5'd31, OP_CUSTOM1); pc += 4; // VPACK
            jmp_tint_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // =============================================================
            // SPHERE 2 PATH (diffuse green)
            // =============================================================
            sphere2_pc = pc;
            // dy2 still in x31 from sphere2 test
            br_s2lit_pc = pc;
            rom[pc>>2] = b_type(0, 5'd31, 5'd0, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // Lit: green RGB(60,180,60) → ABGR 0xFF3CB43C
            rom[pc>>2] = u_type(32'hFF3C_B000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h43C, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s2lit_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // Dark: dark green RGB(15,50,15) → ABGR 0xFF0F320F
            s2_dk_pc = pc;
            rom[pc>>2] = u_type(32'hFF0F_3000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h20F, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s2dk_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // =============================================================
            // WRITE PIXEL
            // =============================================================
            write_pc = pc;
            // addr = row_ptr + (x << 2)
            rom[pc>>2] = i_type(2, 5'd11, 3'b001, 5'd28, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd23, 3'b000, 5'd28, OP_REG); pc += 4;
            rom[pc>>2] = s_type(0, 5'd28, 5'd31, 3'b010, OP_STORE); pc += 4;
            // x++
            rom[pc>>2] = i_type(1, 5'd11, 3'b000, 5'd11, OP_IMM); pc += 4;
            blt_x_pc = pc;
            rom[pc>>2] = b_type(0, 5'd11, 5'd19, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // y += NUM_CORES (stride = 2)
            rom[pc>>2] = i_type(NUM_CORES, 5'd8, 3'b000, 5'd8, OP_IMM); pc += 4;
            blt_y_pc = pc;
            rom[pc>>2] = b_type(0, 5'd8, 5'd20, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // Frame done: FENCE, write DONE, frame++
            rom[pc>>2] = i_type(12'h0FF, 5'd0, 3'b000, 5'd0, OP_FENCE); pc += 4;
            rom[pc>>2] = i_type(1, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;   // x7 = 1
            rom[pc>>2] = s_type(0, 5'd9, 5'd7, 3'b010, OP_STORE); pc += 4;  // SW x7, 0(x9)
            rom[pc>>2] = i_type(12'h0FF, 5'd0, 3'b000, 5'd0, OP_FENCE); pc += 4; // FENCE
            // frame++
            rom[pc>>2] = i_type(1, 5'd6, 3'b000, 5'd6, OP_IMM); pc += 4;
            blt_frame_pc = pc;
            rom[pc>>2] = b_type(0, 5'd6, 5'd13, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // WFI + backwards branch loop
            rom[pc>>2] = i_type(12'h105, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // WFI
            rom[pc>>2] = b_type(-4, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // BEQ x0,x0,-4
            rom[pc>>2] = nop(); pc += 4;

            // =============================================================
            // PATCH BRANCHES
            // =============================================================
            // Sphere 1 hit
            imm = sphere1_pc - br_s1_pc;
            rom[br_s1_pc>>2] = b_type(imm, 5'd26, 5'd17, 3'b100, OP_BRANCH);
            // Sphere 2 hit
            imm = sphere2_pc - br_s2_pc;
            rom[br_s2_pc>>2] = b_type(imm, 5'd7, 5'd29, 3'b100, OP_BRANCH);
            // Ground
            imm = ground_pc - br_gnd_pc;
            rom[br_gnd_pc>>2] = b_type(imm, 5'd10, 5'd0, 3'b100, OP_BRANCH);
            // Sky → write
            imm = write_pc - jmp_sky_pc;
            rom[jmp_sky_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Fog
            imm = fog_sky_pc - br_fog_pc;
            rom[br_fog_pc>>2] = b_type(imm, 5'd15, 5'd28, 3'b101, OP_BRANCH);
            // Ground black
            imm = ground_black_pc - br_black_pc;
            rom[br_black_pc>>2] = b_type(imm, 5'd29, 5'd0, 3'b000, OP_BRANCH);
            // Hard shadow
            imm = red_hard_pc - br_hard_pc;
            rom[br_hard_pc>>2] = b_type(imm, 5'd30, 5'd17, 3'b100, OP_BRANCH);
            // Medium shadow
            imm = red_med_pc - br_med_pc;
            rom[br_med_pc>>2] = b_type(imm, 5'd30, 5'd31, 3'b100, OP_BRANCH);
            // No shadow → write
            imm = write_pc - jmp_noshadow_pc;
            rom[jmp_noshadow_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Med shadow → write
            imm = write_pc - jmp_red_med_pc;
            rom[jmp_red_med_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Hard shadow → write
            imm = write_pc - jmp_red_hard_pc;
            rom[jmp_red_hard_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Black → write
            imm = write_pc - jmp_black_pc;
            rom[jmp_black_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Fog → write
            imm = write_pc - jmp_fog_pc;
            rom[jmp_fog_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Sphere 1 reflected ground
            imm = s1_gnd_pc - br_s1gnd_pc;
            rom[br_s1gnd_pc>>2] = b_type(imm, 5'd27, 5'd0, 3'b100, OP_BRANCH);
            // Sphere 1 reflected sky → spec
            imm = spec_pc - jmp_s1sky_pc;
            rom[jmp_s1sky_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Sphere 1 reflected checker → light
            imm = s1_lt_pc - br_s1chk_pc;
            rom[br_s1chk_pc>>2] = b_type(imm, 5'd28, 5'd0, 3'b001, OP_BRANCH);
            // Sphere 1 dark → spec
            imm = spec_pc - jmp_s1dk_pc;
            rom[jmp_s1dk_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Specular skip
            imm = skip_spec_pc - br_spec_pc;
            rom[br_spec_pc>>2] = b_type(imm, 5'd28, 5'd29, 3'b100, OP_BRANCH);
            // After tint → write
            imm = write_pc - jmp_tint_pc;
            rom[jmp_tint_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Sphere 2 lit → dark
            imm = s2_dk_pc - br_s2lit_pc;
            rom[br_s2lit_pc>>2] = b_type(imm, 5'd31, 5'd0, 3'b100, OP_BRANCH);
            // Sphere 2 lit → write
            imm = write_pc - jmp_s2lit_pc;
            rom[jmp_s2lit_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Sphere 2 dark → write
            imm = write_pc - jmp_s2dk_pc;
            rom[jmp_s2dk_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // X loop
            imm = loop_x_pc - blt_x_pc;
            rom[blt_x_pc>>2] = b_type(imm, 5'd11, 5'd19, 3'b100, OP_BRANCH);
            // Y loop
            imm = loop_y_pc - blt_y_pc;
            rom[blt_y_pc>>2] = b_type(imm, 5'd8, 5'd20, 3'b100, OP_BRANCH);
            // Frame loop
            imm = loop_frame_pc - blt_frame_pc;
            rom[blt_frame_pc>>2] = b_type(imm, 5'd6, 5'd13, 3'b100, OP_BRANCH);

            $display("TB: ROM program loaded (%0d bytes, multi-core 3D RT)", pc);
        end
    endtask

    // ---------------------------------------------------------------------
    // Framebuffer dump
    // ---------------------------------------------------------------------
    task automatic dump_fb_ppm(input int frame_no);
        int y, x, idx;
        logic [31:0] pix;
        logic [7:0] r, g, b;
        string fname;
        int fh;
        begin
            fname = $sformatf("rt_frame_%0d.ppm", frame_no);
            fh = $fopen(fname, "w");
            if (fh == 0) begin
                $display("TB WARN: could not open %s for write", fname);
                return;
            end
            $fwrite(fh, "P3\n%0d %0d\n255\n", W, H);
            for (y = 0; y < H; y++) begin
                for (x = 0; x < W; x++) begin
                    idx = mem_index(BASE_ADDR + FB_OFF + (y * (W*4)) + (x*4));
                    pix = mem[idx];
                    r = pix[7:0];
                    g = pix[15:8];
                    b = pix[23:16];
                    $fwrite(fh, "%0d %0d %0d\n", r, g, b);
                end
            end
            $fclose(fh);
            $display("TB: wrote %s", fname);
        end
    endtask

    // ---------------------------------------------------------------------
    // I-cache + D-cache responders + DONE monitoring
    // Both CUs handled in one always_ff for coherent shared memory access
    // ---------------------------------------------------------------------
    int frame_seen;
    longint unsigned cycle_count;
    localparam longint unsigned MAX_CYCLES = 300_000_000;

    // DONE cache-line-aligned addresses (for matching write-throughs)
    localparam logic [31:0] DONE_CU0_ADDR = BASE_ADDR + DONE_CU0_OFF;
    localparam logic [31:0] DONE_CU1_ADDR = BASE_ADDR + DONE_CU1_OFF;
    // Word index within line for extracting DONE value
    localparam int DONE_CU0_WORD_IN_LINE = (DONE_CU0_OFF[5:0]) >> 2;  // 0
    localparam int DONE_CU1_WORD_IN_LINE = (DONE_CU1_OFF[5:0]) >> 2;  // 0
    // Per-CU done flags (set in write-through handler, cleared after frame dump)
    logic cu0_done_flag, cu1_done_flag;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            // CU0 I-cache
            inst_miss_resp_valid_0 <= 1'b0;
            inst_miss_resp_data_0  <= 64'h0;
            inst_pending_0         <= 1'b0;
            inst_req_addr_q_0      <= 32'h0;
            // CU0 D-cache
            dcache_mem_resp_valid_0 <= 1'b0;
            dcache_mem_resp_data_0  <= 64'h0;
            dcache_mem_resp_id_0    <= 8'h0;
            dcache_tx_active_0      <= 1'b0;
            dcache_tx_rw_0          <= 1'b0;
            dcache_tx_addr_0        <= 32'h0;
            dcache_tx_id_q_0        <= 8'h0;
            dcache_tx_beat_0        <= 3'd0;
            dcache_tx_wdata_0       <= '0;
            dcache_tx_wstrb_0       <= '0;
            // CU1 I-cache
            inst_miss_resp_valid_1 <= 1'b0;
            inst_miss_resp_data_1  <= 64'h0;
            inst_pending_1         <= 1'b0;
            inst_req_addr_q_1      <= 32'h0;
            // CU1 D-cache
            dcache_mem_resp_valid_1 <= 1'b0;
            dcache_mem_resp_data_1  <= 64'h0;
            dcache_mem_resp_id_1    <= 8'h0;
            dcache_tx_active_1      <= 1'b0;
            dcache_tx_rw_1          <= 1'b0;
            dcache_tx_addr_1        <= 32'h0;
            dcache_tx_id_q_1        <= 8'h0;
            dcache_tx_beat_1        <= 3'd0;
            dcache_tx_wdata_1       <= '0;
            dcache_tx_wstrb_1       <= '0;

            frame_seen  <= 0;
            cycle_count <= 0;
            cu0_done_flag <= 1'b0;
            cu1_done_flag <= 1'b0;
        end else begin
            cycle_count <= cycle_count + 1;

            // Heartbeat
            if ((cycle_count != 0) && ((cycle_count % 5_000_000) == 0)) begin
                $display("TB: cyc=%0d  CU0 pc=%08h if_v=%0d stall=%0d sb=%08h lsu=%0d dc=%0d  CU1 pc=%08h if_v=%0d stall=%0d sb=%08h lsu=%0d dc=%0d",
                    cycle_count,
                    cu0.if_pc, cu0.if_valid,
                    cu0.u_scoreboard.stall0, cu0.u_scoreboard.busy_s,
                    cu0.lsu_busy, cu0.u_dcache.mem_busy,
                    cu1.if_pc, cu1.if_valid,
                    cu1.u_scoreboard.stall0, cu1.u_scoreboard.busy_s,
                    cu1.lsu_busy, cu1.u_dcache.mem_busy);
            end

            // Timeout
            if (cycle_count >= MAX_CYCLES) begin
                $display("TB: TIMEOUT at cycle %0d  CU0 pc=%08h  CU1 pc=%08h",
                         cycle_count, cu0.if_pc, cu1.if_pc);
                $fatal(1);
            end

            // ==========================================================
            // CU0 I-cache miss responder
            // ==========================================================
            inst_miss_resp_valid_0 <= 1'b0;
            if (inst_miss_req_valid_0 && inst_miss_req_ready_0 && !inst_pending_0) begin
                inst_pending_0    <= 1'b1;
                inst_req_addr_q_0 <= {inst_miss_req_addr_0[31:3], 3'b000};
            end
            if (inst_pending_0) begin
                int widx;
                widx = int'(inst_req_addr_q_0 >> 2);
                if ((widx < 0) || (widx + 1 >= ROM_WORDS)) begin
                    $display("TB: CU0 OOB IROM addr=%08h", inst_req_addr_q_0);
                    $fatal(1);
                end
                inst_miss_resp_valid_0 <= 1'b1;
                inst_miss_resp_data_0  <= {rom[widx + 1], rom[widx]};
                inst_pending_0 <= 1'b0;
            end

            // ==========================================================
            // CU1 I-cache miss responder
            // ==========================================================
            inst_miss_resp_valid_1 <= 1'b0;
            if (inst_miss_req_valid_1 && inst_miss_req_ready_1 && !inst_pending_1) begin
                inst_pending_1    <= 1'b1;
                inst_req_addr_q_1 <= {inst_miss_req_addr_1[31:3], 3'b000};
            end
            if (inst_pending_1) begin
                int widx;
                widx = int'(inst_req_addr_q_1 >> 2);
                if ((widx < 0) || (widx + 1 >= ROM_WORDS)) begin
                    $display("TB: CU1 OOB IROM addr=%08h", inst_req_addr_q_1);
                    $fatal(1);
                end
                inst_miss_resp_valid_1 <= 1'b1;
                inst_miss_resp_data_1  <= {rom[widx + 1], rom[widx]};
                inst_pending_1 <= 1'b0;
            end

            // ==========================================================
            // CU0 D-cache responder
            // ==========================================================
            dcache_mem_resp_valid_0 <= 1'b0;
            if (dcache_tx_active_0) begin
                if (!dcache_tx_rw_0) begin
                    dcache_mem_resp_valid_0 <= 1'b1;
                    dcache_mem_resp_data_0  <= dcache_read_beat(dcache_tx_addr_0, dcache_tx_beat_0);
                    dcache_mem_resp_id_0    <= dcache_tx_id_q_0;
                    if (dcache_tx_beat_0 == (DCACHE_BEATS - 1)) begin
                        dcache_tx_active_0 <= 1'b0;
                        dcache_tx_beat_0   <= 3'd0;
                    end else begin
                        dcache_tx_beat_0 <= dcache_tx_beat_0 + 1'b1;
                    end
                end else begin
                    dcache_mem_resp_valid_0 <= 1'b1;
                    dcache_mem_resp_data_0  <= 64'h0;
                    dcache_mem_resp_id_0    <= dcache_tx_id_q_0;
                    dcache_tx_active_0 <= 1'b0;
                    dcache_tx_beat_0   <= 3'd0;
                    dcache_write_line(dcache_tx_addr_0, dcache_tx_wdata_0, dcache_tx_wstrb_0);
                    // Detect CU0 DONE write-through (same pattern as working single-core TB)
                    if ((dcache_tx_addr_0 <= (BASE_ADDR + DONE_CU0_OFF)) &&
                        ((dcache_tx_addr_0 + 32'd64) > (BASE_ADDR + DONE_CU0_OFF))) begin
                        if (dcache_tx_wdata_0[DONE_CU0_WORD_IN_LINE*32 +: 32] != 32'h0) begin
                            $display("TB: CU0 DONE detected via WT addr=%08h val=%08h cyc=%0d",
                                dcache_tx_addr_0,
                                dcache_tx_wdata_0[DONE_CU0_WORD_IN_LINE*32 +: 32],
                                cycle_count);
                            cu0_done_flag <= 1'b1;
                        end
                    end
                    // Also detect CU1 DONE if CU0 happens to write-through that line
                    if ((dcache_tx_addr_0 <= (BASE_ADDR + DONE_CU1_OFF)) &&
                        ((dcache_tx_addr_0 + 32'd64) > (BASE_ADDR + DONE_CU1_OFF))) begin
                        if (dcache_tx_wdata_0[DONE_CU1_WORD_IN_LINE*32 +: 32] != 32'h0) begin
                            cu1_done_flag <= 1'b1;
                        end
                    end
                end
            end else if (dcache_mem_req_valid_0 && dcache_mem_req_ready_0) begin
                dcache_tx_active_0 <= 1'b1;
                dcache_tx_rw_0     <= dcache_mem_req_rw_0;
                dcache_tx_addr_0   <= {dcache_mem_req_addr_0[31:6], 6'b0};
                dcache_tx_id_q_0   <= dcache_mem_req_id_0;
                dcache_tx_beat_0   <= 3'd0;
                dcache_tx_wdata_0  <= dcache_mem_req_wdata_0;
                dcache_tx_wstrb_0  <= dcache_mem_req_wstrb_0;
            end

            // ==========================================================
            // CU1 D-cache responder
            // ==========================================================
            dcache_mem_resp_valid_1 <= 1'b0;
            if (dcache_tx_active_1) begin
                if (!dcache_tx_rw_1) begin
                    dcache_mem_resp_valid_1 <= 1'b1;
                    dcache_mem_resp_data_1  <= dcache_read_beat(dcache_tx_addr_1, dcache_tx_beat_1);
                    dcache_mem_resp_id_1    <= dcache_tx_id_q_1;
                    if (dcache_tx_beat_1 == (DCACHE_BEATS - 1)) begin
                        dcache_tx_active_1 <= 1'b0;
                        dcache_tx_beat_1   <= 3'd0;
                    end else begin
                        dcache_tx_beat_1 <= dcache_tx_beat_1 + 1'b1;
                    end
                end else begin
                    dcache_mem_resp_valid_1 <= 1'b1;
                    dcache_mem_resp_data_1  <= 64'h0;
                    dcache_mem_resp_id_1    <= dcache_tx_id_q_1;
                    dcache_tx_active_1 <= 1'b0;
                    dcache_tx_beat_1   <= 3'd0;
                    dcache_write_line(dcache_tx_addr_1, dcache_tx_wdata_1, dcache_tx_wstrb_1);
                    // Detect CU0 DONE write-through from CU1
                    if ((dcache_tx_addr_1 <= (BASE_ADDR + DONE_CU0_OFF)) &&
                        ((dcache_tx_addr_1 + 32'd64) > (BASE_ADDR + DONE_CU0_OFF))) begin
                        if (dcache_tx_wdata_1[DONE_CU0_WORD_IN_LINE*32 +: 32] != 32'h0) begin
                            cu0_done_flag <= 1'b1;
                        end
                    end
                    // Detect CU1 DONE write-through
                    if ((dcache_tx_addr_1 <= (BASE_ADDR + DONE_CU1_OFF)) &&
                        ((dcache_tx_addr_1 + 32'd64) > (BASE_ADDR + DONE_CU1_OFF))) begin
                        if (dcache_tx_wdata_1[DONE_CU1_WORD_IN_LINE*32 +: 32] != 32'h0) begin
                            $display("TB: CU1 DONE detected via WT addr=%08h val=%08h cyc=%0d",
                                dcache_tx_addr_1,
                                dcache_tx_wdata_1[DONE_CU1_WORD_IN_LINE*32 +: 32],
                                cycle_count);
                            cu1_done_flag <= 1'b1;
                        end
                    end
                end
            end else if (dcache_mem_req_valid_1 && dcache_mem_req_ready_1) begin
                dcache_tx_active_1 <= 1'b1;
                dcache_tx_rw_1     <= dcache_mem_req_rw_1;
                dcache_tx_addr_1   <= {dcache_mem_req_addr_1[31:6], 6'b0};
                dcache_tx_id_q_1   <= dcache_mem_req_id_1;
                dcache_tx_beat_1   <= 3'd0;
                dcache_tx_wdata_1  <= dcache_mem_req_wdata_1;
                dcache_tx_wstrb_1  <= dcache_mem_req_wstrb_1;
            end

            // ==========================================================
            // DONE flag monitoring (uses per-CU flags set in WT handler)
            // ==========================================================
            if (cu0_done_flag && cu1_done_flag) begin
                $display("TB: Both CUs DONE, dumping frame %0d at cycle %0d",
                         frame_seen, cycle_count);
                if (frame_seen < FRAMES) begin
                    dump_fb_ppm(frame_seen);
                    if (frame_seen == (FRAMES - 1)) begin
                        $display("TB: reached %0d frames, finishing.", FRAMES);
                        $finish;
                    end
                    frame_seen <= frame_seen + 1;
                end
                cu0_done_flag <= 1'b0;
                cu1_done_flag <= 1'b0;
            end
        end
    end

    // ---------------------------------------------------------------------
    // Init
    // ---------------------------------------------------------------------
    initial begin
        init_memory();
        init_rom();
    end

endmodule
