`timescale 1ns/1ps

// Software ray-tracing testbench (compute unit renders)
// - TB only initializes LUTs
// - CU program renders into framebuffer and signals DONE per frame
// - TB dumps framebuffer to PPM per frame
module gfx_sw_rt_tb;
    import isa_pkg::*;

    // ---------------------------------------------------------------------
    // Tunables
    // ---------------------------------------------------------------------
    localparam int W = 128;
    localparam int H = 128;
    localparam int FRAMES = 3;

    localparam logic [31:0] BASE_ADDR = 32'h8000_0000;

    localparam int FB_SIZE_BYTES = W * H * 4;
    localparam int FB_SIZE_WORDS = FB_SIZE_BYTES / 4;

    // Layout inside global window
    localparam logic [31:0] DONE_OFF         = 32'h0000_01F0;
    localparam logic [31:0] FB_OFF           = 32'h0001_0000;
    localparam logic [31:0] SINCOS_OFF      = 32'h0000_2000; // 256 * 8B
    localparam logic [31:0] INVSQRT_OFF     = 32'h0000_3000; // 1024 * 4B
    localparam logic [31:0] VEC_CONST_OFF   = 32'h0000_4000; // Vector constants (ARGB tint, etc.)
    localparam logic [31:0] SPHERE2_OFF     = 32'h0000_5000; // Second sphere params

    // ---------------------------------------------------------------------
    // Clock/reset
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
    // DUT interfaces
    // ---------------------------------------------------------------------
    // Instruction miss interface (I-cache -> memory)
    logic        inst_miss_req_valid;
    logic [31:0] inst_miss_req_addr;
    logic        inst_miss_req_ready;
    logic        inst_miss_resp_valid;
    logic [63:0] inst_miss_resp_data;

    // Legacy data interface (unused)
    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;
    logic        data_req_ready;
    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    // D-cache memory interface (L1 -> memory)
    logic        dcache_mem_req_valid;
    logic        dcache_mem_req_rw;
    logic [31:0] dcache_mem_req_addr;
    logic [7:0]  dcache_mem_req_size;
    logic [3:0]  dcache_mem_req_qos;
    logic [7:0]  dcache_mem_req_id;
    logic [511:0] dcache_mem_req_wdata;
    logic [7:0]  dcache_mem_req_wstrb;
    logic        dcache_mem_req_ready;
    logic        dcache_mem_resp_valid;
    logic [63:0] dcache_mem_resp_data;
    logic [7:0]  dcache_mem_resp_id;

    // Framebuffer AXI (unused)
    logic        fb_aw_valid;
    logic [31:0] fb_aw_addr;
    logic [7:0]  fb_aw_len;
    logic [2:0]  fb_aw_size;
    logic [1:0]  fb_aw_burst;
    logic        fb_aw_ready;
    logic [31:0] fb_w_data;
    logic [3:0]  fb_w_strb;
    logic        fb_w_last;
    logic        fb_w_valid;
    logic        fb_w_ready;
    logic        fb_b_valid;
    logic        fb_b_ready;

    // Mailbox (tied off)
    import mailbox_pkg::*;
    mailbox_pkg::mailbox_flit_t mailbox_tx_data;
    mailbox_pkg::mailbox_flit_t mailbox_rx_data;
    logic mailbox_tx_valid;
    logic mailbox_tx_ready;
    logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_tx_dest_id;
    logic mailbox_rx_valid;
    logic mailbox_rx_ready;
    logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_rx_dest_id;

    logic err_fp_overflow;
    logic err_fp_invalid;
    logic err_vec_overflow;
    logic err_vec_invalid;

    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    compute_unit_top dut (
        .clk(clk),
        .rst_n(rst_n),
        .inst_miss_req_valid(inst_miss_req_valid),
        .inst_miss_req_addr(inst_miss_req_addr),
        .inst_miss_req_ready(inst_miss_req_ready),
        .inst_miss_resp_valid(inst_miss_resp_valid),
        .inst_miss_resp_data(inst_miss_resp_data),
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
        .data_resp_data(data_resp_data),
        .dcache_mem_req_valid(dcache_mem_req_valid),
        .dcache_mem_req_rw(dcache_mem_req_rw),
        .dcache_mem_req_addr(dcache_mem_req_addr),
        .dcache_mem_req_size(dcache_mem_req_size),
        .dcache_mem_req_qos(dcache_mem_req_qos),
        .dcache_mem_req_id(dcache_mem_req_id),
        .dcache_mem_req_wdata(dcache_mem_req_wdata),
        .dcache_mem_req_wstrb(dcache_mem_req_wstrb),
        .dcache_mem_req_ready(dcache_mem_req_ready),
        .dcache_mem_resp_valid(dcache_mem_resp_valid),
        .dcache_mem_resp_data(dcache_mem_resp_data),
        .dcache_mem_resp_id(dcache_mem_resp_id),
        .fb_aw_valid(fb_aw_valid),
        .fb_aw_addr(fb_aw_addr),
        .fb_aw_len(fb_aw_len),
        .fb_aw_size(fb_aw_size),
        .fb_aw_burst(fb_aw_burst),
        .fb_aw_ready(fb_aw_ready),
        .fb_w_data(fb_w_data),
        .fb_w_strb(fb_w_strb),
        .fb_w_last(fb_w_last),
        .fb_w_valid(fb_w_valid),
        .fb_w_ready(fb_w_ready),
        .fb_b_valid(fb_b_valid),
        .fb_b_ready(fb_b_ready),
        .mailbox_tx_valid(mailbox_tx_valid),
        .mailbox_tx_ready(mailbox_tx_ready),
        .mailbox_tx_data(mailbox_tx_data),
        .mailbox_tx_dest_id(mailbox_tx_dest_id),
        .mailbox_rx_valid(mailbox_rx_valid),
        .mailbox_rx_ready(mailbox_rx_ready),
        .mailbox_rx_data(mailbox_rx_data),
        .mailbox_rx_dest_id(mailbox_rx_dest_id)
    );

    // ---------------------------------------------------------------------
    // Wave dump (optional)
    // ---------------------------------------------------------------------
    initial begin
         $dumpfile("gfx_sw_rt_tb.vcd");
         $dumpvars(0, gfx_sw_rt_tb);
    end

    // ---------------------------------------------------------------------
    // Mini assembler helpers
    // ---------------------------------------------------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] u_type(input integer imm, input [4:0] rd, input [6:0] opcode);
        u_type = {imm[31:12], rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] b_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        b_type = {imm[12], imm[10:5], rs2, rs1, funct3, imm[4:1], imm[11], opcode};
    endfunction

    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_IMM);
    endfunction

    // --- RVV encoding helpers ---
    function automatic [31:0] rvv_vl(input [4:0] vd, input [4:0] rs1, input [2:0] width, input vm);
        rvv_vl = {3'b000, 1'b0, 2'b00, vm, 5'b00000, rs1, width, vd, 7'b0000111};
    endfunction
    function automatic [31:0] vsetvli_inst(input [4:0] rd, input [4:0] rs1, input [10:0] zimm);
        vsetvli_inst = {1'b0, zimm, rs1, 3'b111, rd, 7'b1010111};
    endfunction

    // ---------------------------------------------------------------------
    // Instruction ROM (dual-fetch 64b)
    // ---------------------------------------------------------------------
    localparam int ROM_WORDS = 4096;
    logic [31:0] rom [0:ROM_WORDS-1];

    // I-cache miss handler (1-cycle latency)
    logic        inst_pending;
    logic [31:0] inst_req_addr_q;

    // ---------------------------------------------------------------------
    // Global memory model
    // ---------------------------------------------------------------------
    localparam int MEM_WORDS = (FB_OFF + FB_SIZE_BYTES + 32'h0001_0000) >> 2;
    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - BASE_ADDR) >> 2;
    endfunction

    // D-cache backing memory responder
    localparam int DCACHE_LINE_BYTES = 64;
    localparam int DCACHE_BEATS      = DCACHE_LINE_BYTES / 8;

    logic        dcache_tx_active;
    logic        dcache_tx_rw;
    logic [31:0] dcache_tx_addr;
    logic [7:0]  dcache_tx_id_q;
    logic [2:0]  dcache_tx_beat;
    logic [511:0] dcache_tx_wdata;
    logic [7:0]  dcache_tx_wstrb;

    function automatic [63:0] dcache_read_beat(input logic [31:0] line_addr, input logic [2:0] beat);
        int base_word;
        begin
            dcache_read_beat = 64'h0;
            base_word = mem_index(line_addr) + (beat * 2);
            if ((base_word >= 0) && ((base_word + 1) < MEM_WORDS)) begin
                dcache_read_beat = {mem[base_word + 1], mem[base_word + 0]};
            end
        end
    endfunction

    task automatic dcache_write_line(
        input logic [31:0] line_addr,
        input logic [511:0] line_data,
        input logic [7:0]  line_wstrb  // 1 strobe bit per 8-byte beat
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

    assign data_req_ready  = 1'b1;
    assign data_resp_valid = 1'b0;
    assign data_resp_rd    = '0;
    assign data_resp_data  = 32'h0;

    assign mailbox_tx_ready   = 1'b1;
    assign mailbox_rx_valid   = 1'b0;
    assign mailbox_rx_data    = '0;
    assign mailbox_rx_dest_id = '0;
    assign mailbox_rx_ready   = 1'b1;

    assign fb_aw_ready = 1'b1;
    assign fb_w_ready  = 1'b1;
    assign fb_b_valid  = 1'b0;

    assign dcache_mem_req_ready = 1'b1;
    assign inst_miss_req_ready  = 1'b1;

    // ---------------------------------------------------------------------
    // Memory initialization (LUTs only; compute unit does all rendering)
    // ---------------------------------------------------------------------
    task automatic init_memory();
        int i;
        int ti;
        int idx;
        real ang;
        int s_q;
        int c_q;
        begin
            for (i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;

            // Sine/cosine LUT (Q1.15)
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

            // Inverse sqrt LUT for [0..1] in Q16.16, 1024 entries.
            for (ti = 0; ti < 1024; ti++) begin
                real x;
                real inv;
                x = (ti / 1023.0);
                if (x < 1e-6) inv = 0.0; else inv = 1.0 / $sqrt(x);
                idx = mem_index(BASE_ADDR + INVSQRT_OFF + (ti * 4));
                mem[idx] = $rtoi(inv * 65536.0);
            end

            // Vector constants (FP32 lanes) for mirror tint: (0.80, 0.85, 0.95, 1.00)
            mem[mem_index(BASE_ADDR + VEC_CONST_OFF + 0)]  = 32'h3F4CCCCD; // 0.80
            mem[mem_index(BASE_ADDR + VEC_CONST_OFF + 4)]  = 32'h3F59999A; // 0.85
            mem[mem_index(BASE_ADDR + VEC_CONST_OFF + 8)]  = 32'h3F733333; // 0.95
            mem[mem_index(BASE_ADDR + VEC_CONST_OFF + 12)] = 32'h3F800000; // 1.00

            // Sphere 2 parameters (Q16.16): x, y, r²
            mem[mem_index(BASE_ADDR + SPHERE2_OFF + 0)]  = 32'h00009000; // x = 0.5625
            mem[mem_index(BASE_ADDR + SPHERE2_OFF + 4)]  = 32'h00008000; // y = 0.5
            mem[mem_index(BASE_ADDR + SPHERE2_OFF + 8)]  = 32'h00001000; // r² = 0.0625 (radius 0.25)
        end
    endtask

    // ---------------------------------------------------------------------
    // Instruction ROM program
    // ---------------------------------------------------------------------
    task automatic init_rom();
        int pc;
        // ---- Branch source PCs ----
        int br_s1_pc;           // sphere1 hit
        int br_s2_pc;           // sphere2 hit
        int br_gnd_pc;          // yndc < 0 → ground
        int jmp_sky_pc;         // sky → write
        int br_shad_pc;         // shadow check
        int br_chk_nsh_pc;      // checker (no shadow)
        int jmp_dk_nsh_pc;      // dark no-shadow → write
        int jmp_lt_nsh_pc;      // light no-shadow → write
        int br_chk_sh_pc;       // checker (shadow)
        int jmp_dk_sh_pc;       // dark shadow → write
        int jmp_lt_sh_pc;       // light shadow → write
        int br_s1gnd_pc;        // sphere1 reflected y check
        int jmp_s1sky_pc;       // sphere1 sky → spec
        int br_s1chk_pc;        // sphere1 reflected checker
        int jmp_s1dk_pc;        // sphere1 dark → spec
        int br_spec_pc;         // specular threshold
        int jmp_tint_pc;        // after tint → write
        int br_s2lit_pc;        // sphere2 lit check
        int jmp_s2lit_pc;       // sphere2 lit → write
        int jmp_s2dk_pc;        // sphere2 dark → write
        int blt_x_pc;
        int blt_y_pc;
        int blt_frame_pc;
        // ---- Branch target PCs ----
        int loop_frame_pc, loop_y_pc, loop_x_pc;
        int sphere1_pc;
        int sphere2_pc;
        int bg_gnd_pc;
        int bg_shad_pc;
        int bg_lt_nsh_pc;
        int bg_lt_sh_pc;
        int s1_gnd_pc;
        int s1_lt_pc;
        int spec_pc;
        int skip_spec_pc;
        int mirror_tint_pc;
        int s2_dk_pc;
        int write_pc;
        int imm;
        begin
            for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
            pc = 0;

            // =========================================================
            // SETUP — persistent register constants
            // =========================================================
            // x1 = BASE_ADDR
            rom[pc>>2] = u_type(BASE_ADDR, 5'd1, OP_LUI); pc += 4;
            // x2 = FB base
            rom[pc>>2] = u_type(BASE_ADDR + FB_OFF, 5'd2, OP_LUI); pc += 4;
            // x3 = invW (Q16.16), x4 = invH
            rom[pc>>2] = i_type(642, 5'd0, 3'b000, 5'd3, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(642, 5'd0, 3'b000, 5'd4, OP_IMM); pc += 4;
            // x5 = ONE_FP (Q16.16)
            rom[pc>>2] = u_type(32'h0001_0000, 5'd5, OP_LUI); pc += 4;
            // x17 = r2_sphere1 ≈ 0.36 Q16.16 = 23593
            rom[pc>>2] = u_type(32'h0000_6000, 5'd17, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(-983, 5'd17, 3'b000, 5'd17, OP_IMM); pc += 4;
            // x19 = W, x20 = H
            rom[pc>>2] = i_type(W, 5'd0, 3'b000, 5'd19, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(H, 5'd0, 3'b000, 5'd20, OP_IMM); pc += 4;
            // x21 = sincos base, x22 = invsqrt base
            rom[pc>>2] = u_type(BASE_ADDR + SINCOS_OFF, 5'd21, OP_LUI); pc += 4;
            rom[pc>>2] = u_type(BASE_ADDR + INVSQRT_OFF, 5'd22, OP_LUI); pc += 4;
            // Load mirror tint vector into v2
            rom[pc>>2] = u_type(BASE_ADDR + VEC_CONST_OFF, 5'd12, OP_LUI); pc += 4;
            rom[pc>>2] = vsetvli_inst(5'd0, 5'd0, 11'h010); pc += 4;
            rom[pc>>2] = rvv_vl(5'd2, 5'd12, RVV_VEW32, 1'b1); pc += 4;
            // x13 = FRAMES
            rom[pc>>2] = i_type(FRAMES, 5'd0, 3'b000, 5'd13, OP_IMM); pc += 4;
            // x6 = frame counter = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd6, OP_IMM); pc += 4;

            // =========================================================
            // FRAME LOOP
            // =========================================================
            loop_frame_pc = pc;
            // angle_idx = frame & 0xFF → x7
            rom[pc>>2] = i_type(255, 5'd6, 3'b111, 5'd7, OP_IMM); pc += 4;
            // angle_ptr = sincos_base + (idx << 3)
            rom[pc>>2] = i_type(3, 5'd7, 3'b001, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;
            // sin (Q1.15) → x14, cos → x15
            rom[pc>>2] = i_type(0, 5'd7, 3'b010, 5'd14, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd7, 3'b010, 5'd15, OP_LOAD); pc += 4;
            // sphere1_x = sin << 1 (Q16.16) → x16
            rom[pc>>2] = i_type(1, 5'd14, 3'b001, 5'd16, OP_IMM); pc += 4;
            // sphere1_z = (cos << 1) + 2.0 → x18
            rom[pc>>2] = i_type(1, 5'd15, 3'b001, 5'd18, OP_IMM); pc += 4;
            rom[pc>>2] = u_type(32'h0002_0000, 5'd7, OP_LUI); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd7, 5'd18, 3'b000, 5'd18, OP_REG); pc += 4;
            // y = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd8, OP_IMM); pc += 4;

            // =========================================================
            // Y LOOP
            // =========================================================
            loop_y_pc = pc;
            // yndc = -(2*y - 127) * invH → x10
            rom[pc>>2] = i_type(1, 5'd8, 3'b001, 5'd9, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(-127, 5'd9, 3'b000, 5'd9, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd4, 5'd9, 3'b000, 5'd10, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd10, 5'd0, 3'b000, 5'd10, OP_REG); pc += 4;
            // row_ptr = fb_base + (y << 9) → x23
            rom[pc>>2] = i_type(9, 5'd8, 3'b001, 5'd23, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd2, 5'd23, 3'b000, 5'd23, OP_REG); pc += 4;
            // x = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd11, OP_IMM); pc += 4;

            // =========================================================
            // X LOOP (inner pixel)
            // =========================================================
            loop_x_pc = pc;
            // xndc = invW * (2*x - 127) → x24
            rom[pc>>2] = i_type(1, 5'd11, 3'b001, 5'd24, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(-127, 5'd24, 3'b000, 5'd24, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd3, 5'd24, 3'b000, 5'd24, OP_REG); pc += 4;
            // dx = xndc - sphere1_x → x25
            rom[pc>>2] = r_type(7'b0100000, 5'd16, 5'd24, 3'b000, 5'd25, OP_REG); pc += 4;
            // r2 = (dx>>8)² + (yndc>>8)² → x26  (overflow-safe)
            rom[pc>>2] = i_type(12'h408, 5'd25, 3'b101, 5'd26, OP_IMM); pc += 4; // SRAI dx>>8
            rom[pc>>2] = i_type(12'h408, 5'd10, 3'b101, 5'd27, OP_IMM); pc += 4; // SRAI yndc>>8
            rom[pc>>2] = r_type(7'b0000001, 5'd26, 5'd26, 3'b000, 5'd26, OP_REG); pc += 4; // (dx>>8)²
            rom[pc>>2] = r_type(7'b0000001, 5'd27, 5'd27, 3'b000, 5'd27, OP_REG); pc += 4; // (yndc>>8)²
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_REG); pc += 4; // r2
            // BLT r2, r2_sphere1 → sphere1
            br_s1_pc = pc;
            rom[pc>>2] = b_type(0, 5'd26, 5'd17, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // ---- Sphere 2 test ----
            // Encode sphere2 params as immediates (avoids D-cache thrashing)
            // s2_x = 0.5625 Q16.16 = 0x9000, s2_y = 0.5 = 0x8000, s2_r2 = 0.0625 = 0x1000
            rom[pc>>2] = u_type(32'h0000_9000, 5'd27, OP_LUI); pc += 4;  // s2_x → x27
            rom[pc>>2] = u_type(32'h0000_8000, 5'd28, OP_LUI); pc += 4;  // s2_y → x28
            rom[pc>>2] = u_type(32'h0000_1000, 5'd29, OP_LUI); pc += 4;  // s2_r2 → x29
            // dx2 = xndc - s2_x → x30
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd24, 3'b000, 5'd30, OP_REG); pc += 4;
            // dy2 = yndc - s2_y → x31
            rom[pc>>2] = r_type(7'b0100000, 5'd28, 5'd10, 3'b000, 5'd31, OP_REG); pc += 4;
            // r2_2 = (dx2>>8)² + (dy2>>8)² → x7
            rom[pc>>2] = i_type(12'h408, 5'd30, 3'b101, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd31, 3'b101, 5'd9, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd9, 5'd9, 3'b000, 5'd9, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd9, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;
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

            // ---- Background ground ----
            bg_gnd_pc = pc;
            // 2D checker: ((x>>3) ^ (y>>3) + frame) & 1
            rom[pc>>2] = i_type(3, 5'd11, 3'b101, 5'd27, OP_IMM); pc += 4;  // SRLI x>>3
            rom[pc>>2] = i_type(3, 5'd8, 3'b101, 5'd28, OP_IMM); pc += 4;   // SRLI y>>3
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd27, 3'b100, 5'd27, OP_REG); pc += 4; // XOR
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd27, 3'b000, 5'd27, OP_REG); pc += 4;  // + frame
            rom[pc>>2] = i_type(1, 5'd27, 3'b111, 5'd27, OP_IMM); pc += 4;  // & 1
            // Shadow: r2 < 2*r2_sphere1?
            rom[pc>>2] = i_type(1, 5'd17, 3'b001, 5'd28, OP_IMM); pc += 4;  // x28 = r2_sphere << 1
            br_shad_pc = pc;
            rom[pc>>2] = b_type(0, 5'd26, 5'd28, 3'b100, OP_BRANCH); pc += 4; // BLT r2, 2*r2_s → shadow
            rom[pc>>2] = nop(); pc += 4;

            // ---- Ground no-shadow ----
            br_chk_nsh_pc = pc;
            rom[pc>>2] = b_type(0, 5'd27, 5'd0, 3'b001, OP_BRANCH); pc += 4; // BNE checker → light
            rom[pc>>2] = nop(); pc += 4;
            // Dark (black)
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_dk_nsh_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // Light (red): RGB(255,0,0) → 0xFF0000FF
            bg_lt_nsh_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0FF, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_lt_nsh_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // ---- Ground shadow ----
            bg_shad_pc = pc;
            br_chk_sh_pc = pc;
            rom[pc>>2] = b_type(0, 5'd27, 5'd0, 3'b001, OP_BRANCH); pc += 4; // BNE checker → shadow light
            rom[pc>>2] = nop(); pc += 4;
            // Dark shadow (black)
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_dk_sh_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // Light shadow (dark red): RGB(100,0,0) → 0xFF000064
            bg_lt_sh_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h064, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_lt_sh_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // =========================================================
            // SPHERE 1 PATH (mirror with specular)
            // =========================================================
            sphere1_pc = pc;
            // z2 = r2_sphere - r2 → x28
            rom[pc>>2] = r_type(7'b0100000, 5'd26, 5'd17, 3'b000, 5'd28, OP_REG); pc += 4;
            // invsqrt LUT: idx = z2>>6, word offset
            rom[pc>>2] = i_type(6, 5'd28, 3'b101, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd29, 3'b001, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd22, 5'd29, 3'b000, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = i_type(0, 5'd29, 3'b010, 5'd30, OP_LOAD); pc += 4;
            // z = (z2 * inv) >> 16
            rom[pc>>2] = r_type(7'b0000001, 5'd30, 5'd28, 3'b000, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd29, 3'b101, 5'd29, OP_IMM); pc += 4;
            // inv_radius ≈ 1.6667 Q16.8 → x7  (427 avoids MUL overflow)
            rom[pc>>2] = i_type(427, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // nx = (inv_r * dx) >> 8 → x14  (NOTE: dx in x25)
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd25, 3'b000, 5'd14, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd14, 3'b101, 5'd14, OP_IMM); pc += 4;
            // ny = (inv_r * yndc) >> 8 → x15
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd10, 3'b000, 5'd15, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd15, 3'b101, 5'd15, OP_IMM); pc += 4;
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
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd30, 3'b000, 5'd27, OP_REG); pc += 4;
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

            // Reflected sky: RGB(100,150,230) → 0xFFE69664
            rom[pc>>2] = u_type(32'hFFE6_9000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h664, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s1sky_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Reflected ground checker: (r.x>>14 ^ r.z>>14 + frame) & 1
            s1_gnd_pc = pc;
            rom[pc>>2] = i_type(12'h40E, 5'd26, 3'b101, 5'd28, OP_IMM); pc += 4; // SRAI r.x>>14
            rom[pc>>2] = i_type(12'h40E, 5'd30, 3'b101, 5'd29, OP_IMM); pc += 4; // SRAI r.z>>14
            rom[pc>>2] = r_type(7'b0000000, 5'd29, 5'd28, 3'b100, 5'd28, OP_REG); pc += 4; // XOR
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd28, 3'b000, 5'd28, OP_REG); pc += 4;  // + frame
            rom[pc>>2] = i_type(1, 5'd28, 3'b111, 5'd28, OP_IMM); pc += 4;  // & 1
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
            // (fall through to specular check)

            // ---- Specular check ----
            spec_pc = pc;
            // ny>>8 > threshold → white specular highlight (only top hemisphere)
            rom[pc>>2] = i_type(12'h408, 5'd15, 3'b101, 5'd28, OP_IMM); pc += 4; // SRAI ny>>8
            rom[pc>>2] = i_type(156, 5'd0, 3'b000, 5'd29, OP_IMM); pc += 4; // threshold ~0.61
            // ny<0 (bottom) automatically skipped since SRAI preserves sign
            rom[pc>>2] = nop(); pc += 4;
            br_spec_pc = pc;
            rom[pc>>2] = b_type(0, 5'd28, 5'd29, 3'b100, OP_BRANCH); pc += 4; // BLT → skip spec
            rom[pc>>2] = nop(); pc += 4;
            // Specular: white (ADDI x31, x0, -1 = 0xFFFFFFFF)
            rom[pc>>2] = i_type(-1, 5'd0, 3'b000, 5'd31, OP_IMM); pc += 4;
            skip_spec_pc = pc;

            // ---- Mirror tint ----
            mirror_tint_pc = pc;
            rom[pc>>2] = r_type(7'b0011110, 5'd31, 5'd0, 3'b011, 5'd1, OP_CUSTOM1); pc += 4; // VUNPACK
            rom[pc>>2] = r_type(7'b0011000, 5'd2, 5'd1, 3'b011, 5'd1, OP_CUSTOM1); pc += 4;  // VMUL
            rom[pc>>2] = r_type(7'b0010000, 5'd0, 5'd1, 3'b011, 5'd31, OP_CUSTOM1); pc += 4; // VPACK
            jmp_tint_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // =========================================================
            // SPHERE 2 PATH (diffuse green)
            // =========================================================
            sphere2_pc = pc;
            // x31 = dy2 from sphere2 test.  dy2 < 0 → dark side
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

            // =========================================================
            // WRITE PIXEL
            // =========================================================
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
            // y++
            rom[pc>>2] = i_type(1, 5'd8, 3'b000, 5'd8, OP_IMM); pc += 4;
            blt_y_pc = pc;
            rom[pc>>2] = b_type(0, 5'd8, 5'd20, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // Frame done
            rom[pc>>2] = i_type(12'h0FF, 5'd0, 3'b000, 5'd0, OP_FENCE); pc += 4;
            rom[pc>>2] = i_type(1, 5'd6, 3'b000, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = s_type(DONE_OFF, 5'd1, 5'd7, 3'b010, OP_STORE); pc += 4;
            // frame++
            rom[pc>>2] = i_type(1, 5'd6, 3'b000, 5'd6, OP_IMM); pc += 4;
            blt_frame_pc = pc;
            rom[pc>>2] = b_type(0, 5'd6, 5'd13, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // WFI loop
            rom[pc>>2] = i_type(12'h105, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = b_type(-4, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // =========================================================
            // PATCH BRANCHES
            // =========================================================
            // Sphere1 hit
            imm = sphere1_pc - br_s1_pc;
            rom[br_s1_pc>>2] = b_type(imm, 5'd26, 5'd17, 3'b100, OP_BRANCH);
            // Sphere2 hit
            imm = sphere2_pc - br_s2_pc;
            rom[br_s2_pc>>2] = b_type(imm, 5'd7, 5'd29, 3'b100, OP_BRANCH);
            // Ground
            imm = bg_gnd_pc - br_gnd_pc;
            rom[br_gnd_pc>>2] = b_type(imm, 5'd10, 5'd0, 3'b100, OP_BRANCH);
            // Sky → write
            imm = write_pc - jmp_sky_pc;
            rom[jmp_sky_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Shadow check
            imm = bg_shad_pc - br_shad_pc;
            rom[br_shad_pc>>2] = b_type(imm, 5'd26, 5'd28, 3'b100, OP_BRANCH);
            // No-shadow checker → light
            imm = bg_lt_nsh_pc - br_chk_nsh_pc;
            rom[br_chk_nsh_pc>>2] = b_type(imm, 5'd27, 5'd0, 3'b001, OP_BRANCH);
            // No-shadow dark → write
            imm = write_pc - jmp_dk_nsh_pc;
            rom[jmp_dk_nsh_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // No-shadow light → write
            imm = write_pc - jmp_lt_nsh_pc;
            rom[jmp_lt_nsh_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Shadow checker → light shadow
            imm = bg_lt_sh_pc - br_chk_sh_pc;
            rom[br_chk_sh_pc>>2] = b_type(imm, 5'd27, 5'd0, 3'b001, OP_BRANCH);
            // Shadow dark → write
            imm = write_pc - jmp_dk_sh_pc;
            rom[jmp_dk_sh_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Shadow light → write
            imm = write_pc - jmp_lt_sh_pc;
            rom[jmp_lt_sh_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Sphere1 reflected ground
            imm = s1_gnd_pc - br_s1gnd_pc;
            rom[br_s1gnd_pc>>2] = b_type(imm, 5'd27, 5'd0, 3'b100, OP_BRANCH);
            // Sphere1 reflected sky → specular check
            imm = spec_pc - jmp_s1sky_pc;
            rom[jmp_s1sky_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Sphere1 reflected checker → light
            imm = s1_lt_pc - br_s1chk_pc;
            rom[br_s1chk_pc>>2] = b_type(imm, 5'd28, 5'd0, 3'b001, OP_BRANCH);
            // Sphere1 reflected dark → specular check
            imm = spec_pc - jmp_s1dk_pc;
            rom[jmp_s1dk_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Specular skip
            imm = skip_spec_pc - br_spec_pc;
            rom[br_spec_pc>>2] = b_type(imm, 5'd28, 5'd29, 3'b100, OP_BRANCH);
            // After tint → write
            imm = write_pc - jmp_tint_pc;
            rom[jmp_tint_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Sphere2 lit → dark
            imm = s2_dk_pc - br_s2lit_pc;
            rom[br_s2lit_pc>>2] = b_type(imm, 5'd31, 5'd0, 3'b100, OP_BRANCH);
            // Sphere2 lit → write
            imm = write_pc - jmp_s2lit_pc;
            rom[jmp_s2lit_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // Sphere2 dark → write
            imm = write_pc - jmp_s2dk_pc;
            rom[jmp_s2dk_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            // X loop back
            imm = loop_x_pc - blt_x_pc;
            rom[blt_x_pc>>2] = b_type(imm, 5'd11, 5'd19, 3'b100, OP_BRANCH);
            // Y loop back
            imm = loop_y_pc - blt_y_pc;
            rom[blt_y_pc>>2] = b_type(imm, 5'd8, 5'd20, 3'b100, OP_BRANCH);
            // Frame loop back
            imm = loop_frame_pc - blt_frame_pc;
            rom[blt_frame_pc>>2] = b_type(imm, 5'd6, 5'd13, 3'b100, OP_BRANCH);

            $display("gfx_sw_rt_tb: ROM program loaded (%0d bytes)", pc);
        end
    endtask

    // ---------------------------------------------------------------------
    // Framebuffer dump
    // ---------------------------------------------------------------------
    task automatic dump_fb_ppm(input int frame_no);
        int y;
        int x;
        int idx;
        logic [31:0] pix;
        logic [7:0] r;
        logic [7:0] g;
        logic [7:0] b;
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
    // I-cache miss response + D-cache memory responder + frame trigger
    // ---------------------------------------------------------------------
    int frame_seen;
    int unsigned cycle_count;
    localparam int unsigned MAX_CYCLES = 300_000_000;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            inst_miss_resp_valid <= 1'b0;
            inst_miss_resp_data  <= 64'h0;
            inst_pending         <= 1'b0;
            inst_req_addr_q      <= 32'h0;

            dcache_mem_resp_valid <= 1'b0;
            dcache_mem_resp_data  <= 64'h0;
            dcache_mem_resp_id    <= 8'h0;
            dcache_tx_active      <= 1'b0;
            dcache_tx_rw          <= 1'b0;
            dcache_tx_addr        <= 32'h0;
            dcache_tx_id_q        <= 8'h0;
            dcache_tx_beat        <= 3'd0;
            dcache_tx_wdata       <= '0;
            dcache_tx_wstrb       <= '0;

            frame_seen <= 0;
            cycle_count <= 0;
        end else begin
            cycle_count <= cycle_count + 1;

            // Periodic heartbeat for stall diagnosis
            if ((cycle_count != 0) && ((cycle_count % 1_000_000) == 0)) begin
                $display("TB: cyc=%0d pc=%08h if_v=%0d acc0=%0d stall_sb0=%0d sb_busy_s=%08h sb_busy_v=%08h lsu_busy=%0d dcache_busy=%0d issue0_v=%0d",
                    cycle_count,
                    dut.if_pc,
                    dut.if_valid,
                    dut.accept0,
                    dut.u_scoreboard.stall0,
                    dut.u_scoreboard.busy_s,
                    dut.u_scoreboard.busy_v,
                    dut.lsu_busy,
                    dut.u_dcache.mem_busy,
                    dut.issue0_valid
                );
            end

            // Timeout watchdog
            if (cycle_count >= MAX_CYCLES) begin
                $display("TB: TIMEOUT at cycle %0d  if_pc=%08h", cycle_count, dut.if_pc);
                $fatal(1);
            end
            // I-cache miss response (1-cycle latency)
            inst_miss_resp_valid <= 1'b0;
            if (inst_miss_req_valid && inst_miss_req_ready && !inst_pending) begin
                inst_pending    <= 1'b1;
                inst_req_addr_q <= {inst_miss_req_addr[31:3], 3'b000};
            end
            if (inst_pending) begin
                int widx;
                widx = int'(inst_req_addr_q >> 2);
                if ((widx < 0) || (widx + 1 >= ROM_WORDS)) begin
                    $display("TB: OOB IROM access addr=%08h", inst_req_addr_q);
                    $fatal(1);
                end
                inst_miss_resp_valid <= 1'b1;
                inst_miss_resp_data  <= {rom[widx + 1], rom[widx]};
                inst_pending <= 1'b0;
            end

            // D-cache responder (64B line, 8 beats)
            dcache_mem_resp_valid <= 1'b0;
            if (dcache_tx_active) begin
                if (!dcache_tx_rw) begin
                    dcache_mem_resp_valid <= 1'b1;
                    dcache_mem_resp_data  <= dcache_read_beat(dcache_tx_addr, dcache_tx_beat);
                    dcache_mem_resp_id    <= dcache_tx_id_q;
                    if (dcache_tx_beat == (DCACHE_BEATS - 1)) begin
                        dcache_tx_active <= 1'b0;
                        dcache_tx_beat   <= 3'd0;
                    end else begin
                        dcache_tx_beat <= dcache_tx_beat + 1'b1;
                    end
                end else begin
                    dcache_mem_resp_valid <= 1'b1;
                    dcache_mem_resp_data  <= 64'h0;
                    dcache_mem_resp_id    <= dcache_tx_id_q;
                    dcache_tx_active <= 1'b0;
                    dcache_tx_beat   <= 3'd0;

                    dcache_write_line(dcache_tx_addr, dcache_tx_wdata, dcache_tx_wstrb);

                    // Only check DONE on writes to the cache line containing DONE_OFF
                    if ((dcache_tx_addr <= (BASE_ADDR + DONE_OFF)) &&
                        ((dcache_tx_addr + 32'd64) > (BASE_ADDR + DONE_OFF))) begin
                        if (mem[mem_index(BASE_ADDR + DONE_OFF)] != 32'h0) begin
                            $display("TB: DONE value=%08h, dumping frame %0d",
                                     mem[mem_index(BASE_ADDR + DONE_OFF)], frame_seen);
                            if (frame_seen < FRAMES) begin
                                dump_fb_ppm(frame_seen);
                                if (frame_seen == (FRAMES - 1)) begin
                                    $display("TB: reached %0d frames, finishing.", FRAMES);
                                    $finish;
                                end
                                frame_seen <= frame_seen + 1;
                            end
                            mem[mem_index(BASE_ADDR + DONE_OFF)] = 32'h0;
                        end
                    end
                end
            end else if (dcache_mem_req_valid && dcache_mem_req_ready) begin
                dcache_tx_active <= 1'b1;
                dcache_tx_rw     <= dcache_mem_req_rw;
                dcache_tx_addr   <= {dcache_mem_req_addr[31:6], 6'b0};
                dcache_tx_id_q   <= dcache_mem_req_id;
                dcache_tx_beat   <= 3'd0;
                dcache_tx_wdata  <= dcache_mem_req_wdata;
                dcache_tx_wstrb  <= dcache_mem_req_wstrb;
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
