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
    localparam int FRAMES = 30;

    localparam logic [31:0] BASE_ADDR = 32'h8000_0000;

    localparam int FB_SIZE_BYTES = W * H * 4;
    localparam int FB_SIZE_WORDS = FB_SIZE_BYTES / 4;

    // Layout inside global window
    localparam logic [31:0] DONE_OFF         = 32'h0000_01F0;
    localparam logic [31:0] FB_OFF           = 32'h0001_0000;
    localparam logic [31:0] SINCOS_OFF      = 32'h0000_2000; // 256 * 8B
    localparam logic [31:0] INVSQRT_OFF     = 32'h0000_3000; // 1024 * 4B
    localparam logic [31:0] VEC_CONST_OFF   = 32'h0000_4000; // Vector constants (ARGB tint, etc.)

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
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_INT_IMM);
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
            base_word = mem_index(line_addr) + (beat * 2);
            dcache_read_beat = {mem[base_word + 1], mem[base_word + 0]};
        end
    endfunction

    task automatic dcache_write_line(
        input logic [31:0] line_addr,
        input logic [511:0] line_data,
        input logic [7:0]  line_wstrb
    );
        int base_word;
        int done_idx;
        logic [31:0] done_w;
        begin
            base_word = mem_index(line_addr);
            done_idx = ((BASE_ADDR + DONE_OFF) - line_addr) >> 2;
            done_w = mem[base_word + done_idx];

            if ((line_addr <= (BASE_ADDR + DONE_OFF)) && ((line_addr + 32'd64) > (BASE_ADDR + DONE_OFF))) begin
                for (int b = 0; b < DCACHE_BEATS; b++) begin
                    int word_idx;
                    logic [31:0] w0;
                    logic [31:0] w1;
                    word_idx = base_word + (b * 2);
                    w0 = mem[word_idx + 0];
                    w1 = mem[word_idx + 1];
                    for (int cbyte = 0; cbyte < 4; cbyte++) begin
                        if (line_wstrb[(b*8) + cbyte]) begin
                            w0[(cbyte*8) +: 8] = line_data[(b*64) + (cbyte*8) +: 8];
                        end
                        if (line_wstrb[(b*8) + cbyte + 4]) begin
                            w1[(cbyte*8) +: 8] = line_data[(b*64) + (cbyte*8) + 32 +: 8];
                        end
                    end
                    mem[word_idx + 0] = w0;
                    mem[word_idx + 1] = w1;
                end

                done_w = mem[mem_index(BASE_ADDR + DONE_OFF)];
            end else begin
                for (int b = 0; b < DCACHE_BEATS; b++) begin
                    int word_idx;
                    logic [31:0] w0;
                    logic [31:0] w1;
                    word_idx = base_word + (b * 2);
                    w0 = mem[word_idx + 0];
                    w1 = mem[word_idx + 1];
                    for (int cbyte = 0; cbyte < 4; cbyte++) begin
                        if (line_wstrb[(b*8) + cbyte]) begin
                            w0[(cbyte*8) +: 8] = line_data[(b*64) + (cbyte*8) +: 8];
                        end
                        if (line_wstrb[(b*8) + cbyte + 4]) begin
                            w1[(cbyte*8) +: 8] = line_data[(b*64) + (cbyte*8) + 32 +: 8];
                        end
                    end
                    mem[word_idx + 0] = w0;
                    mem[word_idx + 1] = w1;
                end
            end

            if (done_w != 32'h0) begin
                $display("TB: DONE write value=%08h", done_w);
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
        end
    endtask

    // ---------------------------------------------------------------------
    // Instruction ROM program
    // ---------------------------------------------------------------------
    task automatic init_rom();
        int pc;
        int loop_frame_pc;
        int loop_y_pc;
        int loop_x_pc;
        int blt_y_pc;
        int blt_x_pc;
        int blt_frame_pc;
        int br_sphere_pc;
        int sphere_path_pc;
        int br_ground_pc;
        int bg_ground_pc;
        int bg_sky_jmp_pc;
        int bg_dark_jmp_pc;
        int bg_light_jmp_pc;
        int br_sphere_ground_pc;
        int sphere_ground_pc;
        int sphere_sky_jmp_pc;
        int sphere_dark_jmp_pc;
        int sphere_light_jmp_pc;
        int mirror_tint_pc;
        int write_pc;
        int imm;
        begin
            for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
            pc = 0;

            // x1 = BASE_ADDR
            rom[pc>>2] = u_type(BASE_ADDR, 5'd1, OP_LUI); pc += 4;

            // x2 = FB base
            rom[pc>>2] = u_type(BASE_ADDR + FB_OFF, 5'd2, OP_LUI); pc += 4;

            // x3 = invW (Q16.16) = 65536/102 ~= 642 (wider FOV / zoom out)
            rom[pc>>2] = i_type(642, 5'd0, 3'b000, 5'd3, OP_INT_IMM); pc += 4;
            // x4 = invH (Q16.16)
            rom[pc>>2] = i_type(642, 5'd0, 3'b000, 5'd4, OP_INT_IMM); pc += 4;
            // x5 = one (Q16.16)
            rom[pc>>2] = u_type(32'h0001_0000, 5'd5, OP_LUI); pc += 4;
            // x17 = r2 (sphere radius^2 ~= 0.36)
            rom[pc>>2] = u_type(32'h0000_6000, 5'd17, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(-983, 5'd17, 3'b000, 5'd17, OP_INT_IMM); pc += 4;
            // x19 = W, x20 = H
            rom[pc>>2] = i_type(W, 5'd0, 3'b000, 5'd19, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(H, 5'd0, 3'b000, 5'd20, OP_INT_IMM); pc += 4;

            // x21 = sincos base
            rom[pc>>2] = u_type(BASE_ADDR + SINCOS_OFF, 5'd21, OP_LUI); pc += 4;
            // x22 = invsqrt base
            rom[pc>>2] = u_type(BASE_ADDR + INVSQRT_OFF, 5'd22, OP_LUI); pc += 4;
            // x12 = vector const base (mirror tint)
            rom[pc>>2] = u_type(BASE_ADDR + VEC_CONST_OFF, 5'd12, OP_LUI); pc += 4;
            // v2 = mirror tint vector (FP32)
            rom[pc>>2] = i_type(0, 5'd12, 3'b000, 5'd2, OP_VLD); pc += 4;
            // x13 = FRAMES
            rom[pc>>2] = i_type(FRAMES, 5'd0, 3'b000, 5'd13, OP_INT_IMM); pc += 4;

            // frame = 0 (x6)
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd6, OP_INT_IMM); pc += 4;

            // -------------------------
            // Frame loop
            // -------------------------
            loop_frame_pc = pc;

            // angle_idx = frame & 0xFF -> x7
            rom[pc>>2] = i_type(255, 5'd6, 3'b111, 5'd7, OP_INT_IMM); pc += 4; // ANDI
            // angle_ptr = sincos_base + (idx<<3) -> x7
            rom[pc>>2] = i_type(3, 5'd7, 3'b001, 5'd7, OP_INT_IMM); pc += 4; // SLLI
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd7, 3'b000, 5'd7, OP_INT); pc += 4; // ADD
            // sin (Q1.15) -> x14, cos -> x15
            rom[pc>>2] = i_type(0, 5'd7, 3'b010, 5'd14, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd7, 3'b010, 5'd15, OP_LOAD); pc += 4;
            // sphere_x = sin<<1 (Q16.16) -> x16
            rom[pc>>2] = i_type(1, 5'd14, 3'b001, 5'd16, OP_INT_IMM); pc += 4; // SLLI
            // sphere_z = (cos<<1) + 2.0 -> x18
            rom[pc>>2] = i_type(1, 5'd15, 3'b001, 5'd18, OP_INT_IMM); pc += 4; // cos<<1
            rom[pc>>2] = u_type(32'h0002_0000, 5'd7, OP_LUI); pc += 4;           // 2.0
            rom[pc>>2] = r_type(7'b0000000, 5'd7, 5'd18, 3'b000, 5'd18, OP_INT); pc += 4; // +2.0

            // y = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd8, OP_INT_IMM); pc += 4;
            loop_y_pc = pc;

            // y2 = (y<<1) - (H-1) -> x9
            rom[pc>>2] = i_type(1, 5'd8, 3'b001, 5'd9, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(-127, 5'd9, 3'b000, 5'd9, OP_INT_IMM); pc += 4; // ADDI x9,x9,-127
            // yndc = (y2 * invH) -> x10 (Q16.16). Keep full fixed-point precision.
            rom[pc>>2] = r_type(7'b0000001, 5'd4, 5'd9, 3'b000, 5'd10, OP_INT); pc += 4; // MUL
            rom[pc>>2] = i_type(12'h400, 5'd10, 3'b101, 5'd10, OP_INT_IMM); pc += 4;       // SRAI 0 (no-op)
            // yndc = -yndc
            rom[pc>>2] = r_type(7'b0100000, 5'd10, 5'd0, 3'b000, 5'd10, OP_INT); pc += 4; // SUB x10,x0,x10

            // row_ptr = fb_base + (y << 9) -> x23
            rom[pc>>2] = i_type(9, 5'd8, 3'b001, 5'd23, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd2, 5'd23, 3'b000, 5'd23, OP_INT); pc += 4;

            // x = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd11, OP_INT_IMM); pc += 4;
            loop_x_pc = pc;

            // x2 = (x<<1) - (W-1) -> x12
            rom[pc>>2] = i_type(1, 5'd11, 3'b001, 5'd12, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(-127, 5'd12, 3'b000, 5'd12, OP_INT_IMM); pc += 4;
            // xndc = (x2 * invW) -> x24 (Q16.16). Keep full fixed-point precision.
            rom[pc>>2] = r_type(7'b0000001, 5'd3, 5'd12, 3'b000, 5'd24, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h400, 5'd24, 3'b101, 5'd24, OP_INT_IMM); pc += 4;       // SRAI 0 (no-op)
            // dx = xndc - sphere_x
            rom[pc>>2] = r_type(7'b0100000, 5'd16, 5'd24, 3'b000, 5'd24, OP_INT); pc += 4;

            // r2 = (xndc*xndc + yndc*yndc) >> 16 -> x25
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd24, 3'b000, 5'd25, OP_INT); pc += 4; // x^2
            rom[pc>>2] = r_type(7'b0000001, 5'd10, 5'd10, 3'b000, 5'd26, OP_INT); pc += 4; // y^2
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd25, 3'b000, 5'd25, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd25, 3'b101, 5'd25, OP_INT_IMM); pc += 4; // >>16

            // if r2 < r2_sphere -> sphere
            br_sphere_pc = pc;
            rom[pc>>2] = b_type(0, 5'd25, 5'd17, 3'b100, OP_BRANCH); pc += 4; // BLT r2, r2_sphere (patched)
            rom[pc>>2] = nop(); pc += 4;

            // ---- Background path (ground/sky based on yndc) ----
            // Branch if yndc < 0 to ground
            br_ground_pc = pc;
            rom[pc>>2] = b_type(0, 5'd10, 5'd0, 3'b100, OP_BRANCH); pc += 4; // BLT yndc,0 (patched)
            rom[pc>>2] = nop(); pc += 4;

            // Sky: color = (0.2,0.3,0.6) -> 0xFF996633
            rom[pc>>2] = u_type(32'hFF_99_6000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h633, 5'd31, 3'b000, 5'd31, OP_INT_IMM); pc += 4; // ADDI low bits
            bg_sky_jmp_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // jump to write (patched)
            rom[pc>>2] = nop(); pc += 4;

            // Ground checker: (xndc>>16 + frame) & 1
            bg_ground_pc = pc;
            rom[pc>>2] = i_type(16, 5'd24, 3'b101, 5'd27, OP_INT_IMM); pc += 4; // srai
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd27, 3'b000, 5'd27, OP_INT); pc += 4; // add frame
            rom[pc>>2] = i_type(1, 5'd27, 3'b111, 5'd27, OP_INT_IMM); pc += 4; // andi 1
            // if checker==0 -> dark else light
            rom[pc>>2] = b_type(12, 5'd27, 5'd0, 3'b001, OP_BRANCH); pc += 4; // BNE checker,0 -> light
            rom[pc>>2] = nop(); pc += 4;
            // dark
            rom[pc>>2] = u_type(32'hFF_20_2000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h020, 5'd31, 3'b000, 5'd31, OP_INT_IMM); pc += 4; // ADDI low bits
            bg_dark_jmp_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // patched to write
            rom[pc>>2] = nop(); pc += 4;
            // light
            rom[pc>>2] = u_type(32'hFF_E0_E000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0E0, 5'd31, 3'b000, 5'd31, OP_INT_IMM); pc += 4; // ADDI low bits
            bg_light_jmp_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // patched to write
            rom[pc>>2] = nop(); pc += 4;

            // ---- Sphere path ----
            sphere_path_pc = pc;
            // z2 = r2_sphere - r2 -> x28
            rom[pc>>2] = r_type(7'b0100000, 5'd25, 5'd17, 3'b000, 5'd28, OP_INT); pc += 4;
            // idx = z2 >> 6 -> x29
            rom[pc>>2] = i_type(6, 5'd28, 3'b101, 5'd29, OP_INT_IMM); pc += 4; // SRAI
            // idx <<= 2 (word offset)
            rom[pc>>2] = i_type(2, 5'd29, 3'b001, 5'd29, OP_INT_IMM); pc += 4; // SLLI
            rom[pc>>2] = r_type(7'b0000000, 5'd22, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4; // base + idx
            rom[pc>>2] = i_type(0, 5'd29, 3'b010, 5'd30, OP_LOAD); pc += 4; // inv
            // z = (z2 * inv) >> 16 -> x29
            rom[pc>>2] = r_type(7'b0000001, 5'd30, 5'd28, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            // inv_radius (Q16.16) ~= 1.6667 for radius 0.6 -> x7 (local scratch)
            rom[pc>>2] = u_type(32'h0001_B000, 5'd7, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(-1365, 5'd7, 3'b000, 5'd7, OP_INT_IMM); pc += 4;
            // Normalize nx, ny, nz by inv_radius into temps (x14, x15, x29)
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd24, 3'b000, 5'd14, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd14, 3'b101, 5'd14, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd10, 3'b000, 5'd15, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd15, 3'b101, 5'd15, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            // tmp = (2 * nz) -> x30
            rom[pc>>2] = i_type(1, 5'd29, 3'b001, 5'd30, OP_INT_IMM); pc += 4;
            // r.x = -(tmp * nx) >> 16 -> x26
            rom[pc>>2] = r_type(7'b0000001, 5'd14, 5'd30, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd26, 5'd0, 3'b000, 5'd26, OP_INT); pc += 4; // neg
            // r.y = -(tmp * ny) >> 16 -> x27
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd30, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd27, 3'b101, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd0, 3'b000, 5'd27, OP_INT); pc += 4; // neg
            // r.z = one - (tmp * nz >>16) -> x30
            rom[pc>>2] = r_type(7'b0000001, 5'd29, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd30, 5'd5, 3'b000, 5'd30, OP_INT); pc += 4; // one - tmp

            // if r.y < 0 -> ground else sky
            br_sphere_ground_pc = pc;
            rom[pc>>2] = b_type(0, 5'd27, 5'd0, 3'b100, OP_BRANCH); pc += 4; // BLT r.y,0 (patched)
            rom[pc>>2] = nop(); pc += 4;

            // sphere sky
            rom[pc>>2] = u_type(32'hFF_99_6000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h633, 5'd31, 3'b000, 5'd31, OP_INT_IMM); pc += 4; // ADDI low bits
            sphere_sky_jmp_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // patched to write
            rom[pc>>2] = nop(); pc += 4;

            // sphere ground checker: (r.x>>16 + r.z>>16 + frame) & 1
            sphere_ground_pc = pc;
            rom[pc>>2] = i_type(16, 5'd26, 3'b101, 5'd28, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(16, 5'd30, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd29, 5'd28, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd28, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = i_type(1, 5'd28, 3'b111, 5'd28, OP_INT_IMM); pc += 4;
            rom[pc>>2] = b_type(12, 5'd28, 5'd0, 3'b001, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = u_type(32'hFF_20_2000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h020, 5'd31, 3'b000, 5'd31, OP_INT_IMM); pc += 4; // ADDI low bits
            sphere_dark_jmp_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // patched to write
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = u_type(32'hFF_E0_E000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0E0, 5'd31, 3'b000, 5'd31, OP_INT_IMM); pc += 4; // ADDI low bits
            sphere_light_jmp_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // patched to write
            rom[pc>>2] = nop(); pc += 4;

            // ---- Mirror tint (vector) ----
            mirror_tint_pc = pc;
            // VUNPACK.FP32 v1, x31 (ARGB8888 -> normalized lanes)
            rom[pc>>2] = r_type(7'b0011110, 5'd31, 5'd0, 3'b011, 5'd1, OP_VEC_ALU); pc += 4;
            // VMUL.FP32 v1 = v1 * v2 (mirror tint)
            rom[pc>>2] = r_type(7'b0011000, 5'd2, 5'd1, 3'b011, 5'd1, OP_VEC_ALU); pc += 4;
            // VPACK.FP32 x31 = pack(v1) -> ARGB8888
            rom[pc>>2] = r_type(7'b0010000, 5'd0, 5'd1, 3'b011, 5'd31, OP_VEC_ALU); pc += 4;

            // Write pixel to FB
            write_pc = pc;
            // addr = row_ptr + (x<<2)
            rom[pc>>2] = i_type(2, 5'd11, 3'b001, 5'd28, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd23, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = s_type(0, 5'd28, 5'd31, 3'b010, OP_STORE); pc += 4;

            // x++
            rom[pc>>2] = i_type(1, 5'd11, 3'b000, 5'd11, OP_INT_IMM); pc += 4;
            blt_x_pc = pc;
            rom[pc>>2] = b_type(0, 5'd11, 5'd19, 3'b100, OP_BRANCH); pc += 4; // BLT x, W-1 (patched)
            rom[pc>>2] = nop(); pc += 4;

            // y++
            rom[pc>>2] = i_type(1, 5'd8, 3'b000, 5'd8, OP_INT_IMM); pc += 4;
            blt_y_pc = pc;
            rom[pc>>2] = b_type(0, 5'd8, 5'd20, 3'b100, OP_BRANCH); pc += 4; // BLT y, H-1 (patched)
            rom[pc>>2] = nop(); pc += 4;

            // Signal frame done
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // MEMBAR
            rom[pc>>2] = s_type(DONE_OFF, 5'd1, 5'd6, 3'b010, OP_STORE); pc += 4;

            // frame++
            rom[pc>>2] = i_type(1, 5'd6, 3'b000, 5'd6, OP_INT_IMM); pc += 4;
            // if (frame < FRAMES) loop
            blt_frame_pc = pc;
            rom[pc>>2] = b_type(0, 5'd6, 5'd13, 3'b100, OP_BRANCH); pc += 4; // patched
            rom[pc>>2] = nop(); pc += 4;

            // Done: WFI loop
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b111, 5'd0, OP_SYSTEM); pc += 4; // WFI
            rom[pc>>2] = b_type(-4, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // -------------------------
            // Patch branches
            // -------------------------
            // Sphere path (branch to sphere_path_pc)
            imm = sphere_path_pc - br_sphere_pc;
            rom[br_sphere_pc>>2] = b_type(imm, 5'd25, 5'd17, 3'b100, OP_BRANCH);

            // Background ground branch
            imm = bg_ground_pc - br_ground_pc;
            rom[br_ground_pc>>2] = b_type(imm, 5'd10, 5'd0, 3'b100, OP_BRANCH);

            // Background sky jump to write
            imm = write_pc - bg_sky_jmp_pc;
            rom[bg_sky_jmp_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);

            // Background dark/light jumps to write
            imm = write_pc - bg_dark_jmp_pc;
            rom[bg_dark_jmp_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = write_pc - bg_light_jmp_pc;
            rom[bg_light_jmp_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);

            // Sphere ground branch
            imm = sphere_ground_pc - br_sphere_ground_pc;
            rom[br_sphere_ground_pc>>2] = b_type(imm, 5'd27, 5'd0, 3'b100, OP_BRANCH);

            // Sphere sky/ground dark/light jumps to mirror tint
            imm = mirror_tint_pc - sphere_sky_jmp_pc;
            rom[sphere_sky_jmp_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = mirror_tint_pc - sphere_dark_jmp_pc;
            rom[sphere_dark_jmp_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = mirror_tint_pc - sphere_light_jmp_pc;
            rom[sphere_light_jmp_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);

            // Patch x loop
            imm = loop_x_pc - blt_x_pc;
            rom[blt_x_pc>>2] = b_type(imm, 5'd11, 5'd19, 3'b100, OP_BRANCH);
            // Patch y loop
            imm = loop_y_pc - blt_y_pc;
            rom[blt_y_pc>>2] = b_type(imm, 5'd8, 5'd20, 3'b100, OP_BRANCH);
            // Patch frame loop
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
        end else begin
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

                    if (mem[mem_index(BASE_ADDR + DONE_OFF)] != 32'h0) begin
                        if (frame_seen < FRAMES) begin
                            dump_fb_ppm(frame_seen);
                            if (frame_seen == (FRAMES - 1)) begin
                                $display("TB: reached %0d frames, finishing.", FRAMES);
                                $finish;
                            end
                            frame_seen <= frame_seen + 1;
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
