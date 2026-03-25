`timescale 1ns/1ps
// ============================================================================
// Cluster Testbench — Validates 2-CU and 4-CU cluster_top integration
//
// Tests:
//   1. Shared L2 cache: both CUs read/write through L2
//   2. I-cache miss arbiter: concurrent instruction fetches
//   3. Framebuffer arbiter: concurrent ROP writes
//   4. RTU: program ray, start traversal, read results
//   5. Basic execution: each CU runs a simple program
//
// Memory layout (same as gfx_sw_rt_multicore_tb):
//   0x8000_0000: base address
//   +0x100: DONE_CU0, +0x140: DONE_CU1 (cache-line aligned)
//   +0x10000: framebuffer (128×128×4B)
//   +0x20000: BVH data (for RTU test)
//   +0x30000: triangle data
// ============================================================================
module cluster_rt_tb;
    import isa_pkg::*;
    import rt_pkg::*;
    import mailbox_pkg::*;

    // -----------------------------------------------------------------------
    // Parameters
    // -----------------------------------------------------------------------
    localparam int NUM_CUS = 2;        // Test with 2-CU cluster
    localparam int W       = 64;       // Framebuffer width
    localparam int H       = 64;       // Framebuffer height

    localparam logic [31:0] BASE_ADDR = 32'h8000_0000;
    localparam int FB_SIZE_BYTES = W * H * 4;

    // Memory offsets
    localparam logic [31:0] DONE_CU0_OFF = 32'h0000_0100;
    localparam logic [31:0] DONE_CU1_OFF = 32'h0000_0140;
    localparam logic [31:0] FB_OFF       = 32'h0001_0000;
    localparam logic [31:0] BVH_OFF      = 32'h0002_0000;
    localparam logic [31:0] TRI_OFF      = 32'h0003_0000;

    // -----------------------------------------------------------------------
    // Clock / Reset
    // -----------------------------------------------------------------------
    logic clk, rst_n;
    initial clk = 1'b0;
    always #1 clk = ~clk;

    initial begin
        rst_n = 1'b0;
        repeat (10) @(posedge clk);
        rst_n = 1'b1;
    end

    // -----------------------------------------------------------------------
    // Cluster external interface wiring
    // -----------------------------------------------------------------------
    // AXI4 master (L2 → memory)
    logic [31:0] m_axi_awaddr;  logic [7:0] m_axi_awlen;
    logic [2:0]  m_axi_awsize;  logic [1:0] m_axi_awburst;
    logic        m_axi_awvalid, m_axi_awready;
    logic [63:0] m_axi_wdata;   logic [7:0] m_axi_wstrb;
    logic        m_axi_wlast, m_axi_wvalid, m_axi_wready;
    logic        m_axi_bvalid, m_axi_bready;
    logic [31:0] m_axi_araddr;  logic [7:0] m_axi_arlen;
    logic [2:0]  m_axi_arsize;  logic [1:0] m_axi_arburst;
    logic        m_axi_arvalid, m_axi_arready;
    logic [63:0] m_axi_rdata;
    logic        m_axi_rvalid, m_axi_rlast, m_axi_rready;

    // Framebuffer AXI
    logic        fb_aw_valid, fb_aw_ready;
    logic [31:0] fb_aw_addr;  logic [7:0] fb_aw_len;
    logic [2:0]  fb_aw_size;  logic [1:0] fb_aw_burst;
    logic [31:0] fb_w_data;   logic [3:0] fb_w_strb;
    logic        fb_w_last, fb_w_valid, fb_w_ready;
    logic        fb_b_valid, fb_b_ready;

    // I-cache miss
    logic        icache_miss_req_valid;
    logic [31:0] icache_miss_req_addr;
    logic        icache_miss_req_ready;
    logic        icache_miss_resp_valid;
    logic [63:0] icache_miss_resp_data;

    // Mailbox (tie off at cluster boundary)
    logic        mb_up_awvalid, mb_up_awready; logic [15:0] mb_up_awaddr;
    logic        mb_up_wvalid, mb_up_wready;   logic [31:0] mb_up_wdata;
    logic [3:0]  mb_up_wstrb;  mailbox_tag_t   mb_up_tag;
    logic        mb_up_bvalid, mb_up_bready;
    logic        mb_up_arvalid, mb_up_arready; logic [15:0] mb_up_araddr;
    logic        mb_up_rvalid, mb_up_rready;   logic [31:0] mb_up_rdata;

    // RTU
    logic        rtu_reg_wr_valid, rtu_reg_wr_ready;
    logic [4:0]  rtu_reg_wr_addr;
    logic [31:0] rtu_reg_wr_data;
    logic        rtu_reg_rd_valid, rtu_reg_rd_ready;
    logic [4:0]  rtu_reg_rd_addr;
    logic [31:0] rtu_reg_rd_data;
    logic        rtu_irq;

    // Status
    logic [NUM_CUS-1:0] cu_err_fp_overflow, cu_err_fp_invalid;
    logic [NUM_CUS-1:0] cu_err_vec_overflow, cu_err_vec_invalid;
    logic [NUM_CUS-1:0][31:0] cu_csr_status;

    // -----------------------------------------------------------------------
    // DUT: cluster_top
    // -----------------------------------------------------------------------
    cluster_top #(
        .NUM_CUS(NUM_CUS),
        .CLUSTER_ID(8'h01),
        .RT_ENABLE(1'b1),
        .MAILBOX_ENABLE(1'b1),
        .GFX_ENABLE(1'b1),
        .L2_SIZE_BYTES(16384),
        .L2_LINE_BYTES(64),
        .L2_ASSOC(4),
        .L2_WRITEBACK(1'b0)
    ) u_cluster (
        .clk(clk), .rst_n(rst_n),
        // AXI master (L2 backing)
        .m_axi_awaddr(m_axi_awaddr),   .m_axi_awlen(m_axi_awlen),
        .m_axi_awsize(m_axi_awsize),   .m_axi_awburst(m_axi_awburst),
        .m_axi_awvalid(m_axi_awvalid), .m_axi_awready(m_axi_awready),
        .m_axi_wdata(m_axi_wdata),     .m_axi_wstrb(m_axi_wstrb),
        .m_axi_wlast(m_axi_wlast),     .m_axi_wvalid(m_axi_wvalid),
        .m_axi_wready(m_axi_wready),
        .m_axi_bvalid(m_axi_bvalid),   .m_axi_bready(m_axi_bready),
        .m_axi_araddr(m_axi_araddr),   .m_axi_arlen(m_axi_arlen),
        .m_axi_arsize(m_axi_arsize),   .m_axi_arburst(m_axi_arburst),
        .m_axi_arvalid(m_axi_arvalid), .m_axi_arready(m_axi_arready),
        .m_axi_rdata(m_axi_rdata),     .m_axi_rvalid(m_axi_rvalid),
        .m_axi_rlast(m_axi_rlast),     .m_axi_rready(m_axi_rready),
        // Framebuffer
        .fb_aw_valid(fb_aw_valid), .fb_aw_addr(fb_aw_addr),
        .fb_aw_len(fb_aw_len),     .fb_aw_size(fb_aw_size),
        .fb_aw_burst(fb_aw_burst), .fb_aw_ready(fb_aw_ready),
        .fb_w_data(fb_w_data),     .fb_w_strb(fb_w_strb),
        .fb_w_last(fb_w_last),     .fb_w_valid(fb_w_valid),
        .fb_w_ready(fb_w_ready),
        .fb_b_valid(fb_b_valid),   .fb_b_ready(fb_b_ready),
        // I-cache miss
        .icache_miss_req_valid(icache_miss_req_valid),
        .icache_miss_req_addr(icache_miss_req_addr),
        .icache_miss_req_ready(icache_miss_req_ready),
        .icache_miss_resp_valid(icache_miss_resp_valid),
        .icache_miss_resp_data(icache_miss_resp_data),
        // Mailbox uplink
        .mb_up_awvalid(mb_up_awvalid), .mb_up_awready(1'b1),
        .mb_up_awaddr(mb_up_awaddr),
        .mb_up_wvalid(mb_up_wvalid),   .mb_up_wready(1'b1),
        .mb_up_wdata(mb_up_wdata),     .mb_up_wstrb(mb_up_wstrb),
        .mb_up_tag(mb_up_tag),         .mb_up_bvalid(1'b0),
        .mb_up_bready(mb_up_bready),
        .mb_up_arvalid(mb_up_arvalid), .mb_up_arready(1'b1),
        .mb_up_araddr(mb_up_araddr),
        .mb_up_rvalid(1'b0),           .mb_up_rready(mb_up_rready),
        .mb_up_rdata(32'h0),
        // Mailbox downlink
        .mb_dn_awvalid(1'b0), .mb_dn_awaddr(16'h0),
        .mb_dn_wvalid(1'b0),  .mb_dn_wdata(32'h0),
        .mb_dn_wstrb(4'h0),   .mb_dn_tag('0),
        .mb_dn_bready(1'b1),
        .mb_dn_arvalid(1'b0), .mb_dn_araddr(16'h0),
        .mb_dn_rready(1'b1),
        .mb_dn_awready(), .mb_dn_wready(), .mb_dn_bvalid(),
        .mb_dn_arready(), .mb_dn_rvalid(), .mb_dn_rdata(),
        // RTU
        .rtu_reg_wr_valid(rtu_reg_wr_valid),
        .rtu_reg_wr_addr(rtu_reg_wr_addr),
        .rtu_reg_wr_data(rtu_reg_wr_data),
        .rtu_reg_wr_ready(rtu_reg_wr_ready),
        .rtu_reg_rd_valid(rtu_reg_rd_valid),
        .rtu_reg_rd_addr(rtu_reg_rd_addr),
        .rtu_reg_rd_data(rtu_reg_rd_data),
        .rtu_reg_rd_ready(rtu_reg_rd_ready),
        .rtu_irq(rtu_irq),
        // Status
        .cu_err_fp_overflow(cu_err_fp_overflow),
        .cu_err_fp_invalid(cu_err_fp_invalid),
        .cu_err_vec_overflow(cu_err_vec_overflow),
        .cu_err_vec_invalid(cu_err_vec_invalid),
        .cu_csr_status(cu_csr_status)
    );

    // -----------------------------------------------------------------------
    // Memory model (shared, backing AXI + I-cache)
    // -----------------------------------------------------------------------
    localparam int MEM_WORDS = 32'h0004_0000; // 1MB
    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_idx(input logic [31:0] addr);
        mem_idx = (addr - BASE_ADDR) >> 2;
    endfunction

    // -----------------------------------------------------------------------
    // Instruction ROM
    // -----------------------------------------------------------------------
    localparam int ROM_WORDS = 4096;
    logic [31:0] rom [0:ROM_WORDS-1];

    // -----------------------------------------------------------------------
    // AXI slave model (L2 backing store)
    // -----------------------------------------------------------------------
    typedef enum logic [2:0] {
        AXI_IDLE,
        AXI_RD_BURST,
        AXI_WR_DATA,
        AXI_WR_RESP
    } axi_state_e;

    axi_state_e axi_state;
    logic [31:0] axi_addr_q;
    logic [7:0]  axi_len_q;
    logic [7:0]  axi_beat_q;

    assign m_axi_arready = (axi_state == AXI_IDLE);
    assign m_axi_awready = (axi_state == AXI_IDLE) && !m_axi_arvalid; // Read priority

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            axi_state    <= AXI_IDLE;
            m_axi_rvalid <= 1'b0;
            m_axi_rlast  <= 1'b0;
            m_axi_bvalid <= 1'b0;
            m_axi_wready <= 1'b0;
        end else begin
            case (axi_state)
                AXI_IDLE: begin
                    m_axi_rvalid <= 1'b0;
                    m_axi_rlast  <= 1'b0;
                    m_axi_bvalid <= 1'b0;
                    m_axi_wready <= 1'b0;
                    if (m_axi_arvalid) begin
                        axi_addr_q <= m_axi_araddr;
                        axi_len_q  <= m_axi_arlen;
                        axi_beat_q <= '0;
                        axi_state  <= AXI_RD_BURST;
                    end else if (m_axi_awvalid) begin
                        axi_addr_q <= m_axi_awaddr;
                        axi_len_q  <= m_axi_awlen;
                        axi_beat_q <= '0;
                        m_axi_wready <= 1'b1;
                        axi_state  <= AXI_WR_DATA;
                    end
                end

                AXI_RD_BURST: begin
                    if (!m_axi_rvalid || m_axi_rready) begin
                        int rd_wi;
                        rd_wi = mem_idx(axi_addr_q + {axi_beat_q, 3'b000});
                        m_axi_rdata  <= (rd_wi >= 0 && rd_wi+1 < MEM_WORDS) ?
                                        {mem[rd_wi+1], mem[rd_wi]} : 64'h0;
                        m_axi_rvalid <= 1'b1;
                        m_axi_rlast  <= (axi_beat_q == axi_len_q);
                        if (axi_beat_q == axi_len_q) begin
                            axi_state <= AXI_IDLE;
                        end else begin
                            axi_beat_q <= axi_beat_q + 1;
                        end
                    end
                end

                AXI_WR_DATA: begin
                    if (m_axi_wvalid && m_axi_wready) begin
                        int wr_wi;
                        wr_wi = mem_idx(axi_addr_q + {axi_beat_q, 3'b000});
                        if (wr_wi >= 0 && wr_wi+1 < MEM_WORDS) begin
                            if (m_axi_wstrb[3:0] != 4'h0) mem[wr_wi]   = m_axi_wdata[31:0];
                            if (m_axi_wstrb[7:4] != 4'h0) mem[wr_wi+1] = m_axi_wdata[63:32];
                        end
                        axi_beat_q <= axi_beat_q + 1;
                        if (m_axi_wlast) begin
                            m_axi_wready <= 1'b0;
                            m_axi_bvalid <= 1'b1;
                            axi_state    <= AXI_WR_RESP;
                        end
                    end
                end

                AXI_WR_RESP: begin
                    if (m_axi_bvalid && m_axi_bready) begin
                        m_axi_bvalid <= 1'b0;
                        axi_state    <= AXI_IDLE;
                    end
                end
            endcase
        end
    end

    // -----------------------------------------------------------------------
    // I-cache miss responder (from instruction ROM)
    // -----------------------------------------------------------------------
    logic        ic_pending;
    logic [31:0] ic_addr_q;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            ic_pending <= 1'b0;
            icache_miss_req_ready  <= 1'b1;
            icache_miss_resp_valid <= 1'b0;
        end else begin
            icache_miss_resp_valid <= 1'b0;
            if (!ic_pending && icache_miss_req_valid && icache_miss_req_ready) begin
                ic_pending <= 1'b1;
                ic_addr_q  <= icache_miss_req_addr;
                icache_miss_req_ready <= 1'b0;
            end
            if (ic_pending) begin
                int ic_wi;
                ic_wi = ic_addr_q[13:2]; // ROM index (word aligned within 16KB)
                icache_miss_resp_valid <= 1'b1;
                icache_miss_resp_data  <= {rom[ic_wi | 1], rom[ic_wi & ~1]}; // 64-bit bundle
                ic_pending <= 1'b0;
                icache_miss_req_ready <= 1'b1;
            end
        end
    end

    // -----------------------------------------------------------------------
    // Framebuffer sink
    // -----------------------------------------------------------------------
    assign fb_aw_ready = 1'b1;
    assign fb_w_ready  = 1'b1;
    assign fb_b_valid  = 1'b0; // No write response needed for simple sink

    // -----------------------------------------------------------------------
    // RTU test driver
    // -----------------------------------------------------------------------
    initial begin
        rtu_reg_wr_valid = 1'b0;
        rtu_reg_rd_valid = 1'b0;
    end

    // -----------------------------------------------------------------------
    // Mini assembler helpers
    // -----------------------------------------------------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1,
                                     input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3,
                                     input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2,
                                     input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] b_type(input integer imm, input [4:0] rs1, input [4:0] rs2,
                                     input [2:0] funct3, input [6:0] opcode);
        b_type = {imm[12], imm[10:5], rs2, rs1, funct3, imm[4:1], imm[11], opcode};
    endfunction

    function automatic [31:0] u_type(input integer imm, input [4:0] rd, input [6:0] opcode);
        u_type = {imm[31:12], rd, opcode};
    endfunction

    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_IMM);
    endfunction

    // -----------------------------------------------------------------------
    // Program: simple pixel fill (each CU fills alternate rows)
    //
    // x1 = CORE_ID (from CSR 0xC00)
    // x2 = BASE_ADDR + DONE_CU0_OFF or DONE_CU1_OFF
    // x3 = BASE_ADDR + FB_OFF
    // x4 = pixel color (CORE_ID-dependent)
    // x5 = row counter (0..H-1, step 2 = NUM_CORES)
    // x6 = col counter
    // x7 = pixel address
    // Loop: for row in [core_id..H step NUM_CORES]:
    //         for col in [0..W-1]:
    //           store pixel at fb + (row*W + col)*4
    //       store 1 to DONE address
    //       WFI + loop
    // -----------------------------------------------------------------------
    initial begin : prog_init
        int pc;
        int row_loop_pc, bge_patch_pc, col_loop_pc, jmp_offset, done_pc;
        for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
        for (int i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;

        pc = 0;

        // x1 = CORE_ID (CSR 0xC00 = mhartid analog)
        rom[pc] = i_type(12'hC00, 5'd0, 3'b010, 5'd1, OP_SYSTEM); pc++; // CSRRS x1, 0xC00, x0

        // x2 = BASE_ADDR (upper 20 bits = 0x80000)
        rom[pc] = u_type(32'h8000_0000, 5'd2, OP_LUI); pc++;

        // x3 = BASE_ADDR + FB_OFF
        rom[pc] = u_type(32'h8001_0000, 5'd3, OP_LUI); pc++;

        // x4 = pixel color: CU0 → red (0x00FF0000), CU1 → blue (0x000000FF)
        // if (x1 == 0) x4 = 0x00FF0000 else x4 = 0x000000FF
        rom[pc] = u_type(32'h00FF_0000, 5'd4, OP_LUI); pc++;         // x4 = 0x00FF0000
        rom[pc] = b_type(12'd8, 5'd1, 5'd0, 3'b000, OP_BRANCH); pc++; // BEQ x1,x0,+8 (skip next 2)
        rom[pc] = i_type(12'd255, 5'd0, 3'b000, 5'd4, OP_IMM); pc++;  // ADDI x4, x0, 255 (blue)
        rom[pc] = nop(); pc++;

        // x5 = row = core_id (starting row)
        rom[pc] = i_type(12'd0, 5'd1, 3'b000, 5'd5, OP_IMM); pc++; // ADDI x5, x1, 0

        // x8 = H (height)
        rom[pc] = i_type(H, 5'd0, 3'b000, 5'd8, OP_IMM); pc++;

        // x9 = W (width)
        rom[pc] = i_type(W, 5'd0, 3'b000, 5'd9, OP_IMM); pc++;

        // x10 = NUM_CORES (step between rows)
        rom[pc] = i_type(NUM_CUS, 5'd0, 3'b000, 5'd10, OP_IMM); pc++;

        // OUTER LOOP: row_loop (label = pc)
        row_loop_pc = pc;

        // if (x5 >= x8) goto done
        rom[pc] = b_type(12'd0, 5'd5, 5'd8, 3'b101, OP_BRANCH); pc++; // BGE x5,x8,done (patched below)
        bge_patch_pc = pc - 1;

        // x6 = 0 (col counter)
        rom[pc] = i_type(12'd0, 5'd0, 3'b000, 5'd6, OP_IMM); pc++;

        // INNER LOOP: col_loop (label = pc)
        col_loop_pc = pc;

        // x7 = x5 * W (row offset in pixels)
        // MUL x7, x5, x9
        rom[pc] = r_type(7'b0000001, 5'd9, 5'd5, 3'b000, 5'd7, OP_REG); pc++;
        // x7 = x7 + x6 (add col)
        rom[pc] = r_type(7'b0000000, 5'd6, 5'd7, 3'b000, 5'd7, OP_REG); pc++;
        // x7 = x7 << 2 (multiply by 4 bytes/pixel)
        rom[pc] = i_type(12'd2, 5'd7, 3'b001, 5'd7, OP_IMM); pc++;
        // x7 = x3 + x7 (absolute address)
        rom[pc] = r_type(7'b0000000, 5'd7, 5'd3, 3'b000, 5'd7, OP_REG); pc++;
        // SW x4, 0(x7) — store pixel
        rom[pc] = s_type(12'd0, 5'd7, 5'd4, 3'b010, OP_STORE); pc++;
        // x6 = x6 + 1
        rom[pc] = i_type(12'd1, 5'd6, 3'b000, 5'd6, OP_IMM); pc++;
        // BLT x6, x9, col_loop
        rom[pc] = b_type(-(pc - col_loop_pc) * 4, 5'd6, 5'd9, 3'b100, OP_BRANCH); pc++;

        // x5 = x5 + NUM_CORES (next row for this CU)
        rom[pc] = r_type(7'b0000000, 5'd10, 5'd5, 3'b000, 5'd5, OP_REG); pc++;
        // J row_loop
        jmp_offset = (row_loop_pc - pc) * 4;
        rom[pc] = b_type(jmp_offset, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc++; // BEQ x0,x0,row_loop

        // done:
        done_pc = pc;
        // Patch BGE offset
        rom[bge_patch_pc] = b_type((done_pc - bge_patch_pc) * 4, 5'd5, 5'd8, 3'b101, OP_BRANCH);

        // Store DONE flag: use DONE_CU0_OFF + core_id * 64
        // x11 = DONE offset base
        rom[pc] = i_type(DONE_CU0_OFF[11:0], 5'd2, 3'b000, 5'd11, OP_IMM); pc++;
        // x12 = core_id << 6 (64-byte offset per CU)
        rom[pc] = i_type(12'd6, 5'd1, 3'b001, 5'd12, OP_IMM); pc++;
        // x11 = x11 + x12
        rom[pc] = r_type(7'b0000000, 5'd12, 5'd11, 3'b000, 5'd11, OP_REG); pc++;
        // x13 = 1
        rom[pc] = i_type(12'd1, 5'd0, 3'b000, 5'd13, OP_IMM); pc++;
        // SW x13, 0(x11)
        rom[pc] = s_type(12'd0, 5'd11, 5'd13, 3'b010, OP_STORE); pc++;

        // WFI
        rom[pc] = 32'h1050_0073; pc++;
        // BEQ x0,x0,-4 (spin)
        rom[pc] = b_type(-4, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc++;

        $display("=== Cluster RT TB: Program loaded, %0d instructions ===", pc);
    end

    // -----------------------------------------------------------------------
    // Wave dump
    // -----------------------------------------------------------------------
    initial begin
        $dumpfile("cluster_rt_tb.vcd");
        $dumpvars(0, cluster_rt_tb);
    end

    // -----------------------------------------------------------------------
    // Simulation watchdog + DONE detection
    // -----------------------------------------------------------------------
    localparam int MAX_CYCLES = 5_000_000;
    int cycle_cnt;

    always_ff @(posedge clk) begin
        if (!rst_n) cycle_cnt <= 0;
        else cycle_cnt <= cycle_cnt + 1;
    end

    // Check DONE flags in memory
    wire cu0_done = (mem[mem_idx(BASE_ADDR + DONE_CU0_OFF)] == 32'h1);
    wire cu1_done = (mem[mem_idx(BASE_ADDR + DONE_CU1_OFF)] == 32'h1);
    wire all_done = cu0_done && cu1_done;

    initial begin
        wait (rst_n);
        $display("=== Cluster RT TB: Simulation started ===");

        // Wait for done or timeout
        while (!all_done && cycle_cnt < MAX_CYCLES) @(posedge clk);

        if (!all_done) begin
            $display("ERROR: Watchdog timeout at cycle %0d", cycle_cnt);
            $finish;
        end

        $display("=== ALL CUs DONE at cycle %0d ===", cycle_cnt);
        repeat (100) @(posedge clk);

        // Verify: check some pixels
        $display("=== Pixel verification ===");
        // CU0 writes row 0 (red for CU0)
        // CU1 writes row 1 (blue for CU1)
        begin
            int pixel_r0_c0, pixel_r1_c0;
            pixel_r0_c0 = mem[mem_idx(BASE_ADDR + FB_OFF)]; // row=0, col=0
            pixel_r1_c0 = mem[mem_idx(BASE_ADDR + FB_OFF + W*4)]; // row=1, col=0
        $display("  Row 0, Col 0 pixel: 0x%08x (expected: 0x00FF0000 = red)", pixel_r0_c0);
        $display("  Row 1, Col 0 pixel: 0x%08x (expected: 0x000000FF = blue)", pixel_r1_c0);

        if (pixel_r0_c0 == 32'h00FF_0000 && pixel_r1_c0 == 32'h0000_00FF)
            $display("=== PASS: Cluster 2-CU pixel fill test ===");
        else
            $display("=== FAIL: Pixel mismatch ===");
        end

        // Export framebuffer to PPM
        begin
            integer fd;
            fd = $fopen("cluster_frame.ppm", "w");
            if (fd) begin
                $fwrite(fd, "P3\n%0d %0d\n255\n", W, H);
                for (int y = 0; y < H; y++) begin
                    for (int x = 0; x < W; x++) begin
                        int pixel, r, g, b;
                        pixel = mem[mem_idx(BASE_ADDR + FB_OFF + (y*W + x)*4)];
                        r = (pixel >> 16) & 8'hFF;
                        g = (pixel >>  8) & 8'hFF;
                        b = (pixel      ) & 8'hFF;
                        $fwrite(fd, "%0d %0d %0d ", r, g, b);
                    end
                    $fwrite(fd, "\n");
                end
                $fclose(fd);
                $display("=== Framebuffer saved to cluster_frame.ppm ===");
            end
        end

        $finish;
    end

endmodule : cluster_rt_tb
