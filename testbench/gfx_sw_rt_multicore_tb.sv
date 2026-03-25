`timescale 1ns/1ps

// ============================================================================
// Multi-core Software Ray-Tracing Testbench — Cluster (2CU + 1MAT + 1RT)
//
// Demonstrates CU-driven heterogeneous cluster operation: CU0 firmware
// programs MXU and RTU hardware accelerators via the mailbox fabric
// using the 2-flit register-write protocol, simulating a real-world
// GPU application where shader code dispatches to fixed-function units.
//
//   Slot 0 = CU0  — Renders even rows; programs MXU+RTU via mailbox
//   Slot 1 = CU1  — Renders odd rows of framebuffer
//   Slot 2 = MAT  — Computes 4×4 rotation matrix (CU0-dispatched)
//   Slot 3 = RT   — Validates ray-triangle intersection (CU0-dispatched)
//
// CU0 pre-frame pipeline (firmware, before frame loop):
//   1. MXU: rotation matrix × identity, poll completion via RX FIFO
//   2. RTU forward ray: origin(0,0,0) dir(0,0,1) → expect hit tri_id=1
//   3. RTU backward ray: dir(0,0,-1) → expect miss
//   4. Store completion payloads to shared memory for TB verification
//
// Tests:
//   - MXU payload: non-zero tiles & ops in completion flit
//   - MXU matrix:  C == A (rotation × identity)
//   - RTU forward: hit_flag=1, tri_id=1
//   - RTU backward: hit_flag=0
//   - Frames 0-2:  Both CUs complete DONE
//   - Perf counters: RTU_RAYS=2, MXU_TILES=1
//
// Memory layout (AXI backing store, base = 0x8000_0000):
//   0x0000_0100: CU0 DONE flag  (cache-line aligned)
//   0x0000_0140: CU1 DONE flag  (cache-line aligned)
//   0x0000_0180: MXU completion result (CU0 writes)
//   0x0000_01C0: RTU forward result   (CU0 writes)
//   0x0000_01C4: RTU backward result  (CU0 writes)
//   0x0000_2000: sin/cos LUT
//   0x0000_3000: inverse sqrt LUT
//   0x0000_4000: vector constants
//   0x0000_6000: perspective LUT
//   0x0000_8000: BVH root node  (64B aligned)
//   0x0000_8040: Left leaf       (64B aligned)
//   0x0000_8080: Right leaf      (64B aligned)
//   0x0000_8100: Triangle 0      (64B aligned)
//   0x0000_8140: Triangle 1      (64B aligned)
//   0x0000_9000: MXU matrix A    (64B)
//   0x0000_9040: MXU matrix B    (64B)
//   0x0000_9080: MXU matrix C    (64B)
//   0x0001_0000: Framebuffer     (128×128×4 = 64KB)
// ============================================================================
module gfx_sw_rt_multicore_tb;
    import isa_pkg::*;
    import rt_pkg::*;
    import mat_pkg::*;
    import mailbox_pkg::*;

    // -----------------------------------------------------------------------
    // Tunables
    // -----------------------------------------------------------------------
    localparam int NUM_CORES = 2;
    localparam int W         = 128;
    localparam int H         = 128;
    localparam int FRAMES    = 3;

    localparam logic [31:0] BASE_ADDR = 32'h8000_0000;

    // Memory layout offsets
    localparam logic [31:0] DONE_CU0_OFF    = 32'h0000_0100;
    localparam logic [31:0] DONE_CU1_OFF    = 32'h0000_0140;
    localparam logic [31:0] MXU_RESULT_OFF  = 32'h0000_0180;
    localparam logic [31:0] RTU_FWD_OFF     = 32'h0000_01C0;
    localparam logic [31:0] RTU_BWD_OFF     = 32'h0000_01C4;
    localparam logic [31:0] SINCOS_OFF      = 32'h0000_2000;
    localparam logic [31:0] INVSQRT_OFF     = 32'h0000_3000;
    localparam logic [31:0] VEC_CONST_OFF   = 32'h0000_4000;
    localparam logic [31:0] PERSP_OFF       = 32'h0000_6000;
    localparam logic [31:0] BVH_OFF         = 32'h0000_8000;
    localparam logic [31:0] MXU_SCRATCH_OFF = 32'h0000_9000;
    localparam logic [31:0] FB_OFF          = 32'h0001_0000;

    // Slot indices
    localparam int CU0_SLOT = 0;
    localparam int CU1_SLOT = 1;
    localparam int MAT_SLOT = 2;
    localparam int RT_SLOT  = 3;
    localparam int NUM_SLOTS = 4;

    // AXI parameters
    localparam int AXI_ADDR_W = 32;
    localparam int AXI_DATA_W = 64;

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
    // DUT wiring
    // -----------------------------------------------------------------------
    // AXI4 — L2 backing store
    logic [AXI_ADDR_W-1:0]   m_axi_awaddr;
    logic [7:0]               m_axi_awlen;
    logic [2:0]               m_axi_awsize;
    logic [1:0]               m_axi_awburst;
    logic                     m_axi_awvalid, m_axi_awready;
    logic [AXI_DATA_W-1:0]   m_axi_wdata;
    logic [AXI_DATA_W/8-1:0] m_axi_wstrb;
    logic                     m_axi_wlast, m_axi_wvalid, m_axi_wready;
    logic                     m_axi_bvalid, m_axi_bready;
    logic [AXI_ADDR_W-1:0]   m_axi_araddr;
    logic [7:0]               m_axi_arlen;
    logic [2:0]               m_axi_arsize;
    logic [1:0]               m_axi_arburst;
    logic                     m_axi_arvalid, m_axi_arready;
    logic [AXI_DATA_W-1:0]   m_axi_rdata;
    logic                     m_axi_rvalid, m_axi_rlast, m_axi_rready;

    // AXI4 — Framebuffer
    logic        fb_aw_valid, fb_aw_ready;
    logic [31:0] fb_aw_addr;
    logic [7:0]  fb_aw_len;
    logic [2:0]  fb_aw_size;
    logic [1:0]  fb_aw_burst;
    logic [31:0] fb_w_data;
    logic [3:0]  fb_w_strb;
    logic        fb_w_last, fb_w_valid, fb_w_ready;
    logic        fb_b_valid, fb_b_ready;

    // I-cache miss
    logic        icache_miss_req_valid, icache_miss_req_ready;
    logic [31:0] icache_miss_req_addr;
    logic        icache_miss_resp_valid;
    logic [63:0] icache_miss_resp_data;

    // Mailbox uplink
    mailbox_flit_t                  mb_up_tx_data;
    logic [NODE_ID_WIDTH-1:0]       mb_up_tx_dest_id;
    logic                           mb_up_tx_valid, mb_up_tx_ready;
    mailbox_flit_t                  mb_up_rx_data;
    logic [NODE_ID_WIDTH-1:0]       mb_up_rx_dest_id;
    logic                           mb_up_rx_valid, mb_up_rx_ready;

    // Slot register interface
    logic [NUM_SLOTS-1:0]        slot_reg_wr_valid;
    logic [4:0]                  slot_reg_wr_addr;
    logic [31:0]                 slot_reg_wr_data;
    logic [NUM_SLOTS-1:0]        slot_reg_wr_ready;
    logic [NUM_SLOTS-1:0]        slot_reg_rd_valid;
    logic [4:0]                  slot_reg_rd_addr;
    logic [NUM_SLOTS-1:0][31:0]  slot_reg_rd_data;
    logic [NUM_SLOTS-1:0]        slot_reg_rd_ready;
    logic [NUM_SLOTS-1:0]        slot_irq;
    logic [NUM_SLOTS-1:0][31:0]  slot_csr_status;

    // -----------------------------------------------------------------------
    // DUT: cluster_top (2CU + 1MAT + 1RT)
    // -----------------------------------------------------------------------
    cluster_top #(
        .NUM_SLOTS      (4),
        .SLOT_TYPE_0    (0),   // CU
        .SLOT_TYPE_1    (0),   // CU
        .SLOT_TYPE_2    (2),   // MAT
        .SLOT_TYPE_3    (1),   // RT
        .CLUSTER_ID     (8'h01),
        .MAILBOX_ENABLE (1'b1),
        .GFX_ENABLE     (1'b0),
        .L2_SIZE_BYTES  (32768),
        .L2_LINE_BYTES  (64),
        .L2_ASSOC       (4),
        .AXI_ADDR_W     (AXI_ADDR_W),
        .AXI_DATA_W     (AXI_DATA_W)
    ) u_cluster (
        .clk              (clk),
        .rst_n            (rst_n),
        .m_axi_awaddr     (m_axi_awaddr),
        .m_axi_awlen      (m_axi_awlen),
        .m_axi_awsize     (m_axi_awsize),
        .m_axi_awburst    (m_axi_awburst),
        .m_axi_awvalid    (m_axi_awvalid),
        .m_axi_awready    (m_axi_awready),
        .m_axi_wdata      (m_axi_wdata),
        .m_axi_wstrb      (m_axi_wstrb),
        .m_axi_wlast      (m_axi_wlast),
        .m_axi_wvalid     (m_axi_wvalid),
        .m_axi_wready     (m_axi_wready),
        .m_axi_bvalid     (m_axi_bvalid),
        .m_axi_bready     (m_axi_bready),
        .m_axi_araddr     (m_axi_araddr),
        .m_axi_arlen      (m_axi_arlen),
        .m_axi_arsize     (m_axi_arsize),
        .m_axi_arburst    (m_axi_arburst),
        .m_axi_arvalid    (m_axi_arvalid),
        .m_axi_arready    (m_axi_arready),
        .m_axi_rdata      (m_axi_rdata),
        .m_axi_rvalid     (m_axi_rvalid),
        .m_axi_rlast      (m_axi_rlast),
        .m_axi_rready     (m_axi_rready),
        .fb_aw_valid      (fb_aw_valid),
        .fb_aw_addr       (fb_aw_addr),
        .fb_aw_len        (fb_aw_len),
        .fb_aw_size       (fb_aw_size),
        .fb_aw_burst      (fb_aw_burst),
        .fb_aw_ready      (fb_aw_ready),
        .fb_w_data        (fb_w_data),
        .fb_w_strb        (fb_w_strb),
        .fb_w_last        (fb_w_last),
        .fb_w_valid       (fb_w_valid),
        .fb_w_ready       (fb_w_ready),
        .fb_b_valid       (fb_b_valid),
        .fb_b_ready       (fb_b_ready),
        .icache_miss_req_valid  (icache_miss_req_valid),
        .icache_miss_req_addr   (icache_miss_req_addr),
        .icache_miss_req_ready  (icache_miss_req_ready),
        .icache_miss_resp_valid (icache_miss_resp_valid),
        .icache_miss_resp_data  (icache_miss_resp_data),
        .mb_up_tx_valid   (mb_up_tx_valid),
        .mb_up_tx_ready   (mb_up_tx_ready),
        .mb_up_tx_data    (mb_up_tx_data),
        .mb_up_tx_dest_id (mb_up_tx_dest_id),
        .mb_up_rx_valid   (mb_up_rx_valid),
        .mb_up_rx_ready   (mb_up_rx_ready),
        .mb_up_rx_data    (mb_up_rx_data),
        .mb_up_rx_dest_id (mb_up_rx_dest_id),
        .slot_reg_wr_valid  (slot_reg_wr_valid),
        .slot_reg_wr_addr   (slot_reg_wr_addr),
        .slot_reg_wr_data   (slot_reg_wr_data),
        .slot_reg_wr_ready  (slot_reg_wr_ready),
        .slot_reg_rd_valid  (slot_reg_rd_valid),
        .slot_reg_rd_addr   (slot_reg_rd_addr),
        .slot_reg_rd_data   (slot_reg_rd_data),
        .slot_reg_rd_ready  (slot_reg_rd_ready),
        .slot_irq           (slot_irq),
        .slot_csr_status    (slot_csr_status)
    );

    // -----------------------------------------------------------------------
    // AXI Memory Model (512 KB)
    // -----------------------------------------------------------------------
    localparam int MEM_WORDS = 131072;   // 512K / 4
    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mw(input logic [31:0] addr);
        mw = (addr & 32'h0007_FFFC) >> 2;
    endfunction

    // AXI read channel
    typedef enum logic [1:0] { AR_IDLE, AR_BURST } axi_rd_state_e;
    axi_rd_state_e ar_state;
    logic [31:0] ar_addr;
    logic [7:0]  ar_cnt, ar_len;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            ar_state      <= AR_IDLE;
            m_axi_arready <= 1'b1;
            m_axi_rvalid  <= 1'b0;
            m_axi_rlast   <= 1'b0;
        end else begin
            case (ar_state)
                AR_IDLE: begin
                    m_axi_rvalid <= 1'b0;
                    m_axi_rlast  <= 1'b0;
                    if (m_axi_arvalid && m_axi_arready) begin
                        ar_addr       <= m_axi_araddr;
                        ar_len        <= m_axi_arlen;
                        ar_cnt        <= '0;
                        m_axi_arready <= 1'b0;
                        ar_state      <= AR_BURST;
                    end
                end
                AR_BURST: begin
                    if (!m_axi_rvalid || m_axi_rready) begin
                        int wi;
                        wi = mw(ar_addr);
                        m_axi_rvalid <= 1'b1;
                        m_axi_rdata  <= {mem[wi+1], mem[wi]};
                        m_axi_rlast  <= (ar_cnt == ar_len);
                        ar_addr      <= ar_addr + 8;
                        ar_cnt       <= ar_cnt + 1;
                        if (ar_cnt == ar_len) begin
                            ar_state      <= AR_IDLE;
                            m_axi_arready <= 1'b1;
                        end
                    end
                end
            endcase
        end
    end

    // AXI write channel
    typedef enum logic [1:0] { AW_IDLE, AW_DATA, AW_RESP } axi_wr_state_e;
    axi_wr_state_e aw_state;
    logic [31:0] aw_addr_reg;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aw_state      <= AW_IDLE;
            m_axi_awready <= 1'b1;
            m_axi_wready  <= 1'b0;
            m_axi_bvalid  <= 1'b0;
        end else begin
            case (aw_state)
                AW_IDLE: begin
                    m_axi_bvalid <= 1'b0;
                    if (m_axi_awvalid && m_axi_awready) begin
                        aw_addr_reg   <= m_axi_awaddr;
                        m_axi_awready <= 1'b0;
                        m_axi_wready  <= 1'b1;
                        aw_state      <= AW_DATA;
                    end
                end
                AW_DATA: begin
                    if (m_axi_wvalid && m_axi_wready) begin
                        int wi;
                        wi = mw(aw_addr_reg);
                        if (m_axi_wstrb[0]) mem[wi]   <= m_axi_wdata[31:0];
                        if (m_axi_wstrb[4]) mem[wi+1] <= m_axi_wdata[63:32];
                        aw_addr_reg <= aw_addr_reg + 8;
                        if (m_axi_wlast) begin
                            m_axi_wready <= 1'b0;
                            m_axi_bvalid <= 1'b1;
                            aw_state     <= AW_RESP;
                        end
                    end
                end
                AW_RESP: begin
                    if (m_axi_bready) begin
                        m_axi_bvalid  <= 1'b0;
                        m_axi_awready <= 1'b1;
                        aw_state      <= AW_IDLE;
                    end
                end
            endcase
        end
    end

    // -----------------------------------------------------------------------
    // I-Cache Miss Responder (serves ROM to CUs)
    // -----------------------------------------------------------------------
    localparam int ROM_WORDS = 4096;
    logic [31:0] rom [0:ROM_WORDS-1];

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            icache_miss_req_ready  <= 1'b1;
            icache_miss_resp_valid <= 1'b0;
        end else begin
            icache_miss_resp_valid <= 1'b0;
            if (icache_miss_req_valid && icache_miss_req_ready) begin
                int wi;
                wi = (icache_miss_req_addr >> 2) & (ROM_WORDS - 1);
                icache_miss_resp_valid <= 1'b1;
                icache_miss_resp_data  <= {rom[wi | 1], rom[wi & ~1]};
            end
        end
    end

    // -----------------------------------------------------------------------
    // FB AXI write responder (captures pixels to mem[])
    // -----------------------------------------------------------------------
    logic fb_burst_active;
    logic [31:0] fb_burst_addr;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            fb_aw_ready     <= 1'b1;
            fb_w_ready      <= 1'b0;
            fb_b_valid      <= 1'b0;
            fb_burst_active <= 1'b0;
        end else begin
            fb_b_valid <= 1'b0;
            if (!fb_burst_active) begin
                if (fb_aw_valid && fb_aw_ready) begin
                    fb_burst_addr   <= fb_aw_addr;
                    fb_burst_active <= 1'b1;
                    fb_aw_ready     <= 1'b0;
                    fb_w_ready      <= 1'b1;
                end
            end else begin
                if (fb_w_valid && fb_w_ready) begin
                    int wi;
                    wi = mw(fb_burst_addr);
                    mem[wi] <= fb_w_data;
                    fb_burst_addr <= fb_burst_addr + 4;
                    if (fb_w_last) begin
                        fb_burst_active <= 1'b0;
                        fb_w_ready      <= 1'b0;
                        fb_aw_ready     <= 1'b1;
                        fb_b_valid      <= 1'b1;
                    end
                end
            end
        end
    end

    // Mailbox uplink tie-offs
    assign mb_up_tx_ready   = 1'b1;
    assign mb_up_rx_valid   = 1'b0;
    assign mb_up_rx_data    = '0;
    assign mb_up_rx_dest_id = '0;

    // -----------------------------------------------------------------------
    // Wave dump
    // -----------------------------------------------------------------------
    initial begin
        $dumpfile("gfx_sw_rt_multicore_tb.vcd");
        $dumpvars(0, gfx_sw_rt_multicore_tb);
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

    // Q16.16 helper
    function automatic logic [31:0] fxp(input int int_part, input int frac_65536);
        logic signed [31:0] val;
        val = (int_part <<< 16) + frac_65536;
        return val;
    endfunction

    // -----------------------------------------------------------------------
    // Slot register interface helpers
    // -----------------------------------------------------------------------
    task automatic slot_write(input int slot, input [4:0] addr, input [31:0] data);
        @(posedge clk);
        slot_reg_wr_valid       <= '0;
        slot_reg_wr_valid[slot] <= 1'b1;
        slot_reg_wr_addr        <= addr;
        slot_reg_wr_data        <= data;
        @(posedge clk);
        slot_reg_wr_valid       <= '0;
    endtask

    task automatic slot_read(input int slot, input [4:0] addr, output logic [31:0] data);
        @(posedge clk);
        slot_reg_rd_valid       <= '0;
        slot_reg_rd_valid[slot] <= 1'b1;
        slot_reg_rd_addr        <= addr;
        @(posedge clk);
        data = slot_reg_rd_data[slot];
        slot_reg_rd_valid       <= '0;
    endtask

    task automatic wait_slot_irq(input int slot, input int max_cycles);
        int cnt;
        cnt = 0;
        while (!slot_irq[slot] && cnt < max_cycles) begin
            @(posedge clk);
            cnt++;
        end
        if (cnt >= max_cycles)
            $display("ERROR: Slot %0d IRQ timeout after %0d cycles", slot, max_cycles);
        repeat (30) @(posedge clk);
    endtask

    // -----------------------------------------------------------------------
    // Memory initialization (LUTs + perspective table)
    // -----------------------------------------------------------------------
    task automatic init_memory();
        int i, ti, idx;
        real ang;
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
                mem[mw(BASE_ADDR + SINCOS_OFF + (ti * 8) + 0)] = $signed(s_q);
                mem[mw(BASE_ADDR + SINCOS_OFF + (ti * 8) + 4)] = $signed(c_q);
            end

            // Inverse sqrt LUT [0..1] in Q16.16, 1024 entries
            for (ti = 0; ti < 1024; ti++) begin
                real x_val, inv;
                x_val = (ti / 1023.0);
                if (x_val < 1e-6) inv = 0.0; else inv = 1.0 / $sqrt(x_val);
                idx = mw(BASE_ADDR + INVSQRT_OFF + (ti * 4));
                mem[idx] = $rtoi(inv * 65536.0);
            end

            // Vector constants for mirror tint: (0.80, 0.85, 0.95, 1.00)
            mem[mw(BASE_ADDR + VEC_CONST_OFF + 0)]  = 32'h3F4CCCCD;
            mem[mw(BASE_ADDR + VEC_CONST_OFF + 4)]  = 32'h3F59999A;
            mem[mw(BASE_ADDR + VEC_CONST_OFF + 8)]  = 32'h3F733333;
            mem[mw(BASE_ADDR + VEC_CONST_OFF + 12)] = 32'h3F800000;

            // Perspective LUT: for each row y, pz = 0.5 / |yndc(y)| in Q16.16
            for (ti = 0; ti < H; ti++) begin
                int yndc_raw;
                real yndc_real, t_val;
                yndc_raw = -(2 * ti - (H - 1)) * 642;
                yndc_real = yndc_raw / 65536.0;
                if (yndc_real >= 0.0) begin
                    t_val = 0.0;
                end else begin
                    t_val = -0.5 / yndc_real;
                    if (t_val > 16.0) t_val = 16.0;
                end
                idx = mw(BASE_ADDR + PERSP_OFF + (ti * 4));
                mem[idx] = $rtoi(t_val * 65536.0);
            end

            $display("TB: Memory initialized (sincos, invsqrt, persp, vec_const)");
        end
    endtask

    // -----------------------------------------------------------------------
    // BVH scene setup (2 triangles as sphere proxies)
    // -----------------------------------------------------------------------
    task automatic setup_bvh_scene();
        int wi;

        // Triangle 0 at BVH_OFF + 0x100 (sphere 1 proxy at z=2)
        wi = mw(BASE_ADDR + BVH_OFF + 32'h100);
        mem[wi+ 0] = fxp(-1, 0); mem[wi+ 1] = fxp(-1, 0);  // v0.x, v0.y
        mem[wi+ 2] = fxp( 2, 0); mem[wi+ 3] = fxp( 1, 0);  // v0.z, v1.x
        mem[wi+ 4] = fxp(-1, 0); mem[wi+ 5] = fxp( 2, 0);  // v1.y, v1.z
        mem[wi+ 6] = fxp( 0, 0); mem[wi+ 7] = fxp( 1, 0);  // v2.x, v2.y
        mem[wi+ 8] = fxp( 2, 0); mem[wi+ 9] = 32'h0000_0001; // v2.z, tri_id=1
        mem[wi+10] = 32'h0;      mem[wi+11] = 32'h0;         // reserved

        // Triangle 1 at BVH_OFF + 0x140 (sphere 2 proxy, fixed at z=4)
        wi = mw(BASE_ADDR + BVH_OFF + 32'h140);
        mem[wi+ 0] = fxp(-1, 0); mem[wi+ 1] = fxp(-1, 0);
        mem[wi+ 2] = fxp( 4, 0); mem[wi+ 3] = fxp( 1, 0);
        mem[wi+ 4] = fxp(-1, 0); mem[wi+ 5] = fxp( 4, 0);
        mem[wi+ 6] = fxp( 0, 0); mem[wi+ 7] = fxp( 1, 0);
        mem[wi+ 8] = fxp( 4, 0); mem[wi+ 9] = 32'h0000_0002;
        mem[wi+10] = 32'h0;      mem[wi+11] = 32'h0;

        // BVH root at BVH_OFF + 0x000 (internal node)
        // Child/tri pointers are word addresses = byte_addr >> 2
        wi = mw(BASE_ADDR + BVH_OFF);
        mem[wi+0] = {2'b00, 30'((BASE_ADDR + BVH_OFF + 32'h040) >> 2)};  // internal, left  → leaf @+0x040
        mem[wi+1] = {2'b00, 30'((BASE_ADDR + BVH_OFF + 32'h080) >> 2)};  //           right → leaf @+0x080
        mem[wi+2] = fxp(-1, 0); mem[wi+3] = fxp(-1, 0);  // AABB min x,y
        mem[wi+4] = fxp( 2, 0); mem[wi+5] = fxp( 1, 0);  // AABB min z, max x
        mem[wi+6] = fxp( 1, 0); mem[wi+7] = fxp( 4, 0);  // AABB max y, max z

        // Left leaf at BVH_OFF + 0x040 → triangle 0
        wi = mw(BASE_ADDR + BVH_OFF + 32'h040);
        mem[wi+0] = {2'b01, 30'((BASE_ADDR + BVH_OFF + 32'h100) >> 2)};  // leaf, tri_ptr
        mem[wi+1] = {2'b00, 30'd1};                                       // tri_count=1
        mem[wi+2] = fxp(-1, 0); mem[wi+3] = fxp(-1, 0);
        mem[wi+4] = fxp( 2, 0); mem[wi+5] = fxp( 1, 0);
        mem[wi+6] = fxp( 1, 0); mem[wi+7] = fxp( 2, 0);

        // Right leaf at BVH_OFF + 0x080 → triangle 1
        wi = mw(BASE_ADDR + BVH_OFF + 32'h080);
        mem[wi+0] = {2'b01, 30'((BASE_ADDR + BVH_OFF + 32'h140) >> 2)};  // leaf, tri_ptr
        mem[wi+1] = {2'b00, 30'd1};
        mem[wi+2] = fxp(-1, 0); mem[wi+3] = fxp(-1, 0);
        mem[wi+4] = fxp( 4, 0); mem[wi+5] = fxp( 1, 0);
        mem[wi+6] = fxp( 1, 0); mem[wi+7] = fxp( 4, 0);

        $display("TB: BVH scene loaded (root + 2 leaves + 2 triangles)");
    endtask

    // -----------------------------------------------------------------------
    // MXU scratch matrix setup
    // -----------------------------------------------------------------------
    task automatic setup_mxu_matrices();
        int wi;
        // Matrix B: 4×4 identity at MXU_SCRATCH_OFF + 0x40
        wi = mw(BASE_ADDR + MXU_SCRATCH_OFF + 32'h40);
        for (int r = 0; r < 4; r++)
            for (int c = 0; c < 4; c++)
                mem[wi + r*4 + c] = (r == c) ? fxp(1, 0) : fxp(0, 0);

        // Matrix C area: clear
        wi = mw(BASE_ADDR + MXU_SCRATCH_OFF + 32'h80);
        for (int i = 0; i < 64; i++)  // Extra space for per-frame offsets
            mem[wi + i] = 32'h0;

        $display("TB: MXU scratch matrices initialized");
    endtask

    // Build Y-rotation matrix A for given angle_idx
    task automatic setup_rotation_matrix(input int angle_idx);
        int wi, si;
        int s_q15, c_q15;
        logic signed [31:0] s_fxp, c_fxp;
        begin
            si = mw(BASE_ADDR + SINCOS_OFF + (angle_idx * 8));
            s_q15 = $signed(mem[si]);
            c_q15 = $signed(mem[si + 1]);

            // Q1.15 → Q16.16 (shift left 1)
            s_fxp = (s_q15 <<< 1);
            c_fxp = (c_q15 <<< 1);

            wi = mw(BASE_ADDR + MXU_SCRATCH_OFF);
            // Row 0: [cos, 0, sin, 0]
            mem[wi + 0] = c_fxp;     mem[wi + 1] = fxp(0, 0);
            mem[wi + 2] = s_fxp;     mem[wi + 3] = fxp(0, 0);
            // Row 1: [0, 1, 0, 0]
            mem[wi + 4] = fxp(0, 0); mem[wi + 5] = fxp(1, 0);
            mem[wi + 6] = fxp(0, 0); mem[wi + 7] = fxp(0, 0);
            // Row 2: [-sin, 0, cos, 0]
            mem[wi + 8] = -s_fxp;    mem[wi + 9] = fxp(0, 0);
            mem[wi +10] = c_fxp;     mem[wi +11] = fxp(0, 0);
            // Row 3: [0, 0, 0, 1]
            mem[wi +12] = fxp(0, 0); mem[wi +13] = fxp(0, 0);
            mem[wi +14] = fxp(0, 0); mem[wi +15] = fxp(1, 0);
        end
    endtask

    // -----------------------------------------------------------------------
    // Instruction ROM program for both CUs
    // Full 3D ray-tracing shader with perspective ground, mirror sphere,
    // diffuse green sphere, 3-level shadow, specular, fog
    // -----------------------------------------------------------------------
    task automatic init_rom();
        int pc;
        int br_s1_pc, br_s2_pc, br_gnd_pc;
        int jmp_sky_pc;
        int br_fog_pc;
        int br_black_pc, br_hard_pc, br_med_pc;
        int jmp_noshadow_pc, jmp_red_med_pc, jmp_red_hard_pc, jmp_black_pc, jmp_fog_pc;
        int br_s1gnd_pc, jmp_s1sky_pc, br_s1chk_pc, jmp_s1dk_pc;
        int br_spec_pc, jmp_tint_pc;
        int br_s2lit_pc, jmp_s2lit_pc, jmp_s2dk_pc;
        int blt_x_pc, blt_y_pc, blt_frame_pc;
        int loop_frame_pc, loop_y_pc, loop_x_pc;
        int sphere1_pc, sphere2_pc;
        int ground_pc, fog_sky_pc;
        int ground_black_pc, red_hard_pc, red_med_pc;
        int s1_gnd_pc, s1_lt_pc, spec_pc, skip_spec_pc;
        int s2_dk_pc;
        int write_pc;
        int br_skip_accel_pc, skip_accel_pc;
        int poll_mxu_pc, poll_rtu_fwd_pc, poll_rtu_bwd_pc;
        int imm;
        begin
            for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
            pc = 0;

            // === SETUP — persistent registers ===
            rom[pc>>2] = u_type(BASE_ADDR, 5'd1, OP_LUI); pc += 4;                             // x1 = BASE_ADDR
            rom[pc>>2] = u_type(BASE_ADDR + FB_OFF, 5'd2, OP_LUI); pc += 4;                    // x2 = FB base
            rom[pc>>2] = i_type(642, 5'd0, 3'b000, 5'd3, OP_IMM); pc += 4;                     // x3 = invW
            rom[pc>>2] = i_type(642, 5'd0, 3'b000, 5'd4, OP_IMM); pc += 4;                     // x4 = invH
            rom[pc>>2] = u_type(32'h0001_0000, 5'd5, OP_LUI); pc += 4;                         // x5 = ONE_FP
            rom[pc>>2] = u_type(32'h0000_6000, 5'd17, OP_LUI); pc += 4;                        // x17 = proj_r2 (hi)
            rom[pc>>2] = i_type(-983, 5'd17, 3'b000, 5'd17, OP_IMM); pc += 4;                  // x17 = 0x5C29
            rom[pc>>2] = i_type(W, 5'd0, 3'b000, 5'd19, OP_IMM); pc += 4;                     // x19 = W
            rom[pc>>2] = i_type(H, 5'd0, 3'b000, 5'd20, OP_IMM); pc += 4;                     // x20 = H
            rom[pc>>2] = u_type(BASE_ADDR + SINCOS_OFF, 5'd21, OP_LUI); pc += 4;               // x21 = sincos base
            rom[pc>>2] = u_type(BASE_ADDR + INVSQRT_OFF, 5'd22, OP_LUI); pc += 4;              // x22 = invsqrt base
            rom[pc>>2] = u_type(BASE_ADDR + VEC_CONST_OFF, 5'd7, OP_LUI); pc += 4;             // x7 = vec_const
            rom[pc>>2] = vsetvli_inst(5'd0, 5'd0, 11'h010); pc += 4;                           // vsetvli 4, e32
            rom[pc>>2] = rvv_vl(5'd2, 5'd7, RVV_VEW32, 1'b1); pc += 4;                        // v2 = mirror tint
            rom[pc>>2] = i_type(FRAMES, 5'd0, 3'b000, 5'd13, OP_IMM); pc += 4;                // x13 = FRAMES
            rom[pc>>2] = i_type(12'hC00, 5'd0, 3'b010, 5'd12, OP_SYSTEM); pc += 4;             // x12 = CORE_ID
            // DONE address = BASE_ADDR + 0x100 + core_id * 64
            rom[pc>>2] = i_type(6, 5'd12, 3'b001, 5'd7, OP_IMM); pc += 4;                     // x7 = core_id << 6
            rom[pc>>2] = i_type(256, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4;                    // x7 += 256
            rom[pc>>2] = r_type(7'b0000000, 5'd7, 5'd1, 3'b000, 5'd9, OP_REG); pc += 4;       // x9 = base + x7
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd6, OP_IMM); pc += 4;                      // x6 = frame ctr = 0

            // =============================================================
            // CU0-only: Program MXU + RTU via mailbox (2-flit protocol)
            // CU1 skips directly to the frame loop.
            // Uses x7/x8 as scratch, x14 as mailbox base, x24 as DEADBEEF.
            // =============================================================
            br_skip_accel_pc = pc;
            rom[pc>>2] = b_type(0, 5'd12, 5'd0, 3'b001, OP_BRANCH); pc += 4;                  // BNE x12, x0 → skip_accel (patch later)
            rom[pc>>2] = nop(); pc += 4;

            // ---- Build mailbox base & DEADBEEF sentinel ----
            rom[pc>>2] = u_type(32'h7000_0000, 5'd14, OP_LUI); pc += 4;                        // x14 = 0x70000000
            rom[pc>>2] = u_type(32'hDEAD_C000, 5'd24, OP_LUI); pc += 4;                        // x24 = 0xDEADC000
            rom[pc>>2] = i_type(-273, 5'd24, 3'b000, 5'd24, OP_IMM); pc += 4;                  // x24 = 0xDEADBEEF

            // ---- MXU: clear accumulators ----
            //   addr_latch to 0x70000022 (MAT slot 2, fn=2)
            //   data_write to 0x70000020 (MAT slot 2, fn=0)
            rom[pc>>2] = i_type(MXU_REG_CTRL, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;           // x7 = 0 (CTRL addr)
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;               // addr latch
            rom[pc>>2] = i_type(32'h10, 5'd0, 3'b000, 5'd8, OP_IMM); pc += 4;                  // x8 = 0x10 (clear_acc)
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // data write
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // deassert CTRL
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;               // addr latch (x7 still 0)
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;               // data=0
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // ---- MXU: program SRC_A, SRC_B, DST_C addresses ----
            rom[pc>>2] = i_type(MXU_REG_SRC_A_ADDR, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;     // x7 = 2
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;               // addr latch
            rom[pc>>2] = u_type(BASE_ADDR + MXU_SCRATCH_OFF, 5'd8, OP_LUI); pc += 4;           // x8 = 0x80009000
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // SRC_A = 0x80009000

            rom[pc>>2] = i_type(MXU_REG_SRC_B_ADDR, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;     // x7 = 3
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;               // addr latch
            rom[pc>>2] = i_type(32'h40, 5'd8, 3'b000, 5'd8, OP_IMM); pc += 4;                  // x8 = 0x80009040
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // SRC_B = 0x80009040

            rom[pc>>2] = i_type(MXU_REG_DST_C_ADDR, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;     // x7 = 4
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;               // addr latch
            rom[pc>>2] = i_type(32'h40, 5'd8, 3'b000, 5'd8, OP_IMM); pc += 4;                  // x8 = 0x80009080
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // DST_C = 0x80009080

            // ---- MXU: tile dimensions (M=N=K=4) ----
            rom[pc>>2] = i_type(4, 5'd0, 3'b000, 5'd8, OP_IMM); pc += 4;                       // x8 = 4
            rom[pc>>2] = i_type(MXU_REG_TILE_M, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;          // x7 = 5
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(MXU_REG_TILE_N, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;          // x7 = 6
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(MXU_REG_TILE_K, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;          // x7 = 7
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;

            // ---- MXU: strides (A=B=C=16) ----
            rom[pc>>2] = i_type(16, 5'd0, 3'b000, 5'd8, OP_IMM); pc += 4;                      // x8 = 16
            rom[pc>>2] = i_type(MXU_REG_STRIDE_A, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;        // x7 = 8
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(MXU_REG_STRIDE_B, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;        // x7 = 9
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(MXU_REG_STRIDE_C, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;        // x7 = 10
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;

            // ---- MXU: start (CTRL = 0x09 = start + irq_en) ----
            rom[pc>>2] = i_type(MXU_REG_CTRL, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;           // x7 = 0
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(9, 5'd0, 3'b000, 5'd8, OP_IMM); pc += 4;                       // x8 = 9
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;

            // ---- MXU: poll completion from RX FIFO ----
            poll_mxu_pc = pc;
            rom[pc>>2] = i_type(0, 5'd14, 3'b010, 5'd8, OP_LOAD); pc += 4;                     // x8 = LW 0(x14) → pop RX
            rom[pc>>2] = b_type(-4, 5'd8, 5'd24, 3'b000, OP_BRANCH); pc += 4;                  // BEQ x8, x24, poll_mxu
            rom[pc>>2] = nop(); pc += 4;
            // x8 = MXU completion payload {perf_tiles[15:0], perf_ops[15:0]}
            rom[pc>>2] = s_type(MXU_RESULT_OFF, 5'd1, 5'd8, 3'b010, OP_STORE); pc += 4;        // store to BASE+0x180

            // MXU deassert CTRL
            rom[pc>>2] = i_type(MXU_REG_CTRL, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = s_type(32'h22, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h20, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;

            // =============================================================
            // RTU: Forward ray — origin (0,0,0), dir (0,0,1), expect hit tri_id=1
            // =============================================================
            //   addr_latch to 0x70000032 (RT slot 3, fn=2)
            //   data_write to 0x70000030 (RT slot 3, fn=0)

            // RAY_OX..RAY_DY = 0 (regs 2..6)
            rom[pc>>2] = i_type(RTU_REG_RAY_OX, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;         // x7 = 2
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;               // RAY_OX = 0
            rom[pc>>2] = i_type(1, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4;                       // x7 = 3
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;               // RAY_OY = 0
            rom[pc>>2] = i_type(1, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4;                       // x7 = 4
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;               // RAY_OZ = 0
            rom[pc>>2] = i_type(1, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4;                       // x7 = 5
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;               // RAY_DX = 0
            rom[pc>>2] = i_type(1, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4;                       // x7 = 6
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;               // RAY_DY = 0

            // RAY_DZ = fxp(1,0) = 0x00010000 = x5
            rom[pc>>2] = i_type(1, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4;                       // x7 = 7
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd5, 3'b010, OP_STORE); pc += 4;               // RAY_DZ = ONE_FP

            // INV_DX = FXP_MAX = 0x7FFFFFFF
            rom[pc>>2] = i_type(RTU_REG_INV_DX, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;         // x7 = 19
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = u_type(32'h8000_0000, 5'd8, OP_LUI); pc += 4;                          // x8 = 0x80000000
            rom[pc>>2] = i_type(-1, 5'd8, 3'b000, 5'd8, OP_IMM); pc += 4;                      // x8 = 0x7FFFFFFF
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // INV_DX = FXP_MAX

            // INV_DY = FXP_MAX (x8 still 0x7FFFFFFF)
            rom[pc>>2] = i_type(RTU_REG_INV_DY, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;         // x7 = 20
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // INV_DY = FXP_MAX

            // INV_DZ = fxp(1,0) = ONE_FP = x5
            rom[pc>>2] = i_type(RTU_REG_INV_DZ, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;         // x7 = 21
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd5, 3'b010, OP_STORE); pc += 4;               // INV_DZ = ONE_FP

            // RAY_TMIN = fxp(0,16) = 16
            rom[pc>>2] = i_type(RTU_REG_RAY_TMIN, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;       // x7 = 8
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(16, 5'd0, 3'b000, 5'd8, OP_IMM); pc += 4;                      // x8 = 16 (0.000244)
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // TMIN = 16

            // RAY_TMAX = fxp(100,0) = 0x00640000
            rom[pc>>2] = i_type(RTU_REG_RAY_TMAX, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;       // x7 = 9
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = u_type(32'h0064_0000, 5'd8, OP_LUI); pc += 4;                          // x8 = 0x00640000
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // TMAX = 100.0

            // BVH_ROOT = BASE_ADDR + BVH_OFF = 0x80008000
            rom[pc>>2] = i_type(RTU_REG_BVH_ROOT, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;       // x7 = 10
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = u_type(BASE_ADDR + BVH_OFF, 5'd8, OP_LUI); pc += 4;                   // x8 = 0x80008000
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // BVH_ROOT

            // CTRL = 0x09 (start + irq_en)
            rom[pc>>2] = i_type(RTU_REG_CTRL, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;           // x7 = 0
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(9, 5'd0, 3'b000, 5'd8, OP_IMM); pc += 4;                       // x8 = 9
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // start RTU

            // ---- RTU fwd: poll completion from RX FIFO ----
            poll_rtu_fwd_pc = pc;
            rom[pc>>2] = i_type(0, 5'd14, 3'b010, 5'd8, OP_LOAD); pc += 4;                     // x8 = pop RX
            rom[pc>>2] = b_type(-4, 5'd8, 5'd24, 3'b000, OP_BRANCH); pc += 4;                  // BEQ x8, x24, poll
            rom[pc>>2] = nop(); pc += 4;
            // x8 = {hit_flag, 15'b0, tri_id[15:0]}
            rom[pc>>2] = s_type(RTU_FWD_OFF, 5'd1, 5'd8, 3'b010, OP_STORE); pc += 4;           // store to BASE+0x1C0

            // RTU deassert CTRL
            rom[pc>>2] = i_type(RTU_REG_CTRL, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // =============================================================
            // RTU: Backward ray — change dir_z=-1, inv_dz=-1, expect miss
            // =============================================================
            // RAY_DZ = fxp(-1,0) = 0xFFFF0000
            rom[pc>>2] = i_type(RTU_REG_RAY_DZ, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;         // x7 = 7
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = u_type(32'hFFFF_0000, 5'd8, OP_LUI); pc += 4;                          // x8 = 0xFFFF0000
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // RAY_DZ = -1.0

            // INV_DZ = fxp(-1,0) = 0xFFFF0000 (x8 still has it)
            rom[pc>>2] = i_type(RTU_REG_INV_DZ, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;         // x7 = 21
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // INV_DZ = -1.0

            // CTRL = 0x09
            rom[pc>>2] = i_type(RTU_REG_CTRL, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;           // x7 = 0
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(9, 5'd0, 3'b000, 5'd8, OP_IMM); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd8, 3'b010, OP_STORE); pc += 4;               // start RTU backward

            // ---- RTU bwd: poll completion ----
            poll_rtu_bwd_pc = pc;
            rom[pc>>2] = i_type(0, 5'd14, 3'b010, 5'd8, OP_LOAD); pc += 4;                     // x8 = pop RX
            rom[pc>>2] = b_type(-4, 5'd8, 5'd24, 3'b000, OP_BRANCH); pc += 4;                  // BEQ x8, x24, poll
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = s_type(RTU_BWD_OFF, 5'd1, 5'd8, 3'b010, OP_STORE); pc += 4;           // store to BASE+0x1C4

            // RTU deassert CTRL
            rom[pc>>2] = i_type(RTU_REG_CTRL, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = s_type(32'h32, 5'd14, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(32'h30, 5'd14, 5'd0, 3'b010, OP_STORE); pc += 4;

            // FENCE to push mailbox results to memory
            rom[pc>>2] = i_type(12'h0FF, 5'd0, 3'b000, 5'd0, OP_FENCE); pc += 4;

            skip_accel_pc = pc;  // CU1 jumps here

            // === FRAME LOOP ===
            loop_frame_pc = pc;
            rom[pc>>2] = i_type(255, 5'd6, 3'b111, 5'd7, OP_IMM); pc += 4;                    // angle = frame & 0xFF
            rom[pc>>2] = i_type(3, 5'd7, 3'b001, 5'd7, OP_IMM); pc += 4;                      // angle * 8
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;      // sincos_ptr
            rom[pc>>2] = i_type(0, 5'd7, 3'b010, 5'd25, OP_LOAD); pc += 4;                    // sin(angle)
            rom[pc>>2] = i_type(1, 5'd25, 3'b001, 5'd18, OP_IMM); pc += 4;                    // sphere1.x = sin << 1
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd25, 3'b000, 5'd16, OP_REG); pc += 4;     // proj_cx1 = sin
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd12, 3'b000, 5'd8, OP_REG); pc += 4;      // y = core_id

            // === Y LOOP ===
            loop_y_pc = pc;
            rom[pc>>2] = i_type(1, 5'd8, 3'b001, 5'd7, OP_IMM); pc += 4;                      // 2*y
            rom[pc>>2] = i_type(-127, 5'd7, 3'b000, 5'd7, OP_IMM); pc += 4;                   // 2*y - 127
            rom[pc>>2] = r_type(7'b0000001, 5'd4, 5'd7, 3'b000, 5'd10, OP_REG); pc += 4;      // * invH
            rom[pc>>2] = r_type(7'b0100000, 5'd10, 5'd0, 3'b000, 5'd10, OP_REG); pc += 4;     // yndc = -val
            rom[pc>>2] = i_type(9, 5'd8, 3'b001, 5'd23, OP_IMM); pc += 4;                     // y << 9
            rom[pc>>2] = r_type(7'b0000000, 5'd2, 5'd23, 3'b000, 5'd23, OP_REG); pc += 4;     // row_ptr
            rom[pc>>2] = u_type(BASE_ADDR + PERSP_OFF, 5'd7, OP_LUI); pc += 4;                 // persp base
            rom[pc>>2] = i_type(2, 5'd8, 3'b001, 5'd25, OP_IMM); pc += 4;                     // y*4
            rom[pc>>2] = r_type(7'b0000000, 5'd25, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;      // persp_ptr
            rom[pc>>2] = i_type(0, 5'd7, 3'b010, 5'd15, OP_LOAD); pc += 4;                    // persp_scale
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd11, OP_IMM); pc += 4;                     // x = 0

            // === X LOOP ===
            loop_x_pc = pc;
            rom[pc>>2] = i_type(1, 5'd11, 3'b001, 5'd24, OP_IMM); pc += 4;                   // 2*x
            rom[pc>>2] = i_type(-127, 5'd24, 3'b000, 5'd24, OP_IMM); pc += 4;                 // 2*x - 127
            rom[pc>>2] = r_type(7'b0000001, 5'd3, 5'd24, 3'b000, 5'd24, OP_REG); pc += 4;     // xndc

            // ---- Sphere 1 test ----
            rom[pc>>2] = r_type(7'b0100000, 5'd16, 5'd24, 3'b000, 5'd25, OP_REG); pc += 4;    // dx = xndc-cx
            rom[pc>>2] = i_type(12'h408, 5'd25, 3'b101, 5'd26, OP_IMM); pc += 4;               // dx >> 8
            rom[pc>>2] = i_type(12'h408, 5'd10, 3'b101, 5'd27, OP_IMM); pc += 4;               // dy >> 8
            rom[pc>>2] = r_type(7'b0000001, 5'd26, 5'd26, 3'b000, 5'd26, OP_REG); pc += 4;    // dx²
            rom[pc>>2] = r_type(7'b0000001, 5'd27, 5'd27, 3'b000, 5'd27, OP_REG); pc += 4;    // dy²
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_REG); pc += 4;    // r² = dx²+dy²
            br_s1_pc = pc;
            rom[pc>>2] = b_type(0, 5'd26, 5'd17, 3'b100, OP_BRANCH); pc += 4;                  // if r² < proj_r2 → sphere1
            rom[pc>>2] = nop(); pc += 4;

            // ---- Sphere 2 test ----
            rom[pc>>2] = u_type(32'h0000_9000, 5'd27, OP_LUI); pc += 4;                        // s2 cx = 0.5625
            rom[pc>>2] = u_type(32'h0000_8000, 5'd28, OP_LUI); pc += 4;                        // s2 cy = 0.5
            rom[pc>>2] = u_type(32'h0000_1000, 5'd29, OP_LUI); pc += 4;                        // s2 r² = 0.0625
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd24, 3'b000, 5'd30, OP_REG); pc += 4;    // dx
            rom[pc>>2] = r_type(7'b0100000, 5'd28, 5'd10, 3'b000, 5'd31, OP_REG); pc += 4;    // dy
            rom[pc>>2] = i_type(12'h408, 5'd30, 3'b101, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd31, 3'b101, 5'd25, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd25, 3'b000, 5'd25, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd25, 5'd7, 3'b000, 5'd7, OP_REG); pc += 4;
            br_s2_pc = pc;
            rom[pc>>2] = b_type(0, 5'd7, 5'd29, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // ---- Background ----
            br_gnd_pc = pc;
            rom[pc>>2] = b_type(0, 5'd10, 5'd0, 3'b100, OP_BRANCH); pc += 4;                   // yndc < 0 → ground
            rom[pc>>2] = nop(); pc += 4;

            // Sky
            rom[pc>>2] = u_type(32'hFFE6_9000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h664, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_sky_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // === GROUND PATH ===
            ground_pc = pc;
            rom[pc>>2] = i_type(3, 5'd5, 3'b001, 5'd28, OP_IMM); pc += 4;                     // fog threshold
            br_fog_pc = pc;
            rom[pc>>2] = b_type(0, 5'd15, 5'd28, 3'b101, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Checkerboard
            rom[pc>>2] = i_type(12'h408, 5'd15, 3'b101, 5'd28, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd24, 3'b101, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd29, 5'd28, 3'b000, 5'd28, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h40E, 5'd28, 3'b101, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(12'h40E, 5'd15, 3'b101, 5'd30, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd29, 3'b100, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd29, 3'b000, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = i_type(1, 5'd29, 3'b111, 5'd29, OP_IMM); pc += 4;

            // Shadow
            rom[pc>>2] = r_type(7'b0100000, 5'd18, 5'd28, 3'b000, 5'd30, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd5, 5'd15, 3'b000, 5'd31, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd30, 3'b101, 5'd30, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd31, 3'b101, 5'd31, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd30, 5'd30, 3'b000, 5'd30, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd31, 5'd31, 3'b000, 5'd31, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd31, 5'd30, 3'b000, 5'd30, OP_REG); pc += 4;

            br_black_pc = pc;
            rom[pc>>2] = b_type(0, 5'd29, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            br_hard_pc = pc;
            rom[pc>>2] = b_type(0, 5'd30, 5'd17, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            rom[pc>>2] = i_type(1, 5'd17, 3'b001, 5'd31, OP_IMM); pc += 4;
            br_med_pc = pc;
            rom[pc>>2] = b_type(0, 5'd30, 5'd31, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // No shadow: full red
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0FF, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_noshadow_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Medium shadow
            red_med_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0A0, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_red_med_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Hard shadow
            red_hard_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h050, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_red_hard_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Black square
            ground_black_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            jmp_black_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Fog
            fog_sky_pc = pc;
            rom[pc>>2] = u_type(32'hFFE6_9000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h664, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_fog_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // === SPHERE 1 PATH (mirror) ===
            sphere1_pc = pc;
            rom[pc>>2] = r_type(7'b0100000, 5'd26, 5'd17, 3'b000, 5'd28, OP_REG); pc += 4;
            rom[pc>>2] = i_type(6, 5'd28, 3'b101, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd29, 3'b001, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd22, 5'd29, 3'b000, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = i_type(0, 5'd29, 3'b010, 5'd30, OP_LOAD); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd30, 5'd28, 3'b000, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd29, 3'b101, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(427, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd25, 3'b000, 5'd14, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd14, 3'b101, 5'd14, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd10, 3'b000, 5'd24, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd24, 3'b101, 5'd24, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd7, 5'd29, 3'b000, 5'd29, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h408, 5'd29, 3'b101, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(1, 5'd29, 3'b001, 5'd30, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd14, 5'd30, 3'b000, 5'd26, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd26, 3'b101, 5'd26, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd26, 5'd0, 3'b000, 5'd26, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd30, 3'b000, 5'd27, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd27, 3'b101, 5'd27, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd0, 3'b000, 5'd27, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd29, 5'd30, 3'b000, 5'd30, OP_REG); pc += 4;
            rom[pc>>2] = i_type(12'h410, 5'd30, 3'b101, 5'd30, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd30, 5'd5, 3'b000, 5'd30, OP_REG); pc += 4;

            // Reflected ground or sky
            br_s1gnd_pc = pc;
            rom[pc>>2] = b_type(0, 5'd27, 5'd0, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Reflected sky
            rom[pc>>2] = u_type(32'hFFE6_9000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h664, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s1sky_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Reflected ground checker
            s1_gnd_pc = pc;
            rom[pc>>2] = i_type(12'h40E, 5'd26, 3'b101, 5'd28, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(12'h40E, 5'd30, 3'b101, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd29, 5'd28, 3'b100, 5'd28, OP_REG); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd28, 3'b000, 5'd28, OP_REG); pc += 4;
            rom[pc>>2] = i_type(1, 5'd28, 3'b111, 5'd28, OP_IMM); pc += 4;
            br_s1chk_pc = pc;
            rom[pc>>2] = b_type(0, 5'd28, 5'd0, 3'b001, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s1dk_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            s1_lt_pc = pc;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h0FF, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;

            // Specular
            spec_pc = pc;
            rom[pc>>2] = i_type(12'h408, 5'd24, 3'b101, 5'd28, OP_IMM); pc += 4;
            rom[pc>>2] = i_type(156, 5'd0, 3'b000, 5'd29, OP_IMM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            br_spec_pc = pc;
            rom[pc>>2] = b_type(0, 5'd28, 5'd29, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = i_type(-1, 5'd0, 3'b000, 5'd31, OP_IMM); pc += 4;
            skip_spec_pc = pc;

            // Mirror tint
            rom[pc>>2] = r_type(7'b0011110, 5'd31, 5'd0, 3'b011, 5'd1, OP_CUSTOM1); pc += 4;
            rom[pc>>2] = r_type(7'b0011000, 5'd2, 5'd1, 3'b011, 5'd1, OP_CUSTOM1); pc += 4;
            rom[pc>>2] = r_type(7'b0010000, 5'd0, 5'd1, 3'b011, 5'd31, OP_CUSTOM1); pc += 4;
            jmp_tint_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // === SPHERE 2 PATH (diffuse green) ===
            sphere2_pc = pc;
            br_s2lit_pc = pc;
            rom[pc>>2] = b_type(0, 5'd31, 5'd0, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = u_type(32'hFF3C_B000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h43C, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s2lit_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            s2_dk_pc = pc;
            rom[pc>>2] = u_type(32'hFF0F_3000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(12'h20F, 5'd31, 3'b000, 5'd31, OP_IMM); pc += 4;
            jmp_s2dk_pc = pc;
            rom[pc>>2] = b_type(0, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // === WRITE PIXEL ===
            write_pc = pc;
            rom[pc>>2] = i_type(2, 5'd11, 3'b001, 5'd28, OP_IMM); pc += 4;                   // x*4
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd23, 3'b000, 5'd28, OP_REG); pc += 4;   // pixel addr
            rom[pc>>2] = s_type(0, 5'd28, 5'd31, 3'b010, OP_STORE); pc += 4;                  // store pixel
            rom[pc>>2] = i_type(1, 5'd11, 3'b000, 5'd11, OP_IMM); pc += 4;                    // x++
            blt_x_pc = pc;
            rom[pc>>2] = b_type(0, 5'd11, 5'd19, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = i_type(NUM_CORES, 5'd8, 3'b000, 5'd8, OP_IMM); pc += 4;              // y += 2
            blt_y_pc = pc;
            rom[pc>>2] = b_type(0, 5'd8, 5'd20, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // Frame done: FENCE + DONE + FENCE + frame++
            rom[pc>>2] = i_type(12'h0FF, 5'd0, 3'b000, 5'd0, OP_FENCE); pc += 4;
            rom[pc>>2] = i_type(1, 5'd0, 3'b000, 5'd7, OP_IMM); pc += 4;
            rom[pc>>2] = s_type(0, 5'd9, 5'd7, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(12'h0FF, 5'd0, 3'b000, 5'd0, OP_FENCE); pc += 4;
            rom[pc>>2] = i_type(1, 5'd6, 3'b000, 5'd6, OP_IMM); pc += 4;
            blt_frame_pc = pc;
            rom[pc>>2] = b_type(0, 5'd6, 5'd13, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            // WFI + spin
            rom[pc>>2] = i_type(12'h105, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = b_type(-4, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // === PATCH BRANCHES ===
            // CU0 mailbox skip branch
            imm = skip_accel_pc - br_skip_accel_pc;
            rom[br_skip_accel_pc>>2] = b_type(imm, 5'd12, 5'd0, 3'b001, OP_BRANCH);
            // Sphere/ground/sky branches
            imm = sphere1_pc - br_s1_pc;
            rom[br_s1_pc>>2] = b_type(imm, 5'd26, 5'd17, 3'b100, OP_BRANCH);
            imm = sphere2_pc - br_s2_pc;
            rom[br_s2_pc>>2] = b_type(imm, 5'd7, 5'd29, 3'b100, OP_BRANCH);
            imm = ground_pc - br_gnd_pc;
            rom[br_gnd_pc>>2] = b_type(imm, 5'd10, 5'd0, 3'b100, OP_BRANCH);
            imm = write_pc - jmp_sky_pc;
            rom[jmp_sky_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = fog_sky_pc - br_fog_pc;
            rom[br_fog_pc>>2] = b_type(imm, 5'd15, 5'd28, 3'b101, OP_BRANCH);
            imm = ground_black_pc - br_black_pc;
            rom[br_black_pc>>2] = b_type(imm, 5'd29, 5'd0, 3'b000, OP_BRANCH);
            imm = red_hard_pc - br_hard_pc;
            rom[br_hard_pc>>2] = b_type(imm, 5'd30, 5'd17, 3'b100, OP_BRANCH);
            imm = red_med_pc - br_med_pc;
            rom[br_med_pc>>2] = b_type(imm, 5'd30, 5'd31, 3'b100, OP_BRANCH);
            imm = write_pc - jmp_noshadow_pc;
            rom[jmp_noshadow_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = write_pc - jmp_red_med_pc;
            rom[jmp_red_med_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = write_pc - jmp_red_hard_pc;
            rom[jmp_red_hard_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = write_pc - jmp_black_pc;
            rom[jmp_black_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = write_pc - jmp_fog_pc;
            rom[jmp_fog_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = s1_gnd_pc - br_s1gnd_pc;
            rom[br_s1gnd_pc>>2] = b_type(imm, 5'd27, 5'd0, 3'b100, OP_BRANCH);
            imm = spec_pc - jmp_s1sky_pc;
            rom[jmp_s1sky_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = s1_lt_pc - br_s1chk_pc;
            rom[br_s1chk_pc>>2] = b_type(imm, 5'd28, 5'd0, 3'b001, OP_BRANCH);
            imm = spec_pc - jmp_s1dk_pc;
            rom[jmp_s1dk_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = skip_spec_pc - br_spec_pc;
            rom[br_spec_pc>>2] = b_type(imm, 5'd28, 5'd29, 3'b100, OP_BRANCH);
            imm = write_pc - jmp_tint_pc;
            rom[jmp_tint_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = s2_dk_pc - br_s2lit_pc;
            rom[br_s2lit_pc>>2] = b_type(imm, 5'd31, 5'd0, 3'b100, OP_BRANCH);
            imm = write_pc - jmp_s2lit_pc;
            rom[jmp_s2lit_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = write_pc - jmp_s2dk_pc;
            rom[jmp_s2dk_pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH);
            imm = loop_x_pc - blt_x_pc;
            rom[blt_x_pc>>2] = b_type(imm, 5'd11, 5'd19, 3'b100, OP_BRANCH);
            imm = loop_y_pc - blt_y_pc;
            rom[blt_y_pc>>2] = b_type(imm, 5'd8, 5'd20, 3'b100, OP_BRANCH);
            imm = loop_frame_pc - blt_frame_pc;
            rom[blt_frame_pc>>2] = b_type(imm, 5'd6, 5'd13, 3'b100, OP_BRANCH);

            $display("TB: ROM program loaded (%0d bytes, multi-core 3D RT)", pc);
        end
    endtask

    // -----------------------------------------------------------------------
    // Framebuffer dump
    // -----------------------------------------------------------------------
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
                    idx = mw(BASE_ADDR + FB_OFF + (y * W * 4) + (x * 4));
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

    // -----------------------------------------------------------------------
    // DONE detection — watch AXI writes to DONE addresses
    // L2 write-through pushes these to the AXI backing store
    // -----------------------------------------------------------------------
    longint unsigned cycle_count;
    localparam longint unsigned MAX_CYCLES = 300_000_000;

    logic cu0_done_flag, cu1_done_flag;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            cu0_done_flag <= 1'b0;
            cu1_done_flag <= 1'b0;
        end else begin
            if (aw_state == AW_DATA && m_axi_wvalid && m_axi_wready) begin
                if (aw_addr_reg >= (BASE_ADDR + DONE_CU0_OFF) &&
                    aw_addr_reg <  (BASE_ADDR + DONE_CU0_OFF + 64)) begin
                    if (m_axi_wdata[31:0] != 32'h0 || m_axi_wdata[63:32] != 32'h0) begin
                        $display("TB: CU0 DONE detected via AXI write cyc=%0d", cycle_count);
                        cu0_done_flag <= 1'b1;
                    end
                end
                if (aw_addr_reg >= (BASE_ADDR + DONE_CU1_OFF) &&
                    aw_addr_reg <  (BASE_ADDR + DONE_CU1_OFF + 64)) begin
                    if (m_axi_wdata[31:0] != 32'h0 || m_axi_wdata[63:32] != 32'h0) begin
                        $display("TB: CU1 DONE detected via AXI write cyc=%0d", cycle_count);
                        cu1_done_flag <= 1'b1;
                    end
                end
            end
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) cycle_count <= 0;
        else        cycle_count <= cycle_count + 1;
    end

    always_ff @(posedge clk) begin
        if (rst_n) begin
            if (cycle_count != 0 && (cycle_count % 5_000_000) == 0)
                $display("TB: cyc=%0d  cu0_done=%0d cu1_done=%0d",
                    cycle_count, cu0_done_flag, cu1_done_flag);
            if (cycle_count >= MAX_CYCLES) begin
                $display("TB: TIMEOUT at cycle %0d", cycle_count);
                $fatal(1);
            end
        end
    end

    // =====================================================================
    // MAIN TEST DRIVER
    //
    // CU0 programs MXU + RTU via mailbox before the frame loop.
    // Both CUs render FRAMES frames.  TB verifies CU-driven accelerator
    // results from shared memory after the first frame DONE.
    //
    // Per-frame pipeline:
    //   Phase 1: CUs render frame (CU0 also does accelerator dispatch
    //            before frame 0)
    //   Phase 2: Dump framebuffer
    //   Phase 3: After frame 0 — verify CU-driven MXU/RTU results
    // =====================================================================
    int mxu_pass, rtu_pass, frame_pass;
    int total_pass, total_fail;

    initial begin
        slot_reg_wr_valid = '0;
        slot_reg_rd_valid = '0;
        slot_reg_wr_addr  = '0;
        slot_reg_wr_data  = '0;
        slot_reg_rd_addr  = '0;
        mxu_pass   = 0;
        rtu_pass   = 0;
        frame_pass = 0;
        total_pass = 0;
        total_fail = 0;

        init_memory();
        setup_bvh_scene();
        setup_mxu_matrices();
        // Pre-load rotation matrix A for angle=0 (CU0 firmware MXU test)
        setup_rotation_matrix(0);
        init_rom();

        wait (rst_n);
        repeat (20) @(posedge clk);

        // ===============================================================
        // Frame loop — CUs control accelerators via firmware mailbox
        // ===============================================================
        for (int frame = 0; frame < FRAMES; frame++) begin
            $display("\n============================================================");
            $display("  FRAME %0d", frame);
            $display("============================================================");

            // -----------------------------------------------------------
            // Wait for both CU DONE flags
            // -----------------------------------------------------------
            $display("  [CU] Rendering frame %0d (both CUs)...", frame);

            // Clear done flags
            cu0_done_flag <= 1'b0;
            cu1_done_flag <= 1'b0;
            mem[mw(BASE_ADDR + DONE_CU0_OFF)] <= 32'h0;
            mem[mw(BASE_ADDR + DONE_CU1_OFF)] <= 32'h0;
            @(posedge clk);

            begin
                int wait_cnt;
                wait_cnt = 0;
                while ((!cu0_done_flag || !cu1_done_flag) && wait_cnt < 100_000_000) begin
                    @(posedge clk);
                    wait_cnt++;
                end
                if (wait_cnt >= 100_000_000) begin
                    $display("  [CU] TIMEOUT waiting for DONE! cu0=%0d cu1=%0d",
                             cu0_done_flag, cu1_done_flag);
                end else begin
                    $display("  [CU] Both CUs done at cycle %0d", cycle_count);
                    frame_pass++;
                end
            end

            // Dump framebuffer
            dump_fb_ppm(frame);

            // -----------------------------------------------------------
            // After frame 0: verify CU0-driven accelerator results
            // (CU0 wrote completion payloads to shared memory)
            // -----------------------------------------------------------
            if (frame == 0) begin
                logic [31:0] mxu_result, rtu_fwd_result, rtu_bwd_result;

                // Allow write-through to settle
                repeat (200) @(posedge clk);

                // ---- MXU completion payload ----
                mxu_result = mem[mw(BASE_ADDR + MXU_RESULT_OFF)];
                $display("  [MXU-CU] Completion payload: 0x%08x", mxu_result);
                // Expect: {perf_tiles=1, perf_ops=16} = 0x00010010
                if (mxu_result[31:16] >= 16'h1 && mxu_result[15:0] >= 16'h1) begin
                    $display("  [MXU-CU] PASS: CU0 dispatched MXU via mailbox (tiles=%0d, ops=%0d)",
                             mxu_result[31:16], mxu_result[15:0]);
                    mxu_pass++;
                end else begin
                    $display("  [MXU-CU] FAIL: Expected non-zero tiles and ops");
                end

                // ---- MXU result matrix: C == A (rotation × identity) ----
                begin
                    int wi_a, wi_c, errs;
                    wi_a = mw(BASE_ADDR + MXU_SCRATCH_OFF);
                    wi_c = mw(BASE_ADDR + MXU_SCRATCH_OFF + 32'h80);
                    errs = 0;
                    for (int i = 0; i < 16; i++) begin
                        if (mem[wi_c + i] != mem[wi_a + i]) begin
                            $display("    MXU C[%0d]=%08x expect A[%0d]=%08x",
                                     i, mem[wi_c + i], i, mem[wi_a + i]);
                            errs++;
                        end
                    end
                    if (errs == 0) begin
                        $display("  [MXU-CU] PASS: Rotation * Identity = Rotation");
                        mxu_pass++;
                    end else begin
                        $display("  [MXU-CU] FAIL: %0d mismatches in result matrix", errs);
                    end
                end

                // ---- RTU forward ray ----
                rtu_fwd_result = mem[mw(BASE_ADDR + RTU_FWD_OFF)];
                $display("  [RTU-CU] Forward ray payload: 0x%08x", rtu_fwd_result);
                // Expect: {hit=1, 15'b0, tri_id=1} = 0x80000001
                if (rtu_fwd_result[31] && rtu_fwd_result[15:0] == 16'h0001) begin
                    $display("  [RTU-CU] PASS: Forward ray hit triangle 1");
                    rtu_pass++;
                end else begin
                    $display("  [RTU-CU] FAIL: Expected hit=1, tri_id=1");
                end

                // ---- RTU backward ray ----
                rtu_bwd_result = mem[mw(BASE_ADDR + RTU_BWD_OFF)];
                $display("  [RTU-CU] Backward ray payload: 0x%08x", rtu_bwd_result);
                // Expect: {hit=0, 15'b0, 16'bx} → bit 31 = 0
                if (!rtu_bwd_result[31]) begin
                    $display("  [RTU-CU] PASS: Backward ray missed");
                    rtu_pass++;
                end else begin
                    $display("  [RTU-CU] FAIL: Expected miss (hit=0)");
                end
            end

        end // frame loop

        // ===============================================================
        // Performance counter check (CU0 dispatched 1 MXU tile, 2 RTU rays)
        // ===============================================================
        $display("\n=== Performance Counters ===");
        begin
            logic [31:0] rtu_rays, rtu_nodes, rtu_tests;
            logic [31:0] mxu_tiles, mxu_ops;
            slot_read(RT_SLOT,  RTU_REG_PERF_RAYS,  rtu_rays);
            slot_read(RT_SLOT,  RTU_REG_PERF_NODES, rtu_nodes);
            slot_read(RT_SLOT,  RTU_REG_PERF_TESTS, rtu_tests);
            slot_read(MAT_SLOT, MXU_REG_PERF_TILES, mxu_tiles);
            slot_read(MAT_SLOT, MXU_REG_PERF_OPS,   mxu_ops);
            $display("  RTU: rays=%0d  nodes=%0d  tri_tests=%0d",
                     rtu_rays, rtu_nodes, rtu_tests);
            $display("  MXU: tiles=%0d  ops=%0d", mxu_tiles, mxu_ops);
            if (rtu_rays == 32'd2 && mxu_tiles == 32'd1) begin
                $display("  PASS: Perf counters correct (CU-driven)");
                total_pass++;
            end else begin
                $display("  FAIL: Expected RTU_RAYS=2, MXU_TILES=1");
                total_fail++;
            end
        end

        // CU status check
        begin
            logic [31:0] cu0_st, cu1_st;
            slot_read(CU0_SLOT, 5'd0, cu0_st);
            slot_read(CU1_SLOT, 5'd0, cu1_st);
            $display("  CU0 status=0x%08x  CU1 status=0x%08x", cu0_st, cu1_st);
        end

        // ===============================================================
        // Summary
        // ===============================================================
        total_pass += mxu_pass + rtu_pass + frame_pass;
        total_fail += (2 - mxu_pass) + (2 - rtu_pass) + (FRAMES - frame_pass);

        $display("\n============================================================");
        $display("  GFX SW-RT Cluster TB — CU-Driven Accelerators");
        $display("  MXU: %0d/2    RTU: %0d/2    Frames: %0d/%0d",
                 mxu_pass, rtu_pass, frame_pass, FRAMES);
        $display("  Total: %0d PASS, %0d FAIL", total_pass, total_fail);
        $display("============================================================");
        if (total_fail == 0)
            $display("  ALL TESTS PASSED");
        else
            $display("  SOME TESTS FAILED");

        repeat (20) @(posedge clk);
        $finish;
    end

    // Global watchdog
    initial begin
        #600_000_000;
        $display("ERROR: Global watchdog timeout (600ms)");
        $finish;
    end

    // ---- DEBUG: trace crossbar TX/RX for all slots ----
    always @(posedge clk) begin
        for (int i = 0; i < 4; i++) begin
            if (u_cluster.mb_slot_tx_valid[i] && u_cluster.mb_slot_tx_ready[i])
                $display("DBG xbar TX slot=%0d dest=%04x payload=%08x src=%04x @%0t",
                    i, u_cluster.mb_slot_tx_dest[i],
                    u_cluster.mb_slot_tx_data[i].payload,
                    u_cluster.mb_slot_tx_data[i].hdr.src_id, $time);
            if (u_cluster.mb_slot_rx_valid[i] && u_cluster.mb_slot_rx_ready[i])
                $display("DBG xbar RX slot=%0d dest=%04x payload=%08x @%0t",
                    i, u_cluster.mb_slot_rx_dest[i],
                    u_cluster.mb_slot_rx_data[i].payload, $time);
        end
    end

    // ---- DEBUG: trace CU0 endpoint RX pop ----
    always @(posedge clk) begin
        if (u_cluster.gen_slot[0].gen_cu_slot.u_cu.g_mailbox_ep.u_mailbox_ep.rx_r_en)
            $display("DBG CU0 ep_rx POP payload=%08x @%0t",
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.g_mailbox_ep.u_mailbox_ep.rx_data, $time);
        if (u_cluster.gen_slot[0].gen_cu_slot.u_cu.g_mailbox_ep.u_mailbox_ep.rx_w_en)
            $display("DBG CU0 ep_rx PUSH payload=%08x @%0t",
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.g_mailbox_ep.u_mailbox_ep.link_rx_data.payload, $time);
        // Track TX FIFO writes and drops
        if (u_cluster.gen_slot[0].gen_cu_slot.u_cu.g_mailbox_ep.u_mailbox_ep.tx_w_en)
            $display("DBG CU0 ep_tx PUSH dest=%04x payload=%08x @%0t",
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.lsu_mailbox_tx_dest,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.lsu_mailbox_tx_data, $time);
        if (u_cluster.gen_slot[0].gen_cu_slot.u_cu.g_mailbox_ep.u_mailbox_ep.tx_w_full)
            $display("DBG CU0 ep_tx FULL @%0t", $time);
    end

    // ---- DEBUG: trace MXU completion ----
    always @(posedge clk) begin
        if (u_cluster.gen_slot[2].gen_mat_slot.u_mxu.mb_tx_completion)
            $display("DBG MXU completion pending dest=%04x tiles=%0d ops=%0d @%0t",
                u_cluster.gen_slot[2].gen_mat_slot.u_mxu.mb_completion_dest,
                u_cluster.gen_slot[2].gen_mat_slot.u_mxu.perf_tiles,
                u_cluster.gen_slot[2].gen_mat_slot.u_mxu.perf_ops, $time);
    end

    // ---- DEBUG: trace RTU completion ----
    always @(posedge clk) begin
        if (u_cluster.gen_slot[3].gen_rt_slot.u_rtu.mb_tx_completion)
            $display("DBG RTU completion pending dest=%04x @%0t",
                u_cluster.gen_slot[3].gen_rt_slot.u_rtu.mb_completion_dest, $time);
    end

    // ---- DEBUG: trace CU0 MEM stage pipeline for mailbox stores ----
    always @(posedge clk) begin
        if (u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_valid &&
            (u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_ctrl.is_store ||
             u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_ctrl.is_load) &&
            u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_addr[31:16] == 16'h7000)
            $display("DBG CU0 MEM mb: addr=%08x wdata=%08x stall=%0b submitted=%0b req_rdy=%0b is_store=%0b pc=%08x @%0t",
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_addr,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_scalar_wdata,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.stall_pipe,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_lsu_submitted,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.u_lsu_core.req_ready,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_ctrl.is_store,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_pc,
                $time);
        // Show ALL MEM valid instructions with PC during the mailbox programming window
        if (u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_valid &&
            $time >= 335000 && $time <= 370000)
            $display("DBG CU0 MEM ALL: pc=%08x is_store=%0b is_load=%0b addr=%08x stall=%0b submitted=%0b @%0t",
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_pc,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_ctrl.is_store,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_ctrl.is_load,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_addr,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.stall_pipe,
                u_cluster.gen_slot[0].gen_cu_slot.u_cu.mem_lsu_submitted,
                $time);
    end

endmodule : gfx_sw_rt_multicore_tb
