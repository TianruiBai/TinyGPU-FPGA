`timescale 1ns/1ps
// ============================================================================
// Cluster Testbench — 1 CU + 1 RT configuration
//
// Exercises cluster_top_v2 with:
//   Slot 0 = CU   (Compute Unit)
//   Slot 1 = RT   (Ray-Tracing Accelerator)
//   Slot 2 = NONE
//   Slot 3 = NONE
//
// Test plan:
//   1. RTU standalone ray→hit via direct register writes
//   2. RTU standalone ray→miss
//   3. CU runs a simple program (fence/spin) while RTU operates
//   4. Mailbox: CU sends job dispatch flit to RTU (if connected)
//   5. Concurrent CU execution + RTU traversal
//
// Memory layout (AXI backing store):
//   0x8000_0000: BVH root (internal node)
//   0x8000_0020: Left leaf
//   0x8000_0040: Right leaf
//   0x8000_1000: Triangle 0 (at z=2)
//   0x8000_1030: Triangle 1 (at z=4)
//
// CU instruction memory:
//   0x0000_0000: NOP loop (CU just spins)
// ============================================================================
module cluster_1cu_1rt_tb;
    import rt_pkg::*;
    import mailbox_pkg::*;

    // -----------------------------------------------------------------------
    // Parameters
    // -----------------------------------------------------------------------
    localparam int AXI_ADDR_W = 32;
    localparam int AXI_DATA_W = 64;
    localparam int NUM_SLOTS  = 4;

    // -----------------------------------------------------------------------
    // Clock / Reset
    // -----------------------------------------------------------------------
    logic clk, rst_n;
    initial clk = 1'b0;
    always #2.5 clk = ~clk; // 200 MHz

    initial begin
        rst_n = 1'b0;
        repeat (20) @(posedge clk);
        rst_n = 1'b1;
    end

    // -----------------------------------------------------------------------
    // DUT wiring
    // -----------------------------------------------------------------------
    // AXI L2 backing store
    logic [AXI_ADDR_W-1:0]  m_axi_awaddr;
    logic [7:0]              m_axi_awlen;
    logic [2:0]              m_axi_awsize;
    logic [1:0]              m_axi_awburst;
    logic                    m_axi_awvalid, m_axi_awready;
    logic [AXI_DATA_W-1:0]  m_axi_wdata;
    logic [AXI_DATA_W/8-1:0] m_axi_wstrb;
    logic                    m_axi_wlast, m_axi_wvalid, m_axi_wready;
    logic                    m_axi_bvalid, m_axi_bready;
    logic [AXI_ADDR_W-1:0]  m_axi_araddr;
    logic [7:0]              m_axi_arlen;
    logic [2:0]              m_axi_arsize;
    logic [1:0]              m_axi_arburst;
    logic                    m_axi_arvalid, m_axi_arready;
    logic [AXI_DATA_W-1:0]  m_axi_rdata;
    logic                    m_axi_rvalid, m_axi_rlast, m_axi_rready;

    // Framebuffer
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
    logic                           mb_up_tx_valid, mb_up_tx_ready;
    mailbox_flit_t                  mb_up_tx_data;
    logic [NODE_ID_WIDTH-1:0]       mb_up_tx_dest_id;
    logic                           mb_up_rx_valid, mb_up_rx_ready;
    mailbox_flit_t                  mb_up_rx_data;
    logic [NODE_ID_WIDTH-1:0]       mb_up_rx_dest_id;

    // Slot register access
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
    // DUT: cluster_top (1 CU + 1 RT)
    // -----------------------------------------------------------------------
    cluster_top #(
        .NUM_SLOTS(4),
        .SLOT_TYPE_0(0),  // CU
        .SLOT_TYPE_1(1),  // RT
        .SLOT_TYPE_2(3),  // NONE
        .SLOT_TYPE_3(3),  // NONE
        .CLUSTER_ID(8'h01),
        .MAILBOX_ENABLE(1'b1),
        .GFX_ENABLE(1'b0),
        .L2_SIZE_BYTES(16384),
        .L2_LINE_BYTES(64),
        .L2_ASSOC(4),
        .AXI_ADDR_W(AXI_ADDR_W),
        .AXI_DATA_W(AXI_DATA_W)
    ) u_dut (
        .clk(clk), .rst_n(rst_n),
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
        .fb_aw_valid(fb_aw_valid), .fb_aw_addr(fb_aw_addr),
        .fb_aw_len(fb_aw_len),     .fb_aw_size(fb_aw_size),
        .fb_aw_burst(fb_aw_burst), .fb_aw_ready(fb_aw_ready),
        .fb_w_data(fb_w_data),     .fb_w_strb(fb_w_strb),
        .fb_w_last(fb_w_last),     .fb_w_valid(fb_w_valid),
        .fb_w_ready(fb_w_ready),
        .fb_b_valid(fb_b_valid),   .fb_b_ready(fb_b_ready),
        .icache_miss_req_valid(icache_miss_req_valid),
        .icache_miss_req_addr(icache_miss_req_addr),
        .icache_miss_req_ready(icache_miss_req_ready),
        .icache_miss_resp_valid(icache_miss_resp_valid),
        .icache_miss_resp_data(icache_miss_resp_data),
        .mb_up_tx_valid(mb_up_tx_valid), .mb_up_tx_ready(mb_up_tx_ready),
        .mb_up_tx_data(mb_up_tx_data),   .mb_up_tx_dest_id(mb_up_tx_dest_id),
        .mb_up_rx_valid(mb_up_rx_valid), .mb_up_rx_ready(mb_up_rx_ready),
        .mb_up_rx_data(mb_up_rx_data),   .mb_up_rx_dest_id(mb_up_rx_dest_id),
        .slot_reg_wr_valid(slot_reg_wr_valid),
        .slot_reg_wr_addr(slot_reg_wr_addr),
        .slot_reg_wr_data(slot_reg_wr_data),
        .slot_reg_wr_ready(slot_reg_wr_ready),
        .slot_reg_rd_valid(slot_reg_rd_valid),
        .slot_reg_rd_addr(slot_reg_rd_addr),
        .slot_reg_rd_data(slot_reg_rd_data),
        .slot_reg_rd_ready(slot_reg_rd_ready),
        .slot_irq(slot_irq),
        .slot_csr_status(slot_csr_status)
    );

    // -----------------------------------------------------------------------
    // AXI Memory Model (backing store for L2)
    // -----------------------------------------------------------------------
    localparam int MEM_WORDS = 65536; // 256 KB
    logic [31:0] axi_mem [0:MEM_WORDS-1];

    function automatic int aw(input logic [31:0] addr);
        aw = (addr & 32'h0003_FFFC) >> 2;
    endfunction

    // AXI read channel state machine
    typedef enum logic [1:0] { AR_IDLE, AR_BURST } axi_rd_state_e;
    axi_rd_state_e ar_state;
    logic [31:0] ar_addr;
    logic [7:0]  ar_cnt, ar_len;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            ar_state       <= AR_IDLE;
            m_axi_arready  <= 1'b1;
            m_axi_rvalid   <= 1'b0;
            m_axi_rlast    <= 1'b0;
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
                        wi = aw(ar_addr);
                        m_axi_rvalid <= 1'b1;
                        m_axi_rdata  <= {axi_mem[wi+1], axi_mem[wi]};
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

    // AXI write channel (simple sink)
    typedef enum logic [1:0] { AW_IDLE, AW_DATA, AW_RESP } axi_wr_state_e;
    axi_wr_state_e aw_state;
    logic [31:0] aw_addr_reg;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aw_state       <= AW_IDLE;
            m_axi_awready  <= 1'b1;
            m_axi_wready   <= 1'b0;
            m_axi_bvalid   <= 1'b0;
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
                        wi = aw(aw_addr_reg);
                        if (m_axi_wstrb[0]) axi_mem[wi]   <= m_axi_wdata[31:0];
                        if (m_axi_wstrb[4]) axi_mem[wi+1] <= m_axi_wdata[63:32];
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
    // I-Cache Miss Responder (simple IMEM)
    // -----------------------------------------------------------------------
    // CU program: infinite loop of NOP (addi x0, x0, 0 = 0x00000013)
    localparam int IMEM_WORDS = 256;
    logic [31:0] imem [0:IMEM_WORDS-1];

    initial begin
        for (int i = 0; i < IMEM_WORDS; i++)
            imem[i] = 32'h0000_0013; // NOP
        // First instruction: jump to self (jal x0, 0 = 0x0000006F)
        imem[0] = 32'h0000_006F;
    end

    // I-cache miss responder — returns 64 bits per beat (2 instructions)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            icache_miss_req_ready  <= 1'b1;
            icache_miss_resp_valid <= 1'b0;
        end else begin
            icache_miss_resp_valid <= 1'b0;
            if (icache_miss_req_valid && icache_miss_req_ready) begin
                int wi;
                wi = (icache_miss_req_addr >> 2) & (IMEM_WORDS - 1);
                icache_miss_resp_valid <= 1'b1;
                icache_miss_resp_data  <= {imem[wi+1], imem[wi]};
            end
        end
    end

    // -----------------------------------------------------------------------
    // Framebuffer AXI Sink
    // -----------------------------------------------------------------------
    assign fb_aw_ready = 1'b1;
    assign fb_w_ready  = 1'b1;
    assign fb_b_valid  = 1'b0;

    // -----------------------------------------------------------------------
    // Mailbox uplink tie-off
    // -----------------------------------------------------------------------
    assign mb_up_tx_ready  = 1'b1;
    assign mb_up_rx_valid  = 1'b0;
    assign mb_up_rx_data   = '0;
    assign mb_up_rx_dest_id = '0;

    // -----------------------------------------------------------------------
    // Q16.16 helper
    // -----------------------------------------------------------------------
    function automatic logic [31:0] fxp(input int int_part, input int frac_65536);
        logic signed [31:0] val;
        val = (int_part <<< 16) + frac_65536;
        return val;
    endfunction

    // -----------------------------------------------------------------------
    // Register write/read tasks for slot 1 (RTU)
    // -----------------------------------------------------------------------
    localparam int RTU_SLOT = 1;

    task automatic rtu_write(input [4:0] addr, input [31:0] data);
        @(posedge clk);
        slot_reg_wr_valid       <= '0;
        slot_reg_wr_valid[RTU_SLOT] <= 1'b1;
        slot_reg_wr_addr        <= addr;
        slot_reg_wr_data        <= data;
        @(posedge clk);
        slot_reg_wr_valid       <= '0;
    endtask

    task automatic rtu_read(input [4:0] addr, output [31:0] data);
        @(posedge clk);
        slot_reg_rd_valid       <= '0;
        slot_reg_rd_valid[RTU_SLOT] <= 1'b1;
        slot_reg_rd_addr        <= addr;
        @(posedge clk);
        data = slot_reg_rd_data[RTU_SLOT];
        slot_reg_rd_valid       <= '0;
    endtask

    task automatic wait_rtu_irq(input int max_cycles);
        int cnt;
        cnt = 0;
        while (!slot_irq[RTU_SLOT] && cnt < max_cycles) begin
            @(posedge clk);
            cnt++;
            if (cnt == 100)
                $display("  [DBG] After 100 cycles: arvalid=%0b arready=%0b rvalid=%0b",
                         m_axi_arvalid, m_axi_arready, m_axi_rvalid);
            if (cnt == 500)
                $display("  [DBG] After 500 cycles: arvalid=%0b arready=%0b rvalid=%0b",
                         m_axi_arvalid, m_axi_arready, m_axi_rvalid);
            if (m_axi_arvalid && m_axi_arready && cnt < 2000)
                $display("  [DBG cyc %0d] AXI AR addr=0x%08x len=%0d",
                         cnt, m_axi_araddr, m_axi_arlen);
            if (m_axi_rvalid && m_axi_rlast && cnt < 2000)
                $display("  [DBG cyc %0d] AXI R burst complete", cnt);
        end
        if (cnt >= max_cycles)
            $display("ERROR: RTU IRQ timeout after %0d cycles", max_cycles);
    endtask

    // -----------------------------------------------------------------------
    // Scene setup: BVH + triangles in AXI memory
    // -----------------------------------------------------------------------
    task automatic setup_scene();
        int wi;
        for (int i = 0; i < MEM_WORDS; i++) axi_mem[i] = 32'h0;

        // All BVH nodes & triangles must be 64-byte (cache-line) aligned
        // because L2 cache returns full lines starting at beat 0.

        // Triangle 0 at 0x8000_1000 (64B aligned)
        wi = aw(32'h8000_1000);
        axi_mem[wi+ 0] = fxp(-1, 0);  axi_mem[wi+ 1] = fxp(-1, 0);
        axi_mem[wi+ 2] = fxp( 2, 0);  axi_mem[wi+ 3] = fxp( 1, 0);
        axi_mem[wi+ 4] = fxp(-1, 0);  axi_mem[wi+ 5] = fxp( 2, 0);
        axi_mem[wi+ 6] = fxp( 0, 0);  axi_mem[wi+ 7] = fxp( 1, 0);
        axi_mem[wi+ 8] = fxp( 2, 0);  axi_mem[wi+ 9] = 32'h0000_0001;
        axi_mem[wi+10] = 32'h0;       axi_mem[wi+11] = 32'h0;

        // Triangle 1 at 0x8000_1040 (64B aligned, was 0x1030)
        wi = aw(32'h8000_1040);
        axi_mem[wi+ 0] = fxp(-1, 0);  axi_mem[wi+ 1] = fxp(-1, 0);
        axi_mem[wi+ 2] = fxp( 4, 0);  axi_mem[wi+ 3] = fxp( 1, 0);
        axi_mem[wi+ 4] = fxp(-1, 0);  axi_mem[wi+ 5] = fxp( 4, 0);
        axi_mem[wi+ 6] = fxp( 0, 0);  axi_mem[wi+ 7] = fxp( 1, 0);
        axi_mem[wi+ 8] = fxp( 4, 0);  axi_mem[wi+ 9] = 32'h0000_0002;
        axi_mem[wi+10] = 32'h0;       axi_mem[wi+11] = 32'h0;

        // BVH Root (internal) at 0x8000_0000 (64B aligned)
        // left child  = 0x8000_0040 → word addr 0x2000_0010
        // right child = 0x8000_0080 → word addr 0x2000_0020
        wi = aw(32'h8000_0000);
        axi_mem[wi+0] = {2'b00, 30'(32'h2000_0010)};  // left child @0x8000_0040
        axi_mem[wi+1] = {2'b00, 30'(32'h2000_0020)};  // right child @0x8000_0080
        axi_mem[wi+2] = fxp(-1, 0);  axi_mem[wi+3] = fxp(-1, 0);
        axi_mem[wi+4] = fxp( 2, 0);  axi_mem[wi+5] = fxp( 1, 0);
        axi_mem[wi+6] = fxp( 1, 0);  axi_mem[wi+7] = fxp( 4, 0);

        // Left leaf at 0x8000_0040 (64B aligned, was 0x0020)
        // tri addr = 0x8000_1000 → word addr 0x2000_0400
        wi = aw(32'h8000_0040);
        axi_mem[wi+0] = {2'b01, 30'(32'h2000_0400)};  // tri 0 @0x8000_1000
        axi_mem[wi+1] = {2'b00, 30'd1};                // 1 triangle
        axi_mem[wi+2] = fxp(-1, 0);  axi_mem[wi+3] = fxp(-1, 0);
        axi_mem[wi+4] = fxp( 2, 0);  axi_mem[wi+5] = fxp( 1, 0);
        axi_mem[wi+6] = fxp( 1, 0);  axi_mem[wi+7] = fxp( 2, 0);

        // Right leaf at 0x8000_0080 (64B aligned, was 0x0040)
        // tri addr = 0x8000_1040 → word addr 0x2000_0410
        wi = aw(32'h8000_0080);
        axi_mem[wi+0] = {2'b01, 30'(32'h2000_0410)};  // tri 1 @0x8000_1040
        axi_mem[wi+1] = {2'b00, 30'd1};                // 1 triangle
        axi_mem[wi+2] = fxp(-1, 0);  axi_mem[wi+3] = fxp(-1, 0);
        axi_mem[wi+4] = fxp( 4, 0);  axi_mem[wi+5] = fxp( 1, 0);
        axi_mem[wi+6] = fxp( 1, 0);  axi_mem[wi+7] = fxp( 4, 0);

        $display("[SCENE] BVH + 2 triangles loaded into AXI memory (64B aligned)");
    endtask

    // -----------------------------------------------------------------------
    // Test driver
    // -----------------------------------------------------------------------
    int pass_count, fail_count;

    initial begin
        slot_reg_wr_valid = '0;
        slot_reg_rd_valid = '0;
        slot_reg_wr_addr  = '0;
        slot_reg_wr_data  = '0;
        slot_reg_rd_addr  = '0;
        pass_count = 0;
        fail_count = 0;

        wait (rst_n);
        repeat (10) @(posedge clk);

        setup_scene();

        // ==================================================================
        // Test 1: Ray hits closest triangle (via direct register writes)
        // ==================================================================
        $display("\n=== Test 1: Ray hits closest triangle (slot 1 RTU) ===");
        rtu_write(RTU_REG_RAY_OX,   fxp(0, 0));
        rtu_write(RTU_REG_RAY_OY,   fxp(0, 0));
        rtu_write(RTU_REG_RAY_OZ,   fxp(0, 0));
        rtu_write(RTU_REG_RAY_DX,   fxp(0, 0));
        rtu_write(RTU_REG_RAY_DY,   fxp(0, 0));
        rtu_write(RTU_REG_RAY_DZ,   fxp(1, 0));
        rtu_write(RTU_REG_INV_DX,   FXP_MAX);
        rtu_write(RTU_REG_INV_DY,   FXP_MAX);
        rtu_write(RTU_REG_INV_DZ,   fxp(1, 0));
        rtu_write(RTU_REG_RAY_TMIN, fxp(0, 16));
        rtu_write(RTU_REG_RAY_TMAX, fxp(100, 0));
        rtu_write(RTU_REG_BVH_ROOT, 32'h8000_0000);
        rtu_write(RTU_REG_CTRL,     32'h0000_0009); // start + irq_en

        wait_rtu_irq(100000);

        begin
            logic [31:0] hit_flag, hit_t, hit_tri;
            rtu_read(RTU_REG_HIT_FLAG,   hit_flag);
            rtu_read(RTU_REG_HIT_T,      hit_t);
            rtu_read(RTU_REG_HIT_TRI_ID, hit_tri);

            $display("  hit=%0d  tri_id=%0d  t=0x%08x (expect 0x%08x ≈ 2.0)",
                     hit_flag[0], hit_tri, hit_t, fxp(2, 0));

            if (hit_flag[0] && hit_tri == 32'h1) begin
                $display("  PASS");
                pass_count++;
            end else begin
                $display("  FAIL: Expected hit on tri_id=1");
                fail_count++;
            end
        end

        rtu_write(RTU_REG_CTRL, 32'h0);
        repeat (10) @(posedge clk);

        // ==================================================================
        // Test 2: Ray misses (pointing -Z)
        // ==================================================================
        $display("\n=== Test 2: Ray misses (pointing -Z) ===");
        rtu_write(RTU_REG_RAY_DZ,   fxp(-1, 0));
        rtu_write(RTU_REG_INV_DZ,   fxp(-1, 0));
        rtu_write(RTU_REG_CTRL,     32'h0000_0009);

        wait_rtu_irq(100000);

        begin
            logic [31:0] hit_flag;
            rtu_read(RTU_REG_HIT_FLAG, hit_flag);
            $display("  hit=%0d (expect 0)", hit_flag[0]);
            if (!hit_flag[0]) begin
                $display("  PASS");
                pass_count++;
            end else begin
                $display("  FAIL: Expected miss");
                fail_count++;
            end
        end

        rtu_write(RTU_REG_CTRL, 32'h0);
        repeat (10) @(posedge clk);

        // ==================================================================
        // Test 3: Ray far outside should miss
        // ==================================================================
        $display("\n=== Test 3: Ray outside geometry ===");
        rtu_write(RTU_REG_RAY_OX,   fxp(10, 0));
        rtu_write(RTU_REG_RAY_OY,   fxp(10, 0));
        rtu_write(RTU_REG_RAY_DZ,   fxp(1, 0));
        rtu_write(RTU_REG_INV_DZ,   fxp(1, 0));
        rtu_write(RTU_REG_CTRL,     32'h0000_0009);

        wait_rtu_irq(100000);

        begin
            logic [31:0] hit_flag;
            rtu_read(RTU_REG_HIT_FLAG, hit_flag);
            $display("  hit=%0d (expect 0)", hit_flag[0]);
            if (!hit_flag[0]) begin
                $display("  PASS");
                pass_count++;
            end else begin
                $display("  FAIL");
                fail_count++;
            end
        end

        rtu_write(RTU_REG_CTRL, 32'h0);
        repeat (10) @(posedge clk);

        // ==================================================================
        // Test 4: CU slot 0 status readable
        // ==================================================================
        $display("\n=== Test 4: CU slot 0 status check ===");
        begin
            logic [31:0] cu_status;
            @(posedge clk);
            slot_reg_rd_valid    <= '0;
            slot_reg_rd_valid[0] <= 1'b1;
            slot_reg_rd_addr     <= 5'd0;
            @(posedge clk);
            cu_status = slot_reg_rd_data[0];
            slot_reg_rd_valid    <= '0;
            $display("  CU slot 0 csr_status = 0x%08x", cu_status);
            $display("  PASS: CU slot active and responding");
            pass_count++;
        end

        // ==================================================================
        // Test 5: Performance counters
        // ==================================================================
        $display("\n=== Test 5: RTU perf counters ===");
        begin
            logic [31:0] perf_rays;
            rtu_read(RTU_REG_PERF_RAYS, perf_rays);
            $display("  Total rays traced: %0d (expect 3)", perf_rays);
            if (perf_rays == 32'd3) begin
                $display("  PASS");
                pass_count++;
            end else begin
                $display("  FAIL: Expected 3 rays");
                fail_count++;
            end
        end

        // ==================================================================
        // Summary
        // ==================================================================
        $display("\n========================================");
        $display("  1CU+1RT Cluster TB: %0d PASS, %0d FAIL", pass_count, fail_count);
        $display("========================================");
        if (fail_count == 0)
            $display("  ALL TESTS PASSED");
        else
            $display("  SOME TESTS FAILED");

        repeat (20) @(posedge clk);
        $finish;
    end

    // -----------------------------------------------------------------------
    // Watchdog
    // -----------------------------------------------------------------------
    initial begin
        #20_000_000;
        $display("ERROR: Global watchdog timeout");
        $finish;
    end

    // -----------------------------------------------------------------------
    // VCD dump
    // -----------------------------------------------------------------------
    initial begin
        $dumpfile("cluster_1cu_1rt_tb.vcd");
        $dumpvars(0, cluster_1cu_1rt_tb);
    end

endmodule : cluster_1cu_1rt_tb
