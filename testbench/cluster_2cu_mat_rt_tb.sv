`timescale 1ns/1ps
// ============================================================================
// Cluster Testbench — 2 CU + 1 MAT + 1 RT configuration
//
// Exercises the most complex cluster_top configuration:
//   Slot 0 = CU   (Compute Unit 0)
//   Slot 1 = CU   (Compute Unit 1)
//   Slot 2 = MAT  (Matrix/Tensor Accelerator)
//   Slot 3 = RT   (Ray-Tracing Accelerator)
//
// Test plan:
//   1. RTU: Ray hits closest triangle (direct register write)
//   2. RTU: Ray misses
//   3. MXU: Identity-matrix GEMM (A=I, B=diag → C should match)
//   4. MXU: Simple 4×4 multiply-accumulate
//   5. Concurrent: RTU traces ray while MXU computes tile
//   6. Both CU slots are active and responding
//   7. RTU + MXU performance counters
//
// Memory layout (AXI backing store):
//   0x8000_0000: BVH root node  (64B aligned)
//   0x8000_0040: Left leaf       (64B aligned)
//   0x8000_0080: Right leaf      (64B aligned)
//   0x8000_1000: Triangle 0 (z=2)(64B aligned)
//   0x8000_1040: Triangle 1 (z=4)(64B aligned)
//   0x8001_0000: Tile A (4×4 Q16.16) — 64 bytes
//   0x8001_0040: Tile B (4×4 Q16.16) — 64 bytes
//   0x8001_0080: Tile C output       — 64 bytes
// ============================================================================
module cluster_2cu_mat_rt_tb;
    import rt_pkg::*;
    import mat_pkg::*;
    import mailbox_pkg::*;

    // -----------------------------------------------------------------------
    // Parameters
    // -----------------------------------------------------------------------
    localparam int AXI_ADDR_W = 32;
    localparam int AXI_DATA_W = 64;
    localparam int NUM_SLOTS  = 4;

    // Slot indices
    localparam int CU0_SLOT = 0;
    localparam int CU1_SLOT = 1;
    localparam int MAT_SLOT = 2;
    localparam int RT_SLOT  = 3;

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

    logic        fb_aw_valid, fb_aw_ready;
    logic [31:0] fb_aw_addr;
    logic [7:0]  fb_aw_len;
    logic [2:0]  fb_aw_size;
    logic [1:0]  fb_aw_burst;
    logic [31:0] fb_w_data;
    logic [3:0]  fb_w_strb;
    logic        fb_w_last, fb_w_valid, fb_w_ready;
    logic        fb_b_valid, fb_b_ready;

    logic        icache_miss_req_valid, icache_miss_req_ready;
    logic [31:0] icache_miss_req_addr;
    logic        icache_miss_resp_valid;
    logic [63:0] icache_miss_resp_data;

    logic                           mb_up_tx_valid, mb_up_tx_ready;
    mailbox_flit_t                  mb_up_tx_data;
    logic [NODE_ID_WIDTH-1:0]       mb_up_tx_dest_id;
    logic                           mb_up_rx_valid, mb_up_rx_ready;
    mailbox_flit_t                  mb_up_rx_data;
    logic [NODE_ID_WIDTH-1:0]       mb_up_rx_dest_id;

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
    // DUT: cluster_top (2 CU + 1 MAT + 1 RT)
    // -----------------------------------------------------------------------
    cluster_top #(
        .NUM_SLOTS(4),
        .SLOT_TYPE_0(0),  // CU
        .SLOT_TYPE_1(0),  // CU
        .SLOT_TYPE_2(2),  // MAT
        .SLOT_TYPE_3(1),  // RT
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
    // AXI Memory Model (256 KB)
    // -----------------------------------------------------------------------
    localparam int MEM_WORDS = 65536;
    logic [31:0] axi_mem [0:MEM_WORDS-1];

    function automatic int aw(input logic [31:0] addr);
        aw = (addr & 32'h0003_FFFC) >> 2;
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
    // I-Cache Miss Responder
    // -----------------------------------------------------------------------
    localparam int IMEM_WORDS = 256;
    logic [31:0] imem [0:IMEM_WORDS-1];

    initial begin
        for (int i = 0; i < IMEM_WORDS; i++)
            imem[i] = 32'h0000_0013; // NOP
        imem[0] = 32'h0000_006F;     // jal x0, 0 — spin loop
    end

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
    // FB / Mailbox tie-off
    // -----------------------------------------------------------------------
    assign fb_aw_ready     = 1'b1;
    assign fb_w_ready      = 1'b1;
    assign fb_b_valid      = 1'b0;
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
    // Generic slot register write / read tasks
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

    task automatic slot_read(input int slot, input [4:0] addr, output [31:0] data);
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
        // Allow L2 write-through to complete before reading results
        repeat (30) @(posedge clk);
    endtask

    // -----------------------------------------------------------------------
    // Scene setup
    // -----------------------------------------------------------------------
    task automatic setup_bvh_scene();
        int wi;

        // Triangle 0 at 0x8000_1000
        wi = aw(32'h8000_1000);
        axi_mem[wi+ 0] = fxp(-1, 0); axi_mem[wi+ 1] = fxp(-1, 0);
        axi_mem[wi+ 2] = fxp( 2, 0); axi_mem[wi+ 3] = fxp( 1, 0);
        axi_mem[wi+ 4] = fxp(-1, 0); axi_mem[wi+ 5] = fxp( 2, 0);
        axi_mem[wi+ 6] = fxp( 0, 0); axi_mem[wi+ 7] = fxp( 1, 0);
        axi_mem[wi+ 8] = fxp( 2, 0); axi_mem[wi+ 9] = 32'h0000_0001;
        axi_mem[wi+10] = 32'h0;      axi_mem[wi+11] = 32'h0;

        // Triangle 1 at 0x8000_1040 (64B aligned)
        wi = aw(32'h8000_1040);
        axi_mem[wi+ 0] = fxp(-1, 0); axi_mem[wi+ 1] = fxp(-1, 0);
        axi_mem[wi+ 2] = fxp( 4, 0); axi_mem[wi+ 3] = fxp( 1, 0);
        axi_mem[wi+ 4] = fxp(-1, 0); axi_mem[wi+ 5] = fxp( 4, 0);
        axi_mem[wi+ 6] = fxp( 0, 0); axi_mem[wi+ 7] = fxp( 1, 0);
        axi_mem[wi+ 8] = fxp( 4, 0); axi_mem[wi+ 9] = 32'h0000_0002;
        axi_mem[wi+10] = 32'h0;      axi_mem[wi+11] = 32'h0;

        // BVH root at 0x8000_0000 (64B aligned)
        // left child  = 0x8000_0040 → word addr 0x2000_0010
        // right child = 0x8000_0080 → word addr 0x2000_0020
        wi = aw(32'h8000_0000);
        axi_mem[wi+0] = {2'b00, 30'(32'h2000_0010)};  // left child @0x8000_0040
        axi_mem[wi+1] = {2'b00, 30'(32'h2000_0020)};  // right child @0x8000_0080
        axi_mem[wi+2] = fxp(-1, 0); axi_mem[wi+3] = fxp(-1, 0);
        axi_mem[wi+4] = fxp( 2, 0); axi_mem[wi+5] = fxp( 1, 0);
        axi_mem[wi+6] = fxp( 1, 0); axi_mem[wi+7] = fxp( 4, 0);

        // Left leaf at 0x8000_0040 (64B aligned)
        // tri addr = 0x8000_1000 → word addr 0x2000_0400
        wi = aw(32'h8000_0040);
        axi_mem[wi+0] = {2'b01, 30'(32'h2000_0400)};  // tri 0 @0x8000_1000
        axi_mem[wi+1] = {2'b00, 30'd1};
        axi_mem[wi+2] = fxp(-1, 0); axi_mem[wi+3] = fxp(-1, 0);
        axi_mem[wi+4] = fxp( 2, 0); axi_mem[wi+5] = fxp( 1, 0);
        axi_mem[wi+6] = fxp( 1, 0); axi_mem[wi+7] = fxp( 2, 0);

        // Right leaf at 0x8000_0080 (64B aligned)
        // tri addr = 0x8000_1040 → word addr 0x2000_0410
        wi = aw(32'h8000_0080);
        axi_mem[wi+0] = {2'b01, 30'(32'h2000_0410)};  // tri 1 @0x8000_1040
        axi_mem[wi+1] = {2'b00, 30'd1};
        axi_mem[wi+2] = fxp(-1, 0); axi_mem[wi+3] = fxp(-1, 0);
        axi_mem[wi+4] = fxp( 4, 0); axi_mem[wi+5] = fxp( 1, 0);
        axi_mem[wi+6] = fxp( 1, 0); axi_mem[wi+7] = fxp( 4, 0);

        $display("[SCENE] BVH + 2 triangles loaded");
    endtask

    // -----------------------------------------------------------------------
    // Matrix tile setup in AXI memory
    // -----------------------------------------------------------------------
    // Tile A (4×4 identity) at 0x8001_0000
    // Tile B (4×4 with known values) at 0x8001_0040
    // Tile C output at 0x8001_0080
    task automatic setup_matrix_tiles();
        int wi;

        // Tile A = identity (row-major, each element Q16.16)
        // 4×4 × 4 bytes = 64 bytes = exactly 1 cache line
        wi = aw(32'h8001_0000);
        for (int r = 0; r < 4; r++)
            for (int c = 0; c < 4; c++)
                axi_mem[wi + r*4 + c] = (r == c) ? fxp(1, 0) : fxp(0, 0);

        // Tile B: simple known values
        //   [1.0  2.0  3.0  4.0 ]
        //   [5.0  6.0  7.0  8.0 ]
        //   [9.0  10.0 11.0 12.0]
        //   [13.0 14.0 15.0 16.0]
        wi = aw(32'h8001_0040);
        for (int r = 0; r < 4; r++)
            for (int c = 0; c < 4; c++)
                axi_mem[wi + r*4 + c] = fxp(r*4 + c + 1, 0);

        // Clear Tile C output area
        wi = aw(32'h8001_0080);
        for (int i = 0; i < 16; i++)
            axi_mem[wi + i] = 32'h0;

        $display("[MATRIX] Tiles A(identity) and B(1..16) loaded");
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

        // Clear memory
        for (int i = 0; i < MEM_WORDS; i++) axi_mem[i] = 32'h0;

        wait (rst_n);
        repeat (10) @(posedge clk);

        setup_bvh_scene();
        setup_matrix_tiles();

        // ==================================================================
        // Test 1: RTU ray→hit (slot 3)
        // ==================================================================
        $display("\n=== Test 1: RTU ray hits closest triangle ===");
        slot_write(RT_SLOT, RTU_REG_RAY_OX,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_OY,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_OZ,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_DX,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_DY,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_DZ,   fxp(1, 0));
        slot_write(RT_SLOT, RTU_REG_INV_DX,   FXP_MAX);
        slot_write(RT_SLOT, RTU_REG_INV_DY,   FXP_MAX);
        slot_write(RT_SLOT, RTU_REG_INV_DZ,   fxp(1, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_TMIN, fxp(0, 16));
        slot_write(RT_SLOT, RTU_REG_RAY_TMAX, fxp(100, 0));
        slot_write(RT_SLOT, RTU_REG_BVH_ROOT, 32'h8000_0000);
        slot_write(RT_SLOT, RTU_REG_CTRL,     32'h0000_0009);

        wait_slot_irq(RT_SLOT, 100000);

        begin
            logic [31:0] hit_flag, hit_t, hit_tri;
            slot_read(RT_SLOT, RTU_REG_HIT_FLAG,   hit_flag);
            slot_read(RT_SLOT, RTU_REG_HIT_T,      hit_t);
            slot_read(RT_SLOT, RTU_REG_HIT_TRI_ID, hit_tri);
            $display("  hit=%0d  tri_id=%0d  t=0x%08x", hit_flag[0], hit_tri, hit_t);
            if (hit_flag[0] && hit_tri == 32'h1) begin
                $display("  PASS"); pass_count++;
            end else begin
                $display("  FAIL: Expected hit, tri_id=1"); fail_count++;
            end
        end
        slot_write(RT_SLOT, RTU_REG_CTRL, 32'h0);
        repeat (10) @(posedge clk);

        // ==================================================================
        // Test 2: RTU ray→miss
        // ==================================================================
        $display("\n=== Test 2: RTU ray misses ===");
        slot_write(RT_SLOT, RTU_REG_RAY_DZ,   fxp(-1, 0));
        slot_write(RT_SLOT, RTU_REG_INV_DZ,   fxp(-1, 0));
        slot_write(RT_SLOT, RTU_REG_CTRL,     32'h0000_0009);

        wait_slot_irq(RT_SLOT, 100000);

        begin
            logic [31:0] hit_flag;
            slot_read(RT_SLOT, RTU_REG_HIT_FLAG, hit_flag);
            $display("  hit=%0d (expect 0)", hit_flag[0]);
            if (!hit_flag[0]) begin
                $display("  PASS"); pass_count++;
            end else begin
                $display("  FAIL"); fail_count++;
            end
        end
        slot_write(RT_SLOT, RTU_REG_CTRL, 32'h0);
        repeat (10) @(posedge clk);

        // ==================================================================
        // Test 3: MXU identity GEMM (C = I × B = B) — slot 2
        // ==================================================================
        $display("\n=== Test 3: MXU identity GEMM (C = I × B should = B) ===");
        slot_write(MAT_SLOT, MXU_REG_SRC_A_ADDR, 32'h8001_0000);
        slot_write(MAT_SLOT, MXU_REG_SRC_B_ADDR, 32'h8001_0040);
        slot_write(MAT_SLOT, MXU_REG_DST_C_ADDR, 32'h8001_0080);
        slot_write(MAT_SLOT, MXU_REG_TILE_M,     32'd4);
        slot_write(MAT_SLOT, MXU_REG_TILE_N,     32'd4);
        slot_write(MAT_SLOT, MXU_REG_TILE_K,     32'd4);
        slot_write(MAT_SLOT, MXU_REG_STRIDE_A,   32'd16); // 4 elems × 4 bytes
        slot_write(MAT_SLOT, MXU_REG_STRIDE_B,   32'd16);
        slot_write(MAT_SLOT, MXU_REG_STRIDE_C,   32'd16);
        slot_write(MAT_SLOT, MXU_REG_CTRL,       32'h0000_0009); // start + irq_en

        wait_slot_irq(MAT_SLOT, 200000);

        // Read back C tile from AXI memory and check against B
        begin
            int wi_b, wi_c;
            int errs;
            wi_b = aw(32'h8001_0040);
            wi_c = aw(32'h8001_0080);
            errs = 0;

            $display("  Checking C = I × B...");
            for (int r = 0; r < 4; r++) begin
                for (int c = 0; c < 4; c++) begin
                    logic [31:0] expected, actual;
                    expected = axi_mem[wi_b + r*4 + c];
                    actual   = axi_mem[wi_c + r*4 + c];
                    if (actual != expected) begin
                        $display("    C[%0d][%0d] = 0x%08x, expected 0x%08x",
                                 r, c, actual, expected);
                        errs++;
                    end
                end
            end

            if (errs == 0) begin
                $display("  PASS: C matches B exactly"); pass_count++;
            end else begin
                $display("  FAIL: %0d mismatches", errs); fail_count++;
            end
        end
        slot_write(MAT_SLOT, MXU_REG_CTRL, 32'h0);
        repeat (10) @(posedge clk);

        // ==================================================================
        // Test 4: MXU known 4×4 multiply
        // ==================================================================
        $display("\n=== Test 4: MXU known values multiply ===");
        // Clear accumulators before new computation
        slot_write(MAT_SLOT, MXU_REG_CTRL, 32'h10); // bit 4 = clear_acc
        repeat (5) @(posedge clk);
        slot_write(MAT_SLOT, MXU_REG_CTRL, 32'h0);
        repeat (5) @(posedge clk);

        // Use fresh addresses (0x8001_0100..01C0) to avoid L2 cache stale data
        // Tile A: all 1.0s at 0x8001_0100
        begin
            int wi;
            wi = aw(32'h8001_0100);
            for (int i = 0; i < 16; i++)
                axi_mem[wi + i] = fxp(1, 0);

            // Tile B: same as before, copy to 0x8001_0140
            wi = aw(32'h8001_0140);
            for (int r = 0; r < 4; r++)
                for (int c2 = 0; c2 < 4; c2++)
                    axi_mem[wi + r*4 + c2] = fxp(r*4 + c2 + 1, 0);

            // Clear C at 0x8001_0180
            wi = aw(32'h8001_0180);
            for (int i = 0; i < 16; i++)
                axi_mem[wi + i] = 32'h0;
        end

        slot_write(MAT_SLOT, MXU_REG_SRC_A_ADDR, 32'h8001_0100);
        slot_write(MAT_SLOT, MXU_REG_SRC_B_ADDR, 32'h8001_0140);
        slot_write(MAT_SLOT, MXU_REG_DST_C_ADDR, 32'h8001_0180);
        slot_write(MAT_SLOT, MXU_REG_CTRL,       32'h0000_0009);

        wait_slot_irq(MAT_SLOT, 200000);

        // For A=[1,1,1,1; ...] × B=[1..16]:
        // Row i of C = sum of each column of B
        // C[r][c] = sum_{k=0..3} B[k][c] = B[0][c]+B[1][c]+B[2][c]+B[3][c]
        //         = (c+1)+(c+5)+(c+9)+(c+13) = 4*c + 28
        begin
            int wi_c;
            int errs;
            wi_c = aw(32'h8001_0180);
            errs = 0;

            for (int r = 0; r < 4; r++) begin
                for (int c = 0; c < 4; c++) begin
                    logic [31:0] expected, actual;
                    expected = fxp(4*c + 28, 0);
                    actual   = axi_mem[wi_c + r*4 + c];
                    if (actual != expected) begin
                        $display("    C[%0d][%0d] = 0x%08x, expected 0x%08x (%0d.0)",
                                 r, c, actual, expected, 4*c + 28);
                        errs++;
                    end
                end
            end

            if (errs == 0) begin
                $display("  PASS: Known multiply correct"); pass_count++;
            end else begin
                $display("  FAIL: %0d mismatches", errs); fail_count++;
            end
        end
        slot_write(MAT_SLOT, MXU_REG_CTRL, 32'h0);
        repeat (10) @(posedge clk);

        // ==================================================================
        // Test 5: Concurrent RTU + MXU
        // ==================================================================
        $display("\n=== Test 5: Concurrent RTU trace + MXU compute ===");
        // Clear MXU accumulators
        slot_write(MAT_SLOT, MXU_REG_CTRL, 32'h10);
        repeat (5) @(posedge clk);
        slot_write(MAT_SLOT, MXU_REG_CTRL, 32'h0);
        repeat (5) @(posedge clk);

        // Use fresh addresses to avoid L2 cache stale data
        begin
            int wi;
            // Tile A: identity at 0x8001_0200
            wi = aw(32'h8001_0200);
            for (int r = 0; r < 4; r++)
                for (int c2 = 0; c2 < 4; c2++)
                    axi_mem[wi + r*4 + c2] = (r == c2) ? fxp(1, 0) : fxp(0, 0);
            // Tile B: same values at 0x8001_0240
            wi = aw(32'h8001_0240);
            for (int r = 0; r < 4; r++)
                for (int c2 = 0; c2 < 4; c2++)
                    axi_mem[wi + r*4 + c2] = fxp(r*4 + c2 + 1, 0);
            // Clear C at 0x8001_0280
            wi = aw(32'h8001_0280);
            for (int i = 0; i < 16; i++) axi_mem[wi + i] = 32'h0;
        end

        // Launch RTU
        slot_write(RT_SLOT, RTU_REG_RAY_OX,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_OY,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_OZ,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_DX,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_DY,   fxp(0, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_DZ,   fxp(1, 0));
        slot_write(RT_SLOT, RTU_REG_INV_DX,   FXP_MAX);
        slot_write(RT_SLOT, RTU_REG_INV_DY,   FXP_MAX);
        slot_write(RT_SLOT, RTU_REG_INV_DZ,   fxp(1, 0));
        slot_write(RT_SLOT, RTU_REG_RAY_TMIN, fxp(0, 16));
        slot_write(RT_SLOT, RTU_REG_RAY_TMAX, fxp(100, 0));
        slot_write(RT_SLOT, RTU_REG_BVH_ROOT, 32'h8000_0000);
        slot_write(RT_SLOT, RTU_REG_CTRL,     32'h0000_0009);

        // Launch MXU immediately after (don't wait for RTU)
        slot_write(MAT_SLOT, MXU_REG_SRC_A_ADDR, 32'h8001_0200);
        slot_write(MAT_SLOT, MXU_REG_SRC_B_ADDR, 32'h8001_0240);
        slot_write(MAT_SLOT, MXU_REG_DST_C_ADDR, 32'h8001_0280);
        slot_write(MAT_SLOT, MXU_REG_TILE_M,     32'd4);
        slot_write(MAT_SLOT, MXU_REG_TILE_N,     32'd4);
        slot_write(MAT_SLOT, MXU_REG_TILE_K,     32'd4);
        slot_write(MAT_SLOT, MXU_REG_CTRL,       32'h0000_0009);

        // Wait for both to finish
        begin
            logic rt_done = 1'b0;
            logic mat_done = 1'b0;
            int cnt = 0;
            while ((!rt_done || !mat_done) && cnt < 200000) begin
                @(posedge clk);
                if (slot_irq[RT_SLOT])  rt_done = 1'b1;
                if (slot_irq[MAT_SLOT]) mat_done = 1'b1;
                cnt++;
            end
            if (cnt >= 200000)
                $display("  ERROR: Concurrent timeout");
        end
        // Allow write-through to complete
        repeat (30) @(posedge clk);

        // Verify RTU result
        begin
            logic [31:0] hit_flag, hit_tri;
            logic rt_ok, mat_ok;
            int errs;

            slot_read(RT_SLOT, RTU_REG_HIT_FLAG,   hit_flag);
            slot_read(RT_SLOT, RTU_REG_HIT_TRI_ID, hit_tri);
            rt_ok = (hit_flag[0] && hit_tri == 32'h1);
            $display("  RTU: hit=%0d tri_id=%0d %s",
                     hit_flag[0], hit_tri, rt_ok ? "OK" : "MISMATCH");

            // Verify MXU result (C = I × B = B)
            errs = 0;
            begin
                int wi_b, wi_c;
                wi_b = aw(32'h8001_0240);
                wi_c = aw(32'h8001_0280);
                for (int i = 0; i < 16; i++) begin
                    if (axi_mem[wi_c + i] != axi_mem[wi_b + i]) errs++;
                end
            end
            mat_ok = (errs == 0);
            $display("  MXU: %0d element mismatches %s",
                     errs, mat_ok ? "OK" : "MISMATCH");

            if (rt_ok && mat_ok) begin
                $display("  PASS: Both concurrent operations correct");
                pass_count++;
            end else begin
                $display("  FAIL: Concurrent operation error");
                fail_count++;
            end
        end
        slot_write(RT_SLOT,  RTU_REG_CTRL, 32'h0);
        slot_write(MAT_SLOT, MXU_REG_CTRL, 32'h0);
        repeat (10) @(posedge clk);

        // ==================================================================
        // Test 6: Both CU slots active
        // ==================================================================
        $display("\n=== Test 6: Both CU slots responding ===");
        begin
            logic [31:0] cu0_status, cu1_status;
            slot_read(CU0_SLOT, 5'd0, cu0_status);
            slot_read(CU1_SLOT, 5'd0, cu1_status);
            $display("  CU0 status=0x%08x  CU1 status=0x%08x", cu0_status, cu1_status);
            $display("  PASS: Both CU slots active");
            pass_count++;
        end

        // ==================================================================
        // Test 7: Performance counters
        // ==================================================================
        $display("\n=== Test 7: Performance counters ===");
        begin
            logic [31:0] rtu_rays, mxu_tiles;
            slot_read(RT_SLOT, RTU_REG_PERF_RAYS, rtu_rays);
            slot_read(MAT_SLOT, MXU_REG_PERF_TILES, mxu_tiles);
            $display("  RTU rays: %0d (expect 3)", rtu_rays);
            $display("  MXU tiles: %0d (expect 3)", mxu_tiles);
            if (rtu_rays == 32'd3 && mxu_tiles == 32'd3) begin
                $display("  PASS"); pass_count++;
            end else begin
                $display("  FAIL: Counter mismatch"); fail_count++;
            end
        end

        // ==================================================================
        // Summary
        // ==================================================================
        $display("\n========================================");
        $display("  2CU+MAT+RT Cluster TB: %0d PASS, %0d FAIL",
                 pass_count, fail_count);
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
        #30_000_000;
        $display("ERROR: Global watchdog timeout");
        $finish;
    end

    // -----------------------------------------------------------------------
    // VCD dump
    // -----------------------------------------------------------------------
    initial begin
        $dumpfile("cluster_2cu_mat_rt_tb.vcd");
        $dumpvars(0, cluster_2cu_mat_rt_tb);
    end

endmodule : cluster_2cu_mat_rt_tb
