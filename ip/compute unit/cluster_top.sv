`timescale 1ns/1ps
// ============================================================================
// Cluster Top — Parameterized multi-CU cluster with shared L2 + RTU
//
// Integrates NUM_CUS compute units with:
//   - Shared L2 data cache (per-CU port, way partitioning)
//   - Shared ray-tracing unit (optional, bus-master via extra L2 port)
//   - Mailbox switch fabric (2x1 for 2 CUs, 4x1 for 4 CUs)
//   - I-cache miss arbiter (round-robin across CUs)
//   - Framebuffer AXI arbiter (round-robin across CUs)
//   - Configurable features via parameters
//
// Memory map (cluster-local):
//   Per-CU IMEM broadcast:   0x6000_0000 + CU*0x1000
//   RTU registers:           0x6200_0000 (32 words)
//   L2 control (mailbox):    via MailboxFabric CSRs
//
// External interfaces:
//   - AXI4 master: DRAM/PSRAM (L2 backing store)
//   - AXI4 master: Framebuffer (merged from all CU ROP paths)
//   - I-cache miss port: to system memory or higher cache
//   - Mailbox uplink: to center router or system bus
// ============================================================================
module cluster_top
  import rt_pkg::*;
  import mailbox_pkg::*;
#(
    // -----------------------------------------------------------------------
    // Cluster geometry
    // -----------------------------------------------------------------------
    parameter int  NUM_CUS        = 2,      // 2 or 4 compute units
    parameter logic [7:0] CLUSTER_ID = 8'h01,

    // -----------------------------------------------------------------------
    // Feature enables (reconfigurable CU)
    // -----------------------------------------------------------------------
    parameter bit  RT_ENABLE      = 1'b1,   // Instantiate shared RTU
    parameter bit  MAILBOX_ENABLE = 1'b1,   // Enable mailbox fabric
    parameter bit  GFX_ENABLE     = 1'b1,   // Enable graphics pipeline in CUs

    // -----------------------------------------------------------------------
    // Cache parameters
    // -----------------------------------------------------------------------
    parameter int  L2_SIZE_BYTES  = 16384,  // 16 KB default
    parameter int  L2_LINE_BYTES  = 64,
    parameter int  L2_ASSOC       = 4,
    parameter bit  L2_WRITEBACK   = 1'b0,   // 0=WT, 1=WB

    // -----------------------------------------------------------------------
    // CU parameters (forwarded)
    // -----------------------------------------------------------------------
    parameter int  GFX_ISSUE_Q_DEPTH  = 8,
    parameter int  TEX_REQ_Q_DEPTH    = 2,

    // -----------------------------------------------------------------------
    // AXI parameters
    // -----------------------------------------------------------------------
    parameter int  AXI_ADDR_W   = 32,
    parameter int  AXI_DATA_W   = 64
)(
    input  logic        clk,
    input  logic        rst_n,

    // ===== External AXI4 master — L2 backing store (DRAM/PSRAM) =====
    output logic [AXI_ADDR_W-1:0]  m_axi_awaddr,
    output logic [7:0]             m_axi_awlen,
    output logic [2:0]             m_axi_awsize,
    output logic [1:0]             m_axi_awburst,
    output logic                   m_axi_awvalid,
    input  logic                   m_axi_awready,

    output logic [AXI_DATA_W-1:0]  m_axi_wdata,
    output logic [AXI_DATA_W/8-1:0] m_axi_wstrb,
    output logic                   m_axi_wlast,
    output logic                   m_axi_wvalid,
    input  logic                   m_axi_wready,

    input  logic                   m_axi_bvalid,
    output logic                   m_axi_bready,

    output logic [AXI_ADDR_W-1:0]  m_axi_araddr,
    output logic [7:0]             m_axi_arlen,
    output logic [2:0]             m_axi_arsize,
    output logic [1:0]             m_axi_arburst,
    output logic                   m_axi_arvalid,
    input  logic                   m_axi_arready,

    input  logic [AXI_DATA_W-1:0]  m_axi_rdata,
    input  logic                   m_axi_rvalid,
    input  logic                   m_axi_rlast,
    output logic                   m_axi_rready,

    // ===== External AXI4 master — Framebuffer (merged ROP writes) =====
    output logic                   fb_aw_valid,
    output logic [31:0]            fb_aw_addr,
    output logic [7:0]             fb_aw_len,
    output logic [2:0]             fb_aw_size,
    output logic [1:0]             fb_aw_burst,
    input  logic                   fb_aw_ready,

    output logic [31:0]            fb_w_data,
    output logic [3:0]             fb_w_strb,
    output logic                   fb_w_last,
    output logic                   fb_w_valid,
    input  logic                   fb_w_ready,

    input  logic                   fb_b_valid,
    output logic                   fb_b_ready,

    // ===== I-cache miss port (shared, arbitrated) =====
    output logic                   icache_miss_req_valid,
    output logic [31:0]            icache_miss_req_addr,
    input  logic                   icache_miss_req_ready,
    input  logic                   icache_miss_resp_valid,
    input  logic [63:0]            icache_miss_resp_data,

    // ===== Mailbox uplink (to center router) =====
    output logic                           mb_up_awvalid,
    input  logic                           mb_up_awready,
    output logic [15:0]                    mb_up_awaddr,
    output logic                           mb_up_wvalid,
    input  logic                           mb_up_wready,
    output logic [31:0]                    mb_up_wdata,
    output logic [3:0]                     mb_up_wstrb,
    output mailbox_tag_t                   mb_up_tag,
    input  logic                           mb_up_bvalid,
    output logic                           mb_up_bready,
    output logic                           mb_up_arvalid,
    input  logic                           mb_up_arready,
    output logic [15:0]                    mb_up_araddr,
    input  logic                           mb_up_rvalid,
    output logic                           mb_up_rready,
    input  logic [31:0]                    mb_up_rdata,

    // Downlink from center to this cluster
    input  logic                           mb_dn_awvalid,
    output logic                           mb_dn_awready,
    input  logic [15:0]                    mb_dn_awaddr,
    input  logic                           mb_dn_wvalid,
    output logic                           mb_dn_wready,
    input  logic [31:0]                    mb_dn_wdata,
    input  logic [3:0]                     mb_dn_wstrb,
    input  mailbox_tag_t                   mb_dn_tag,
    output logic                           mb_dn_bvalid,
    input  logic                           mb_dn_bready,
    input  logic                           mb_dn_arvalid,
    output logic                           mb_dn_arready,
    input  logic [15:0]                    mb_dn_araddr,
    output logic                           mb_dn_rvalid,
    input  logic                           mb_dn_rready,
    output logic [31:0]                    mb_dn_rdata,

    // ===== RTU interface (direct access from software via memory-mapped) =====
    input  logic        rtu_reg_wr_valid,
    input  logic [4:0]  rtu_reg_wr_addr,
    input  logic [31:0] rtu_reg_wr_data,
    output logic        rtu_reg_wr_ready,
    input  logic        rtu_reg_rd_valid,
    input  logic [4:0]  rtu_reg_rd_addr,
    output logic [31:0] rtu_reg_rd_data,
    output logic        rtu_reg_rd_ready,
    output logic        rtu_irq,

    // ===== Status / debug =====
    output logic [NUM_CUS-1:0]    cu_err_fp_overflow,
    output logic [NUM_CUS-1:0]    cu_err_fp_invalid,
    output logic [NUM_CUS-1:0]    cu_err_vec_overflow,
    output logic [NUM_CUS-1:0]    cu_err_vec_invalid,
    output logic [NUM_CUS-1:0][31:0] cu_csr_status
);

    // -----------------------------------------------------------------------
    // L2 port count: NUM_CUS + 1 if RT enabled (RTU gets its own port)
    // -----------------------------------------------------------------------
    localparam int L2_NUM_PORTS = RT_ENABLE ? (NUM_CUS + 1) : NUM_CUS;
    localparam int RTU_L2_PORT  = NUM_CUS; // RTU uses the last port

    // -----------------------------------------------------------------------
    // Per-CU wiring arrays
    // -----------------------------------------------------------------------
    // I-cache miss
    logic [NUM_CUS-1:0]        cu_icache_req_valid;
    logic [NUM_CUS-1:0][31:0]  cu_icache_req_addr;
    logic [NUM_CUS-1:0]        cu_icache_req_ready;
    logic [NUM_CUS-1:0]        cu_icache_resp_valid;
    logic [NUM_CUS-1:0][63:0]  cu_icache_resp_data;

    // D-cache → L2
    logic [NUM_CUS-1:0]         cu_dc_req_valid;
    logic [NUM_CUS-1:0]         cu_dc_req_rw;
    logic [NUM_CUS-1:0][31:0]   cu_dc_req_addr;
    logic [NUM_CUS-1:0][7:0]    cu_dc_req_size;
    logic [NUM_CUS-1:0][3:0]    cu_dc_req_qos;
    logic [NUM_CUS-1:0][7:0]    cu_dc_req_id;
    logic [NUM_CUS-1:0][511:0]  cu_dc_req_wdata;
    logic [NUM_CUS-1:0][7:0]    cu_dc_req_wstrb;
    logic [NUM_CUS-1:0]         cu_dc_req_ready;
    logic [NUM_CUS-1:0]         cu_dc_resp_valid;
    logic [NUM_CUS-1:0][63:0]   cu_dc_resp_data;
    logic [NUM_CUS-1:0][7:0]    cu_dc_resp_id;

    // Framebuffer AXI per CU
    logic [NUM_CUS-1:0]         cu_fb_aw_valid;
    logic [NUM_CUS-1:0][31:0]   cu_fb_aw_addr;
    logic [NUM_CUS-1:0][7:0]    cu_fb_aw_len;
    logic [NUM_CUS-1:0][2:0]    cu_fb_aw_size;
    logic [NUM_CUS-1:0][1:0]    cu_fb_aw_burst;
    logic [NUM_CUS-1:0]         cu_fb_aw_ready;
    logic [NUM_CUS-1:0][31:0]   cu_fb_w_data;
    logic [NUM_CUS-1:0][3:0]    cu_fb_w_strb;
    logic [NUM_CUS-1:0]         cu_fb_w_last;
    logic [NUM_CUS-1:0]         cu_fb_w_valid;
    logic [NUM_CUS-1:0]         cu_fb_w_ready;
    logic [NUM_CUS-1:0]         cu_fb_b_valid;
    logic [NUM_CUS-1:0]         cu_fb_b_ready;

    // Mailbox per CU (stream interface)
    logic [NUM_CUS-1:0]                           cu_mb_tx_valid;
    logic [NUM_CUS-1:0]                           cu_mb_tx_ready;
    mailbox_flit_t [NUM_CUS-1:0]                  cu_mb_tx_data;
    logic [NUM_CUS-1:0][NODE_ID_WIDTH-1:0]        cu_mb_tx_dest;
    logic [NUM_CUS-1:0]                           cu_mb_rx_valid;
    logic [NUM_CUS-1:0]                           cu_mb_rx_ready;
    mailbox_flit_t [NUM_CUS-1:0]                  cu_mb_rx_data;
    logic [NUM_CUS-1:0][NODE_ID_WIDTH-1:0]        cu_mb_rx_dest;

    // Per-CU status
    logic [NUM_CUS-1:0][31:0]  cu_csr_fstatus;
    logic [NUM_CUS-1:0][31:0]  cu_csr_vstatus;

    // -----------------------------------------------------------------------
    // Compute Unit instances
    // -----------------------------------------------------------------------
    generate
        for (genvar c = 0; c < NUM_CUS; c++) begin : gen_cu
            compute_unit_top #(
                .CORE_ID(32'(c)),
                .TILE_OFFSET(32'(c)),
                .GFX_ISSUE_Q_DEPTH(GFX_ENABLE ? GFX_ISSUE_Q_DEPTH : 1),
                .TEX_REQ_Q_DEPTH(TEX_REQ_Q_DEPTH),
                .MAILBOX_ENABLE(MAILBOX_ENABLE),
                .MAILBOX_SRC_ID({CLUSTER_ID[3:0], 4'(c)})
            ) u_cu (
                .clk(clk),
                .rst_n(rst_n),
                // I-cache miss
                .inst_miss_req_valid(cu_icache_req_valid[c]),
                .inst_miss_req_addr(cu_icache_req_addr[c]),
                .inst_miss_req_ready(cu_icache_req_ready[c]),
                .inst_miss_resp_valid(cu_icache_resp_valid[c]),
                .inst_miss_resp_data(cu_icache_resp_data[c]),
                // Legacy data interface (unused — tie off)
                .data_req_valid(),
                .data_req_is_load(),
                .data_req_addr(),
                .data_req_wdata(),
                .data_req_rd(),
                .data_req_ready(1'b1),
                .data_resp_valid(1'b0),
                .data_resp_rd(5'd0),
                .data_resp_data(32'd0),
                // D-cache → L2
                .dcache_mem_req_valid(cu_dc_req_valid[c]),
                .dcache_mem_req_rw(cu_dc_req_rw[c]),
                .dcache_mem_req_addr(cu_dc_req_addr[c]),
                .dcache_mem_req_size(cu_dc_req_size[c]),
                .dcache_mem_req_qos(cu_dc_req_qos[c]),
                .dcache_mem_req_id(cu_dc_req_id[c]),
                .dcache_mem_req_wdata(cu_dc_req_wdata[c]),
                .dcache_mem_req_wstrb(cu_dc_req_wstrb[c]),
                .dcache_mem_req_ready(cu_dc_req_ready[c]),
                .dcache_mem_resp_valid(cu_dc_resp_valid[c]),
                .dcache_mem_resp_data(cu_dc_resp_data[c]),
                .dcache_mem_resp_id(cu_dc_resp_id[c]),
                // Framebuffer AXI
                .fb_aw_valid(cu_fb_aw_valid[c]),
                .fb_aw_addr(cu_fb_aw_addr[c]),
                .fb_aw_len(cu_fb_aw_len[c]),
                .fb_aw_size(cu_fb_aw_size[c]),
                .fb_aw_burst(cu_fb_aw_burst[c]),
                .fb_aw_ready(cu_fb_aw_ready[c]),
                .fb_w_data(cu_fb_w_data[c]),
                .fb_w_strb(cu_fb_w_strb[c]),
                .fb_w_last(cu_fb_w_last[c]),
                .fb_w_valid(cu_fb_w_valid[c]),
                .fb_w_ready(cu_fb_w_ready[c]),
                .fb_b_valid(cu_fb_b_valid[c]),
                .fb_b_ready(cu_fb_b_ready[c]),
                // Mailbox
                .mailbox_tx_valid(cu_mb_tx_valid[c]),
                .mailbox_tx_ready(cu_mb_tx_ready[c]),
                .mailbox_tx_data(cu_mb_tx_data[c]),
                .mailbox_tx_dest_id(cu_mb_tx_dest[c]),
                .mailbox_rx_valid(cu_mb_rx_valid[c]),
                .mailbox_rx_ready(cu_mb_rx_ready[c]),
                .mailbox_rx_data(cu_mb_rx_data[c]),
                .mailbox_rx_dest_id(cu_mb_rx_dest[c]),
                // Error / status
                .err_fp_overflow(cu_err_fp_overflow[c]),
                .err_fp_invalid(cu_err_fp_invalid[c]),
                .err_vec_overflow(cu_err_vec_overflow[c]),
                .err_vec_invalid(cu_err_vec_invalid[c]),
                .csr_status(cu_csr_status[c]),
                .csr_fstatus(cu_csr_fstatus[c]),
                .csr_vstatus(cu_csr_vstatus[c])
            );
        end
    endgenerate

    // -----------------------------------------------------------------------
    // Shared L2 Data Cache
    // -----------------------------------------------------------------------
    logic [L2_NUM_PORTS-1:0]                      l2_req_valid;
    logic [L2_NUM_PORTS-1:0]                      l2_req_rw;
    logic [L2_NUM_PORTS-1:0][31:0]                l2_req_addr;
    logic [L2_NUM_PORTS-1:0][7:0]                 l2_req_size;
    logic [L2_NUM_PORTS-1:0][3:0]                 l2_req_qos;
    logic [L2_NUM_PORTS-1:0][7:0]                 l2_req_id;
    logic [L2_NUM_PORTS-1:0][L2_LINE_BYTES*8-1:0] l2_req_wdata;
    logic [L2_NUM_PORTS-1:0][L2_LINE_BYTES-1:0]   l2_req_wstrb;
    logic [L2_NUM_PORTS-1:0]                      l2_req_ready;
    logic [L2_NUM_PORTS-1:0]                      l2_resp_valid;
    logic [L2_NUM_PORTS-1:0][AXI_DATA_W-1:0]     l2_resp_data;
    logic [L2_NUM_PORTS-1:0][7:0]                 l2_resp_id;

    // Connect CU D-cache ports to L2
    generate
        for (genvar c = 0; c < NUM_CUS; c++) begin : gen_l2_cu_conn
            assign l2_req_valid[c] = cu_dc_req_valid[c];
            assign l2_req_rw[c]    = cu_dc_req_rw[c];
            assign l2_req_addr[c]  = cu_dc_req_addr[c];
            assign l2_req_size[c]  = cu_dc_req_size[c];
            assign l2_req_qos[c]   = cu_dc_req_qos[c];
            assign l2_req_id[c]    = cu_dc_req_id[c];
            assign l2_req_wdata[c] = cu_dc_req_wdata[c];
            assign l2_req_wstrb[c] = cu_dc_req_wstrb[c][L2_LINE_BYTES-1:0];
            assign cu_dc_req_ready[c] = l2_req_ready[c];
            assign cu_dc_resp_valid[c] = l2_resp_valid[c];
            assign cu_dc_resp_data[c]  = l2_resp_data[c];
            assign cu_dc_resp_id[c]    = l2_resp_id[c];
        end
    endgenerate

    // RTU L2 port (if enabled)
    logic        rtu_mem_req_valid;
    logic [31:0] rtu_mem_req_addr;
    logic [7:0]  rtu_mem_req_size;
    logic [3:0]  rtu_mem_req_qos;
    logic [7:0]  rtu_mem_req_id;
    logic        rtu_mem_req_ready;
    logic        rtu_mem_resp_valid;
    logic [63:0] rtu_mem_resp_data;
    logic [7:0]  rtu_mem_resp_id;

    generate
        if (RT_ENABLE) begin : gen_rtu_l2
            assign l2_req_valid[RTU_L2_PORT] = rtu_mem_req_valid;
            assign l2_req_rw[RTU_L2_PORT]    = 1'b0; // RTU is read-only
            assign l2_req_addr[RTU_L2_PORT]  = rtu_mem_req_addr;
            assign l2_req_size[RTU_L2_PORT]  = rtu_mem_req_size;
            assign l2_req_qos[RTU_L2_PORT]   = rtu_mem_req_qos;
            assign l2_req_id[RTU_L2_PORT]    = rtu_mem_req_id;
            assign l2_req_wdata[RTU_L2_PORT] = '0;
            assign l2_req_wstrb[RTU_L2_PORT] = '0;
            assign rtu_mem_req_ready  = l2_req_ready[RTU_L2_PORT];
            assign rtu_mem_resp_valid = l2_resp_valid[RTU_L2_PORT];
            assign rtu_mem_resp_data  = l2_resp_data[RTU_L2_PORT];
            assign rtu_mem_resp_id    = l2_resp_id[RTU_L2_PORT];
        end else begin : gen_no_rtu_l2
            assign rtu_mem_req_ready  = 1'b0;
            assign rtu_mem_resp_valid = 1'b0;
            assign rtu_mem_resp_data  = '0;
            assign rtu_mem_resp_id    = '0;
        end
    endgenerate

    l2_data_cache #(
        .L2_ENABLED(1),
        .L2_SIZE_BYTES(L2_SIZE_BYTES),
        .LINE_BYTES(L2_LINE_BYTES),
        .ASSOCIATIVITY(L2_ASSOC),
        .NUM_PORTS(L2_NUM_PORTS),
        .AXI_ADDR_BITS(AXI_ADDR_W),
        .AXI_DATA_BITS(AXI_DATA_W),
        .WRITEBACK(L2_WRITEBACK),
        .PARTITION_ENABLE(1'b1),
        .USE_MAILBOX_CTRL(1'b0)
    ) u_l2_cache (
        .clk(clk),
        .rst_n(rst_n),
        // L1 ports
        .l1_req_valid(l2_req_valid),
        .l1_req_rw(l2_req_rw),
        .l1_req_addr(l2_req_addr),
        .l1_req_size(l2_req_size),
        .l1_req_qos(l2_req_qos),
        .l1_req_id(l2_req_id),
        .l1_req_wdata(l2_req_wdata),
        .l1_req_wstrb(l2_req_wstrb),
        .l1_req_ready(l2_req_ready),
        .l1_resp_valid(l2_resp_valid),
        .l1_resp_data(l2_resp_data),
        .l1_resp_id(l2_resp_id),
        // Mailbox control (disabled)
        .mb_s_awvalid(1'b0), .mb_s_awaddr(16'h0),
        .mb_s_wvalid(1'b0), .mb_s_wdata(32'h0), .mb_s_wstrb(4'h0),
        .mb_s_tag('0), .mb_s_bready(1'b1),
        .mb_s_arvalid(1'b0), .mb_s_araddr(16'h0), .mb_s_rready(1'b1),
        .mb_s_awready(), .mb_s_wready(), .mb_s_bvalid(),
        .mb_s_arready(), .mb_s_rvalid(), .mb_s_rdata(),
        // AXI4 master
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
        .m_axi_rlast(m_axi_rlast),     .m_axi_rready(m_axi_rready)
    );

    // -----------------------------------------------------------------------
    // Ray Tracing Unit (shared, optional)
    // -----------------------------------------------------------------------
    generate
        if (RT_ENABLE) begin : gen_rtu
            ray_tracing_unit u_rtu (
                .clk(clk), .rst_n(rst_n),
                .reg_wr_valid(rtu_reg_wr_valid),
                .reg_wr_addr(rtu_reg_wr_addr),
                .reg_wr_data(rtu_reg_wr_data),
                .reg_wr_ready(rtu_reg_wr_ready),
                .reg_rd_valid(rtu_reg_rd_valid),
                .reg_rd_addr(rtu_reg_rd_addr),
                .reg_rd_data(rtu_reg_rd_data),
                .reg_rd_ready(rtu_reg_rd_ready),
                .irq(rtu_irq),
                .mem_req_valid(rtu_mem_req_valid),
                .mem_req_addr(rtu_mem_req_addr),
                .mem_req_size(rtu_mem_req_size),
                .mem_req_qos(rtu_mem_req_qos),
                .mem_req_id(rtu_mem_req_id),
                .mem_req_ready(rtu_mem_req_ready),
                .mem_resp_valid(rtu_mem_resp_valid),
                .mem_resp_data(rtu_mem_resp_data),
                .mem_resp_id(rtu_mem_resp_id),
                .rtu_busy(),
                .rtu_done()
            );
        end else begin : gen_no_rtu
            assign rtu_reg_wr_ready = 1'b1;
            assign rtu_reg_rd_data  = 32'h0;
            assign rtu_reg_rd_ready = 1'b1;
            assign rtu_irq          = 1'b0;
            assign rtu_mem_req_valid = 1'b0;
            assign rtu_mem_req_addr  = '0;
            assign rtu_mem_req_size  = '0;
            assign rtu_mem_req_qos   = '0;
            assign rtu_mem_req_id    = '0;
        end
    endgenerate

    // -----------------------------------------------------------------------
    // I-Cache Miss Arbiter (round-robin across CUs)
    // -----------------------------------------------------------------------
    logic [$clog2(NUM_CUS)-1:0] ic_rr_ptr;
    logic [$clog2(NUM_CUS)-1:0] ic_sel;
    logic                        ic_sel_valid;
    logic                        ic_grant_active; // One outstanding I-cache miss at a time

    always_comb begin
        ic_sel_valid = 1'b0;
        ic_sel       = '0;
        for (int i = 0; i < NUM_CUS; i++) begin
            automatic int idx = (int'(ic_rr_ptr) + i) % NUM_CUS;
            if (!ic_sel_valid && cu_icache_req_valid[idx] && !ic_grant_active) begin
                ic_sel_valid = 1'b1;
                ic_sel       = $clog2(NUM_CUS)'(idx);
            end
        end
    end

    // Drive external I-cache miss port
    assign icache_miss_req_valid = ic_sel_valid ? cu_icache_req_valid[ic_sel] : 1'b0;
    assign icache_miss_req_addr  = cu_icache_req_addr[ic_sel];

    // Ready/response routing
    always_comb begin
        for (int c = 0; c < NUM_CUS; c++) begin
            cu_icache_req_ready[c]  = (ic_sel_valid && (ic_sel == c)) ? icache_miss_req_ready : 1'b0;
            cu_icache_resp_valid[c] = 1'b0;
            cu_icache_resp_data[c]  = icache_miss_resp_data;
        end
        // Route response to the CU that has the outstanding grant
        if (ic_grant_active)
            cu_icache_resp_valid[ic_grant_cu] = icache_miss_resp_valid;
    end

    logic [$clog2(NUM_CUS)-1:0] ic_grant_cu;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            ic_rr_ptr      <= '0;
            ic_grant_active <= 1'b0;
            ic_grant_cu     <= '0;
        end else begin
            // Grant request
            if (ic_sel_valid && icache_miss_req_ready && !ic_grant_active) begin
                ic_grant_active <= 1'b1;
                ic_grant_cu     <= ic_sel;
                ic_rr_ptr       <= $clog2(NUM_CUS)'((int'(ic_sel) + 1) % NUM_CUS);
            end
            // Release on response
            if (ic_grant_active && icache_miss_resp_valid) begin
                ic_grant_active <= 1'b0;
            end
        end
    end

    // -----------------------------------------------------------------------
    // Framebuffer AXI Arbiter (round-robin, full AW+W+B handshake)
    // -----------------------------------------------------------------------
    logic [$clog2(NUM_CUS)-1:0] fb_rr_ptr;
    logic [$clog2(NUM_CUS)-1:0] fb_sel;
    logic                        fb_sel_valid;

    typedef enum logic [1:0] {
        FB_IDLE = 2'd0,
        FB_ADDR = 2'd1,
        FB_DATA = 2'd2,
        FB_RESP = 2'd3
    } fb_arb_state_e;

    fb_arb_state_e fb_state;
    logic [$clog2(NUM_CUS)-1:0] fb_active_cu;

    // Selection
    always_comb begin
        fb_sel_valid = 1'b0;
        fb_sel       = '0;
        for (int i = 0; i < NUM_CUS; i++) begin
            automatic int idx = (int'(fb_rr_ptr) + i) % NUM_CUS;
            if (!fb_sel_valid && cu_fb_aw_valid[idx]) begin
                fb_sel_valid = 1'b1;
                fb_sel       = $clog2(NUM_CUS)'(idx);
            end
        end
    end

    // Drive outputs based on state
    always_comb begin
        fb_aw_valid = 1'b0; fb_aw_addr = '0; fb_aw_len = '0;
        fb_aw_size = '0; fb_aw_burst = '0;
        fb_w_valid = 1'b0; fb_w_data = '0; fb_w_strb = '0; fb_w_last = 1'b0;
        fb_b_ready = 1'b0;
        for (int c = 0; c < NUM_CUS; c++) begin
            cu_fb_aw_ready[c] = 1'b0;
            cu_fb_w_ready[c]  = 1'b0;
            cu_fb_b_valid[c]  = 1'b0;
        end

        case (fb_state)
            FB_ADDR: begin
                fb_aw_valid = cu_fb_aw_valid[fb_active_cu];
                fb_aw_addr  = cu_fb_aw_addr[fb_active_cu];
                fb_aw_len   = cu_fb_aw_len[fb_active_cu];
                fb_aw_size  = cu_fb_aw_size[fb_active_cu];
                fb_aw_burst = cu_fb_aw_burst[fb_active_cu];
                cu_fb_aw_ready[fb_active_cu] = fb_aw_ready;
            end
            FB_DATA: begin
                fb_w_valid = cu_fb_w_valid[fb_active_cu];
                fb_w_data  = cu_fb_w_data[fb_active_cu];
                fb_w_strb  = cu_fb_w_strb[fb_active_cu];
                fb_w_last  = cu_fb_w_last[fb_active_cu];
                cu_fb_w_ready[fb_active_cu] = fb_w_ready;
            end
            FB_RESP: begin
                cu_fb_b_valid[fb_active_cu] = fb_b_valid;
                fb_b_ready = cu_fb_b_ready[fb_active_cu];
            end
            default: ;
        endcase
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            fb_state     <= FB_IDLE;
            fb_rr_ptr    <= '0;
            fb_active_cu <= '0;
        end else begin
            case (fb_state)
                FB_IDLE: begin
                    if (fb_sel_valid) begin
                        fb_active_cu <= fb_sel;
                        fb_state     <= FB_ADDR;
                        fb_rr_ptr    <= $clog2(NUM_CUS)'((int'(fb_sel) + 1) % NUM_CUS);
                    end
                end
                FB_ADDR: begin
                    if (fb_aw_valid && fb_aw_ready)
                        fb_state <= FB_DATA;
                end
                FB_DATA: begin
                    if (fb_w_valid && fb_w_ready && fb_w_last)
                        fb_state <= FB_RESP;
                end
                FB_RESP: begin
                    if (fb_b_valid && fb_b_ready)
                        fb_state <= FB_IDLE;
                end
            endcase
        end
    end

    // -----------------------------------------------------------------------
    // Mailbox Switch Fabric
    // -----------------------------------------------------------------------
    // For 2 CUs: use mailbox_switch_2x1
    // For 4 CUs: use mailbox_switch_4x1
    // Mailbox endpoints are internal to compute_unit_top (stream interface)
    //
    // The stream-to-AXI-Lite bridge needs to convert the CU's stream
    // tx/rx interface to the switch's AXI-Lite interface. For synthesis
    // simplicity, we tie off mailbox for now and leave the stream wiring
    // in place — the CU's mailbox_endpoint handles the CSR-addressed
    // protocol internally.
    generate
        if (MAILBOX_ENABLE && NUM_CUS == 2) begin : gen_mb_2x1
            // Tie mailbox TX/RX for now — full fabric integration requires
            // stream-to-AXI bridge per CU endpoint. The CU stream interface
            // is compatible with direct crossbar wiring for 2-CU topologies.
            //
            // Simple cross-connect: CU0.tx → CU1.rx and vice versa
            assign cu_mb_rx_valid[0] = cu_mb_tx_valid[1];
            assign cu_mb_rx_data[0]  = cu_mb_tx_data[1];
            assign cu_mb_rx_dest[0]  = cu_mb_tx_dest[1];
            assign cu_mb_tx_ready[1] = cu_mb_rx_ready[0];

            assign cu_mb_rx_valid[1] = cu_mb_tx_valid[0];
            assign cu_mb_rx_data[1]  = cu_mb_tx_data[0];
            assign cu_mb_rx_dest[1]  = cu_mb_tx_dest[0];
            assign cu_mb_tx_ready[0] = cu_mb_rx_ready[1];

            // Uplink: tie off (no system-level mailbox in standalone cluster)
            assign mb_up_awvalid = 1'b0; assign mb_up_awaddr = '0;
            assign mb_up_wvalid = 1'b0;  assign mb_up_wdata = '0;
            assign mb_up_wstrb = '0;     assign mb_up_tag = '0;
            assign mb_up_bready = 1'b1;
            assign mb_up_arvalid = 1'b0; assign mb_up_araddr = '0;
            assign mb_up_rready = 1'b1;
            assign mb_dn_awready = 1'b1; assign mb_dn_wready = 1'b1;
            assign mb_dn_bvalid = 1'b0;  assign mb_dn_arready = 1'b1;
            assign mb_dn_rvalid = 1'b0;  assign mb_dn_rdata = '0;

        end else if (MAILBOX_ENABLE && NUM_CUS == 4) begin : gen_mb_4x1
            // 4-CU: pair into 2 groups of 2, cross-connect within each pair,
            // then cross-connect between pairs via a simple 2-level switch.
            // For now: full crossbar among 4 CUs (simplified)
            //
            // Ring topology: CU[i].tx → CU[(i+1)%4].rx
            for (genvar c = 0; c < 4; c++) begin : gen_mb_ring
                localparam int next = (c + 1) % 4;
                assign cu_mb_rx_valid[next] = cu_mb_tx_valid[c];
                assign cu_mb_rx_data[next]  = cu_mb_tx_data[c];
                assign cu_mb_rx_dest[next]  = cu_mb_tx_dest[c];
                assign cu_mb_tx_ready[c]    = cu_mb_rx_ready[next];
            end

            assign mb_up_awvalid = 1'b0; assign mb_up_awaddr = '0;
            assign mb_up_wvalid = 1'b0;  assign mb_up_wdata = '0;
            assign mb_up_wstrb = '0;     assign mb_up_tag = '0;
            assign mb_up_bready = 1'b1;
            assign mb_up_arvalid = 1'b0; assign mb_up_araddr = '0;
            assign mb_up_rready = 1'b1;
            assign mb_dn_awready = 1'b1; assign mb_dn_wready = 1'b1;
            assign mb_dn_bvalid = 1'b0;  assign mb_dn_arready = 1'b1;
            assign mb_dn_rvalid = 1'b0;  assign mb_dn_rdata = '0;

        end else begin : gen_mb_stub
            // Mailbox disabled: tie off all CU mailbox ports
            for (genvar c = 0; c < NUM_CUS; c++) begin : gen_mb_tieoff
                assign cu_mb_tx_ready[c] = 1'b1;
                assign cu_mb_rx_valid[c] = 1'b0;
                assign cu_mb_rx_data[c]  = '0;
                assign cu_mb_rx_dest[c]  = '0;
            end
            assign mb_up_awvalid = 1'b0; assign mb_up_awaddr = '0;
            assign mb_up_wvalid = 1'b0;  assign mb_up_wdata = '0;
            assign mb_up_wstrb = '0;     assign mb_up_tag = '0;
            assign mb_up_bready = 1'b1;
            assign mb_up_arvalid = 1'b0; assign mb_up_araddr = '0;
            assign mb_up_rready = 1'b1;
            assign mb_dn_awready = 1'b1; assign mb_dn_wready = 1'b1;
            assign mb_dn_bvalid = 1'b0;  assign mb_dn_arready = 1'b1;
            assign mb_dn_rvalid = 1'b0;  assign mb_dn_rdata = '0;
        end
    endgenerate

endmodule : cluster_top
