`timescale 1ns/1ps
// ============================================================================
// Cluster Top — Configurable slot-based heterogeneous cluster
//
// Supports up to 4 slots, each configurable as:
//   SLOT_CU  (0) — Standard Compute Unit
//   SLOT_RT  (1) — Ray-Tracing Accelerator (RTU)
//   SLOT_MAT (2) — Matrix/Tensor Accelerator (MXU)
//
// Supported configurations (from spec):
//   1. 2 CUs              — SLOT_TYPES = '{CU, CU, NONE, NONE}
//   2. 4 CUs              — SLOT_TYPES = '{CU, CU, CU, CU}
//   3. 2 MATs             — SLOT_TYPES = '{MAT, MAT, NONE, NONE}
//   4. 1 CU + 1 MAT       — SLOT_TYPES = '{CU, MAT, NONE, NONE}
//   5. 1 CU + 1 RT        — SLOT_TYPES = '{CU, RT, NONE, NONE}
//   6. 2 CU + 2 MAT       — SLOT_TYPES = '{CU, CU, MAT, MAT}
//   7. 2 CU + 1 MAT + 1 RT — SLOT_TYPES = '{CU, CU, MAT, RT}
//   8. 2 CU + 2 RT        — SLOT_TYPES = '{CU, CU, RT, RT}
//   9. 1 CU + 3 RT/MAT    — SLOT_TYPES = '{CU, ?, ?, ?}
//
// Each active slot gets:
//   - An L2 cache port
//   - A MailboxFabric stream endpoint
//
// CU slots additionally get:
//   - I-cache miss arbiter share
//   - Framebuffer AXI arbiter share
//
// External interfaces remain the same as the old cluster_top.
// ============================================================================
module cluster_top
  import rt_pkg::*;
  import mat_pkg::*;
  import mailbox_pkg::*;
#(
    // -----------------------------------------------------------------------
    // Slot configuration
    // -----------------------------------------------------------------------
    parameter int  NUM_SLOTS    = 4,    // Physical slots (max 4)
    // Slot types: 0=CU, 1=RT, 2=MAT, 3=NONE (disabled)
    parameter int  SLOT_TYPE_0 = 0,
    parameter int  SLOT_TYPE_1 = 0,
    parameter int  SLOT_TYPE_2 = 3,
    parameter int  SLOT_TYPE_3 = 3,

    parameter logic [7:0] CLUSTER_ID = 8'h01,

    // -----------------------------------------------------------------------
    // Feature enables
    // -----------------------------------------------------------------------
    parameter bit  MAILBOX_ENABLE = 1'b1,
    parameter bit  GFX_ENABLE     = 1'b1,

    // -----------------------------------------------------------------------
    // Cache parameters
    // -----------------------------------------------------------------------
    parameter int  L2_SIZE_BYTES  = 16384,
    parameter int  L2_LINE_BYTES  = 64,
    parameter int  L2_ASSOC       = 4,
    parameter bit  L2_WRITEBACK   = 1'b0,

    // -----------------------------------------------------------------------
    // CU parameters (forwarded)
    // -----------------------------------------------------------------------
    parameter int  GFX_ISSUE_Q_DEPTH = 8,
    parameter int  TEX_REQ_Q_DEPTH   = 2,

    // -----------------------------------------------------------------------
    // AXI parameters
    // -----------------------------------------------------------------------
    parameter int  AXI_ADDR_W = 32,
    parameter int  AXI_DATA_W = 64
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

    // ===== I-cache miss port (shared, arbitrated across CU slots) =====
    output logic                   icache_miss_req_valid,
    output logic [31:0]            icache_miss_req_addr,
    input  logic                   icache_miss_req_ready,
    input  logic                   icache_miss_resp_valid,
    input  logic [63:0]            icache_miss_resp_data,

    // ===== Mailbox uplink (to center router) =====
    output logic                           mb_up_tx_valid,
    input  logic                           mb_up_tx_ready,
    output mailbox_flit_t                  mb_up_tx_data,
    output logic [NODE_ID_WIDTH-1:0]       mb_up_tx_dest_id,
    input  logic                           mb_up_rx_valid,
    output logic                           mb_up_rx_ready,
    input  mailbox_flit_t                  mb_up_rx_data,
    input  logic [NODE_ID_WIDTH-1:0]       mb_up_rx_dest_id,

    // ===== Per-slot register access (direct, optional) =====
    input  logic [NUM_SLOTS-1:0]        slot_reg_wr_valid,
    input  logic [4:0]                  slot_reg_wr_addr,
    input  logic [31:0]                 slot_reg_wr_data,
    output logic [NUM_SLOTS-1:0]        slot_reg_wr_ready,
    input  logic [NUM_SLOTS-1:0]        slot_reg_rd_valid,
    input  logic [4:0]                  slot_reg_rd_addr,
    output logic [NUM_SLOTS-1:0][31:0]  slot_reg_rd_data,
    output logic [NUM_SLOTS-1:0]        slot_reg_rd_ready,
    output logic [NUM_SLOTS-1:0]        slot_irq,

    // ===== Status =====
    output logic [NUM_SLOTS-1:0][31:0]  slot_csr_status
);

    // -----------------------------------------------------------------------
    // Slot type constants
    // -----------------------------------------------------------------------
    localparam int SLOT_CU   = 0;
    localparam int SLOT_RT   = 1;
    localparam int SLOT_MAT  = 2;
    localparam int SLOT_NONE = 3;

    // Slot type array (indexed by slot number)
    localparam int SLOT_TYPES [NUM_SLOTS] = '{SLOT_TYPE_0, SLOT_TYPE_1,
                                               SLOT_TYPE_2, SLOT_TYPE_3};

    // Count active slots and CU slots
    function automatic int count_type(input int st);
        int cnt = 0;
        for (int i = 0; i < NUM_SLOTS; i++)
            if (SLOT_TYPES[i] == st) cnt++;
        return cnt;
    endfunction

    function automatic int count_active();
        int cnt = 0;
        for (int i = 0; i < NUM_SLOTS; i++)
            if (SLOT_TYPES[i] != SLOT_NONE) cnt++;
        return cnt;
    endfunction

    localparam int NUM_CUS   = count_type(SLOT_CU);
    localparam int NUM_RTS   = count_type(SLOT_RT);
    localparam int NUM_MATS  = count_type(SLOT_MAT);
    localparam int NUM_ACTIVE = count_active();

    // L2 port count = number of active slots
    localparam int L2_NUM_PORTS = NUM_ACTIVE;

    // -----------------------------------------------------------------------
    // L2 cache port wiring (shared across all active slots)
    // -----------------------------------------------------------------------
    logic [L2_NUM_PORTS-1:0]                       l2_req_valid;
    logic [L2_NUM_PORTS-1:0]                       l2_req_rw;
    logic [L2_NUM_PORTS-1:0][31:0]                 l2_req_addr;
    logic [L2_NUM_PORTS-1:0][7:0]                  l2_req_size;
    logic [L2_NUM_PORTS-1:0][3:0]                  l2_req_qos;
    logic [L2_NUM_PORTS-1:0][7:0]                  l2_req_id;
    logic [L2_NUM_PORTS-1:0][L2_LINE_BYTES*8-1:0]  l2_req_wdata;
    logic [L2_NUM_PORTS-1:0][L2_LINE_BYTES/8-1:0]  l2_req_wstrb;
    logic [L2_NUM_PORTS-1:0]                       l2_req_ready;
    logic [L2_NUM_PORTS-1:0]                       l2_resp_valid;
    logic [L2_NUM_PORTS-1:0][AXI_DATA_W-1:0]      l2_resp_data;
    logic [L2_NUM_PORTS-1:0][7:0]                  l2_resp_id;

    // -----------------------------------------------------------------------
    // Mailbox fabric wiring (per active slot)
    // -----------------------------------------------------------------------
    logic [NUM_ACTIVE-1:0]                          mb_slot_tx_valid;
    logic [NUM_ACTIVE-1:0]                          mb_slot_tx_ready;
    mailbox_flit_t [NUM_ACTIVE-1:0]                 mb_slot_tx_data;
    logic [NUM_ACTIVE-1:0][NODE_ID_WIDTH-1:0]       mb_slot_tx_dest;
    logic [NUM_ACTIVE-1:0]                          mb_slot_rx_valid;
    logic [NUM_ACTIVE-1:0]                          mb_slot_rx_ready;
    mailbox_flit_t [NUM_ACTIVE-1:0]                 mb_slot_rx_data;
    logic [NUM_ACTIVE-1:0][NODE_ID_WIDTH-1:0]       mb_slot_rx_dest;

    // -----------------------------------------------------------------------
    // CU-specific wiring (indexed by CU number, not slot number)
    // -----------------------------------------------------------------------
    // Map slot index to L2 port index
    function automatic int slot_to_l2_port(input int slot);
        int port = 0;
        for (int i = 0; i < slot; i++)
            if (SLOT_TYPES[i] != SLOT_NONE) port++;
        return port;
    endfunction

    // I-cache miss (only for CU slots)
    logic [NUM_SLOTS-1:0]        cu_icache_req_valid;
    logic [NUM_SLOTS-1:0][31:0]  cu_icache_req_addr;
    logic [NUM_SLOTS-1:0]        cu_icache_req_ready;
    logic [NUM_SLOTS-1:0]        cu_icache_resp_valid;
    logic [NUM_SLOTS-1:0][63:0]  cu_icache_resp_data;

    // Framebuffer AXI (only for CU slots)
    logic [NUM_SLOTS-1:0]        cu_fb_aw_valid;
    logic [NUM_SLOTS-1:0][31:0]  cu_fb_aw_addr;
    logic [NUM_SLOTS-1:0][7:0]   cu_fb_aw_len;
    logic [NUM_SLOTS-1:0][2:0]   cu_fb_aw_size;
    logic [NUM_SLOTS-1:0][1:0]   cu_fb_aw_burst;
    logic [NUM_SLOTS-1:0]        cu_fb_aw_ready;
    logic [NUM_SLOTS-1:0][31:0]  cu_fb_w_data;
    logic [NUM_SLOTS-1:0][3:0]   cu_fb_w_strb;
    logic [NUM_SLOTS-1:0]        cu_fb_w_last;
    logic [NUM_SLOTS-1:0]        cu_fb_w_valid;
    logic [NUM_SLOTS-1:0]        cu_fb_w_ready;
    logic [NUM_SLOTS-1:0]        cu_fb_b_valid;
    logic [NUM_SLOTS-1:0]        cu_fb_b_ready;

    // -----------------------------------------------------------------------
    // Slot instantiation
    // -----------------------------------------------------------------------
    generate
        for (genvar s = 0; s < NUM_SLOTS; s++) begin : gen_slot
            localparam int LP = slot_to_l2_port(s);
            localparam logic [NODE_ID_WIDTH-1:0] SLOT_MB_ID = {CLUSTER_ID, 4'(s), 4'h0};

            if (SLOT_TYPES[s] == SLOT_CU) begin : gen_cu_slot
                // ---- Compute Unit ----
                logic        dc_req_valid, dc_req_rw;
                logic [31:0] dc_req_addr;
                logic [7:0]  dc_req_size;
                logic [3:0]  dc_req_qos;
                logic [7:0]  dc_req_id;
                logic [L2_LINE_BYTES*8-1:0] dc_req_wdata;
                logic [L2_LINE_BYTES/8-1:0] dc_req_wstrb;
                logic        dc_req_ready;
                logic        dc_resp_valid;
                logic [63:0] dc_resp_data;
                logic [7:0]  dc_resp_id;

                compute_unit_top #(
                    .CORE_ID(32'(s)),
                    .TILE_OFFSET(32'(s)),
                    .GFX_ISSUE_Q_DEPTH(GFX_ENABLE ? GFX_ISSUE_Q_DEPTH : 1),
                    .TEX_REQ_Q_DEPTH(TEX_REQ_Q_DEPTH),
                    .MAILBOX_ENABLE(MAILBOX_ENABLE),
                    .MAILBOX_SRC_ID({4'(s), 4'h0})
                ) u_cu (
                    .clk(clk), .rst_n(rst_n),
                    .inst_miss_req_valid(cu_icache_req_valid[s]),
                    .inst_miss_req_addr(cu_icache_req_addr[s]),
                    .inst_miss_req_ready(cu_icache_req_ready[s]),
                    .inst_miss_resp_valid(cu_icache_resp_valid[s]),
                    .inst_miss_resp_data(cu_icache_resp_data[s]),
                    .data_req_valid(), .data_req_is_load(), .data_req_addr(),
                    .data_req_wdata(), .data_req_rd(), .data_req_ready(1'b1),
                    .data_resp_valid(1'b0), .data_resp_rd(5'd0), .data_resp_data(32'd0),
                    .dcache_mem_req_valid(dc_req_valid),
                    .dcache_mem_req_rw(dc_req_rw),
                    .dcache_mem_req_addr(dc_req_addr),
                    .dcache_mem_req_size(dc_req_size),
                    .dcache_mem_req_qos(dc_req_qos),
                    .dcache_mem_req_id(dc_req_id),
                    .dcache_mem_req_wdata(dc_req_wdata),
                    .dcache_mem_req_wstrb(dc_req_wstrb),
                    .dcache_mem_req_ready(dc_req_ready),
                    .dcache_mem_resp_valid(dc_resp_valid),
                    .dcache_mem_resp_data(dc_resp_data),
                    .dcache_mem_resp_id(dc_resp_id),
                    .fb_aw_valid(cu_fb_aw_valid[s]), .fb_aw_addr(cu_fb_aw_addr[s]),
                    .fb_aw_len(cu_fb_aw_len[s]),     .fb_aw_size(cu_fb_aw_size[s]),
                    .fb_aw_burst(cu_fb_aw_burst[s]), .fb_aw_ready(cu_fb_aw_ready[s]),
                    .fb_w_data(cu_fb_w_data[s]),     .fb_w_strb(cu_fb_w_strb[s]),
                    .fb_w_last(cu_fb_w_last[s]),     .fb_w_valid(cu_fb_w_valid[s]),
                    .fb_w_ready(cu_fb_w_ready[s]),
                    .fb_b_valid(cu_fb_b_valid[s]),   .fb_b_ready(cu_fb_b_ready[s]),
                    .mailbox_tx_valid(mb_slot_tx_valid[LP]),
                    .mailbox_tx_ready(mb_slot_tx_ready[LP]),
                    .mailbox_tx_data(mb_slot_tx_data[LP]),
                    .mailbox_tx_dest_id(mb_slot_tx_dest[LP]),
                    .mailbox_rx_valid(mb_slot_rx_valid[LP]),
                    .mailbox_rx_ready(mb_slot_rx_ready[LP]),
                    .mailbox_rx_data(mb_slot_rx_data[LP]),
                    .mailbox_rx_dest_id(mb_slot_rx_dest[LP]),
                    .err_fp_overflow(),  .err_fp_invalid(),
                    .err_vec_overflow(), .err_vec_invalid(),
                    .csr_status(slot_csr_status[s]),
                    .csr_fstatus(),      .csr_vstatus()
                );

                // Connect CU D-cache to L2 port
                assign l2_req_valid[LP] = dc_req_valid;
                assign l2_req_rw[LP]    = dc_req_rw;
                assign l2_req_addr[LP]  = dc_req_addr;
                assign l2_req_size[LP]  = dc_req_size;
                assign l2_req_qos[LP]   = dc_req_qos;
                assign l2_req_id[LP]    = dc_req_id;
                assign l2_req_wdata[LP] = dc_req_wdata;
                assign l2_req_wstrb[LP] = dc_req_wstrb;
                assign dc_req_ready     = l2_req_ready[LP];
                assign dc_resp_valid    = l2_resp_valid[LP];
                assign dc_resp_data     = l2_resp_data[LP];
                assign dc_resp_id       = l2_resp_id[LP];

                // Register interface for CU is just status
                assign slot_reg_wr_ready[s] = 1'b1;
                assign slot_reg_rd_data[s]  = slot_csr_status[s];
                assign slot_reg_rd_ready[s] = 1'b1;
                assign slot_irq[s]          = 1'b0;

            end else if (SLOT_TYPES[s] == SLOT_RT) begin : gen_rt_slot
                // ---- Ray-Tracing Unit ----
                logic        rt_mem_req_valid;
                logic [31:0] rt_mem_req_addr;
                logic [7:0]  rt_mem_req_size;
                logic [3:0]  rt_mem_req_qos;
                logic [7:0]  rt_mem_req_id;
                logic        rt_mem_req_ready;
                logic        rt_mem_resp_valid;
                logic [63:0] rt_mem_resp_data;
                logic [7:0]  rt_mem_resp_id;

                ray_tracing_unit #(
                    .MAILBOX_SRC_ID(SLOT_MB_ID)
                ) u_rtu (
                    .clk(clk), .rst_n(rst_n),
                    .reg_wr_valid(slot_reg_wr_valid[s]),
                    .reg_wr_addr(slot_reg_wr_addr),
                    .reg_wr_data(slot_reg_wr_data),
                    .reg_wr_ready(slot_reg_wr_ready[s]),
                    .reg_rd_valid(slot_reg_rd_valid[s]),
                    .reg_rd_addr(slot_reg_rd_addr),
                    .reg_rd_data(slot_reg_rd_data[s]),
                    .reg_rd_ready(slot_reg_rd_ready[s]),
                    .irq(slot_irq[s]),
                    .mb_tx_valid(mb_slot_tx_valid[LP]),
                    .mb_tx_ready(mb_slot_tx_ready[LP]),
                    .mb_tx_data(mb_slot_tx_data[LP]),
                    .mb_tx_dest_id(mb_slot_tx_dest[LP]),
                    .mb_rx_valid(mb_slot_rx_valid[LP]),
                    .mb_rx_ready(mb_slot_rx_ready[LP]),
                    .mb_rx_data(mb_slot_rx_data[LP]),
                    .mb_rx_dest_id(mb_slot_rx_dest[LP]),
                    .mem_req_valid(rt_mem_req_valid),
                    .mem_req_addr(rt_mem_req_addr),
                    .mem_req_size(rt_mem_req_size),
                    .mem_req_qos(rt_mem_req_qos),
                    .mem_req_id(rt_mem_req_id),
                    .mem_req_ready(rt_mem_req_ready),
                    .mem_resp_valid(rt_mem_resp_valid),
                    .mem_resp_data(rt_mem_resp_data),
                    .mem_resp_id(rt_mem_resp_id),
                    .rtu_busy(), .rtu_done()
                );

                // Connect RTU to L2 port (read-only)
                assign l2_req_valid[LP] = rt_mem_req_valid;
                assign l2_req_rw[LP]    = 1'b0;
                assign l2_req_addr[LP]  = rt_mem_req_addr;
                assign l2_req_size[LP]  = rt_mem_req_size;
                assign l2_req_qos[LP]   = rt_mem_req_qos;
                assign l2_req_id[LP]    = rt_mem_req_id;
                assign l2_req_wdata[LP] = '0;
                assign l2_req_wstrb[LP] = '0;
                assign rt_mem_req_ready  = l2_req_ready[LP];
                assign rt_mem_resp_valid = l2_resp_valid[LP];
                assign rt_mem_resp_data  = l2_resp_data[LP];
                assign rt_mem_resp_id    = l2_resp_id[LP];

                assign slot_csr_status[s] = '0;

                // No FB/icache for RT slots
                assign cu_icache_req_valid[s] = 1'b0;
                assign cu_icache_req_addr[s]  = '0;
                assign cu_fb_aw_valid[s] = 1'b0; assign cu_fb_aw_addr[s] = '0;
                assign cu_fb_aw_len[s] = '0;     assign cu_fb_aw_size[s] = '0;
                assign cu_fb_aw_burst[s] = '0;
                assign cu_fb_w_data[s] = '0; assign cu_fb_w_strb[s] = '0;
                assign cu_fb_w_last[s] = 1'b0; assign cu_fb_w_valid[s] = 1'b0;
                assign cu_fb_b_ready[s] = 1'b1;

            end else if (SLOT_TYPES[s] == SLOT_MAT) begin : gen_mat_slot
                // ---- Matrix/Tensor Unit ----
                logic        mat_mem_req_valid, mat_mem_req_rw;
                logic [31:0] mat_mem_req_addr;
                logic [7:0]  mat_mem_req_size;
                logic [3:0]  mat_mem_req_qos;
                logic [7:0]  mat_mem_req_id;
                logic [63:0] mat_mem_req_wdata;
                logic [7:0]  mat_mem_req_wstrb;
                logic        mat_mem_req_ready;
                logic        mat_mem_resp_valid;
                logic [63:0] mat_mem_resp_data;
                logic [7:0]  mat_mem_resp_id;

                matrix_unit #(
                    .MAILBOX_SRC_ID(SLOT_MB_ID)
                ) u_mxu (
                    .clk(clk), .rst_n(rst_n),
                    .reg_wr_valid(slot_reg_wr_valid[s]),
                    .reg_wr_addr(slot_reg_wr_addr),
                    .reg_wr_data(slot_reg_wr_data),
                    .reg_wr_ready(slot_reg_wr_ready[s]),
                    .reg_rd_valid(slot_reg_rd_valid[s]),
                    .reg_rd_addr(slot_reg_rd_addr),
                    .reg_rd_data(slot_reg_rd_data[s]),
                    .reg_rd_ready(slot_reg_rd_ready[s]),
                    .irq(slot_irq[s]),
                    .mb_tx_valid(mb_slot_tx_valid[LP]),
                    .mb_tx_ready(mb_slot_tx_ready[LP]),
                    .mb_tx_data(mb_slot_tx_data[LP]),
                    .mb_tx_dest_id(mb_slot_tx_dest[LP]),
                    .mb_rx_valid(mb_slot_rx_valid[LP]),
                    .mb_rx_ready(mb_slot_rx_ready[LP]),
                    .mb_rx_data(mb_slot_rx_data[LP]),
                    .mb_rx_dest_id(mb_slot_rx_dest[LP]),
                    .mem_req_valid(mat_mem_req_valid),
                    .mem_req_rw(mat_mem_req_rw),
                    .mem_req_addr(mat_mem_req_addr),
                    .mem_req_size(mat_mem_req_size),
                    .mem_req_qos(mat_mem_req_qos),
                    .mem_req_id(mat_mem_req_id),
                    .mem_req_wdata(mat_mem_req_wdata),
                    .mem_req_wstrb(mat_mem_req_wstrb),
                    .mem_req_ready(mat_mem_req_ready),
                    .mem_resp_valid(mat_mem_resp_valid),
                    .mem_resp_data(mat_mem_resp_data),
                    .mem_resp_id(mat_mem_resp_id),
                    .mxu_busy(), .mxu_done()
                );

                // Connect MXU to L2 port (read/write)
                // Place 64-bit MXU wdata at the correct 8-byte chunk within the cache line
                wire [2:0] mat_wr_chunk = mat_mem_req_addr[5:3];
                logic [L2_LINE_BYTES*8-1:0] mat_wdata_shifted;
                logic [L2_LINE_BYTES/8-1:0] mat_wstrb_shifted;
                always_comb begin
                    mat_wdata_shifted = '0;
                    mat_wdata_shifted[mat_wr_chunk*64 +: 64] = mat_mem_req_wdata;
                    mat_wstrb_shifted = '0;
                    mat_wstrb_shifted[mat_wr_chunk] = |mat_mem_req_wstrb;
                end

                assign l2_req_valid[LP]  = mat_mem_req_valid;
                assign l2_req_rw[LP]     = mat_mem_req_rw;
                assign l2_req_addr[LP]   = mat_mem_req_addr;
                assign l2_req_size[LP]   = mat_mem_req_size;
                assign l2_req_qos[LP]    = mat_mem_req_qos;
                assign l2_req_id[LP]     = mat_mem_req_id;
                assign l2_req_wdata[LP]  = mat_wdata_shifted;
                assign l2_req_wstrb[LP]  = mat_wstrb_shifted;
                assign mat_mem_req_ready  = l2_req_ready[LP];
                assign mat_mem_resp_valid = l2_resp_valid[LP];
                assign mat_mem_resp_data  = l2_resp_data[LP];
                assign mat_mem_resp_id    = l2_resp_id[LP];

                assign slot_csr_status[s] = '0;

                // No FB/icache for MAT slots
                assign cu_icache_req_valid[s] = 1'b0;
                assign cu_icache_req_addr[s]  = '0;
                assign cu_fb_aw_valid[s] = 1'b0; assign cu_fb_aw_addr[s] = '0;
                assign cu_fb_aw_len[s] = '0;     assign cu_fb_aw_size[s] = '0;
                assign cu_fb_aw_burst[s] = '0;
                assign cu_fb_w_data[s] = '0; assign cu_fb_w_strb[s] = '0;
                assign cu_fb_w_last[s] = 1'b0; assign cu_fb_w_valid[s] = 1'b0;
                assign cu_fb_b_ready[s] = 1'b1;

            end else begin : gen_none_slot
                // ---- Empty slot — tie off everything ----
                assign slot_reg_wr_ready[s] = 1'b1;
                assign slot_reg_rd_data[s]  = 32'hDEAD_BEEF;
                assign slot_reg_rd_ready[s] = 1'b1;
                assign slot_irq[s]          = 1'b0;
                assign slot_csr_status[s]   = '0;

                assign cu_icache_req_valid[s] = 1'b0;
                assign cu_icache_req_addr[s]  = '0;
                assign cu_fb_aw_valid[s] = 1'b0; assign cu_fb_aw_addr[s] = '0;
                assign cu_fb_aw_len[s] = '0;     assign cu_fb_aw_size[s] = '0;
                assign cu_fb_aw_burst[s] = '0;
                assign cu_fb_w_data[s] = '0; assign cu_fb_w_strb[s] = '0;
                assign cu_fb_w_last[s] = 1'b0; assign cu_fb_w_valid[s] = 1'b0;
                assign cu_fb_b_ready[s] = 1'b1;
            end
        end
    endgenerate

    // -----------------------------------------------------------------------
    // Shared L2 Data Cache
    // -----------------------------------------------------------------------
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
        .clk(clk), .rst_n(rst_n),
        .l1_req_valid(l2_req_valid),   .l1_req_rw(l2_req_rw),
        .l1_req_addr(l2_req_addr),     .l1_req_size(l2_req_size),
        .l1_req_qos(l2_req_qos),       .l1_req_id(l2_req_id),
        .l1_req_wdata(l2_req_wdata),   .l1_req_wstrb(l2_req_wstrb),
        .l1_req_ready(l2_req_ready),
        .l1_resp_valid(l2_resp_valid), .l1_resp_data(l2_resp_data),
        .l1_resp_id(l2_resp_id),
        .mb_s_awvalid(1'b0), .mb_s_awaddr(16'h0),
        .mb_s_wvalid(1'b0), .mb_s_wdata(32'h0), .mb_s_wstrb(4'h0),
        .mb_s_tag('0), .mb_s_bready(1'b1),
        .mb_s_arvalid(1'b0), .mb_s_araddr(16'h0), .mb_s_rready(1'b1),
        .mb_s_awready(), .mb_s_wready(), .mb_s_bvalid(),
        .mb_s_arready(), .mb_s_rvalid(), .mb_s_rdata(),
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
    // I-Cache Miss Arbiter (round-robin across CU slots only)
    // -----------------------------------------------------------------------
    // Build CU slot index map at elaboration time
    localparam int MAX_CUS = (NUM_CUS > 1) ? NUM_CUS : 2;  // min 2 so $clog2>=1
    logic [$clog2(MAX_CUS)-1:0] ic_rr_ptr;
    logic [$clog2(MAX_CUS)-1:0] ic_sel;
    logic                        ic_sel_valid;
    logic                        ic_grant_active;
    logic [$clog2(MAX_CUS)-1:0]  ic_grant_cu;

    // Find the n-th CU slot index
    function automatic int cu_slot_idx(input int n);
        int cnt = 0;
        for (int i = 0; i < NUM_SLOTS; i++) begin
            if (SLOT_TYPES[i] == SLOT_CU) begin
                if (cnt == n) return i;
                cnt++;
            end
        end
        return 0;
    endfunction

    generate
        if (NUM_CUS > 0) begin : gen_ic_arb
            always_comb begin
                ic_sel_valid = 1'b0;
                ic_sel       = '0;
                for (int i = 0; i < NUM_CUS; i++) begin
                    automatic int idx = (int'(ic_rr_ptr) + i) % NUM_CUS;
                    automatic int slot = cu_slot_idx(idx);
                    if (!ic_sel_valid && cu_icache_req_valid[slot] && !ic_grant_active) begin
                        ic_sel_valid = 1'b1;
                        ic_sel       = $clog2(MAX_CUS)'(idx);
                    end
                end
            end

            // Drive external I-cache miss port
            wire [31:0] ic_sel_addr = cu_icache_req_addr[cu_slot_idx(ic_sel)];
            assign icache_miss_req_valid = ic_sel_valid ? cu_icache_req_valid[cu_slot_idx(ic_sel)] : 1'b0;
            assign icache_miss_req_addr  = ic_sel_addr;

            always_comb begin
                for (int s = 0; s < NUM_SLOTS; s++) begin
                    cu_icache_req_ready[s]  = 1'b0;
                    cu_icache_resp_valid[s] = 1'b0;
                    cu_icache_resp_data[s]  = icache_miss_resp_data;
                end
                if (ic_sel_valid) begin
                    automatic int sel_slot = cu_slot_idx(ic_sel);
                    cu_icache_req_ready[sel_slot] = icache_miss_req_ready;
                end
                if (ic_grant_active) begin
                    automatic int grant_slot = cu_slot_idx(ic_grant_cu);
                    cu_icache_resp_valid[grant_slot] = icache_miss_resp_valid;
                end
            end

            always_ff @(posedge clk or negedge rst_n) begin
                if (!rst_n) begin
                    ic_rr_ptr       <= '0;
                    ic_grant_active <= 1'b0;
                    ic_grant_cu     <= '0;
                end else begin
                    if (ic_sel_valid && icache_miss_req_ready && !ic_grant_active) begin
                        ic_grant_active <= 1'b1;
                        ic_grant_cu     <= ic_sel;
                        ic_rr_ptr       <= $clog2(MAX_CUS)'((int'(ic_sel) + 1) % NUM_CUS);
                    end
                    if (ic_grant_active && icache_miss_resp_valid)
                        ic_grant_active <= 1'b0;
                end
            end
        end else begin : gen_no_ic_arb
            assign icache_miss_req_valid = 1'b0;
            assign icache_miss_req_addr  = '0;
        end
    endgenerate

    // -----------------------------------------------------------------------
    // Framebuffer AXI Arbiter (round-robin across CU slots only)
    // -----------------------------------------------------------------------
    typedef enum logic [1:0] {
        FB_IDLE = 2'd0,
        FB_ADDR = 2'd1,
        FB_DATA = 2'd2,
        FB_RESP = 2'd3
    } fb_arb_state_e;

    generate
        if (NUM_CUS > 0) begin : gen_fb_arb
            logic [$clog2(MAX_CUS)-1:0] fb_rr_ptr, fb_sel;
            logic fb_sel_valid;
            fb_arb_state_e fb_state;
            logic [$clog2(MAX_CUS)-1:0] fb_active_cu;

            always_comb begin
                fb_sel_valid = 1'b0;
                fb_sel       = '0;
                for (int i = 0; i < NUM_CUS; i++) begin
                    automatic int idx = (int'(fb_rr_ptr) + i) % NUM_CUS;
                    automatic int slot = cu_slot_idx(idx);
                    if (!fb_sel_valid && cu_fb_aw_valid[slot]) begin
                        fb_sel_valid = 1'b1;
                        fb_sel       = $clog2(MAX_CUS)'(idx);
                    end
                end
            end

            always_comb begin
                fb_aw_valid = 1'b0; fb_aw_addr = '0; fb_aw_len = '0;
                fb_aw_size = '0; fb_aw_burst = '0;
                fb_w_valid = 1'b0; fb_w_data = '0; fb_w_strb = '0; fb_w_last = 1'b0;
                fb_b_ready = 1'b0;
                for (int s = 0; s < NUM_SLOTS; s++) begin
                    cu_fb_aw_ready[s] = 1'b0;
                    cu_fb_w_ready[s]  = 1'b0;
                    cu_fb_b_valid[s]  = 1'b0;
                end
                begin
                    automatic int active_slot = cu_slot_idx(fb_active_cu);
                    case (fb_state)
                        FB_ADDR: begin
                            fb_aw_valid = cu_fb_aw_valid[active_slot];
                            fb_aw_addr  = cu_fb_aw_addr[active_slot];
                            fb_aw_len   = cu_fb_aw_len[active_slot];
                            fb_aw_size  = cu_fb_aw_size[active_slot];
                            fb_aw_burst = cu_fb_aw_burst[active_slot];
                            cu_fb_aw_ready[active_slot] = fb_aw_ready;
                        end
                        FB_DATA: begin
                            fb_w_valid = cu_fb_w_valid[active_slot];
                            fb_w_data  = cu_fb_w_data[active_slot];
                            fb_w_strb  = cu_fb_w_strb[active_slot];
                            fb_w_last  = cu_fb_w_last[active_slot];
                            cu_fb_w_ready[active_slot] = fb_w_ready;
                        end
                        FB_RESP: begin
                            cu_fb_b_valid[active_slot] = fb_b_valid;
                            fb_b_ready = cu_fb_b_ready[active_slot];
                        end
                        default: ;
                    endcase
                end
            end

            always_ff @(posedge clk or negedge rst_n) begin
                if (!rst_n) begin
                    fb_state     <= FB_IDLE;
                    fb_rr_ptr    <= '0;
                    fb_active_cu <= '0;
                end else begin
                    case (fb_state)
                        FB_IDLE: if (fb_sel_valid) begin
                            fb_active_cu <= fb_sel;
                            fb_state     <= FB_ADDR;
                            fb_rr_ptr    <= $clog2(MAX_CUS)'((int'(fb_sel) + 1) % NUM_CUS);
                        end
                        FB_ADDR: if (fb_aw_valid && fb_aw_ready) fb_state <= FB_DATA;
                        FB_DATA: if (fb_w_valid && fb_w_ready && fb_w_last) fb_state <= FB_RESP;
                        FB_RESP: if (fb_b_valid && fb_b_ready) fb_state <= FB_IDLE;
                    endcase
                end
            end
        end else begin : gen_no_fb_arb
            assign fb_aw_valid = 1'b0; assign fb_aw_addr = '0; assign fb_aw_len = '0;
            assign fb_aw_size = '0; assign fb_aw_burst = '0;
            assign fb_w_valid = 1'b0; assign fb_w_data = '0; assign fb_w_strb = '0;
            assign fb_w_last = 1'b0;
            assign fb_b_ready = 1'b1;
        end
    endgenerate

    // -----------------------------------------------------------------------
    // Mailbox Switch Fabric
    // -----------------------------------------------------------------------
    // All active slots connect via stream crossbar.
    // For 2 active slots: simple cross-connect.
    // For 3-4 active slots: ring topology (extends to full switch later).
    // Uplink: connect to mb_up_* ports.
    generate
        if (MAILBOX_ENABLE && NUM_ACTIVE == 2) begin : gen_mb_xbar_2
            // Cross-connect
            assign mb_slot_rx_valid[0] = mb_slot_tx_valid[1];
            assign mb_slot_rx_data[0]  = mb_slot_tx_data[1];
            assign mb_slot_rx_dest[0]  = mb_slot_tx_dest[1];
            assign mb_slot_tx_ready[1] = mb_slot_rx_ready[0];

            assign mb_slot_rx_valid[1] = mb_slot_tx_valid[0];
            assign mb_slot_rx_data[1]  = mb_slot_tx_data[0];
            assign mb_slot_rx_dest[1]  = mb_slot_tx_dest[0];
            assign mb_slot_tx_ready[0] = mb_slot_rx_ready[1];

        end else if (MAILBOX_ENABLE && NUM_ACTIVE >= 3) begin : gen_mb_xbar_N
            // Destination-routed crossbar: dest_id[7:4] selects target slot.
            // Lowest-numbered source wins on contention.
            // Each destination port picks at most one source per cycle.
            logic [NUM_ACTIVE-1:0] xbar_grant [NUM_ACTIVE];

            for (genvar d = 0; d < NUM_ACTIVE; d++) begin : gen_xbar_dst
                always_comb begin
                    xbar_grant[d]       = '0;
                    mb_slot_rx_valid[d] = 1'b0;
                    mb_slot_rx_data[d]  = '0;
                    mb_slot_rx_dest[d]  = '0;
                    for (int s = 0; s < NUM_ACTIVE; s++) begin
                        if (!mb_slot_rx_valid[d] && (s != d) &&
                            mb_slot_tx_valid[s] &&
                            (mb_slot_tx_dest[s][7:4] == d[3:0])) begin
                            xbar_grant[d][s]    = 1'b1;
                            mb_slot_rx_valid[d] = 1'b1;
                            mb_slot_rx_data[d]  = mb_slot_tx_data[s];
                            mb_slot_rx_dest[d]  = mb_slot_tx_dest[s];
                        end
                    end
                end
            end

            for (genvar s = 0; s < NUM_ACTIVE; s++) begin : gen_xbar_rdy
                always_comb begin
                    mb_slot_tx_ready[s] = 1'b0;
                    for (int d = 0; d < NUM_ACTIVE; d++) begin
                        if (xbar_grant[d][s])
                            mb_slot_tx_ready[s] = mb_slot_rx_ready[d];
                    end
                end
            end

        end else begin : gen_mb_stub
            for (genvar p = 0; p < NUM_ACTIVE; p++) begin : gen_mb_tieoff
                assign mb_slot_tx_ready[p] = 1'b1;
                assign mb_slot_rx_valid[p] = 1'b0;
                assign mb_slot_rx_data[p]  = '0;
                assign mb_slot_rx_dest[p]  = '0;
            end
        end

        // Uplink: tie off for now (full integration requires connecting
        // to mailbox_switch uplink port)
        if (1) begin : gen_mb_uplink_stub
            assign mb_up_tx_valid   = 1'b0;
            assign mb_up_tx_data    = '0;
            assign mb_up_tx_dest_id = '0;
            assign mb_up_rx_ready   = 1'b1;
        end
    endgenerate

endmodule : cluster_top
