`timescale 1ns/1ps
/* LEGACY AXI‑Lite mailbox_tb (disabled)

// Simple unit testbench for mailbox interconnect (center + switch + endpoints)
module mailbox_tb;
  import mailbox_pkg::*;

  // Clock / reset
  logic clk;
  logic rst_n;
  initial clk = 1'b0;
  always #1 clk <= ~clk;

  initial begin
    rst_n = 1'b0;
    repeat (10) @(posedge clk);
    rst_n = 1'b1;
  end

  // -----------------------------
  // DUT instances and wiring (nets created and connected explicitly)
  // -----------------------------

  // nets for center<->sw0 uplink (AXI-Lite full-duplex)
  logic sw0_m_up_awvalid, sw0_m_up_awready;
  logic [15:0] sw0_m_up_awaddr;
  logic sw0_m_up_wvalid, sw0_m_up_wready;
  logic [31:0] sw0_m_up_wdata; logic [3:0] sw0_m_up_wstrb; mailbox_pkg::mailbox_tag_t sw0_m_up_tag;
  logic sw0_m_up_bvalid, sw0_m_up_bready;
  logic sw0_m_up_arvalid, sw0_m_up_arready; logic [15:0] sw0_m_up_araddr; logic sw0_m_up_rvalid, sw0_m_up_rready; logic [31:0] sw0_m_up_rdata;

  logic center_m_sw0_awvalid, center_m_sw0_awready;
  logic [15:0] center_m_sw0_awaddr;
  logic center_m_sw0_wvalid, center_m_sw0_wready;
  logic [31:0] center_m_sw0_wdata; logic [3:0] center_m_sw0_wstrb; mailbox_pkg::mailbox_tag_t center_m_sw0_tag;
  logic center_m_sw0_bvalid, center_m_sw0_bready;
  logic center_m_sw0_arvalid, center_m_sw0_arready; logic [15:0] center_m_sw0_araddr; logic center_m_sw0_rvalid, center_m_sw0_rready; logic [31:0] center_m_sw0_rdata;

  // nets for sw0 <-> ep0 (dl0)
  logic ep0_m_awvalid, ep0_m_awready; logic [15:0] ep0_m_awaddr; logic ep0_m_wvalid, ep0_m_wready; logic [31:0] ep0_m_wdata; logic [3:0] ep0_m_wstrb; mailbox_pkg::mailbox_tag_t ep0_m_tag; logic ep0_m_bvalid, ep0_m_bready;
  logic ep0_m_arvalid, ep0_m_arready; logic [15:0] ep0_m_araddr; logic ep0_m_rvalid, ep0_m_rready; logic [31:0] ep0_m_rdata;
  logic m_dl0_awvalid, m_dl0_awready; logic [15:0] m_dl0_awaddr; logic m_dl0_wvalid, m_dl0_wready; logic [31:0] m_dl0_wdata; logic [3:0] m_dl0_wstrb; mailbox_pkg::mailbox_tag_t m_dl0_tag; logic m_dl0_bvalid, m_dl0_bready;
  logic m_dl0_arvalid, m_dl0_arready; logic [15:0] m_dl0_araddr; logic m_dl0_rvalid, m_dl0_rready; logic [31:0] m_dl0_rdata;

  // nets for sw0 <-> ep1 (dl1)
  logic ep1_m_awvalid, ep1_m_awready; logic [15:0] ep1_m_awaddr; logic ep1_m_wvalid, ep1_m_wready; logic [31:0] ep1_m_wdata; logic [3:0] ep1_m_wstrb; mailbox_pkg::mailbox_tag_t ep1_m_tag; logic ep1_m_bvalid, ep1_m_bready;
  logic ep1_m_arvalid, ep1_m_arready; logic [15:0] ep1_m_araddr; logic ep1_m_rvalid, ep1_m_rready; logic [31:0] ep1_m_rdata;
  logic m_dl1_awvalid, m_dl1_awready; logic [15:0] m_dl1_awaddr; logic m_dl1_wvalid, m_dl1_wready; logic [31:0] m_dl1_wdata; logic [3:0] m_dl1_wstrb; mailbox_pkg::mailbox_tag_t m_dl1_tag; logic m_dl1_bvalid, m_dl1_bready;
  logic m_dl1_arvalid, m_dl1_arready; logic [15:0] m_dl1_araddr; logic m_dl1_rvalid, m_dl1_rready; logic [31:0] m_dl1_rdata;

  // nets for hp <-> center
  logic hp_m_awvalid, hp_m_awready; logic [15:0] hp_m_awaddr; logic hp_m_wvalid, hp_m_wready; logic [31:0] hp_m_wdata; logic [3:0] hp_m_wstrb; mailbox_pkg::mailbox_tag_t hp_m_tag; logic hp_m_bvalid, hp_m_bready;
  logic hp_m_arvalid, hp_m_arready; logic [15:0] hp_m_araddr; logic hp_m_rvalid, hp_m_rready; logic [31:0] hp_m_rdata;
  logic center_m_hp_awvalid, center_m_hp_awready; logic [15:0] center_m_hp_awaddr; logic center_m_hp_wvalid, center_m_hp_wready; logic [31:0] center_m_hp_wdata; logic [3:0] center_m_hp_wstrb; mailbox_pkg::mailbox_tag_t center_m_hp_tag; logic center_m_hp_bvalid, center_m_hp_bready;
  logic center_m_hp_arvalid, center_m_hp_arready; logic [15:0] center_m_hp_araddr; logic center_m_hp_rvalid, center_m_hp_rready; logic [31:0] center_m_hp_rdata;

  // Center instantiation
  mailbox_center #(.CHILD_CLUSTER_ID('{8'h11, 8'h22, 8'h33, 8'h44}), .FIFO_DEPTH(4)) center (
    .clk(clk), .rst_n(rst_n),
    .sw0_awvalid(sw0_m_up_awvalid), .sw0_awready(sw0_m_up_awready), .sw0_awaddr(sw0_m_up_awaddr),
    .sw0_wvalid(sw0_m_up_wvalid), .sw0_wready(sw0_m_up_wready), .sw0_wdata(sw0_m_up_wdata), .sw0_wstrb(sw0_m_up_wstrb), .sw0_tag(sw0_m_up_tag), .sw0_bvalid(sw0_m_up_bvalid), .sw0_bready(sw0_m_up_bready),
    .sw0_arvalid(sw0_m_up_arvalid), .sw0_arready(sw0_m_up_arready), .sw0_araddr(sw0_m_up_araddr), .sw0_rvalid(sw0_m_up_rvalid), .sw0_rready(sw0_m_up_rready), .sw0_rdata(sw0_m_up_rdata),
    .sw1_awvalid(), .sw1_awready(), .sw1_awaddr(), .sw1_wvalid(), .sw1_wready(), .sw1_wdata(), .sw1_wstrb(), .sw1_tag(), .sw1_bvalid(), .sw1_bready(),
    .sw1_arvalid(), .sw1_arready(), .sw1_araddr(), .sw1_rvalid(), .sw1_rready(), .sw1_rdata(),
    .sw2_awvalid(), .sw2_awready(), .sw2_awaddr(), .sw2_wvalid(), .sw2_wready(), .sw2_wdata(), .sw2_wstrb(), .sw2_tag(), .sw2_bvalid(), .sw2_bready(),
    .sw2_arvalid(), .sw2_arready(), .sw2_araddr(), .sw2_rvalid(), .sw2_rready(), .sw2_rdata(),
    .sw3_awvalid(), .sw3_awready(), .sw3_awaddr(), .sw3_wvalid(), .sw3_wready(), .sw3_wdata(), .sw3_wstrb(), .sw3_tag(), .sw3_bvalid(), .sw3_bready(),
    .sw3_arvalid(), .sw3_arready(), .sw3_araddr(), .sw3_rvalid(), .sw3_rready(), .sw3_rdata(),

    .hp_awvalid(hp_m_awvalid), .hp_awready(hp_m_awready), .hp_awaddr(hp_m_awaddr),
    .hp_wvalid(hp_m_wvalid), .hp_wready(hp_m_wready), .hp_wdata(hp_m_wdata), .hp_wstrb(hp_m_wstrb), .hp_tag(hp_m_tag), .hp_bvalid(hp_m_bvalid), .hp_bready(hp_m_bready),
    .hp_arvalid(hp_m_arvalid), .hp_arready(hp_m_arready), .hp_araddr(hp_m_araddr), .hp_rvalid(hp_m_rvalid), .hp_rready(hp_m_rready), .hp_rdata(hp_m_rdata),

    .m_sw0_awvalid(center_m_sw0_awvalid), .m_sw0_awready(center_m_sw0_awready), .m_sw0_awaddr(center_m_sw0_awaddr),
    .m_sw0_wvalid(center_m_sw0_wvalid), .m_sw0_wready(center_m_sw0_wready), .m_sw0_wdata(center_m_sw0_wdata), .m_sw0_wstrb(center_m_sw0_wstrb), .m_sw0_tag(center_m_sw0_tag), .m_sw0_bready(center_m_sw0_bready), .m_sw0_bvalid(center_m_sw0_bvalid),
    .m_sw0_arvalid(center_m_sw0_arvalid), .m_sw0_arready(center_m_sw0_arready), .m_sw0_araddr(center_m_sw0_araddr), .m_sw0_rvalid(center_m_sw0_rvalid), .m_sw0_rready(center_m_sw0_rready), .m_sw0_rdata(center_m_sw0_rdata),
    .m_sw1_awvalid(), .m_sw1_awready(), .m_sw1_awaddr(), .m_sw1_wvalid(), .m_sw1_wready(), .m_sw1_wdata(), .m_sw1_wstrb(), .m_sw1_tag(), .m_sw1_bready(), .m_sw1_bvalid(),
    .m_sw1_arvalid(), .m_sw1_arready(), .m_sw1_araddr(), .m_sw1_rvalid(), .m_sw1_rready(), .m_sw1_rdata(),
    .m_sw2_awvalid(), .m_sw2_awready(), .m_sw2_awaddr(), .m_sw2_wvalid(), .m_sw2_wready(), .m_sw2_wdata(), .m_sw2_wstrb(), .m_sw2_tag(), .m_sw2_bready(), .m_sw2_bvalid(),
    .m_sw2_arvalid(), .m_sw2_arready(), .m_sw2_araddr(), .m_sw2_rvalid(), .m_sw2_rready(), .m_sw2_rdata(),
    .m_sw3_awvalid(), .m_sw3_awready(), .m_sw3_awaddr(), .m_sw3_wvalid(), .m_sw3_wready(), .m_sw3_wdata(), .m_sw3_wstrb(), .m_sw3_tag(), .m_sw3_bready(), .m_sw3_bvalid(),
    .m_sw3_arvalid(), .m_sw3_arready(), .m_sw3_araddr(), .m_sw3_rvalid(), .m_sw3_rready(), .m_sw3_rdata(),

    .m_hp_awvalid(center_m_hp_awvalid), .m_hp_awready(center_m_hp_awready), .m_hp_awaddr(center_m_hp_awaddr),
    .m_hp_wvalid(center_m_hp_wvalid), .m_hp_wready(center_m_hp_wready), .m_hp_wdata(center_m_hp_wdata), .m_hp_wstrb(center_m_hp_wstrb), .m_hp_tag(center_m_hp_tag), .m_hp_bready(center_m_hp_bready), .m_hp_bvalid(center_m_hp_bvalid)
    , .m_hp_arvalid(center_m_hp_arvalid), .m_hp_arready(center_m_hp_arready), .m_hp_araddr(center_m_hp_araddr), .m_hp_rvalid(center_m_hp_rvalid), .m_hp_rready(center_m_hp_rready), .m_hp_rdata(center_m_hp_rdata)
  );

  // Switch instance
  mailbox_switch_2x1 #(.MY_CLUSTER_ID(8'h11), .FIFO_DEPTH(4)) sw0 (
    .clk(clk), .rst_n(rst_n),

    .dl0_awvalid(ep0_m_awvalid), .dl0_awready(ep0_m_awready), .dl0_awaddr(ep0_m_awaddr), .dl0_wvalid(ep0_m_wvalid), .dl0_wready(ep0_m_wready), .dl0_wdata(ep0_m_wdata), .dl0_wstrb(ep0_m_wstrb), .dl0_tag(ep0_m_tag), .dl0_bvalid(ep0_m_bvalid), .dl0_bready(ep0_m_bready),
    .dl0_arvalid(ep0_m_arvalid), .dl0_arready(ep0_m_arready), .dl0_araddr(ep0_m_araddr), .dl0_rvalid(ep0_m_rvalid), .dl0_rready(ep0_m_rready), .dl0_rdata(ep0_m_rdata),
    .dl1_awvalid(ep1_m_awvalid), .dl1_awready(ep1_m_awready), .dl1_awaddr(ep1_m_awaddr), .dl1_wvalid(ep1_m_wvalid), .dl1_wready(ep1_m_wready), .dl1_wdata(ep1_m_wdata), .dl1_wstrb(ep1_m_wstrb), .dl1_tag(ep1_m_tag), .dl1_bvalid(ep1_m_bvalid), .dl1_bready(ep1_m_bready),
    .dl1_arvalid(ep1_m_arvalid), .dl1_arready(ep1_m_arready), .dl1_araddr(ep1_m_araddr), .dl1_rvalid(ep1_m_rvalid), .dl1_rready(ep1_m_rready), .dl1_rdata(ep1_m_rdata),

    .up_awvalid(center_m_sw0_awvalid), .up_awready(center_m_sw0_awready), .up_awaddr(center_m_sw0_awaddr), .up_wvalid(center_m_sw0_wvalid), .up_wready(center_m_sw0_wready), .up_wdata(center_m_sw0_wdata), .up_wstrb(center_m_sw0_wstrb), .up_tag(center_m_sw0_tag), .up_bvalid(center_m_sw0_bvalid), .up_bready(center_m_sw0_bready),
    .up_arvalid(center_m_sw0_arvalid), .up_arready(center_m_sw0_arready), .up_araddr(center_m_sw0_araddr), .up_rvalid(center_m_sw0_rvalid), .up_rready(center_m_sw0_rready), .up_rdata(center_m_sw0_rdata),

    .m_up_awvalid(sw0_m_up_awvalid), .m_up_awready(sw0_m_up_awready), .m_up_awaddr(sw0_m_up_awaddr), .m_up_wvalid(sw0_m_up_wvalid), .m_up_wready(sw0_m_up_wready), .m_up_wdata(sw0_m_up_wdata), .m_up_wstrb(sw0_m_up_wstrb), .m_up_tag(sw0_m_up_tag), .m_up_bready(sw0_m_up_bready), .m_up_bvalid(sw0_m_up_bvalid),
    .m_up_arvalid(sw0_m_up_arvalid), .m_up_arready(sw0_m_up_arready), .m_up_araddr(sw0_m_up_araddr), .m_up_rvalid(sw0_m_up_rvalid), .m_up_rready(sw0_m_up_rready), .m_up_rdata(sw0_m_up_rdata),

    .m_dl0_awvalid(m_dl0_awvalid), .m_dl0_awready(m_dl0_awready), .m_dl0_awaddr(m_dl0_awaddr), .m_dl0_wvalid(m_dl0_wvalid), .m_dl0_wready(m_dl0_wready), .m_dl0_wdata(m_dl0_wdata), .m_dl0_wstrb(m_dl0_wstrb), .m_dl0_tag(m_dl0_tag), .m_dl0_bready(m_dl0_bready), .m_dl0_bvalid(m_dl0_bvalid),
    .m_dl0_arvalid(m_dl0_arvalid), .m_dl0_arready(m_dl0_arready), .m_dl0_araddr(m_dl0_araddr), .m_dl0_rvalid(m_dl0_rvalid), .m_dl0_rready(m_dl0_rready), .m_dl0_rdata(m_dl0_rdata),
    .m_dl1_awvalid(m_dl1_awvalid), .m_dl1_awready(m_dl1_awready), .m_dl1_awaddr(m_dl1_awaddr), .m_dl1_wvalid(m_dl1_wvalid), .m_dl1_wready(m_dl1_wready), .m_dl1_wdata(m_dl1_wdata), .m_dl1_wstrb(m_dl1_wstrb), .m_dl1_tag(m_dl1_tag), .m_dl1_bready(m_dl1_bready), .m_dl1_bvalid(m_dl1_bvalid),
    .m_dl1_arvalid(m_dl1_arvalid), .m_dl1_arready(m_dl1_arready), .m_dl1_araddr(m_dl1_araddr), .m_dl1_rvalid(m_dl1_rvalid), .m_dl1_rready(m_dl1_rready), .m_dl1_rdata(m_dl1_rdata)
  );

  // Simple endpoints: ep0 (local id 0) and ep1 (local id 1)

  // -----------------------------
  // Endpoint signal wiring + Test helpers
  // -----------------------------
  // ep0 signals
  logic ep0_tx_valid, ep0_tx_prio, ep0_tx_eop; logic [15:0] ep0_tx_dest; logic [31:0] ep0_tx_data; logic [3:0] ep0_tx_opcode; logic ep0_tx_ready;
  logic ep0_rx_valid; logic [31:0] ep0_rx_data; mailbox_pkg::mailbox_tag_t ep0_rx_tag; logic ep0_rx_irq;
  logic ep0_rd_valid, ep0_rd_ready; logic [15:0] ep0_rd_dest; logic ep0_rd_prio; logic [3:0] ep0_rd_opcode; logic ep0_rd_resp_valid, ep0_rd_resp_ready; logic [31:0] ep0_rd_resp_data; mailbox_pkg::mailbox_tag_t ep0_rd_resp_tag;

  // ep1 signals
  logic ep1_tx_valid, ep1_tx_prio, ep1_tx_eop; logic [15:0] ep1_tx_dest; logic [31:0] ep1_tx_data; logic [3:0] ep1_tx_opcode; logic ep1_tx_ready;
  logic ep1_rx_valid; logic [31:0] ep1_rx_data; mailbox_pkg::mailbox_tag_t ep1_rx_tag; logic ep1_rx_irq;
  logic ep1_rd_valid, ep1_rd_ready; logic [15:0] ep1_rd_dest; logic ep1_rd_prio; logic [3:0] ep1_rd_opcode; logic ep1_rd_resp_valid, ep1_rd_resp_ready; logic [31:0] ep1_rd_resp_data; mailbox_pkg::mailbox_tag_t ep1_rd_resp_tag;

  // hp endpoint signals
  logic hp_tx_valid = 1'b0; logic hp_tx_ready; logic hp_rx_valid; logic [31:0] hp_rx_data; mailbox_pkg::mailbox_tag_t hp_rx_tag; logic hp_rx_irq;
  logic hp_rd_valid, hp_rd_ready; logic [15:0] hp_rd_dest; logic hp_rd_prio; logic [3:0] hp_rd_opcode; logic hp_rd_resp_valid, hp_rd_resp_ready; logic [31:0] hp_rd_resp_data; mailbox_pkg::mailbox_tag_t hp_rd_resp_tag;

  // We'll accept RX when ready is asserted
  logic ep0_rx_ready = 1'b0; logic ep1_rx_ready = 1'b0; logic hp_rx_ready = 1'b1;

  // Drive unused fields to known values to avoid X-propagation
  initial begin
    ep0_tx_valid = 1'b0; ep0_tx_prio = 1'b0; ep0_tx_eop = 1'b0; ep0_tx_dest = '0; ep0_tx_data = '0; ep0_tx_opcode = '0;
    ep1_tx_valid = 1'b0; ep1_tx_prio = 1'b0; ep1_tx_eop = 1'b0; ep1_tx_dest = '0; ep1_tx_data = '0; ep1_tx_opcode = '0;
    hp_tx_valid  = 1'b0;

    ep0_rd_valid = 1'b0; ep0_rd_dest = '0; ep0_rd_prio = 1'b0; ep0_rd_opcode = '0; ep0_rd_resp_ready = 1'b0;
    ep1_rd_valid = 1'b0; ep1_rd_dest = '0; ep1_rd_prio = 1'b0; ep1_rd_opcode = '0; ep1_rd_resp_ready = 1'b0;
    hp_rd_valid  = 1'b0; hp_rd_dest  = '0; hp_rd_prio  = 1'b0; hp_rd_opcode  = '0; hp_rd_resp_ready = 1'b0;
  end

  // connect endpoint I/O to these nets via instantiation below

  // -----------------------------
  // Endpoint instantiations (connected to nets)
  // -----------------------------
  mailbox_endpoint #(.SRC_ID(8'h10), .TX_DEPTH(8), .RX_DEPTH(8)) ep0 (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(ep0_tx_valid), .tx_ready(ep0_tx_ready), .tx_dest(ep0_tx_dest), .tx_data(ep0_tx_data), .tx_prio(ep0_tx_prio), .tx_eop(ep0_tx_eop), .tx_opcode(ep0_tx_opcode),
    .rx_valid(ep0_rx_valid), .rx_ready(ep0_rx_ready), .rx_data(ep0_rx_data), .rx_tag(ep0_rx_tag), .rx_irq(ep0_rx_irq),
    .rd_valid(ep0_rd_valid), .rd_ready(ep0_rd_ready), .rd_dest(ep0_rd_dest), .rd_prio(ep0_rd_prio), .rd_opcode(ep0_rd_opcode), .rd_resp_valid(ep0_rd_resp_valid), .rd_resp_ready(ep0_rd_resp_ready), .rd_resp_data(ep0_rd_resp_data), .rd_resp_tag(ep0_rd_resp_tag),
    .m_awvalid(ep0_m_awvalid), .m_awready(ep0_m_awready), .m_awaddr(ep0_m_awaddr), .m_wvalid(ep0_m_wvalid), .m_wready(ep0_m_wready), .m_wdata(ep0_m_wdata), .m_wstrb(ep0_m_wstrb), .m_tag(ep0_m_tag), .m_bready(ep0_m_bready), .m_bvalid(ep0_m_bvalid),
    .m_arvalid(ep0_m_arvalid), .m_arready(ep0_m_arready), .m_araddr(ep0_m_araddr), .m_rvalid(ep0_m_rvalid), .m_rready(ep0_m_rready), .m_rdata(ep0_m_rdata),
    .s_awvalid(m_dl0_awvalid), .s_awready(m_dl0_awready), .s_awaddr(m_dl0_awaddr), .s_wvalid(m_dl0_wvalid), .s_wready(m_dl0_wready), .s_wdata(m_dl0_wdata), .s_wstrb(m_dl0_wstrb), .s_tag(m_dl0_tag), .s_bready(m_dl0_bready), .s_bvalid(m_dl0_bvalid),
    .s_arvalid(m_dl0_arvalid), .s_arready(m_dl0_arready), .s_araddr(m_dl0_araddr), .s_rvalid(m_dl0_rvalid), .s_rready(m_dl0_rready), .s_rdata(m_dl0_rdata)
  );

  mailbox_endpoint #(.SRC_ID(8'h11), .TX_DEPTH(8), .RX_DEPTH(8)) ep1 (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(ep1_tx_valid), .tx_ready(ep1_tx_ready), .tx_dest(ep1_tx_dest), .tx_data(ep1_tx_data), .tx_prio(ep1_tx_prio), .tx_eop(ep1_tx_eop), .tx_opcode(ep1_tx_opcode),
    .rx_valid(ep1_rx_valid), .rx_ready(ep1_rx_ready), .rx_data(ep1_rx_data), .rx_tag(ep1_rx_tag), .rx_irq(ep1_rx_irq),
    .rd_valid(ep1_rd_valid), .rd_ready(ep1_rd_ready), .rd_dest(ep1_rd_dest), .rd_prio(ep1_rd_prio), .rd_opcode(ep1_rd_opcode), .rd_resp_valid(ep1_rd_resp_valid), .rd_resp_ready(ep1_rd_resp_ready), .rd_resp_data(ep1_rd_resp_data), .rd_resp_tag(ep1_rd_resp_tag),
    .m_awvalid(ep1_m_awvalid), .m_awready(ep1_m_awready), .m_awaddr(ep1_m_awaddr), .m_wvalid(ep1_m_wvalid), .m_wready(ep1_m_wready), .m_wdata(ep1_m_wdata), .m_wstrb(ep1_m_wstrb), .m_tag(ep1_m_tag), .m_bready(ep1_m_bready), .m_bvalid(ep1_m_bvalid),
    .m_arvalid(ep1_m_arvalid), .m_arready(ep1_m_arready), .m_araddr(ep1_m_araddr), .m_rvalid(ep1_m_rvalid), .m_rready(ep1_m_rready), .m_rdata(ep1_m_rdata),
    .s_awvalid(m_dl1_awvalid), .s_awready(m_dl1_awready), .s_awaddr(m_dl1_awaddr), .s_wvalid(m_dl1_wvalid), .s_wready(m_dl1_wready), .s_wdata(m_dl1_wdata), .s_wstrb(m_dl1_wstrb), .s_tag(m_dl1_tag), .s_bready(m_dl1_bready), .s_bvalid(m_dl1_bvalid),
    .s_arvalid(m_dl1_arvalid), .s_arready(m_dl1_arready), .s_araddr(m_dl1_araddr), .s_rvalid(m_dl1_rvalid), .s_rready(m_dl1_rready), .s_rdata(m_dl1_rdata)
  );

  mailbox_endpoint #(.SRC_ID(8'h01), .TX_DEPTH(8), .RX_DEPTH(8)) hp (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(hp_tx_valid), .tx_ready(hp_tx_ready), .tx_dest(), .tx_data(), .tx_prio(), .tx_eop(), .tx_opcode(),
    .rx_valid(hp_rx_valid), .rx_ready(hp_rx_ready), .rx_data(hp_rx_data), .rx_tag(hp_rx_tag), .rx_irq(hp_rx_irq),
    .rd_valid(hp_rd_valid), .rd_ready(hp_rd_ready), .rd_dest(hp_rd_dest), .rd_prio(hp_rd_prio), .rd_opcode(hp_rd_opcode), .rd_resp_valid(hp_rd_resp_valid), .rd_resp_ready(hp_rd_resp_ready), .rd_resp_data(hp_rd_resp_data), .rd_resp_tag(hp_rd_resp_tag),
    .m_awvalid(hp_m_awvalid), .m_awready(hp_m_awready), .m_awaddr(hp_m_awaddr), .m_wvalid(hp_m_wvalid), .m_wready(hp_m_wready), .m_wdata(hp_m_wdata), .m_wstrb(hp_m_wstrb), .m_tag(hp_m_tag), .m_bready(hp_m_bready), .m_bvalid(hp_m_bvalid),
    .m_arvalid(hp_m_arvalid), .m_arready(hp_m_arready), .m_araddr(hp_m_araddr), .m_rvalid(hp_m_rvalid), .m_rready(hp_m_rready), .m_rdata(hp_m_rdata),
    .s_awvalid(center_m_hp_awvalid), .s_awready(center_m_hp_awready), .s_awaddr(center_m_hp_awaddr), .s_wvalid(center_m_hp_wvalid), .s_wready(center_m_hp_wready), .s_wdata(center_m_hp_wdata), .s_wstrb(center_m_hp_wstrb), .s_tag(center_m_hp_tag), .s_bready(center_m_hp_bready), .s_bvalid(center_m_hp_bvalid),
    .s_arvalid(center_m_hp_arvalid), .s_arready(center_m_hp_arready), .s_araddr(center_m_hp_araddr), .s_rvalid(center_m_hp_rvalid), .s_rready(center_m_hp_rready), .s_rdata(center_m_hp_rdata)
  );
  // Helper: send single-beat packet from ep0
  task automatic send_ep0(input logic [15:0] dest, input logic [31:0] data, input logic prio = 1'b0);
    int i;
    begin
      @(posedge clk);
      ep0_tx_prio = prio;
      ep0_tx_eop  = 1'b1;
      ep0_tx_opcode = OPC_DATA;
      ep0_tx_dest = dest;
      ep0_tx_data = data;
      // wait until endpoint is ready
      ep0_tx_valid = 1'b1;
      wait (ep0.tx_ready == 1'b1);

      // Hold valid until we see AXI AW/W asserted or timeout
      for (i = 0; i < 16; i++) begin
        @(posedge clk);
        if (ep0_m_awvalid && ep0_m_wvalid) begin
          $display("%0t: send_ep0 observed ep0_m_awvalid/wvalid (adr=%h dat=%h)", $time, ep0_m_awaddr, ep0_m_wdata);
          ep0_tx_valid = 1'b0;
          break;
        end
      end
      if ((ep0_m_awvalid && ep0_m_wvalid) == 1'b0) begin
        ep0_tx_valid = 1'b0;
        $display("%0t: WARNING: ep0 did not assert m_awvalid/m_wvalid after send. wr_ptr=%0d rd_ptr=%0d tx_r_empty=%0b u_tx_fifo.w_en=%0b u_tx_fifo.w_full=%0b", $time, ep0.u_tx_fifo.wr_ptr, ep0.u_tx_fifo.rd_ptr, ep0.tx_r_empty, ep0.u_tx_fifo.w_en, ep0.u_tx_fifo.w_full);
      end
    end
  endtask

  // Helper: send single-beat packet from ep1
  task automatic send_ep1(input logic [15:0] dest, input logic [31:0] data, input logic prio = 1'b0);
    int i;
    begin
      @(posedge clk);
      ep1_tx_prio = prio;
      ep1_tx_eop  = 1'b1;
      ep1_tx_opcode = OPC_DATA;
      ep1_tx_dest = dest;
      ep1_tx_data = data;
      ep1_tx_valid = 1'b1;
      wait (ep1.tx_ready == 1'b1);
      for (i = 0; i < 16; i++) begin
        @(posedge clk);
        if (ep1_m_awvalid && ep1_m_wvalid) begin
          ep1_tx_valid = 1'b0;
          break;
        end
      end
      if ((ep1_m_awvalid && ep1_m_wvalid) == 1'b0) begin
        ep1_tx_valid = 1'b0;
        $display("%0t: WARNING: ep1 did not assert m_awvalid/m_wvalid after send.", $time);
      end
    end
  endtask

  // Helper: blocking read via hp endpoint (rd_resp_ready held high)
  task automatic hp_read_expect(input logic [15:0] dest, input logic [31:0] exp, input int timeout_cycles=200);
    int tcnt;
    begin
      hp_rd_dest  = dest;
      hp_rd_prio  = 1'b0;
      rst_n = 1'b0;
      repeat (10) @(posedge clk);
      rst_n = 1'b1;
      hp_rd_valid = 1'b1;
      tcnt = 0;
      while ((tcnt < timeout_cycles) && !hp_rd_ready) begin
        @(posedge clk);
        tcnt++;
      end
      if (!hp_rd_ready) begin
        $display("[FAIL] hp rd_ready timeout dest=%h", dest);
        $finish;
      end
      @(posedge clk);
      hp_rd_valid = 1'b0;
      tcnt = 0;
      while ((tcnt < timeout_cycles) && !hp_rd_resp_valid) begin
        @(posedge clk);
        tcnt++;
      end
      if (!hp_rd_resp_valid) begin
        $display("[FAIL] hp read timeout dest=%h", dest);
        $finish;
      end
      if (hp_rd_resp_data !== exp) begin
        $display("[FAIL] hp read dest=%h got=0x%08x exp=0x%08x", dest, hp_rd_resp_data, exp);
        $finish;
      end else begin
        $display("[PASS] hp read dest=%h data=0x%08x", dest, exp);
      end
      @(posedge clk);
    end
  endtask

  // Helper: read one from endpoint by id (0=ep0,1=ep1,2=hp) (blocking) with timeout
  task automatic expect_rx_by_id(input int id, input logic [31:0] exp, input int timeout_cycles=200);
    int tcnt;
    logic received;
    logic [31:0] val;
    begin
      $display("%0t: expect_rx_by_id start id=%0d exp=0x%08x", $time, id, exp);
      tcnt = 0;
      received = 1'b0;

      // First, check current cycle in case rx_valid was asserted before we entered
      case (id)
        0: if (ep0_rx_valid) begin val = ep0_rx_data; received = 1'b1; end
        1: if (ep1_rx_valid) begin val = ep1_rx_data; received = 1'b1; end
        default: if (hp_rx_valid) begin val = hp_rx_data; received = 1'b1; end
      endcase

      while ((tcnt < timeout_cycles) && !received) begin
        @(posedge clk);
        case (id)
          0: if (ep0_rx_valid) begin val = ep0_rx_data; received = 1'b1; end
          1: if (ep1_rx_valid) begin val = ep1_rx_data; received = 1'b1; end
          default: if (hp_rx_valid) begin val = hp_rx_data; received = 1'b1; end
        endcase
        tcnt++;
      end

      if (!received) begin
        $display("[FAIL] id=%0d did not receive data within timeout", id);
        $finish;
      end

      $display("%0t: expect_rx_by_id got id=%0d val=0x%08x", $time, id, val);

      if (val !== exp) begin
        $display("[FAIL] id=%0d received 0x%08x (expected 0x%08x)", id, val, exp);
        $finish;
      end else begin
        $display("[PASS] id=%0d received expected 0x%08x", id, exp);
      end

      // allow one cycle for FIFO pop (rx_ready tied high)
      @(posedge clk);
    end
  endtask

  // -----------------------------
  // Wave dump + simple monitors
  // -----------------------------
  initial begin
    $dumpfile("mailbox_tb.vcd");
    $dumpvars(0, mailbox_tb);
  end

  // Small monitors to help debugging
  always @(posedge clk) begin
    if (ep0_tx_valid) $display("%0t: ep0_tx_valid asserted (tx_ready=%0b)", $time, ep0_tx_ready);
    if (ep0.tx_w_en) $display("%0t: ep0 tx_w_en (tx_w_full=%0b) wr_ptr=%0d rd_ptr=%0d tx_r_empty=%0b u_tx_fifo.w_en=%0b u_tx_fifo.w_full=%0b", $time, ep0.tx_w_full, ep0.u_tx_fifo.wr_ptr, ep0.u_tx_fifo.rd_ptr, ep0.tx_r_empty, ep0.u_tx_fifo.w_en, ep0.u_tx_fifo.w_full);
    if (ep0.tx_r_en) $display("%0t: ep0 tx_r_en (m_bvalid=%0b) wr_ptr=%0d rd_ptr=%0d tx_r_empty=%0b u_tx_fifo.w_en=%0b u_tx_fifo.w_full=%0b", $time, ep0.m_bvalid, ep0.u_tx_fifo.wr_ptr, ep0.u_tx_fifo.rd_ptr, ep0.tx_r_empty, ep0.u_tx_fifo.w_en, ep0.u_tx_fifo.w_full);
    if (ep0_m_awvalid && ep0_m_wvalid) $display("%0t: ep0 -> sw0 dl0 adr=%h dat=%h", $time, ep0_m_awaddr, ep0_m_wdata);
    if (sw0.dl0_awvalid && sw0.dl0_wvalid) begin
      $display("%0t: sw0 got dl0 input adr=%h dat=%h", $time, ep0_m_awaddr, ep0_m_wdata);
      $display("    sw0.sel_dl0=%0d sel_dl1=%0d grant_src=%b dl0_w_en=%0b dl1_w_en=%0b dl1_fifo_full=%0b", $time, sw0.sel_dl0, sw0.sel_dl1, sw0.grant_src, sw0.dl0_fifo_w_en, sw0.dl1_fifo_w_en, sw0.dl1_fifo_full);
    end
    if (sw0.m_up_awvalid && sw0.m_up_wvalid) $display("%0t: sw0 uplink -> center adr=%h dat=%h", $time, sw0.m_up_awaddr, sw0.m_up_wdata);
    if (center.sw0_awvalid && center.sw0_wvalid) $display("%0t: center got sw0 uplink adr=%h dat=%h", $time, sw0.m_up_awaddr, sw0.m_up_wdata);
    if (ep1_m_awvalid && ep1_m_wvalid) $display("%0t: ep1 -> sw0 dl1 adr=%h dat=%h", $time, ep1_m_awaddr, ep1_m_wdata);
    if (sw0.m_dl1_awvalid && sw0.m_dl1_wvalid) $display("%0t: sw0 -> dl1 send adr=%h dat=%h", $time, m_dl1_awaddr, m_dl1_wdata);
    if (sw0.m_dl0_awvalid && sw0.m_dl0_wvalid) $display("%0t: sw0 -> dl0 send adr=%h dat=%h", $time, m_dl0_awaddr, m_dl0_wdata);
    if (center.m_sw0_awvalid && center.m_sw0_wvalid) $display("%0t: center -> sw0 adr=%h dat=%h", $time, center_m_sw0_awaddr, center_m_sw0_wdata);
    if (ep1_rx_valid) $display("%0t: ep1 rx_valid data=%h", $time, ep1_rx_data);
    if (hp_rx_valid) $display("%0t: hp rx_valid data=%h", $time, hp_rx_data);
    if (hp_rd_valid) $display("%0t: hp rd_valid dest=%h", $time, hp_rd_dest);
    if (hp_rd_resp_valid) $display("%0t: hp rd_resp data=%h", $time, hp_rd_resp_data);
  end

  // -----------------------------
  // Test sequence
  // -----------------------------
  initial begin
    // Initialize inputs
    ep0_tx_valid = 1'b0; ep0_tx_prio = 1'b0; ep0_tx_eop = 1'b1; ep0_tx_dest = 16'h0; ep0_tx_data = 32'h0; ep0_tx_opcode = OPC_DATA;
    ep1_tx_valid = 1'b0; ep1_tx_prio = 1'b0; ep1_tx_eop = 1'b1; ep1_tx_dest = 16'h0; ep1_tx_data = 32'h0; ep1_tx_opcode = OPC_DATA;
    ep0_rd_valid = 1'b0; ep0_rd_prio = 1'b0; ep0_rd_opcode = OPC_DATA; ep0_rd_resp_ready = 1'b1;
    ep1_rd_valid = 1'b0; ep1_rd_prio = 1'b0; ep1_rd_opcode = OPC_DATA; ep1_rd_resp_ready = 1'b1;
    hp_rd_valid  = 1'b0; hp_rd_prio  = 1'b0; hp_rd_opcode  = OPC_DATA; hp_rd_resp_ready  = 1'b1;

    // Allow reset release
    @(posedge clk);
    @(posedge clk);

    // Test1: ep1 -> ep0 data push (idx0) then pop via HP read
    $display("Test1: ep1 -> ep0 push then hp pop");
    send_ep1({8'h11, 8'h00}, 32'hDEAD_BEEF);
    hp_read_expect({8'h11, 8'h00}, 32'hDEAD_BEEF);

    // Test2: pop empty returns DEADBEEF sentinel
    $display("Test2: pop empty returns DEADBEEF");
    hp_read_expect({8'h11, 8'h00}, 32'hDEAD_BEEF);

    $display("All tests passed.");
    $finish;
  end

endmodule
*/

// Simple stream-based testbench for AXI‑MailboxFabric (center + switch + endpoints)
module mailbox_tb;
  import mailbox_pkg::*;

  // Clock / reset
  logic clk;
  logic rst_n;
  initial clk = 1'b0;
  always #1 clk <= ~clk;

  initial begin
    rst_n = 1'b0;
    repeat (10) @(posedge clk);
    rst_n = 1'b1;
  end

  // Endpoint core-side signals
  logic ep0_tx_valid, ep0_tx_ready;
  logic [DATA_WIDTH-1:0] ep0_tx_data;
  logic [NODE_ID_WIDTH-1:0] ep0_tx_dest;
  logic [3:0] ep0_tx_opcode;
  logic [1:0] ep0_tx_prio;
  logic ep0_tx_eop, ep0_tx_debug;

  logic ep1_tx_valid, ep1_tx_ready;
  logic [DATA_WIDTH-1:0] ep1_tx_data;
  logic [NODE_ID_WIDTH-1:0] ep1_tx_dest;
  logic [3:0] ep1_tx_opcode;
  logic [1:0] ep1_tx_prio;
  logic ep1_tx_eop, ep1_tx_debug;

  logic hp_tx_valid, hp_tx_ready;
  logic [DATA_WIDTH-1:0] hp_tx_data;
  logic [NODE_ID_WIDTH-1:0] hp_tx_dest;
  logic [3:0] hp_tx_opcode;
  logic [1:0] hp_tx_prio;
  logic hp_tx_eop, hp_tx_debug;

  logic ep0_rx_valid, ep1_rx_valid, hp_rx_valid;
  logic [DATA_WIDTH-1:0] ep0_rx_data, ep1_rx_data, hp_rx_data;
  mailbox_header_t ep0_rx_hdr, ep1_rx_hdr, hp_rx_hdr;
  logic [NODE_ID_WIDTH-1:0] ep0_rx_dest_id, ep1_rx_dest_id, hp_rx_dest_id;
  logic ep0_rx_ready, ep1_rx_ready, hp_rx_ready;
  logic ep0_rx_irq, ep1_rx_irq, hp_rx_irq;
  logic ep0_rx_err, ep1_rx_err, hp_rx_err;

  // Fabric link wires
  logic ep0_link_tx_valid, ep0_link_tx_ready;
  mailbox_flit_t ep0_link_tx_data;
  logic [NODE_ID_WIDTH-1:0] ep0_link_tx_dest;
  logic ep0_link_rx_valid, ep0_link_rx_ready;
  mailbox_flit_t ep0_link_rx_data;
  logic [NODE_ID_WIDTH-1:0] ep0_link_rx_dest;

  logic ep1_link_tx_valid, ep1_link_tx_ready;
  mailbox_flit_t ep1_link_tx_data;
  logic [NODE_ID_WIDTH-1:0] ep1_link_tx_dest;
  logic ep1_link_rx_valid, ep1_link_rx_ready;
  mailbox_flit_t ep1_link_rx_data;
  logic [NODE_ID_WIDTH-1:0] ep1_link_rx_dest;

  // Switch -> Center (uplink) raw
  logic sw_up_valid, sw_up_ready;
  mailbox_flit_t sw_up_data;
  logic [NODE_ID_WIDTH-1:0] sw_up_dest;

  // Center -> Switch (downlink) raw
  logic center_sw_valid, center_sw_ready;
  mailbox_flit_t center_sw_data;
  logic [NODE_ID_WIDTH-1:0] center_sw_dest;

  // Registered slices to break combinational loops
  logic sw0_to_center_valid;
  mailbox_flit_t sw0_to_center_data;
  logic [NODE_ID_WIDTH-1:0] sw0_to_center_dest;
  logic sw0_to_center_ready;

  logic center_to_sw0_valid;
  mailbox_flit_t center_to_sw0_data;
  logic [NODE_ID_WIDTH-1:0] center_to_sw0_dest;
  logic center_to_sw0_ready;

  logic hp_link_tx_valid, hp_link_tx_ready;
  mailbox_flit_t hp_link_tx_data;
  logic [NODE_ID_WIDTH-1:0] hp_link_tx_dest;
  logic hp_link_rx_valid, hp_link_rx_ready;
  mailbox_flit_t hp_link_rx_data;
  logic [NODE_ID_WIDTH-1:0] hp_link_rx_dest;


  mailbox_endpoint_stream #(.SRC_ID(16'h1100), .TX_DEPTH(4), .RX_DEPTH(4)) ep0 (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(ep0_tx_valid), .tx_ready(ep0_tx_ready), .tx_data(ep0_tx_data), .tx_dest_id(ep0_tx_dest),
    .tx_opcode(ep0_tx_opcode), .tx_prio(ep0_tx_prio), .tx_eop(ep0_tx_eop), .tx_debug(ep0_tx_debug),
    .rx_valid(ep0_rx_valid), .rx_ready(ep0_rx_ready), .rx_data(ep0_rx_data), .rx_hdr(ep0_rx_hdr), .rx_irq(ep0_rx_irq), .rx_error(ep0_rx_err), .rx_dest_id(ep0_rx_dest_id),
    .link_tx_valid(ep0_link_tx_valid), .link_tx_ready(ep0_link_tx_ready), .link_tx_data(ep0_link_tx_data), .link_tx_dest_id(ep0_link_tx_dest),
    .link_rx_valid(ep0_link_rx_valid), .link_rx_ready(ep0_link_rx_ready), .link_rx_data(ep0_link_rx_data), .link_rx_dest_id(ep0_link_rx_dest)
  );

  mailbox_endpoint_stream #(.SRC_ID(16'h1110), .TX_DEPTH(4), .RX_DEPTH(4)) ep1 (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(ep1_tx_valid), .tx_ready(ep1_tx_ready), .tx_data(ep1_tx_data), .tx_dest_id(ep1_tx_dest),
    .tx_opcode(ep1_tx_opcode), .tx_prio(ep1_tx_prio), .tx_eop(ep1_tx_eop), .tx_debug(ep1_tx_debug),
    .rx_valid(ep1_rx_valid), .rx_ready(ep1_rx_ready), .rx_data(ep1_rx_data), .rx_hdr(ep1_rx_hdr), .rx_irq(ep1_rx_irq), .rx_error(ep1_rx_err), .rx_dest_id(ep1_rx_dest_id),
    .link_tx_valid(ep1_link_tx_valid), .link_tx_ready(ep1_link_tx_ready), .link_tx_data(ep1_link_tx_data), .link_tx_dest_id(ep1_link_tx_dest),
    .link_rx_valid(ep1_link_rx_valid), .link_rx_ready(ep1_link_rx_ready), .link_rx_data(ep1_link_rx_data), .link_rx_dest_id(ep1_link_rx_dest)
  );

  mailbox_endpoint_stream #(.SRC_ID(16'h0000), .TX_DEPTH(4), .RX_DEPTH(4)) hp (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(hp_tx_valid), .tx_ready(hp_tx_ready), .tx_data(hp_tx_data), .tx_dest_id(hp_tx_dest),
    .tx_opcode(hp_tx_opcode), .tx_prio(hp_tx_prio), .tx_eop(hp_tx_eop), .tx_debug(hp_tx_debug),
    .rx_valid(hp_rx_valid), .rx_ready(hp_rx_ready), .rx_data(hp_rx_data), .rx_hdr(hp_rx_hdr), .rx_irq(hp_rx_irq), .rx_error(hp_rx_err), .rx_dest_id(hp_rx_dest_id),
    .link_tx_valid(hp_link_tx_valid), .link_tx_ready(hp_link_tx_ready), .link_tx_data(hp_link_tx_data), .link_tx_dest_id(hp_link_tx_dest),
    .link_rx_valid(hp_link_rx_valid), .link_rx_ready(hp_link_rx_ready), .link_rx_data(hp_link_rx_data), .link_rx_dest_id(hp_link_rx_dest)
  );


  // -----------------------------
  // Register slices (break combinational loops)
  // -----------------------------
  // Switch -> Center
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      sw0_to_center_valid <= 1'b0;
      sw0_to_center_data  <= '0;
      sw0_to_center_dest  <= '0;
    end else if (sw_up_ready) begin
      sw0_to_center_valid <= sw_up_valid;
      sw0_to_center_data  <= sw_up_data;
      sw0_to_center_dest  <= sw_up_dest;
    end
  end
  assign sw_up_ready = !sw0_to_center_valid || sw0_to_center_ready;

  // Center -> Switch
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      center_to_sw0_valid <= 1'b0;
      center_to_sw0_data  <= '0;
      center_to_sw0_dest  <= '0;
    end else if (center_sw_ready) begin
      center_to_sw0_valid <= center_sw_valid;
      center_to_sw0_data  <= center_sw_data;
      center_to_sw0_dest  <= center_sw_dest;
    end
  end
  assign center_sw_ready = !center_to_sw0_valid || center_to_sw0_ready;

  mailbox_switch_2x1_stream #(.MY_CLUSTER_ID(8'h11)) sw0 (
    .clk(clk), .rst_n(rst_n),
    .dl0_valid(ep0_link_tx_valid), .dl0_ready(ep0_link_tx_ready), .dl0_data(ep0_link_tx_data), .dl0_dest_id(ep0_link_tx_dest),
    .dl1_valid(ep1_link_tx_valid), .dl1_ready(ep1_link_tx_ready), .dl1_data(ep1_link_tx_data), .dl1_dest_id(ep1_link_tx_dest),
    .up_valid(center_to_sw0_valid), .up_ready(center_to_sw0_ready), .up_data(center_to_sw0_data), .up_dest_id(center_to_sw0_dest),
    .m_up_valid(sw_up_valid), .m_up_ready(sw_up_ready), .m_up_data(sw_up_data), .m_up_dest_id(sw_up_dest),
    .m_dl0_valid(ep0_link_rx_valid), .m_dl0_ready(ep0_link_rx_ready), .m_dl0_data(ep0_link_rx_data), .m_dl0_dest_id(ep0_link_rx_dest),
    .m_dl1_valid(ep1_link_rx_valid), .m_dl1_ready(ep1_link_rx_ready), .m_dl1_data(ep1_link_rx_data), .m_dl1_dest_id(ep1_link_rx_dest)
  );

  mailbox_center_stream #(.CHILD_CLUSTER_ID('{8'h11, 8'h22, 8'h33, 8'h44})) center (
    .clk(clk), .rst_n(rst_n),
    .sw0_valid(sw0_to_center_valid), .sw0_ready(sw0_to_center_ready), .sw0_data(sw0_to_center_data), .sw0_dest_id(sw0_to_center_dest),
    .sw1_valid(1'b0), .sw1_ready(), .sw1_data('0), .sw1_dest_id('0),
    .sw2_valid(1'b0), .sw2_ready(), .sw2_data('0), .sw2_dest_id('0),
    .sw3_valid(1'b0), .sw3_ready(), .sw3_data('0), .sw3_dest_id('0),
    .hp_valid(hp_link_tx_valid), .hp_ready(hp_link_tx_ready), .hp_data(hp_link_tx_data), .hp_dest_id(hp_link_tx_dest),

    .m_sw0_valid(center_sw_valid), .m_sw0_ready(center_sw_ready), .m_sw0_data(center_sw_data), .m_sw0_dest_id(center_sw_dest),
    .m_sw1_valid(), .m_sw1_ready(1'b1), .m_sw1_data(), .m_sw1_dest_id(),
    .m_sw2_valid(), .m_sw2_ready(1'b1), .m_sw2_data(), .m_sw2_dest_id(),
    .m_sw3_valid(), .m_sw3_ready(1'b1), .m_sw3_data(), .m_sw3_dest_id(),
    .m_hp_valid(hp_link_rx_valid), .m_hp_ready(hp_link_rx_ready), .m_hp_data(hp_link_rx_data), .m_hp_dest_id(hp_link_rx_dest)
  );

  assign ep0_rx_ready = 1'b1;
  assign ep1_rx_ready = 1'b1;
  assign hp_rx_ready  = 1'b1;

  task automatic send_ep0(input logic [NODE_ID_WIDTH-1:0] dest, input logic [31:0] data);
    begin
      ep0_tx_dest   <= dest;
      ep0_tx_data   <= data;
      ep0_tx_opcode <= OPC_DATA;
      ep0_tx_prio   <= 2'd0;
      ep0_tx_eop    <= 1'b1;
      ep0_tx_debug  <= 1'b0;
      ep0_tx_valid  <= 1'b1;
      while (!ep0_tx_ready) @(posedge clk);
      @(posedge clk);
      ep0_tx_valid  <= 1'b0;
    end
  endtask

  task automatic send_ep1(input logic [NODE_ID_WIDTH-1:0] dest, input logic [31:0] data);
    begin
      ep1_tx_dest   <= dest;
      ep1_tx_data   <= data;
      ep1_tx_opcode <= OPC_DATA;
      ep1_tx_prio   <= 2'd0;
      ep1_tx_eop    <= 1'b1;
      ep1_tx_debug  <= 1'b0;
      ep1_tx_valid  <= 1'b1;
      while (!ep1_tx_ready) @(posedge clk);
      @(posedge clk);
      ep1_tx_valid  <= 1'b0;
    end
  endtask

  task automatic send_hp(input logic [NODE_ID_WIDTH-1:0] dest, input logic [31:0] data);
    begin
      hp_tx_dest   <= dest;
      hp_tx_data   <= data;
      hp_tx_opcode <= OPC_DATA;
      hp_tx_prio   <= 2'd0;
      hp_tx_eop    <= 1'b1;
      hp_tx_debug  <= 1'b0;
      hp_tx_valid  <= 1'b1;
      while (!hp_tx_ready) @(posedge clk);
      @(posedge clk);
      hp_tx_valid  <= 1'b0;
    end
  endtask

  task automatic wait_for_valid(ref logic sig, input string name, input int timeout_cycles = 200);
    int tcnt;
    begin
      tcnt = 0;
      while (!sig && (tcnt < timeout_cycles)) begin
        @(posedge clk);
        tcnt++;
      end
      if (!sig) $fatal("[TB] timeout waiting for %s", name);
    end
  endtask

  initial begin
    ep0_tx_valid = 1'b0; ep1_tx_valid = 1'b0; hp_tx_valid = 1'b0;
    ep0_tx_data = 32'h0; ep1_tx_data = 32'h0; hp_tx_data = 32'h0;
    ep0_tx_dest = '0; ep1_tx_dest = '0; hp_tx_dest = '0;

    wait (rst_n);
    repeat (5) @(posedge clk);

    send_ep0(16'h1110, 32'hA0A0_0001);
    wait_for_valid(ep1_rx_valid, "ep1_rx_valid");
    if (ep1_rx_data !== 32'hA0A0_0001) $fatal("[TB] ep1 unicast data mismatch");

    send_ep1(16'h0000, 32'hB0B0_0002);
    wait_for_valid(hp_rx_valid, "hp_rx_valid");
    if (hp_rx_data !== 32'hB0B0_0002) $fatal("[TB] hp receive mismatch");

    send_hp(16'h11F0, 32'hC0C0_0003);
    begin
      int tcnt;
      bit got_ep0;
      bit got_ep1;
      logic [31:0] ep0_bcast_data;
      logic [31:0] ep1_bcast_data;
      tcnt = 0;
      got_ep0 = 1'b0;
      got_ep1 = 1'b0;
      while (!(got_ep0 && got_ep1) && (tcnt < 200)) begin
        @(posedge clk);
        if (ep0_rx_valid && !got_ep0) begin
          got_ep0 = 1'b1;
          ep0_bcast_data = ep0_rx_data;
        end
        if (ep1_rx_valid && !got_ep1) begin
          got_ep1 = 1'b1;
          ep1_bcast_data = ep1_rx_data;
        end
        tcnt++;
      end
      if (!(got_ep0 && got_ep1)) $fatal("[TB] timeout waiting for ep0/ep1 broadcast");
      if (ep0_bcast_data !== 32'hC0C0_0003) $fatal("[TB] ep0 broadcast mismatch");
      if (ep1_bcast_data !== 32'hC0C0_0003) $fatal("[TB] ep1 broadcast mismatch");
    end

    send_hp(16'hFFF0, 32'hD0D0_0004);
    begin
      int tcnt;
      bit got_ep0;
      bit got_ep1;
      bit got_hp;
      logic [31:0] ep0_g_data;
      logic [31:0] ep1_g_data;
      logic [31:0] hp_g_data;
      tcnt = 0;
      got_ep0 = 1'b0;
      got_ep1 = 1'b0;
      got_hp  = 1'b0;
      while (!(got_ep0 && got_ep1 && got_hp) && (tcnt < 200)) begin
        @(posedge clk);
        if (ep0_rx_valid && !got_ep0) begin
          got_ep0 = 1'b1;
          ep0_g_data = ep0_rx_data;
        end
        if (ep1_rx_valid && !got_ep1) begin
          got_ep1 = 1'b1;
          ep1_g_data = ep1_rx_data;
        end
        if (hp_rx_valid && !got_hp) begin
          got_hp = 1'b1;
          hp_g_data = hp_rx_data;
        end
        tcnt++;
      end
      if (!(got_ep0 && got_ep1 && got_hp)) $fatal("[TB] timeout waiting for global broadcast");
      if (ep0_g_data !== 32'hD0D0_0004) $fatal("[TB] ep0 global mismatch");
      if (ep1_g_data !== 32'hD0D0_0004) $fatal("[TB] ep1 global mismatch");
      if (hp_g_data  !== 32'hD0D0_0004) $fatal("[TB] hp global mismatch");
    end

    $display("[TB] AXI‑MailboxFabric stream tests complete");
    $finish;
  end

endmodule
