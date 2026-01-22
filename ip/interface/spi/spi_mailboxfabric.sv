`timescale 1ns/1ps

// SPI SAT-IP with MailboxFabric interface
// - Mailbox CSR0 write triggers a SPI transfer (cmd)
// - On completion, sends response to configured destination (CSR2)

module spi_mailboxfabric #(
  parameter int TX_LEN   = 1,
  parameter int RX_LEN   = 1,
  parameter int CS_NUM   = 1,
  parameter int CS_INDEX = 0,
  parameter int SCLK_DIV = 4,
  parameter logic [7:0]  SRC_ID = 8'hF1,
  parameter logic [15:0] DEFAULT_DEST = 16'h0000
)(
  input  logic clk,
  input  logic rst_n,

  // Mailbox master (toward switch/center)
  output logic        m_awvalid,
  input  logic        m_awready,
  output logic [15:0] m_awaddr,
  output logic        m_wvalid,
  input  logic        m_wready,
  output logic [31:0] m_wdata,
  output logic [3:0]  m_wstrb,
  output mailbox_pkg::mailbox_tag_t m_tag,
  output logic        m_bready,
  input  logic        m_bvalid,

  output logic        m_arvalid,
  input  logic        m_arready,
  output logic [15:0] m_araddr,
  input  logic        m_rvalid,
  output logic        m_rready,
  input  logic [31:0] m_rdata,

  // Mailbox slave (from switch/center)
  input  logic        s_awvalid,
  output logic        s_awready,
  input  logic [15:0] s_awaddr,
  input  logic        s_wvalid,
  output logic        s_wready,
  input  logic [31:0] s_wdata,
  input  logic [3:0]  s_wstrb,
  input  mailbox_pkg::mailbox_tag_t s_tag,
  input  logic        s_bready,
  output logic        s_bvalid,

  input  logic        s_arvalid,
  output logic        s_arready,
  input  logic [15:0] s_araddr,
  output logic        s_rvalid,
  input  logic        s_rready,
  output logic [31:0] s_rdata,

  // SPI pins
  output logic SPI_SCLK,
  output logic SPI_MOSI,
  input  logic SPI_MISO,
  output logic [CS_NUM-1:0] SPI_CS
);

  import mailbox_pkg::*;

  // Endpoint RX/TX streams
  logic ep_rx_valid, ep_rx_ready;
  logic [31:0] ep_rx_data;
  mailbox_tag_t ep_rx_tag;
  logic ep_rx_irq;

  logic ep_tx_valid, ep_tx_ready;
  logic [15:0] ep_tx_dest;
  logic [31:0] ep_tx_data;
  logic ep_tx_prio;
  logic ep_tx_eop;
  logic [3:0] ep_tx_opcode;

  // Mailbox endpoint
  mailbox_endpoint #(.SRC_ID(SRC_ID), .TX_DEPTH(4), .RX_DEPTH(4)) u_mbox_ep (
    .clk(clk), .rst_n(rst_n),

    .tx_valid(ep_tx_valid), .tx_ready(ep_tx_ready), .tx_dest(ep_tx_dest), .tx_data(ep_tx_data), .tx_prio(ep_tx_prio), .tx_eop(ep_tx_eop), .tx_opcode(ep_tx_opcode),
    .rx_valid(ep_rx_valid), .rx_ready(ep_rx_ready), .rx_data(ep_rx_data), .rx_tag(ep_rx_tag), .rx_irq(ep_rx_irq),

    .rd_valid(1'b0), .rd_ready(), .rd_dest(), .rd_prio(1'b0), .rd_opcode(4'h0), .rd_resp_valid(), .rd_resp_ready(1'b0), .rd_resp_data(), .rd_resp_tag(),

    .m_awvalid(m_awvalid), .m_awready(m_awready), .m_awaddr(m_awaddr), .m_wvalid(m_wvalid), .m_wready(m_wready), .m_wdata(m_wdata), .m_wstrb(m_wstrb), .m_tag(m_tag), .m_bready(m_bready), .m_bvalid(m_bvalid),
    .m_arvalid(m_arvalid), .m_arready(m_arready), .m_araddr(m_araddr), .m_rvalid(m_rvalid), .m_rready(m_rready), .m_rdata(m_rdata),

    .s_awvalid(s_awvalid), .s_awready(s_awready), .s_awaddr(s_awaddr), .s_wvalid(s_wvalid), .s_wready(s_wready), .s_wdata(s_wdata), .s_wstrb(s_wstrb), .s_tag(s_tag), .s_bready(s_bready), .s_bvalid(s_bvalid),
    .s_arvalid(s_arvalid), .s_arready(s_arready), .s_araddr(s_araddr), .s_rvalid(s_rvalid), .s_rready(s_rready), .s_rdata(s_rdata)
  );

  // SPI core
  logic [TX_LEN*8-1:0] cmd_reg;
  logic [RX_LEN*8-1:0] resp_reg;
  logic trmt_pulse;
  logic rx_rdy;
  logic clr_rdy;

  spi_sat #(
    .TX_LEN(TX_LEN),
    .RX_LEN(RX_LEN),
    .CS_NUM(CS_NUM),
    .CS_INDEX(CS_INDEX),
    .SCLK_DIV(SCLK_DIV)
  ) u_spi_sat (
    .clk(clk), .rst_n(rst_n),
    .cmd(cmd_reg), .resp(resp_reg), .trmt(trmt_pulse), .rx_rdy(rx_rdy), .clr_rdy(clr_rdy),
    .SPI_SCLK(SPI_SCLK), .SPI_MOSI(SPI_MOSI), .SPI_MISO(SPI_MISO), .SPI_CS(SPI_CS)
  );

  // Default destination register (CSR2)
  logic [15:0] cfg_dest;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) cfg_dest <= DEFAULT_DEST;
    else if (s_awvalid && s_wvalid && (s_awaddr[3:0] == 4'd2)) cfg_dest <= s_wdata[15:0];
  end

  // Accept mailbox writes to CSR0 as SPI commands
  logic busy;
  assign ep_rx_ready = !busy;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      cmd_reg <= '0;
      trmt_pulse <= 1'b0;
      busy <= 1'b0;
    end else begin
      trmt_pulse <= 1'b0;
      if (ep_rx_valid && ep_rx_ready) begin
        cmd_reg <= ep_rx_data[TX_LEN*8-1:0];
        trmt_pulse <= 1'b1;
        busy <= 1'b1;
      end
      if (rx_rdy) busy <= 1'b0;
    end
  end

  // Clear RX ready after capture
  assign clr_rdy = rx_rdy;

  // Mailbox TX response state (supports up to 64-bit resp, 2 beats)
  localparam int RESP_BITS = RX_LEN * 8;
  localparam int RESP_BEATS = (RESP_BITS > 32) ? 2 : 1;

  logic [63:0] resp_buf;
  logic [1:0]  send_state; // 0 idle, 1 send low, 2 send high

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      resp_buf   <= 64'h0;
      send_state <= 2'd0;
    end else begin
      if (rx_rdy) begin
        resp_buf <= {{(64-RESP_BITS){1'b0}}, resp_reg};
        send_state <= (RESP_BEATS == 2) ? 2'd1 : 2'd1;
      end else if (send_state != 0 && ep_tx_valid && ep_tx_ready) begin
        if (send_state == 2'd1) send_state <= (RESP_BEATS == 2) ? 2'd2 : 2'd0;
        else if (send_state == 2'd2) send_state <= 2'd0;
      end
    end
  end

  always_comb begin
    ep_tx_valid  = (send_state != 0);
    ep_tx_dest   = cfg_dest;
    ep_tx_prio   = 1'b0;
    ep_tx_opcode = OPC_DATA;
    if (send_state == 2'd2) begin
      ep_tx_data = resp_buf[63:32];
      ep_tx_eop  = 1'b1;
    end else begin
      ep_tx_data = resp_buf[31:0];
      ep_tx_eop  = (RESP_BEATS == 1);
    end
  end

endmodule
