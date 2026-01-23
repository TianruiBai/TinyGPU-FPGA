`timescale 1ns/1ps

// SPI SAT-IP with AXI‑MailboxFabric stream interface
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

  // MailboxFabric stream link (to/from switch/center)
  output logic                        mb_tx_valid,
  input  logic                        mb_tx_ready,
  output mailbox_pkg::mailbox_flit_t  mb_tx_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mb_tx_dest_id,

  input  logic                        mb_rx_valid,
  output logic                        mb_rx_ready,
  input  mailbox_pkg::mailbox_flit_t  mb_rx_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mb_rx_dest_id,

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
  mailbox_header_t ep_rx_hdr;
  logic ep_rx_irq;
  logic ep_rx_err;
  logic [NODE_ID_WIDTH-1:0] ep_rx_dest_id;

  logic ep_tx_valid, ep_tx_ready;
  logic [15:0] ep_tx_dest;
  logic [31:0] ep_tx_data;
  logic ep_tx_prio;
  logic ep_tx_eop;
  logic [3:0] ep_tx_opcode;

  mailbox_endpoint_stream #(
    .SRC_ID({8'h00, SRC_ID}),
    .TX_DEPTH(4),
    .RX_DEPTH(4)
  ) u_mbox_ep (
    .clk(clk), .rst_n(rst_n),

    .tx_valid(ep_tx_valid), .tx_ready(ep_tx_ready), .tx_data(ep_tx_data), .tx_dest_id(ep_tx_dest),
    .tx_opcode(ep_tx_opcode), .tx_prio(ep_tx_prio), .tx_eop(ep_tx_eop), .tx_debug(1'b0),
    .rx_valid(ep_rx_valid), .rx_ready(ep_rx_ready), .rx_data(ep_rx_data), .rx_hdr(ep_rx_hdr), .rx_irq(ep_rx_irq), .rx_error(ep_rx_err), .rx_dest_id(ep_rx_dest_id),

    .link_tx_valid(mb_tx_valid), .link_tx_ready(mb_tx_ready), .link_tx_data(mb_tx_data), .link_tx_dest_id(mb_tx_dest_id),
    .link_rx_valid(mb_rx_valid), .link_rx_ready(mb_rx_ready), .link_rx_data(mb_rx_data), .link_rx_dest_id(mb_rx_dest_id)
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
    .tx_bits(tx_bits_reg), .rx_bits(rx_bits_reg),
    .cmd(cmd_reg), .resp(resp_reg), .trmt(trmt_pulse), .rx_rdy(rx_rdy), .clr_rdy(clr_rdy),
    .SPI_SCLK(SPI_SCLK), .SPI_MOSI(SPI_MOSI), .SPI_MISO(SPI_MISO), .SPI_CS(SPI_CS)
  );

  // Local CSR storage
  logic [63:0] tx_buf;
  logic [63:0] rx_buf;
  logic [7:0]  tx_bits_reg;
  logic [7:0]  rx_bits_reg;
  logic [15:0] cfg_dest;
  logic        busy;
  logic        done_flag;

  // Stream CSR handling
  wire wr_fire = ep_rx_valid && ep_rx_ready;
  wire [3:0] rx_csr_idx = ep_rx_dest_id[3:0];
  assign ep_rx_ready = 1'b1;

  // Accept mailbox writes to CSR0/1/4/5 as SPI control
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      tx_buf     <= 64'h0;
      rx_buf     <= 64'h0;
      tx_bits_reg <= TX_LEN*8;
      rx_bits_reg <= RX_LEN*8;
      cfg_dest   <= DEFAULT_DEST;
      trmt_pulse <= 1'b0;
      busy       <= 1'b0;
      done_flag  <= 1'b0;
      // no AXI-Lite response in stream mode
    end else begin
      trmt_pulse <= 1'b0;
      if (wr_fire) begin
        unique case (rx_csr_idx)
          4'd0: tx_buf[31:0]  <= ep_rx_data;
          4'd1: tx_buf[63:32] <= ep_rx_data;
          4'd4: begin
            tx_bits_reg <= ep_rx_data[7:0];
            rx_bits_reg <= ep_rx_data[15:8];
            if (ep_rx_data[18]) done_flag <= 1'b0;
            if (ep_rx_data[16] && !busy) begin
              trmt_pulse <= 1'b1;
              busy <= 1'b1;
              done_flag <= 1'b0;
              cmd_reg <= tx_buf[TX_LEN*8-1:0];
            end
          end
          4'd5: cfg_dest <= ep_rx_data[15:0];
          default: ;
        endcase
      end

      if (rx_rdy) begin
        rx_buf <= {{(64-RESP_BITS){1'b0}}, resp_reg};
        done_flag <= 1'b1;
        busy <= 1'b0;
      end
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
