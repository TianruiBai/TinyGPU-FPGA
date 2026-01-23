`timescale 1ns/1ps
// AXI‑MailboxFabric Endpoint (Leaf): stream-based dual-role endpoint
module mailbox_endpoint_stream #(
  parameter logic [mailbox_pkg::NODE_ID_WIDTH-1:0] SRC_ID = 16'h0000,
  parameter int TX_DEPTH = 4,
  parameter int RX_DEPTH = 4,
  parameter int STALL_MAX = 256
) (
  input  logic clk,
  input  logic rst_n,

  // Core TX (egress)
  input  logic                         tx_valid,
  output logic                         tx_ready,
  input  logic [mailbox_pkg::DATA_WIDTH-1:0] tx_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] tx_dest_id,
  input  logic [3:0]                   tx_opcode,
  input  logic [1:0]                   tx_prio,
  input  logic                         tx_eop,
  input  logic                         tx_debug,

  // Core RX (ingress)
  output logic                         rx_valid,
  input  logic                         rx_ready,
  output logic [mailbox_pkg::DATA_WIDTH-1:0] rx_data,
  output mailbox_pkg::mailbox_header_t rx_hdr,
  output logic                         rx_irq,
  output logic                         rx_error,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] rx_dest_id,

  // Fabric TX (to switch/center)
  output logic                         link_tx_valid,
  input  logic                         link_tx_ready,
  output mailbox_pkg::mailbox_flit_t   link_tx_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] link_tx_dest_id,

  // Fabric RX (from switch/center)
  input  logic                         link_rx_valid,
  output logic                         link_rx_ready,
  input  mailbox_pkg::mailbox_flit_t   link_rx_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] link_rx_dest_id
);

  import mailbox_pkg::*;

  typedef struct packed {
    logic [NODE_ID_WIDTH-1:0] dest_id;
    mailbox_flit_t            flit;
  } tx_entry_t;

  typedef struct packed {
    logic [NODE_ID_WIDTH-1:0] dest_id;
    mailbox_flit_t            flit;
  } rx_entry_t;

  localparam int TXW = $bits(tx_entry_t);
  localparam int RXW = $bits(rx_entry_t);

  // TX FIFO
  logic tx_w_en, tx_w_full, tx_r_en, tx_r_empty;
  logic [TXW-1:0] tx_w_data, tx_r_data;
  tx_entry_t tx_head;

  mailbox_flit_t tx_flit;
  always_comb begin
    tx_flit.hdr.src_id = SRC_ID;
    tx_flit.hdr.opcode = tx_opcode;
    tx_flit.hdr.prio   = tx_prio;
    tx_flit.hdr.eop    = tx_eop;
    tx_flit.hdr.debug  = tx_debug;
    tx_flit.payload    = tx_data;
  end

  assign tx_w_en   = tx_valid && tx_ready;
  assign tx_w_data = tx_entry_t'{dest_id: tx_dest_id, flit: tx_flit};

  mailbox_fifo #(.WIDTH(TXW), .DEPTH(TX_DEPTH)) u_tx_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(tx_w_en), .w_data(tx_w_data), .w_full(tx_w_full),
    .r_en(tx_r_en), .r_data(tx_r_data), .r_empty(tx_r_empty)
  );

  assign tx_head       = tx_entry_t'(tx_r_data);
  assign tx_ready      = !tx_w_full;
  assign link_tx_valid = !tx_r_empty;
  assign link_tx_data  = tx_head.flit;
  assign link_tx_dest_id = tx_head.dest_id;
  assign tx_r_en       = link_tx_valid && link_tx_ready;

  // RX FIFO + Must‑Sink policy
  logic rx_w_en, rx_w_full, rx_r_en, rx_r_empty;
  logic [RXW-1:0] rx_w_data, rx_r_data;
  rx_entry_t rx_head;

  localparam int STALL_W = (STALL_MAX <= 1) ? 1 : $clog2(STALL_MAX+1);
  logic [STALL_W-1:0] stall_counter;
  logic drop_accept;

  assign drop_accept  = rx_w_full && (stall_counter == STALL_MAX[STALL_W-1:0]);
  assign link_rx_ready = !rx_w_full || drop_accept;

  assign rx_w_en   = link_rx_valid && link_rx_ready && !rx_w_full;
  assign rx_w_data = rx_entry_t'{dest_id: link_rx_dest_id, flit: link_rx_data};

  mailbox_fifo #(.WIDTH(RXW), .DEPTH(RX_DEPTH)) u_rx_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(rx_w_en), .w_data(rx_w_data), .w_full(rx_w_full),
    .r_en(rx_r_en), .r_data(rx_r_data), .r_empty(rx_r_empty)
  );

  assign rx_head     = rx_entry_t'(rx_r_data);
  assign rx_valid    = !rx_r_empty;
  assign rx_data     = rx_head.flit.payload;
  assign rx_hdr      = rx_head.flit.hdr;
  assign rx_dest_id  = rx_head.dest_id;
  assign rx_irq   = rx_valid;
  assign rx_r_en  = rx_valid && rx_ready;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      stall_counter <= '0;
      rx_error      <= 1'b0;
    end else begin
      rx_error <= 1'b0;
      if (link_rx_valid && rx_w_full) begin
        if (stall_counter != STALL_MAX[STALL_W-1:0]) stall_counter <= stall_counter + 1'b1;
        if (drop_accept) rx_error <= 1'b1;
      end else begin
        stall_counter <= '0;
      end
    end
  end

endmodule
