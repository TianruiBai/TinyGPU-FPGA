// Mailbox Endpoint (Leaf): TX FIFO + RX FIFO around Wishbone-lite mailbox link
module mailbox_endpoint #(
  parameter logic [7:0] SRC_ID = 8'h00,
  parameter int TX_DEPTH = 4,
  parameter int RX_DEPTH = 4
) (
  input  logic clk,
  input  logic rst_n,

  // Core TX sideband interface
  input  logic        tx_valid,
  output logic        tx_ready,
  input  logic [15:0] tx_dest,
  input  logic [31:0] tx_data,
  input  logic        tx_prio,
  input  logic        tx_eop,
  input  logic [3:0]  tx_opcode,

  // Core RX interface
  output logic        rx_valid,
  input  logic        rx_ready,
  output logic [31:0] rx_data,
  output mailbox_pkg::mailbox_tag_t rx_tag,
  output logic        rx_irq,

  // Wishbone master out toward switch (TX direction)
  output logic        m_wb_cyc,
  output logic        m_wb_stb,
  output logic        m_wb_we,
  output logic [15:0] m_wb_adr,
  output logic [31:0] m_wb_dat,
  output logic [3:0]  m_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_wb_tag,
  input  logic        m_wb_ack,

  // Wishbone slave in from switch (RX direction)
  input  logic        s_wb_cyc,
  input  logic        s_wb_stb,
  input  logic        s_wb_we,
  input  logic [15:0] s_wb_adr,
  input  logic [31:0] s_wb_dat,
  input  logic [3:0]  s_wb_sel,
  input  mailbox_pkg::mailbox_tag_t s_wb_tag,
  output logic        s_wb_ack
);

  import mailbox_pkg::*;

  typedef struct packed {
    logic [15:0] adr;
    logic [31:0] dat;
    logic [3:0]  sel;
    mailbox_tag_t tag;
  } flit_t;
  localparam int FLIT_W = $bits(flit_t);

  // TX FIFO
  logic tx_w_en, tx_w_full, tx_r_en, tx_r_empty;
  logic [FLIT_W-1:0] tx_w_data, tx_r_data;
  flit_t tx_head;

  // RX FIFO
  logic rx_w_en, rx_w_full;
  logic [FLIT_W-1:0] rx_w_data;
  logic rx_r_en, rx_r_empty;
  logic [FLIT_W-1:0] rx_r_data;
  flit_t rx_head;
  flit_t rx_w_flit;

  // Build TX flit from core input
  mailbox_tag_t tx_tag;
  mailbox_tag_t tx_tag_np;
  flit_t tx_w_flit;
  always_comb begin
    tx_tag = '{src_id:SRC_ID, eop:tx_eop, prio:tx_prio, opcode:tx_opcode, hops:4'd0, parity:1'b0};
    tx_tag_np = tx_tag;
    tx_tag_np.parity = 1'b0;
    tx_tag.parity = compute_parity(tx_data, tx_tag_np);
    tx_w_en   = tx_valid && tx_ready;
    tx_w_flit = '{adr:tx_dest, dat:tx_data, sel:4'hF, tag:tx_tag};
    tx_w_data = tx_w_flit;
  end

  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(TX_DEPTH)) u_tx_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(tx_w_en), .w_data(tx_w_data), .w_full(tx_w_full),
    .r_en(tx_r_en), .r_data(tx_r_data), .r_empty(tx_r_empty)
  );

  assign tx_head = flit_t'(tx_r_data);
  assign tx_ready = !tx_w_full;

  assign m_wb_cyc = !tx_r_empty;
  assign m_wb_stb = !tx_r_empty;
  assign m_wb_we  = 1'b1;
  assign m_wb_adr = tx_head.adr;
  assign m_wb_dat = tx_head.dat;
  assign m_wb_sel = tx_head.sel;
  assign m_wb_tag = tx_head.tag;
  assign tx_r_en  = !tx_r_empty && m_wb_ack;

  // RX path: accept when FIFO not full
  assign rx_w_en   = s_wb_cyc && s_wb_stb && !rx_w_full;
  assign rx_w_flit = '{adr:s_wb_adr, dat:s_wb_dat, sel:s_wb_sel, tag:s_wb_tag};
  assign rx_w_data = rx_w_flit;
  assign s_wb_ack  = rx_w_en;

  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(RX_DEPTH)) u_rx_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(rx_w_en), .w_data(rx_w_data), .w_full(rx_w_full),
    .r_en(rx_r_en), .r_data(rx_r_data), .r_empty(rx_r_empty)
  );

  assign rx_head = flit_t'(rx_r_data);
  assign rx_valid = !rx_r_empty;
  assign rx_data  = rx_head.dat;
  assign rx_tag   = rx_head.tag;
  assign rx_irq   = !rx_r_empty;

  assign rx_r_en = rx_valid && rx_ready;

endmodule
