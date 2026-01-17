`timescale 1ns/1ps
// Mailbox Endpoint (Leaf): full-duplex AXI4-Lite with pop-on-read + CSR controls
module mailbox_endpoint #(
  parameter logic [7:0] SRC_ID   = 8'h00,
  parameter int TX_DEPTH         = 4,
  parameter int RX_DEPTH         = 4
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

  // Core RX streaming interface (pop by rx_ready)
  output logic        rx_valid,
  input  logic        rx_ready,
  output logic [31:0] rx_data,
  output mailbox_pkg::mailbox_tag_t rx_tag,
  output logic        rx_irq,

  // Core mailbox read request interface (drives AR out)
  input  logic        rd_valid,
  output logic        rd_ready,
  input  logic [15:0] rd_dest,
  input  logic        rd_prio,
  input  logic [3:0]  rd_opcode,
  output logic        rd_resp_valid,
  input  logic        rd_resp_ready,
  output logic [31:0] rd_resp_data,
  output mailbox_pkg::mailbox_tag_t rd_resp_tag,

  // AXI4-Lite master out toward switch (TX writes + read requests)
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

  // AXI4-Lite slave in from switch (RX writes + read pops)
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
  output logic [31:0] s_rdata
);

  import mailbox_pkg::*;

  typedef struct packed {
    logic [15:0] adr;
    logic [31:0] dat;
    logic [3:0]  strb;
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

  // Control/status CSRs (indices 1-15)
  logic mute_tx, mute_rx, flood_mute, mute_bcast;
  logic clr_tx_pulse, clr_rx_pulse;
  logic [31:0] csr_reg [15:1];

  // ----------------------------
  // TX build + enqueue
  // ----------------------------
  mailbox_tag_t tx_tag;
  mailbox_tag_t tx_tag_np;
  flit_t tx_w_flit;
  always_comb begin
    tx_tag = '{src_id:SRC_ID, eop:tx_eop, prio:tx_prio, opcode:tx_opcode, hops:4'd0, parity:1'b0};
    tx_tag_np = tx_tag;
    tx_tag_np.parity = 1'b0;
    tx_tag.parity = compute_parity(tx_data, tx_tag_np);

    tx_w_en   = tx_valid && tx_ready;
    tx_w_flit = '{adr:tx_dest, dat:tx_data, strb:4'hF, tag:tx_tag};
    tx_w_data = tx_w_flit;
  end

  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(TX_DEPTH)) u_tx_fifo (
     .clk(clk), .rst_n(rst_n), .clr(clr_tx_pulse),
    .w_en(tx_w_en && !mute_tx), .w_data(tx_w_data), .w_full(tx_w_full),
    .r_en(tx_r_en), .r_data(tx_r_data), .r_empty(tx_r_empty)
  );

  assign tx_head  = flit_t'(tx_r_data);
  assign tx_ready = !tx_w_full && !mute_tx;

  assign m_awvalid = !tx_r_empty;
  assign m_wvalid  = !tx_r_empty;
  assign m_awaddr  = tx_head.adr;
  assign m_wdata   = tx_head.dat;
  assign m_wstrb   = tx_head.strb;
  assign m_tag     = tx_head.tag;
  assign m_bready  = 1'b1;
  assign tx_r_en   = !tx_r_empty && m_awready && m_wready;

  // ----------------------------
  // Outbound AR (core read requests)
  // ----------------------------
  logic        ar_pending;
  logic [15:0] ar_addr_q;
  mailbox_tag_t ar_tag_q;

  assign rd_ready = !ar_pending;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      ar_pending <= 1'b0;
      ar_addr_q  <= 16'h0;
      ar_tag_q   <= '0;
    end else begin
      if (rd_valid && rd_ready) begin
        ar_pending <= 1'b1;
        ar_addr_q  <= rd_dest;
        ar_tag_q   <= '{src_id:SRC_ID, eop:1'b1, prio:rd_prio, opcode:rd_opcode, hops:4'd0, parity:1'b0};
      end else if (m_arvalid && m_arready) begin
        ar_pending <= 1'b0; // clear once address accepted; waiting on rvalid
      end
    end
  end

  assign m_arvalid = ar_pending;
  assign m_araddr  = ar_addr_q;
  assign m_rready  = rd_resp_ready;

  // Return path: single outstanding
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      rd_resp_valid <= 1'b0;
      rd_resp_data  <= 32'h0;
      rd_resp_tag   <= '0;
    end else begin
      if (rd_resp_valid && rd_resp_ready) rd_resp_valid <= 1'b0;
      if (m_rvalid) begin
        rd_resp_valid <= 1'b1;
        rd_resp_data  <= m_rdata;
        rd_resp_tag   <= ar_tag_q;
      end
    end
  end

  // ----------------------------
  // RX path (slave AW/W) + CSR decode
  // ----------------------------
  logic s_bvalid_r;
  logic can_accept_rx;
  logic [3:0] rx_csr_idx;
  logic is_data_push;

  assign rx_csr_idx   = s_awaddr[3:0];
  assign is_data_push = (rx_csr_idx == 4'd0);

  assign can_accept_rx = !rx_w_full && !s_bvalid_r && !mute_rx;
  assign rx_w_en   = s_awvalid && s_wvalid && can_accept_rx && is_data_push;
  assign rx_w_flit = '{adr:s_awaddr, dat:s_wdata, strb:s_wstrb, tag:s_tag};
  assign rx_w_data = rx_w_flit;

  // Control register write (idx!=0)
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      s_bvalid_r   <= 1'b0;
      mute_tx      <= 1'b0;
      mute_rx      <= 1'b0;
      flood_mute   <= 1'b0;
      mute_bcast   <= 1'b0;
      clr_tx_pulse <= 1'b0;
      clr_rx_pulse <= 1'b0;
      for (int i = 1; i < 16; i++) csr_reg[i] <= 32'h0;
    end else begin
      clr_tx_pulse <= 1'b0;
      clr_rx_pulse <= 1'b0;

      if (s_bvalid_r && s_bready) s_bvalid_r <= 1'b0;

      if (s_awvalid && s_wvalid && can_accept_rx) begin
        s_bvalid_r <= 1'b1;
        if (!is_data_push) begin
          csr_reg[rx_csr_idx] <= s_wdata;
          if (rx_csr_idx == 4'd1) begin
            mute_tx      <= s_wdata[2];
            mute_rx      <= s_wdata[3];
            flood_mute   <= s_wdata[4];
            mute_bcast   <= s_wdata[5];
            if (s_wdata[0]) clr_rx_pulse <= 1'b1;
            if (s_wdata[1]) clr_tx_pulse <= 1'b1;
          end
        end
      end
    end
  end

  assign s_awready = can_accept_rx;
  assign s_wready  = can_accept_rx;
  assign s_bvalid  = s_bvalid_r;

  // RX FIFO
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(RX_DEPTH)) u_rx_fifo (
     .clk(clk), .rst_n(rst_n), .clr(clr_rx_pulse),
    .w_en(rx_w_en), .w_data(rx_w_data), .w_full(rx_w_full),
    .r_en(rx_r_en), .r_data(rx_r_data), .r_empty(rx_r_empty)
  );

  assign rx_head = flit_t'(rx_r_data);
  assign rx_valid = !rx_r_empty;
  assign rx_data  = rx_head.dat;
  assign rx_tag   = rx_head.tag;
  assign rx_irq   = !rx_r_empty;

  // ----------------------------
  // Slave AR/R (pop on read, DEADBEEF on empty)
  // ----------------------------
  logic ar_pending_s;
  logic [3:0] ar_csr_idx;
  logic pop_pending;

  assign s_arready = !ar_pending_s;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      ar_pending_s <= 1'b0;
      ar_csr_idx   <= 4'h0;
      s_rvalid     <= 1'b0;
      s_rdata      <= 32'h0;
      pop_pending  <= 1'b0;
    end else begin
      if (s_rvalid && s_rready) begin
        s_rvalid    <= 1'b0;
        pop_pending <= 1'b0;
      end

      if (s_arvalid && s_arready) begin
        ar_pending_s <= 1'b1;
        ar_csr_idx   <= s_araddr[3:0];
      end

      if (ar_pending_s && !s_rvalid) begin
        ar_pending_s <= 1'b0;
        pop_pending  <= (ar_csr_idx == 4'd0) && !rx_r_empty;
        if (ar_csr_idx == 4'd0) begin
          s_rdata  <= rx_r_empty ? 32'hDEADBEEF : rx_head.dat;
        end else begin
          // Live status for idx1 plus stored regs for others
          if (ar_csr_idx == 4'd1) s_rdata <= {26'h0, mute_bcast, flood_mute, mute_rx, mute_tx, rx_r_empty, tx_r_empty};
          else s_rdata <= csr_reg[ar_csr_idx];
        end
        s_rvalid <= 1'b1;
      end
    end
  end

  // Pop when AR read of idx0 completes or core stream pop
  assign rx_r_en = (rx_valid && rx_ready) || (s_rvalid && s_rready && pop_pending);


endmodule
