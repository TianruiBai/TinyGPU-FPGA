`timescale 1ns/1ps

// UART peripheral as a MailboxFabric Leaf (Endpoint)
// - Instantiates `mailbox_endpoint` for the actual AXI4-Lite leaf behavior
// - Converts inbound mailbox writes (CSR idx 0) into UART TX bytes
// - Converts UART RX bytes into outbound mailbox TX messages (to a configured dest)

module uart_mailboxfabric #(
  parameter logic [7:0] SRC_ID = 8'hF0,
  parameter logic [15:0] DEFAULT_DEST = 16'h0000, // default mailbox destination for RX->mailbox TX
  parameter int BAUD_DIV = 217,
  parameter int TX_FIFO_DEPTH = 8,
  parameter int RX_FIFO_DEPTH = 8
)(
  input  logic clk,
  input  logic rst_n,

  // Top-level mailbox master (toward switch/center)
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

  // Top-level mailbox slave (from switch/center)
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

  // UART pins
  output logic UART_TX,
  input  logic  UART_RX,
  input  logic  UART_CTS, // optional input flow control
  output logic UART_RTS  // optional output flow control
);

  import mailbox_pkg::*;

  // ---------------------------
  // Instantiate mailbox endpoint
  // ---------------------------
  mailbox_endpoint #(.SRC_ID(SRC_ID), .TX_DEPTH(TX_FIFO_DEPTH), .RX_DEPTH(RX_FIFO_DEPTH)) u_mbox_ep (
    .clk(clk), .rst_n(rst_n),

    // Core TX side (from this UART -> fabric)
    .tx_valid(ep_tx_valid), .tx_ready(ep_tx_ready), .tx_dest(ep_tx_dest), .tx_data(ep_tx_data), .tx_prio(ep_tx_prio), .tx_eop(ep_tx_eop), .tx_opcode(ep_tx_opcode),

    // Core RX streaming (incoming from fabric -> UART transmit)
    .rx_valid(ep_rx_valid), .rx_ready(ep_rx_ready), .rx_data(ep_rx_data), .rx_tag(ep_rx_tag), .rx_irq(ep_rx_irq),

    // Core mailbox read request (not used by UART core but left available)
    .rd_valid(1'b0), .rd_ready(), .rd_dest(), .rd_prio(1'b0), .rd_opcode(4'h0), .rd_resp_valid(), .rd_resp_ready(1'b0), .rd_resp_data(), .rd_resp_tag(),

    // AXI4-Lite master ports (to switch/center)
    .m_awvalid(m_awvalid), .m_awready(m_awready), .m_awaddr(m_awaddr), .m_wvalid(m_wvalid), .m_wready(m_wready), .m_wdata(m_wdata), .m_wstrb(m_wstrb), .m_tag(m_tag), .m_bready(m_bready), .m_bvalid(m_bvalid),
    .m_arvalid(m_arvalid), .m_arready(m_arready), .m_araddr(m_araddr), .m_rvalid(m_rvalid), .m_rready(m_rready), .m_rdata(m_rdata),

    // AXI4-Lite slave ports (from switch/center)
    .s_awvalid(s_awvalid), .s_awready(s_awready), .s_awaddr(s_awaddr), .s_wvalid(s_wvalid), .s_wready(s_wready), .s_wdata(s_wdata), .s_wstrb(s_wstrb), .s_tag(s_tag), .s_bready(s_bready), .s_bvalid(s_bvalid),
    .s_arvalid(s_arvalid), .s_arready(s_arready), .s_araddr(s_araddr), .s_rvalid(s_rvalid), .s_rready(s_rready), .s_rdata(s_rdata)
  );

  // ---------------------------
  // UART transmit path (fabric -> UART TX)
  // - When mailbox writes to CSR idx 0 arrive, they appear on ep_rx_valid/ep_rx_data
  // - We consume ep_rx_data (32-bit) as bytes to be transmitted on UART serial TX
  // ---------------------------

  localparam int TX_PTR_W = (TX_FIFO_DEPTH <= 1) ? 1 : $clog2(TX_FIFO_DEPTH);
  localparam int TX_CNT_W = $clog2(TX_FIFO_DEPTH+1);
  logic [7:0] tx_fifo [0:TX_FIFO_DEPTH-1];
  logic [TX_PTR_W-1:0] tx_wptr, tx_rptr;
  logic [TX_CNT_W-1:0] tx_count;
  logic tx_full, tx_empty;
  logic tx_w_en, tx_r_en;

  // Simple write pointer driven by endpoint RX stream
  assign ep_rx_ready = !tx_full;

  assign tx_w_en = ep_rx_valid && ep_rx_ready;
  assign tx_r_en = tx_finished && !tx_empty;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      tx_wptr  <= '0;
      tx_rptr  <= '0;
      tx_count <= '0;
    end else begin
      if (tx_w_en) begin
        tx_fifo[tx_wptr] <= ep_rx_data[7:0];
        tx_wptr <= tx_wptr + 1'b1;
      end
      if (tx_r_en) begin
        tx_rptr <= tx_rptr + 1'b1;
      end

      case ({tx_w_en, tx_r_en})
        2'b10: tx_count <= tx_count + 1'b1;
        2'b01: tx_count <= tx_count - 1'b1;
        default: tx_count <= tx_count;
      endcase
    end
  end

  assign tx_full  = (tx_count == TX_FIFO_DEPTH[TX_CNT_W-1:0]);
  assign tx_empty = (tx_count == {TX_CNT_W{1'b0}});

  assign UART_RTS = !tx_full; // tell peer we can accept data when not full

  // UART transmitter (baud generator + shift reg)
  localparam int BAUD_CNT_W = (BAUD_DIV <= 1) ? 1 : $clog2(BAUD_DIV);
  logic [BAUD_CNT_W-1:0] tx_baud_cnt;
  logic tx_busy;
  logic [9:0] tx_shift; // start, 8 data, stop
  logic [3:0] tx_bits;  // bit count 0..9

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      tx_baud_cnt <= '0;
      tx_busy <= 1'b0;
      tx_shift <= 10'h3FF; // idle high
      tx_bits <= 4'h0;
    end else begin
      if (!tx_busy && !tx_empty && !UART_CTS) begin
        // start new byte
        tx_shift <= {1'b1, tx_fifo[tx_rptr], 1'b0}; // {stop, data[7:0], start(0)} -> LSB first
        tx_bits  <= 4'd0;
        tx_busy  <= 1'b1;
        tx_baud_cnt <= '0;
      end else if (tx_busy) begin
        if (tx_baud_cnt == BAUD_DIV[BAUD_CNT_W-1:0]-1'b1) begin
          tx_baud_cnt <= '0;
          // shift one bit out
          tx_shift <= {1'b1, tx_shift[9:1]}; // shift right, fill with stop=1
          tx_bits <= tx_bits + 1'b1;
          if (tx_bits == 4'd9) begin
            tx_busy <= 1'b0;
            // consume FIFO slot after frame done (handled by tx_finished below)
          end
        end else begin
          tx_baud_cnt <= tx_baud_cnt + 1'b1;
        end
      end
    end
  end



  // Detect frame completion by watching tx_busy falling edge
  logic tx_busy_q;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) tx_busy_q <= 1'b0; else tx_busy_q <= tx_busy;
  end
  logic tx_finished = (tx_busy_q && !tx_busy);
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) tx_rptr <= '0; else if (tx_finished && !tx_empty) tx_rptr <= tx_rptr + 1'b1;
  end

  // Drive UART_TX pin: LSB of shift register
  // When idle, TX line is high
  assign UART_TX = tx_shift[0];

  // ---------------------------
  // UART receive path (serial -> mailbox TX)
  // - sample start bit, shift bits in at BAUD_DIV intervals
  // - when received byte ready, enqueue a mailbox TX message to send to DEFAULT_DEST
  // ---------------------------
  logic [BAUD_CNT_W-1:0] rx_baud_cnt;
  logic rx_sampling;
  logic [3:0] rx_bits_cnt;
  logic [9:0] rx_shift;
  logic rx_byte_valid;
  logic [7:0] rx_byte;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      rx_sampling <= 1'b0;
      rx_baud_cnt <= '0;
      rx_bits_cnt <= 4'h0;
      rx_shift <= 10'h3FF;
      rx_byte_valid <= 1'b0;
      rx_byte <= 8'h0;
    end else begin
      rx_byte_valid <= 1'b0;
      if (!rx_sampling) begin
        if (UART_RX == 1'b0) begin // start bit detected (edge low)
          rx_sampling <= 1'b1;
          rx_baud_cnt <= (BAUD_DIV[BAUD_CNT_W-1:0] >> 1); // sample in middle
          rx_bits_cnt <= 4'h0;
        end
      end else begin
        if (rx_baud_cnt == BAUD_DIV[BAUD_CNT_W-1:0]-1'b1) begin
          rx_baud_cnt <= '0;
          // shift in sampled bit
          rx_shift <= {UART_RX, rx_shift[9:1]};
          rx_bits_cnt <= rx_bits_cnt + 1'b1;
          if (rx_bits_cnt == 4'd9) begin
            // full frame collected: {stop, data[7:0], start}
            rx_sampling <= 1'b0;
            rx_byte <= rx_shift[8:1]; // data[7:0]
            rx_byte_valid <= 1'b1;
          end
        end else begin
          rx_baud_cnt <= rx_baud_cnt + 1'b1;
        end
      end
    end
  end

  // When a byte arrives, send it via mailbox_endpoint TX interface
  // We use a simple one-beat packet with opcode=OPC_DATA, prio=0, eop=1
  logic ep_tx_valid;
  logic ep_tx_ready;
  logic [15:0] ep_tx_dest;
  logic [31:0] ep_tx_data;
  logic ep_tx_prio;
  logic ep_tx_eop;
  logic [3:0] ep_tx_opcode;

  // Signals from endpoint RX streaming (inbound mailbox writes -> UART TX)
  logic ep_rx_valid;
  logic ep_rx_ready;
  logic [31:0] ep_rx_data;
  mailbox_tag_t ep_rx_tag;
  logic ep_rx_irq;

  // Config registers (written by mailbox writes to CSR idx 2 and readable by CSR idx2 through mailbox_endpoint's csr_reg)
  logic [15:0] cfg_dest;
  logic [31:0] cfg_baud_div;

  // sample config writes from s_awvalid/s_wvalid when idx != 0
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      cfg_dest <= DEFAULT_DEST;
      cfg_baud_div <= BAUD_DIV;
    end else begin
      if (s_awvalid && s_wvalid && (s_awaddr[3:0] == 4'd2)) begin
        cfg_dest <= s_wdata[15:0];
      end
      if (s_awvalid && s_wvalid && (s_awaddr[3:0] == 4'd3)) begin
        cfg_baud_div <= s_wdata;
      end
    end
  end

  // Drive the endpoint TX interface when a UART RX byte is ready
  // Provide a single-entry staging buffer to handle backpressure from the endpoint
  logic pending_tx_valid;
  logic [7:0] pending_tx_byte;
  logic ep_tx_valid_reg;
  logic [31:0] ep_tx_data_reg;
  logic [15:0] ep_tx_dest_reg;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      pending_tx_valid <= 1'b0;
      pending_tx_byte <= 8'h0;
      ep_tx_valid_reg <= 1'b0;
      ep_tx_data_reg <= 32'h0;
      ep_tx_dest_reg <= cfg_dest;
    end else begin
      // New incoming byte -> either send immediately (if endpoint ready) or store to pending
      if (rx_byte_valid) begin
        if (ep_tx_ready && !ep_tx_valid_reg) begin
          ep_tx_valid_reg <= 1'b1;
          ep_tx_data_reg <= {24'h0, rx_byte};
          ep_tx_dest_reg <= cfg_dest;
        end else if (!pending_tx_valid) begin
          pending_tx_valid <= 1'b1;
          pending_tx_byte <= rx_byte;
        end
      end

      // If endpoint accepted our sent word, clear ep_tx_valid_reg
      if (ep_tx_valid_reg && ep_tx_ready) ep_tx_valid_reg <= 1'b0;

      // If we have a pending byte and endpoint is free, promote it
      if (!ep_tx_valid_reg && pending_tx_valid && ep_tx_ready) begin
        ep_tx_valid_reg <= 1'b1;
        ep_tx_data_reg  <= {24'h0, pending_tx_byte};
        ep_tx_dest_reg  <= cfg_dest;
        pending_tx_valid <= 1'b0;
      end
    end
  end

  // Drive the combinational view
  always_comb begin
    ep_tx_valid  = ep_tx_valid_reg;
    ep_tx_dest   = ep_tx_dest_reg;
    ep_tx_data   = ep_tx_data_reg;
    ep_tx_prio   = 1'b0;
    ep_tx_eop    = 1'b1;
    ep_tx_opcode = OPC_DATA;
  end

  // mailbox_endpoint module provides tx_ready (ep_tx_ready) to accept data
  // The endpoint's TX FIFO provides backpressure via ep_tx_ready

  // ---------------------------
  // Status outputs
  // ---------------------------
  // expose a simple status CSR in index 1 via writes handled by mailbox_endpoint already
  // Also update s_rdata for other indices is handled inside mailbox_endpoint; to provide visibility for cfg_dest and cfg_baud_div we rely on CSR 2 and 3 being written/read by SW (these are stored in endpoint's csr_reg array by endpoint on writes but we keep a local copy too so SW can read it back via our stored copy if desired by reading CSR 2/3)

endmodule
