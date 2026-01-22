`timescale 1ns/1ps

// Simple direct-port UART SAT-IP (no mailbox):
// - Single-byte TX (write to tx_data + tx_valid)
// - Single-byte RX presented on rx_data with rx_rdy/clr_rdy handshake
// - Optional CTS/RTS flow control

module uart_sat #(
  parameter int BAUD_DIV = 217
)(
  input  logic clk,
  input  logic rst_n,

  // Direct lightweight TX interface (software writes bytes here)
  input  logic [7:0] tx_data,
  input  logic       tx_valid,
  output logic       tx_ready, // accepts tx byte when high

  // Direct lightweight RX interface (software reads bytes here)
  output logic [7:0] rx_data,
  output logic       rx_rdy,   // level set while byte present
  input  logic        clr_rdy, // software pulses to consume

  // UART pins
  output logic UART_TX,
  input  logic  UART_RX,
  input  logic  UART_CTS,
  output logic UART_RTS
);

  // Simple internal tx FIFO depth 4
  localparam int TX_DEPTH = 4;
  localparam int TX_PTR_W = (TX_DEPTH <= 1) ? 1 : $clog2(TX_DEPTH);
  localparam int TX_CNT_W = $clog2(TX_DEPTH+1);
  logic [7:0] tx_fifo [0:TX_DEPTH-1];
  logic [TX_PTR_W-1:0] tx_wptr, tx_rptr;
  logic [TX_CNT_W-1:0] tx_count;
  logic tx_full, tx_empty;
  logic tx_w_en, tx_r_en;

  assign tx_w_en = tx_valid && tx_ready;
  assign tx_r_en = tx_frame_done && !tx_empty;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      tx_wptr  <= '0;
      tx_rptr  <= '0;
      tx_count <= '0;
    end else begin
      if (tx_w_en) begin
        tx_fifo[tx_wptr] <= tx_data;
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

  assign tx_full  = (tx_count == TX_DEPTH[TX_CNT_W-1:0]);
  assign tx_empty = (tx_count == {TX_CNT_W{1'b0}});
  assign tx_ready = !tx_full;
  assign UART_RTS = !tx_full;

  // TX shift / baud
  localparam int BAUD_CNT_W = (BAUD_DIV <= 1) ? 1 : $clog2(BAUD_DIV);
  logic [BAUD_CNT_W-1:0] tx_baud_cnt;
  logic tx_busy;
  logic [9:0] tx_shift;
  logic [3:0] tx_bits;
  logic tx_frame_done;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      tx_baud_cnt <= '0; tx_busy <= 1'b0; tx_shift <= 10'h3FF; tx_bits <= '0; tx_frame_done <= 1'b0;
    end else begin
      tx_frame_done <= 1'b0;
      if (!tx_busy && !tx_empty && !UART_CTS) begin
        tx_shift <= {1'b1, tx_fifo[tx_rptr], 1'b0};
        tx_bits <= 4'd0;
        tx_busy <= 1'b1;
        tx_baud_cnt <= '0;
      end else if (tx_busy) begin
        if (tx_baud_cnt == BAUD_DIV[BAUD_CNT_W-1:0]-1'b1) begin
          tx_baud_cnt <= '0;
          tx_shift <= {1'b1, tx_shift[9:1]};
          tx_bits <= tx_bits + 1'b1;
          if (tx_bits == 4'd9) begin
            tx_busy <= 1'b0;
            tx_frame_done <= 1'b1;
          end
        end else tx_baud_cnt <= tx_baud_cnt + 1'b1;
      end
    end
  end

  assign UART_TX = tx_shift[0];

  // RX: simple sampler similar to mailbox variant
  logic [BAUD_CNT_W-1:0] rx_baud_cnt;
  logic rx_sampling;
  logic [3:0] rx_bits_cnt;
  logic [9:0] rx_shift;
  logic rx_byte_valid;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      rx_sampling <= 1'b0; rx_baud_cnt <= '0; rx_bits_cnt <= '0; rx_shift <= 10'h3FF; rx_byte_valid <= 1'b0; rx_data <= 8'h0;
    end else begin
      if (!rx_sampling && !rx_byte_valid) begin
        if (UART_RX == 1'b0) begin
          rx_sampling <= 1'b1;
          rx_baud_cnt <= BAUD_DIV[BAUD_CNT_W-1:0] >> 1;
          rx_bits_cnt <= 4'h0;
        end
      end else if (rx_sampling) begin
        if (rx_baud_cnt == BAUD_DIV[BAUD_CNT_W-1:0]-1'b1) begin
          rx_baud_cnt <= '0;
          rx_shift <= {UART_RX, rx_shift[9:1]};
          rx_bits_cnt <= rx_bits_cnt + 1'b1;
          if (rx_bits_cnt == 4'd9) begin
            rx_sampling <= 1'b0;
            rx_byte_valid <= 1'b1;
            rx_data <= rx_shift[8:1];
          end
        end else begin
          rx_baud_cnt <= rx_baud_cnt + 1'b1;
        end
      end

      if (rx_byte_valid && clr_rdy) begin
        rx_byte_valid <= 1'b0; // consumed by SW
      end
    end
  end

  assign rx_rdy = rx_byte_valid;

endmodule
