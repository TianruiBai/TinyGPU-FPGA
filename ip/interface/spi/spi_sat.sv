`timescale 1ns/1ps

// Simple SPI SAT-IP (master, mode 0):
// - Configurable transfer length (1..8 bytes)
// - Simple direct interface (cmd/trmt/resp/rx_rdy)
// - Multiple CS lines via parameter

module spi_sat #(
  parameter int TX_LEN   = 1,   // bytes (1..8)
  parameter int RX_LEN   = 1,   // bytes (1..8)
  parameter int CS_NUM   = 1,
  parameter int CS_INDEX = 0,
  parameter int SCLK_DIV = 4    // clk cycles per half SCLK period
)(
  input  logic clk,
  input  logic rst_n,

  // Variable-length control (bits). If 0, use TX_LEN/RX_LEN defaults.
  input  logic [7:0] tx_bits,
  input  logic [7:0] rx_bits,

  // Direct interface
  input  logic [TX_LEN*8-1:0] cmd,
  output logic [RX_LEN*8-1:0] resp,
  input  logic trmt,
  output logic rx_rdy,
  input  logic clr_rdy,

  // SPI pins
  output logic SPI_SCLK,
  output logic SPI_MOSI,
  input  logic SPI_MISO,
  output logic [CS_NUM-1:0] SPI_CS
);

  localparam int TX_BITS = TX_LEN * 8;
  localparam int RX_BITS = RX_LEN * 8;
  localparam int TOTAL_BITS = (TX_BITS > RX_BITS) ? TX_BITS : RX_BITS;
  localparam int CNT_W = (TOTAL_BITS <= 1) ? 1 : $clog2(TOTAL_BITS+1);
  localparam int DIV_W = (SCLK_DIV <= 1) ? 1 : $clog2(SCLK_DIV);

  logic [63:0] tx_shift;
  logic [63:0] rx_shift;
  logic [CNT_W-1:0] bit_cnt;
  logic [DIV_W-1:0] div_cnt;
  logic [63:0] rx_shift_next;
  logic busy;
  logic sclk_reg;
  logic [7:0] tx_bits_lat;
  logic [7:0] rx_bits_lat;
  logic [7:0] total_bits_lat;
  logic [7:0] tx_bits_eff;
  logic [7:0] rx_bits_eff;
  logic [7:0] total_bits_eff;

  always_comb begin
    tx_bits_eff = (tx_bits == 0) ? TX_BITS[7:0] : (tx_bits > TX_BITS[7:0] ? TX_BITS[7:0] : tx_bits);
    rx_bits_eff = (rx_bits == 0) ? RX_BITS[7:0] : (rx_bits > RX_BITS[7:0] ? RX_BITS[7:0] : rx_bits);
    total_bits_eff = (tx_bits_eff > rx_bits_eff) ? tx_bits_eff : rx_bits_eff;
  end

  // Chip select (active low)
  always_comb begin
    SPI_CS = {CS_NUM{1'b1}};
    if (busy) SPI_CS[CS_INDEX] = 1'b0;
  end

  // SCLK output (mode 0: idle low)
  assign SPI_SCLK = sclk_reg;

  // MOSI: driven from MSB of shift register (MSB-first)
  always_comb begin
    if (!busy) begin
      SPI_MOSI = 1'b0;
    end else begin
      SPI_MOSI = tx_shift[63];
    end
  end

  // RX ready (latched on transfer completion)
  logic done_pulse;
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) rx_rdy <= 1'b0;
    else begin
      if (trmt) rx_rdy <= 1'b0; // auto-clear on new transfer
      else if (done_pulse) rx_rdy <= 1'b1;
      if (clr_rdy) rx_rdy <= 1'b0;
    end
  end

  // Transfer engine
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      tx_shift   <= 64'h0;
      rx_shift   <= 64'h0;
      bit_cnt    <= '0;
      div_cnt    <= '0;
      busy       <= 1'b0;
      sclk_reg   <= 1'b0;
      resp       <= '0;
      done_pulse <= 1'b0;
      tx_bits_lat <= TX_BITS[7:0];
      rx_bits_lat <= RX_BITS[7:0];
      total_bits_lat <= (TX_BITS > RX_BITS) ? TX_BITS[7:0] : RX_BITS[7:0];
    end else begin
      done_pulse <= 1'b0;
      if (trmt && !busy) begin
        // launch transfer
        tx_bits_lat <= tx_bits_eff;
        rx_bits_lat <= rx_bits_eff;
        total_bits_lat <= total_bits_eff;

        tx_shift <= {cmd, {(64-TX_BITS){1'b0}}};
        rx_shift <= 64'h0;
        bit_cnt  <= total_bits_eff[CNT_W-1:0];
        div_cnt  <= '0;
        busy     <= 1'b1;
        sclk_reg <= 1'b0;
      end else if (busy) begin
        if (div_cnt == SCLK_DIV[DIV_W-1:0]-1'b1) begin
          div_cnt  <= '0;
          sclk_reg <= ~sclk_reg;

          // rising edge: sample MISO and count bits
          if (!sclk_reg) begin
            if (bit_cnt != 0) begin
              rx_shift_next = {rx_shift[62:0], SPI_MISO};
              rx_shift <= rx_shift_next;
              bit_cnt  <= bit_cnt - 1'b1;

              if (bit_cnt == 1) begin
                if (rx_bits_lat == 0) resp <= '0;
                else resp <= rx_shift_next[RX_BITS-1:0] >> (RX_BITS - rx_bits_lat);
                busy <= 1'b0;
                sclk_reg <= 1'b0;
                done_pulse <= 1'b1;
              end
            end
          end

          // falling edge: shift MOSI to next bit
          if (sclk_reg) begin
            if (bit_cnt != 0) tx_shift <= {tx_shift[62:0], 1'b0};
          end
        end else begin
          div_cnt <= div_cnt + 1'b1;
        end
      end
    end
  end

endmodule
