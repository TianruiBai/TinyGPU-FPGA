`timescale 1ns/1ps

module uart_sat_tb;

  logic clk;
  logic rst_n;

  // DUT ports
  logic [7:0] tx_data;
  logic       tx_valid;
  logic       tx_ready;

  logic [7:0] rx_data;
  logic       rx_rdy;
  logic       clr_rdy;

  logic UART_TX;
  logic UART_RX;
  logic UART_CTS;
  logic UART_RTS;

  // Instantiate DUT with small BAUD_DIV for fast simulation
  uart_sat #(.BAUD_DIV(8)) dut (
    .clk(clk), .rst_n(rst_n),
    .tx_data(tx_data), .tx_valid(tx_valid), .tx_ready(tx_ready),
    .rx_data(rx_data), .rx_rdy(rx_rdy), .clr_rdy(clr_rdy),
    .UART_TX(UART_TX), .UART_RX(UART_RX), .UART_CTS(UART_CTS), .UART_RTS(UART_RTS)
  );

  // clock
  initial begin
    clk = 0;
    forever #5 clk = ~clk; // 100MHz
  end

  // reset
  initial begin
    rst_n = 0;
    tx_valid = 0;
    tx_data = 8'h00;
    UART_RX = 1'b1; // idle
    UART_CTS = 1'b0;
    clr_rdy = 1'b0;

    #100;
    rst_n = 1;

    #100;
    $display("[TB] Starting UART SAT smoke test");

    // Send a byte via direct TX and check waveform
    send_tx_byte(8'hA5);
    check_tx_waveform(8'hA5);

    // Now send a serial byte into RX path
    send_serial_rx(8'h5A);

    // Wait for DUT to assert rx_rdy
    wait (rx_rdy == 1);
    $display("[TB] Received byte via RX path: 0x%0h", rx_data);

    // Parity is not used in this simple SAT UART; check rx_data correctness
    if (rx_data !== 8'h5A) $fatal("[TB] ERROR: RX byte mismatch: got 0x%0h expected 0x5A", rx_data);

    // Clear it
    clr_rdy = 1'b1; #10; clr_rdy = 1'b0;

    #200;
    $display("[TB] UART SAT test complete");
    $finish;
  end

  localparam int BAUD = 8; // must match DUT BAUD_DIV
  localparam int BAUD_HALF = (BAUD/2);

  // Task: push a byte into the DUT via direct port
  task automatic send_tx_byte(input logic [7:0] b);
    begin
      @(posedge clk);
      tx_data <= b;
      tx_valid <= 1'b1;
      // wait for ready
      wait (tx_ready == 1);
      @(posedge clk);
      tx_valid <= 1'b0;
      $display("[TB] Sent byte 0x%0h via direct TX", b);
    end
  endtask

  // Task: simulate a serial RX of one byte LSB first, start=0, stop=1
  task automatic send_serial_rx(input logic [7:0] b);
    integer i;
    begin
      // start bit
      UART_RX <= 1'b0;
      repeat (BAUD_HALF) @(posedge clk); // hold start for half-baud to match RX sampling

      for (i = 0; i < 8; i = i + 1) begin
        UART_RX <= b[i];
        repeat (BAUD) @(posedge clk); // hold bit for one baud
      end
      // stop bit
      UART_RX <= 1'b1;
      repeat (BAUD) @(posedge clk);
      $display("[TB] Injected serial byte 0x%0h into RX line", b);

      // Wait until DUT latches byte (rx_rdy goes high)
      wait (rx_rdy == 1);
    end
  endtask

  // Task: capture and check the serial waveform transmitted on UART_TX for 'b'
  task automatic check_tx_waveform(input logic [7:0] b);
    integer i;
    logic [9:0] bits;
    logic [7:0] sampled;
    begin
      // Wait for start bit (falling edge)
      wait (UART_TX == 1'b0);

      // Move to middle of start bit, then sample 10 bits at BAUD intervals
      repeat (BAUD_HALF) @(posedge clk);
      for (i = 0; i < 10; i = i + 1) begin
        bits[i] = UART_TX;
        repeat (BAUD) @(posedge clk);
      end

      sampled = {bits[8], bits[7], bits[6], bits[5], bits[4], bits[3], bits[2], bits[1]};

      if (bits[0] !== 1'b0) $fatal("[TB] ERROR: start bit not low (bits[0]=%0b)", bits[0]);
      if (bits[9] !== 1'b1) $fatal("[TB] ERROR: stop bit not high (bits[9]=%0b)", bits[9]);
      if (sampled !== b) begin
        $display("[TB] UART TX sampled=0x%0h expected=0x%0h bits=%b", sampled, b, bits);
        $fatal("[TB] ERROR: UART TX data mismatch");
      end
      $display("[TB] UART TX waveform matches expected byte 0x%0h", b);
    end
  endtask

endmodule
