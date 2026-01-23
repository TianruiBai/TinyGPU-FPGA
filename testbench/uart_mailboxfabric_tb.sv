`timescale 1ns/1ps

module uart_mailboxfabric_tb;
  import mailbox_pkg::*;

  logic clk;
  logic rst_n;

  // MailboxFabric stream link
  logic mb_tx_valid;
  logic mb_tx_ready;
  mailbox_flit_t mb_tx_data;
  logic [NODE_ID_WIDTH-1:0] mb_tx_dest_id;

  logic mb_rx_valid;
  logic mb_rx_ready;
  mailbox_flit_t mb_rx_data;
  logic [NODE_ID_WIDTH-1:0] mb_rx_dest_id;

  // UART pins
  logic UART_TX;
  logic UART_RX;
  logic UART_CTS;
  logic UART_RTS;

  int timeout_cycles;

  // DUT with small BAUD_DIV
  uart_mailboxfabric #(.SRC_ID(8'h10), .DEFAULT_DEST(16'hCAFE), .BAUD_DIV(8)) dut (
    .clk(clk), .rst_n(rst_n),
    .mb_tx_valid(mb_tx_valid), .mb_tx_ready(mb_tx_ready), .mb_tx_data(mb_tx_data), .mb_tx_dest_id(mb_tx_dest_id),
    .mb_rx_valid(mb_rx_valid), .mb_rx_ready(mb_rx_ready), .mb_rx_data(mb_rx_data), .mb_rx_dest_id(mb_rx_dest_id),
    .UART_TX(UART_TX), .UART_RX(UART_RX), .UART_CTS(UART_CTS), .UART_RTS(UART_RTS)
  );

  // clock
  initial begin
    clk = 0;
    forever #5 clk = ~clk;
  end

  // reset and initial values
  initial begin
    rst_n = 0;
    mb_rx_valid = 1'b0;
    mb_rx_data  = '0;
    mb_rx_dest_id = '0;
    mb_tx_ready = 1'b1;

    UART_RX = 1'b1; UART_CTS = 1'b0;

    #100;
    rst_n = 1;
    #100;

    $display("[TB] UART Mailbox smoke tests starting");

    // 1) Send a mailbox write (CSR 0) -> should cause the DUT to transmit via UART_TX
    send_csr(16'h0000, 32'h00000041); // CSR idx0: ASCII 'A'
    $display("[TB] Write to CSR0 done, checking UART TX waveform");

    // check serial waveform for outgoing 'A' (use BAUD value consistent with DUT)
    check_tx_waveform(8'h41);

    // 2) Configure CSR2 to a known destination (already DEFAULT_DEST = 0xCAFE), but try writing it explicitly
    send_csr(16'h0002, 32'h00001234); // set cfg_dest to 0x1234
    $display("[TB] Updated CSR 2 to 0x1234");

    // 3) Inject a serial RX byte and expect the DUT to issue a mailbox write (m_awvalid with m_wdata LSB = byte)
    send_serial_rx(8'h7E);

    // wait for outbound m_awvalid (with timeout)
    timeout_cycles = 0;
    while (mb_tx_valid == 1'b0) begin
      @(posedge clk);
      timeout_cycles = timeout_cycles + 1;
      if (timeout_cycles > 2000) $fatal("[TB] ERROR: timeout waiting for outbound mailbox write");
    end
    $display("[TB] Observed outbound mailbox flit to 0x%0h with data 0x%0h", mb_tx_dest_id, mb_tx_data.payload);

    // check expected destination (should match CSR 2 write we did 0x1234)
    if (mb_tx_dest_id !== 16'h1234) $fatal("[TB] ERROR: unexpected dest 0x%0h (expected 0x1234)", mb_tx_dest_id);

    // check payload LSB is received byte
    if (mb_tx_data.payload[7:0] !== 8'h7E) $fatal("[TB] ERROR: outbound mailbox payload mismatch 0x%0h (expected 0x7E)", mb_tx_data.payload[7:0]);

    #200;
    $display("[TB] UART Mailbox tests complete");
    $finish;
  end

  // helper tasks
  task automatic send_csr(input logic [15:0] addr, input logic [31:0] data);
    begin
      @(posedge clk);
      mb_rx_dest_id <= addr;
      mb_rx_data <= '{hdr:'{src_id:16'h00FF, opcode:OPC_DATA, prio:2'd0, eop:1'b1, debug:1'b0}, payload:data};
      mb_rx_valid <= 1'b1;
      wait (mb_rx_ready);
      @(posedge clk);
      mb_rx_valid <= 1'b0;
      $display("[TB] send_csr: wrote 0x%0h to addr 0x%0h", data, addr);
    end
  endtask

  // simulate serial RX: produce start(0), 8 data bits LSB-first, stop(1)
  task automatic send_serial_rx(input logic [7:0] b);
    integer i;
    begin
      // start
      UART_RX <= 1'b0;
      repeat (BAUD_HALF) @(posedge clk);
      for (i = 0; i < 8; i = i + 1) begin
        UART_RX <= b[i];
        repeat (BAUD) @(posedge clk);
      end
      // stop
      UART_RX <= 1'b1;
      repeat (BAUD) @(posedge clk);
      $display("[TB] Injected serial byte 0x%0h into RX", b);
    end
  endtask

  localparam int BAUD = 8; // must match DUT BAUD_DIV
  localparam int BAUD_HALF = (BAUD/2);

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
