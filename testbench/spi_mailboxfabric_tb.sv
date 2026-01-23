`timescale 1ns/1ps

module spi_mailboxfabric_tb;
  import mailbox_pkg::*;

  localparam int TX_LEN = 2;
  localparam int RX_LEN = 2;

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

  // SPI pins (loopback)
  logic SPI_SCLK;
  logic SPI_MOSI;
  logic SPI_MISO;
  logic [0:0] SPI_CS;
  assign SPI_MISO = SPI_MOSI;

  int timeout;
  logic got_resp;
  logic [15:0] resp_addr;
  logic [31:0] resp_data;

  spi_mailboxfabric #(
    .TX_LEN(TX_LEN),
    .RX_LEN(RX_LEN),
    .CS_NUM(1),
    .CS_INDEX(0),
    .SCLK_DIV(2),
    .SRC_ID(8'h20),
    .DEFAULT_DEST(16'h1234)
  ) dut (
    .clk(clk), .rst_n(rst_n),

    .mb_tx_valid(mb_tx_valid), .mb_tx_ready(mb_tx_ready), .mb_tx_data(mb_tx_data), .mb_tx_dest_id(mb_tx_dest_id),
    .mb_rx_valid(mb_rx_valid), .mb_rx_ready(mb_rx_ready), .mb_rx_data(mb_rx_data), .mb_rx_dest_id(mb_rx_dest_id),

    .SPI_SCLK(SPI_SCLK), .SPI_MOSI(SPI_MOSI), .SPI_MISO(SPI_MISO), .SPI_CS(SPI_CS)
  );

  initial begin
    clk = 0;
    forever #5 clk = ~clk;
  end

  initial begin
    rst_n = 0;
    mb_rx_valid = 1'b0; mb_rx_dest_id = 16'h0; mb_rx_data = '0;
    mb_tx_ready = 1'b1;

    #50; rst_n = 1;

    // Set destination (CSR5)
    send_csr(16'h0005, 32'h00001234);

    // Load TX data (CSR0/CSR1)
    send_csr(16'h0000, 32'h0000A55A);
    send_csr(16'h0001, 32'h00000000);

    // Set control: tx_bits=16, rx_bits=16, start=1 (CSR4)
    send_csr(16'h0004, 32'h00011010);

    // Wait for response on mailbox master (sample on clock edge)
    timeout = 0;
    while (!got_resp) begin
      @(posedge clk);
      timeout = timeout + 1;
      if (timeout > 2000) $fatal("[TB] Timeout waiting for SPI mailbox response");
    end

    if (resp_addr !== 16'h1234) begin
      $display("[TB] Dest mismatch: got 0x%0h", resp_addr);
      $fatal;
    end
    if (resp_data[15:0] !== 16'hA55A) begin
      $display("[TB] Resp mismatch: got 0x%0h", resp_data[15:0]);
      $fatal;
    end

    $display("[TB] SPI Mailbox test complete");
    $finish;
  end

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      got_resp  <= 1'b0;
      resp_addr <= 16'h0;
      resp_data <= 32'h0;
    end else if (!got_resp && mb_tx_valid) begin
      got_resp  <= 1'b1;
      resp_addr <= mb_tx_dest_id;
      resp_data <= mb_tx_data.payload;
    end
  end

  task automatic send_csr(input logic [15:0] addr, input logic [31:0] data);
    begin
      @(posedge clk);
      mb_rx_dest_id <= addr;
      mb_rx_data <= '{hdr:'{src_id:16'h00FF, opcode:OPC_DATA, prio:2'd0, eop:1'b1, debug:1'b0}, payload:data};
      mb_rx_valid <= 1'b1;
      wait (mb_rx_ready);
      @(posedge clk);
      mb_rx_valid <= 1'b0;
    end
  endtask

endmodule
