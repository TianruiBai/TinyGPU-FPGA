`timescale 1ns/1ps

module spi_sat_tb;
  localparam int TX_LEN = 2;
  localparam int RX_LEN = 2;

  logic clk;
  logic rst_n;

  logic [TX_LEN*8-1:0] cmd;
  logic [RX_LEN*8-1:0] resp;
  logic trmt;
  logic rx_rdy;
  logic clr_rdy;

  logic SPI_SCLK;
  logic SPI_MOSI;
  logic SPI_MISO;
  logic [0:0] SPI_CS;

  // loopback
  assign SPI_MISO = SPI_MOSI;

  spi_sat #(
    .TX_LEN(TX_LEN),
    .RX_LEN(RX_LEN),
    .CS_NUM(1),
    .CS_INDEX(0),
    .SCLK_DIV(2)
  ) dut (
    .clk(clk), .rst_n(rst_n),
    .cmd(cmd), .resp(resp), .trmt(trmt), .rx_rdy(rx_rdy), .clr_rdy(clr_rdy),
    .SPI_SCLK(SPI_SCLK), .SPI_MOSI(SPI_MOSI), .SPI_MISO(SPI_MISO), .SPI_CS(SPI_CS)
  );

  initial begin
    clk = 0;
    forever #5 clk = ~clk;
  end

  initial begin
    rst_n = 0;
    cmd = '0;
    trmt = 0;
    clr_rdy = 0;

    #50;
    rst_n = 1;

    // send cmd and expect resp to match (loopback)
    cmd = 16'hA55A;
    @(posedge clk);
    trmt = 1;
    @(posedge clk);
    trmt = 0;

    wait (rx_rdy == 1'b1);
    @(posedge clk); // allow resp to settle
    if (resp !== cmd) begin
      $display("[TB] SPI SAT resp mismatch: got 0x%0h expected 0x%0h", resp, cmd);
      $fatal;
    end

    clr_rdy = 1'b1; @(posedge clk); clr_rdy = 1'b0;

    $display("[TB] SPI SAT test complete");
    $finish;
  end

endmodule
