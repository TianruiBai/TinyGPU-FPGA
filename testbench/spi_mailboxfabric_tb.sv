`timescale 1ns/1ps

module spi_mailboxfabric_tb;
  import mailbox_pkg::*;

  localparam int TX_LEN = 2;
  localparam int RX_LEN = 2;

  logic clk;
  logic rst_n;

  // Mailbox slave ports (driven by TB)
  logic s_awvalid;
  logic s_awready;
  logic [15:0] s_awaddr;
  logic s_wvalid;
  logic s_wready;
  logic [31:0] s_wdata;
  logic [3:0] s_wstrb;
  mailbox_tag_t s_tag;
  logic s_bready;
  logic s_bvalid;

  logic s_arvalid;
  logic s_arready;
  logic [15:0] s_araddr;
  logic s_rvalid;
  logic s_rready;
  logic [31:0] s_rdata;

  // Mailbox master ports (observed)
  logic m_awvalid;
  logic m_awready;
  logic [15:0] m_awaddr;
  logic m_wvalid;
  logic m_wready;
  logic [31:0] m_wdata;
  logic [3:0] m_wstrb;
  mailbox_tag_t m_tag;
  logic m_bready;
  logic m_bvalid;

  logic m_arvalid;
  logic m_arready;
  logic [15:0] m_araddr;
  logic m_rvalid;
  logic m_rready;
  logic [31:0] m_rdata;

  // SPI pins (loopback)
  logic SPI_SCLK;
  logic SPI_MOSI;
  logic SPI_MISO;
  logic [0:0] SPI_CS;
  assign SPI_MISO = SPI_MOSI;

  int timeout;

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

    .m_awvalid(m_awvalid), .m_awready(m_awready), .m_awaddr(m_awaddr), .m_wvalid(m_wvalid), .m_wready(m_wready), .m_wdata(m_wdata), .m_wstrb(m_wstrb), .m_tag(m_tag), .m_bready(m_bready), .m_bvalid(m_bvalid),
    .m_arvalid(m_arvalid), .m_arready(m_arready), .m_araddr(m_araddr), .m_rvalid(m_rvalid), .m_rready(m_rready), .m_rdata(m_rdata),

    .s_awvalid(s_awvalid), .s_awready(s_awready), .s_awaddr(s_awaddr), .s_wvalid(s_wvalid), .s_wready(s_wready), .s_wdata(s_wdata), .s_wstrb(s_wstrb), .s_tag(s_tag), .s_bready(s_bready), .s_bvalid(s_bvalid),
    .s_arvalid(s_arvalid), .s_arready(s_arready), .s_araddr(s_araddr), .s_rvalid(s_rvalid), .s_rready(s_rready), .s_rdata(s_rdata),

    .SPI_SCLK(SPI_SCLK), .SPI_MOSI(SPI_MOSI), .SPI_MISO(SPI_MISO), .SPI_CS(SPI_CS)
  );

  initial begin
    clk = 0;
    forever #5 clk = ~clk;
  end

  initial begin
    rst_n = 0;
    s_awvalid = 0; s_wvalid = 0; s_awaddr = 16'h0; s_wdata = 32'h0; s_wstrb = 4'hF;
    s_tag = '{src_id:8'hFF, eop:1'b1, prio:1'b0, opcode:OPC_DATA, hops:4'h0, parity:1'b0};
    s_bready = 1'b1;
    s_arvalid = 0; s_araddr = 16'h0; s_rready = 1'b1;

    m_awready = 1'b1; m_wready = 1'b1; m_bvalid = 1'b0; m_arready = 1'b1; m_rvalid = 1'b0; m_rdata = 32'h0;

    #50; rst_n = 1;

    // Send command (CSR0)
    do_write(16'h0000, 32'h0000A55A);

    // Wait for response on mailbox master
    timeout = 0;
    while (m_awvalid == 1'b0) begin
      @(posedge clk);
      timeout = timeout + 1;
      if (timeout > 2000) $fatal("[TB] Timeout waiting for SPI mailbox response");
    end

    if (m_awaddr !== 16'h1234) $fatal("[TB] Dest mismatch: got 0x%0h", m_awaddr);
    if (m_wdata[15:0] !== 16'hA55A) $fatal("[TB] Resp mismatch: got 0x%0h", m_wdata[15:0]);

    $display("[TB] SPI Mailbox test complete");
    $finish;
  end

  task automatic do_write(input logic [15:0] addr, input logic [31:0] data);
    begin
      @(posedge clk);
      s_awaddr <= addr;
      s_wdata <= data;
      s_awvalid <= 1'b1;
      s_wvalid <= 1'b1;
      wait (s_awready && s_wready);
      @(posedge clk);
      s_awvalid <= 1'b0; s_wvalid <= 1'b0;
      wait (s_bvalid == 1);
      @(posedge clk);
    end
  endtask

endmodule
