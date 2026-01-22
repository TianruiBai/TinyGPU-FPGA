`timescale 1ns/1ps

module uart_mailboxfabric_tb;
  import mailbox_pkg::*;

  logic clk;
  logic rst_n;

  // Top-level mailbox slave ports (driven by TB)
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

  // Top-level mailbox master ports (observed by TB)
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

  // UART pins
  logic UART_TX;
  logic UART_RX;
  logic UART_CTS;
  logic UART_RTS;

  int timeout_cycles;

  // DUT with small BAUD_DIV
  uart_mailboxfabric #(.SRC_ID(8'h10), .DEFAULT_DEST(16'hCAFE), .BAUD_DIV(8)) dut (
    .clk(clk), .rst_n(rst_n),
    .m_awvalid(m_awvalid), .m_awready(m_awready), .m_awaddr(m_awaddr), .m_wvalid(m_wvalid), .m_wready(m_wready), .m_wdata(m_wdata), .m_wstrb(m_wstrb), .m_tag(m_tag), .m_bready(m_bready), .m_bvalid(m_bvalid),
    .m_arvalid(m_arvalid), .m_arready(m_arready), .m_araddr(m_araddr), .m_rvalid(m_rvalid), .m_rready(m_rready), .m_rdata(m_rdata),

    .s_awvalid(s_awvalid), .s_awready(s_awready), .s_awaddr(s_awaddr), .s_wvalid(s_wvalid), .s_wready(s_wready), .s_wdata(s_wdata), .s_wstrb(s_wstrb), .s_tag(s_tag), .s_bready(s_bready), .s_bvalid(s_bvalid),
    .s_arvalid(s_arvalid), .s_arready(s_arready), .s_araddr(s_araddr), .s_rvalid(s_rvalid), .s_rready(s_rready), .s_rdata(s_rdata),

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
    s_awvalid = 0; s_wvalid = 0; s_awaddr = 16'h0; s_wdata = 32'h0; s_wstrb = 4'hF; s_tag = '{src_id:8'hFF, eop:1'b1, prio:1'b0, opcode:OPC_DATA, hops:4'h0, parity:1'b0};
    s_bready = 1'b1;
    s_arvalid = 0; s_araddr = 16'h0; s_rready = 1'b1;


    // mailbox tag helper (parity check will use an inline struct literal)

    m_awready = 1'b1; m_wready = 1'b1; m_bvalid = 1'b0; m_arready = 1'b1; m_rvalid = 1'b0; m_rdata = 32'h0;

    UART_RX = 1'b1; UART_CTS = 1'b0;

    #100;
    rst_n = 1;
    #100;

    $display("[TB] UART Mailbox smoke tests starting");

    // 1) Send a mailbox write (CSR 0) -> should cause the DUT to transmit via UART_TX
    do_write(16'h0000, 32'h00000041); // CSR idx0: ASCII 'A'
    $display("[TB] Write to CSR0 done, checking UART TX waveform");

    // check serial waveform for outgoing 'A' (use BAUD value consistent with DUT)
    check_tx_waveform(8'h41);

    // 2) Configure CSR2 to a known destination (already DEFAULT_DEST = 0xCAFE), but try writing it explicitly
    do_write(16'h0002, 32'h00001234); // set cfg_dest to 0x1234
    $display("[TB] Updated CSR 2 to 0x1234");

    // 3) Inject a serial RX byte and expect the DUT to issue a mailbox write (m_awvalid with m_wdata LSB = byte)
    send_serial_rx(8'h7E);

    // wait for outbound m_awvalid (with timeout)
    timeout_cycles = 0;
    while (m_awvalid == 1'b0) begin
      @(posedge clk);
      timeout_cycles = timeout_cycles + 1;
      if (timeout_cycles > 2000) $fatal("[TB] ERROR: timeout waiting for outbound mailbox write");
    end
    $display("[TB] Observed outbound mailbox write to 0x%0h with data 0x%0h", m_awaddr, m_wdata);

    // check expected destination (should match CSR 2 write we did 0x1234)
    if (m_awaddr !== 16'h1234) $fatal("[TB] ERROR: unexpected dest 0x%0h (expected 0x1234)", m_awaddr);

    // check payload LSB is received byte
    if (m_wdata[7:0] !== 8'h7E) $fatal("[TB] ERROR: outbound mailbox payload mismatch 0x%0h (expected 0x7E)", m_wdata[7:0]);

    // Check parity bit in m_tag matches mailbox_pkg's compute_parity (inline struct literal)
    if (m_tag.parity !== compute_parity(m_wdata, '{src_id:8'h10, eop:1'b1, prio:1'b0, opcode:OPC_DATA, hops:4'h0, parity:1'b0})) $fatal("[TB] ERROR: parity mismatch: m_tag.parity=%0b expected=%0b", m_tag.parity, compute_parity(m_wdata, '{src_id:8'h10, eop:1'b1, prio:1'b0, opcode:OPC_DATA, hops:4'h0, parity:1'b0}));

    // verify that AW+W are asserted together (simple correctness check)
    if (!m_awvalid || !m_wvalid) $fatal("[TB] ERROR: outbound write not valid on both AW/W channels (aw=%0b w=%0b)", m_awvalid, m_wvalid);

    #200;
    $display("[TB] UART Mailbox tests complete");
    $finish;
  end

  // helper tasks
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
      // wait for bvalid
      wait (s_bvalid == 1);
      @(posedge clk);
      // s_bready is tied high; bvalid will clear when endpoint deasserts it
      $display("[TB] do_write: wrote 0x%0h to addr 0x%0h", data, addr);
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
