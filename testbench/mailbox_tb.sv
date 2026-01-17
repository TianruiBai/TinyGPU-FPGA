`timescale 1ns/1ps

// Simple unit testbench for mailbox interconnect (center + switch + endpoints)
module mailbox_tb;
  import mailbox_pkg::*;

  // Clock / reset
  logic clk;
  logic rst_n;
  initial clk = 1'b0;
  always #1 clk = ~clk;

  initial begin
    rst_n = 1'b0;
    repeat (10) @(posedge clk);
    rst_n = 1'b1;
  end

  // -----------------------------
  // DUT instances and wiring (nets created and connected explicitly)
  // -----------------------------

  // nets for center<->sw0 uplink
  logic sw0_m_up_wb_cyc; logic sw0_m_up_wb_stb; logic sw0_m_up_wb_we;
  logic [15:0] sw0_m_up_wb_adr; logic [31:0] sw0_m_up_wb_dat; logic [3:0] sw0_m_up_wb_sel; mailbox_pkg::mailbox_tag_t sw0_m_up_tag; logic sw0_m_up_wb_ack;

  logic center_m_sw0_wb_cyc; logic center_m_sw0_wb_stb; logic center_m_sw0_wb_we;
  logic [15:0] center_m_sw0_wb_adr; logic [31:0] center_m_sw0_wb_dat; logic [3:0] center_m_sw0_wb_sel; mailbox_pkg::mailbox_tag_t center_m_sw0_tag; logic center_m_sw0_wb_ack;

  // nets for sw0 <-> ep0 (dl0)
  logic ep0_m_wb_cyc; logic ep0_m_wb_stb; logic ep0_m_wb_we; logic [15:0] ep0_m_wb_adr; logic [31:0] ep0_m_wb_dat; logic [3:0] ep0_m_wb_sel; mailbox_pkg::mailbox_tag_t ep0_m_wb_tag; logic ep0_m_wb_ack;
  logic m_dl0_wb_cyc; logic m_dl0_wb_stb; logic m_dl0_wb_we; logic [15:0] m_dl0_wb_adr; logic [31:0] m_dl0_wb_dat; logic [3:0] m_dl0_wb_sel; mailbox_pkg::mailbox_tag_t m_dl0_tag; logic m_dl0_wb_ack;

  // nets for sw0 <-> ep1 (dl1)
  logic ep1_m_wb_cyc; logic ep1_m_wb_stb; logic ep1_m_wb_we; logic [15:0] ep1_m_wb_adr; logic [31:0] ep1_m_wb_dat; logic [3:0] ep1_m_wb_sel; mailbox_pkg::mailbox_tag_t ep1_m_wb_tag; logic ep1_m_wb_ack;
  logic m_dl1_wb_cyc; logic m_dl1_wb_stb; logic m_dl1_wb_we; logic [15:0] m_dl1_wb_adr; logic [31:0] m_dl1_wb_dat; logic [3:0] m_dl1_wb_sel; mailbox_pkg::mailbox_tag_t m_dl1_tag; logic m_dl1_wb_ack;

  // nets for hp <-> center
  logic hp_m_wb_cyc; logic hp_m_wb_stb; logic hp_m_wb_we; logic [15:0] hp_m_wb_adr; logic [31:0] hp_m_wb_dat; logic [3:0] hp_m_wb_sel; mailbox_pkg::mailbox_tag_t hp_m_wb_tag; logic hp_m_wb_ack;
  logic center_m_hp_wb_cyc; logic center_m_hp_wb_stb; logic center_m_hp_wb_we; logic [15:0] center_m_hp_wb_adr; logic [31:0] center_m_hp_wb_dat; logic [3:0] center_m_hp_wb_sel; mailbox_pkg::mailbox_tag_t center_m_hp_tag; logic center_m_hp_wb_ack;

  // Center instantiation
  mailbox_center #(.CHILD_CLUSTER_ID('{8'h11, 8'h22, 8'h33, 8'h44}), .FIFO_DEPTH(4)) center (
    .clk(clk), .rst_n(rst_n),

    .sw0_wb_cyc(sw0_m_up_wb_cyc), .sw0_wb_stb(sw0_m_up_wb_stb), .sw0_wb_we(sw0_m_up_wb_we), .sw0_wb_adr(sw0_m_up_wb_adr), .sw0_wb_dat(sw0_m_up_wb_dat), .sw0_wb_sel(sw0_m_up_wb_sel), .sw0_tag(sw0_m_up_tag), .sw0_wb_ack(sw0_m_up_wb_ack),
    .sw1_wb_cyc(), .sw1_wb_stb(), .sw1_wb_we(), .sw1_wb_adr(), .sw1_wb_dat(), .sw1_wb_sel(), .sw1_tag(), .sw1_wb_ack(),
    .sw2_wb_cyc(), .sw2_wb_stb(), .sw2_wb_we(), .sw2_wb_adr(), .sw2_wb_dat(), .sw2_wb_sel(), .sw2_tag(), .sw2_wb_ack(),
    .sw3_wb_cyc(), .sw3_wb_stb(), .sw3_wb_we(), .sw3_wb_adr(), .sw3_wb_dat(), .sw3_wb_sel(), .sw3_tag(), .sw3_wb_ack(),

    .hp_wb_cyc(hp_m_wb_cyc), .hp_wb_stb(hp_m_wb_stb), .hp_wb_we(hp_m_wb_we), .hp_wb_adr(hp_m_wb_adr), .hp_wb_dat(hp_m_wb_dat), .hp_wb_sel(hp_m_wb_sel), .hp_tag(hp_m_wb_tag), .hp_wb_ack(hp_m_wb_ack),

    .m_sw0_wb_cyc(center_m_sw0_wb_cyc), .m_sw0_wb_stb(center_m_sw0_wb_stb), .m_sw0_wb_we(center_m_sw0_wb_we), .m_sw0_wb_adr(center_m_sw0_wb_adr), .m_sw0_wb_dat(center_m_sw0_wb_dat), .m_sw0_wb_sel(center_m_sw0_wb_sel), .m_sw0_tag(center_m_sw0_tag), .m_sw0_wb_ack(center_m_sw0_wb_ack),
    .m_sw1_wb_cyc(), .m_sw1_wb_stb(), .m_sw1_wb_we(), .m_sw1_wb_adr(), .m_sw1_wb_dat(), .m_sw1_wb_sel(), .m_sw1_tag(), .m_sw1_wb_ack(),
    .m_sw2_wb_cyc(), .m_sw2_wb_stb(), .m_sw2_wb_we(), .m_sw2_wb_adr(), .m_sw2_wb_dat(), .m_sw2_wb_sel(), .m_sw2_tag(), .m_sw2_wb_ack(),
    .m_sw3_wb_cyc(), .m_sw3_wb_stb(), .m_sw3_wb_we(), .m_sw3_wb_adr(), .m_sw3_wb_dat(), .m_sw3_wb_sel(), .m_sw3_tag(), .m_sw3_wb_ack(),

    .m_hp_wb_cyc(center_m_hp_wb_cyc), .m_hp_wb_stb(center_m_hp_wb_stb), .m_hp_wb_we(center_m_hp_wb_we), .m_hp_wb_adr(center_m_hp_wb_adr), .m_hp_wb_dat(center_m_hp_wb_dat), .m_hp_wb_sel(center_m_hp_wb_sel), .m_hp_tag(center_m_hp_tag), .m_hp_wb_ack(center_m_hp_wb_ack)
  );

  // Switch instance
  mailbox_switch_2x1 #(.MY_CLUSTER_ID(8'h11), .FIFO_DEPTH(4)) sw0 (
    .clk(clk), .rst_n(rst_n),

    .dl0_wb_cyc(ep0_m_wb_cyc), .dl0_wb_stb(ep0_m_wb_stb), .dl0_wb_we(ep0_m_wb_we), .dl0_wb_adr(ep0_m_wb_adr), .dl0_wb_dat(ep0_m_wb_dat), .dl0_wb_sel(ep0_m_wb_sel), .dl0_tag(ep0_m_wb_tag), .dl0_wb_ack(ep0_m_wb_ack),
    .dl1_wb_cyc(ep1_m_wb_cyc), .dl1_wb_stb(ep1_m_wb_stb), .dl1_wb_we(ep1_m_wb_we), .dl1_wb_adr(ep1_m_wb_adr), .dl1_wb_dat(ep1_m_wb_dat), .dl1_wb_sel(ep1_m_wb_sel), .dl1_tag(ep1_m_wb_tag), .dl1_wb_ack(ep1_m_wb_ack),

    .up_wb_cyc(center_m_sw0_wb_cyc), .up_wb_stb(center_m_sw0_wb_stb), .up_wb_we(center_m_sw0_wb_we), .up_wb_adr(center_m_sw0_wb_adr), .up_wb_dat(center_m_sw0_wb_dat), .up_wb_sel(center_m_sw0_wb_sel), .up_tag(center_m_sw0_tag), .up_wb_ack(center_m_sw0_wb_ack),

    .m_up_wb_cyc(sw0_m_up_wb_cyc), .m_up_wb_stb(sw0_m_up_wb_stb), .m_up_wb_we(sw0_m_up_wb_we), .m_up_wb_adr(sw0_m_up_wb_adr), .m_up_wb_dat(sw0_m_up_wb_dat), .m_up_wb_sel(sw0_m_up_wb_sel), .m_up_tag(sw0_m_up_tag), .m_up_wb_ack(sw0_m_up_wb_ack),

    .m_dl0_wb_cyc(m_dl0_wb_cyc), .m_dl0_wb_stb(m_dl0_wb_stb), .m_dl0_wb_we(m_dl0_wb_we), .m_dl0_wb_adr(m_dl0_wb_adr), .m_dl0_wb_dat(m_dl0_wb_dat), .m_dl0_wb_sel(m_dl0_wb_sel), .m_dl0_tag(m_dl0_tag), .m_dl0_wb_ack(m_dl0_wb_ack),
    .m_dl1_wb_cyc(m_dl1_wb_cyc), .m_dl1_wb_stb(m_dl1_wb_stb), .m_dl1_wb_we(m_dl1_wb_we), .m_dl1_wb_adr(m_dl1_wb_adr), .m_dl1_wb_dat(m_dl1_wb_dat), .m_dl1_wb_sel(m_dl1_wb_sel), .m_dl1_tag(m_dl1_tag), .m_dl1_wb_ack(m_dl1_wb_ack)
  );

  // Simple endpoints: ep0 (local id 0) and ep1 (local id 1)

  // -----------------------------
  // Endpoint signal wiring + Test helpers
  // -----------------------------
  // ep0 signals
  logic ep0_tx_valid, ep0_tx_prio, ep0_tx_eop; logic [15:0] ep0_tx_dest; logic [31:0] ep0_tx_data; logic [3:0] ep0_tx_opcode; logic ep0_tx_ready;
  logic ep0_rx_valid; logic [31:0] ep0_rx_data; mailbox_pkg::mailbox_tag_t ep0_rx_tag; logic ep0_rx_irq;

  // ep1 signals
  logic ep1_tx_valid, ep1_tx_prio, ep1_tx_eop; logic [15:0] ep1_tx_dest; logic [31:0] ep1_tx_data; logic [3:0] ep1_tx_opcode; logic ep1_tx_ready;
  logic ep1_rx_valid; logic [31:0] ep1_rx_data; mailbox_pkg::mailbox_tag_t ep1_rx_tag; logic ep1_rx_irq;

  // hp endpoint signals
  logic hp_tx_valid = 1'b0; logic hp_tx_ready; logic hp_rx_valid; logic [31:0] hp_rx_data; mailbox_pkg::mailbox_tag_t hp_rx_tag; logic hp_rx_irq;

  // We'll accept RX when ready is asserted
  logic ep0_rx_ready = 1'b1; logic ep1_rx_ready = 1'b1; logic hp_rx_ready = 1'b1;

  // connect endpoint I/O to these nets via instantiation below

  // -----------------------------
  // Endpoint instantiations (connected to nets)
  // -----------------------------
  mailbox_endpoint #(.SRC_ID(8'h10), .TX_DEPTH(8), .RX_DEPTH(8)) ep0 (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(ep0_tx_valid), .tx_ready(ep0_tx_ready), .tx_dest(ep0_tx_dest), .tx_data(ep0_tx_data), .tx_prio(ep0_tx_prio), .tx_eop(ep0_tx_eop), .tx_opcode(ep0_tx_opcode),
    .rx_valid(ep0_rx_valid), .rx_ready(ep0_rx_ready), .rx_data(ep0_rx_data), .rx_tag(ep0_rx_tag), .rx_irq(ep0_rx_irq),
    .m_wb_cyc(ep0_m_wb_cyc), .m_wb_stb(ep0_m_wb_stb), .m_wb_we(ep0_m_wb_we), .m_wb_adr(ep0_m_wb_adr), .m_wb_dat(ep0_m_wb_dat), .m_wb_sel(ep0_m_wb_sel), .m_wb_tag(ep0_m_wb_tag), .m_wb_ack(ep0_m_wb_ack),
    .s_wb_cyc(m_dl0_wb_cyc), .s_wb_stb(m_dl0_wb_stb), .s_wb_we(m_dl0_wb_we), .s_wb_adr(m_dl0_wb_adr), .s_wb_dat(m_dl0_wb_dat), .s_wb_sel(m_dl0_wb_sel), .s_wb_tag(m_dl0_tag), .s_wb_ack(m_dl0_wb_ack)
  );

  mailbox_endpoint #(.SRC_ID(8'h11), .TX_DEPTH(8), .RX_DEPTH(8)) ep1 (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(ep1_tx_valid), .tx_ready(ep1_tx_ready), .tx_dest(ep1_tx_dest), .tx_data(ep1_tx_data), .tx_prio(ep1_tx_prio), .tx_eop(ep1_tx_eop), .tx_opcode(ep1_tx_opcode),
    .rx_valid(ep1_rx_valid), .rx_ready(ep1_rx_ready), .rx_data(ep1_rx_data), .rx_tag(ep1_rx_tag), .rx_irq(ep1_rx_irq),
    .m_wb_cyc(ep1_m_wb_cyc), .m_wb_stb(ep1_m_wb_stb), .m_wb_we(ep1_m_wb_we), .m_wb_adr(ep1_m_wb_adr), .m_wb_dat(ep1_m_wb_dat), .m_wb_sel(ep1_m_wb_sel), .m_wb_tag(ep1_m_wb_tag), .m_wb_ack(ep1_m_wb_ack),
    .s_wb_cyc(m_dl1_wb_cyc), .s_wb_stb(m_dl1_wb_stb), .s_wb_we(m_dl1_wb_we), .s_wb_adr(m_dl1_wb_adr), .s_wb_dat(m_dl1_wb_dat), .s_wb_sel(m_dl1_wb_sel), .s_wb_tag(m_dl1_tag), .s_wb_ack(m_dl1_wb_ack)
  );

  mailbox_endpoint #(.SRC_ID(8'h01), .TX_DEPTH(8), .RX_DEPTH(8)) hp (
    .clk(clk), .rst_n(rst_n),
    .tx_valid(hp_tx_valid), .tx_ready(hp_tx_ready), .tx_dest(), .tx_data(), .tx_prio(), .tx_eop(), .tx_opcode(),
    .rx_valid(hp_rx_valid), .rx_ready(hp_rx_ready), .rx_data(hp_rx_data), .rx_tag(hp_rx_tag), .rx_irq(hp_rx_irq),
    .m_wb_cyc(hp_m_wb_cyc), .m_wb_stb(hp_m_wb_stb), .m_wb_we(hp_m_wb_we), .m_wb_adr(hp_m_wb_adr), .m_wb_dat(hp_m_wb_dat), .m_wb_sel(hp_m_wb_sel), .m_wb_tag(hp_m_wb_tag), .m_wb_ack(hp_m_wb_ack),
    .s_wb_cyc(center_m_hp_wb_cyc), .s_wb_stb(center_m_hp_wb_stb), .s_wb_we(center_m_hp_wb_we), .s_wb_adr(center_m_hp_wb_adr), .s_wb_dat(center_m_hp_wb_dat), .s_wb_sel(center_m_hp_wb_sel), .s_wb_tag(center_m_hp_tag), .s_wb_ack(center_m_hp_wb_ack)
  );
  // Helper: send single-beat packet from ep0
  task automatic send_ep0(input logic [15:0] dest, input logic [31:0] data, input logic prio = 1'b0);
    int i;
    begin
      @(posedge clk);
      ep0_tx_prio = prio;
      ep0_tx_eop  = 1'b1;
      ep0_tx_opcode = OPC_DATA;
      ep0_tx_dest = dest;
      ep0_tx_data = data;
      // wait until endpoint is ready
      ep0_tx_valid = 1'b1;
      wait (ep0.tx_ready == 1'b1);

      // Hold valid until we see m_wb_stb or timeout
      for (i = 0; i < 16; i++) begin
        @(posedge clk);
        if (ep0_m_wb_stb) begin
          $display("%0t: send_ep0 observed ep0_m_wb_stb (adr=%h dat=%h)", $time, ep0_m_wb_adr, ep0_m_wb_dat);
          ep0_tx_valid = 1'b0;
          break;
        end
      end
      if (ep0_m_wb_stb == 1'b0) begin
        ep0_tx_valid = 1'b0;
        $display("%0t: WARNING: ep0 did not assert m_wb_stb after send. wr_ptr=%0d rd_ptr=%0d tx_r_empty=%0b u_tx_fifo.w_en=%0b u_tx_fifo.w_full=%0b", $time, ep0.u_tx_fifo.wr_ptr, ep0.u_tx_fifo.rd_ptr, ep0.tx_r_empty, ep0.u_tx_fifo.w_en, ep0.u_tx_fifo.w_full);
      end
    end
  endtask

  // Helper: read one from endpoint by id (0=ep0,1=ep1,2=hp) (blocking) with timeout
  task automatic expect_rx_by_id(input int id, input logic [31:0] exp, input int timeout_cycles=200);
    int tcnt;
    logic received;
    logic [31:0] val;
    begin
      $display("%0t: expect_rx_by_id start id=%0d exp=0x%08x", $time, id, exp);
      tcnt = 0;
      received = 1'b0;

      // First, check current cycle in case rx_valid was asserted before we entered
      case (id)
        0: if (ep0_rx_valid) begin val = ep0_rx_data; received = 1'b1; end
        1: if (ep1_rx_valid) begin val = ep1_rx_data; received = 1'b1; end
        default: if (hp_rx_valid) begin val = hp_rx_data; received = 1'b1; end
      endcase

      while ((tcnt < timeout_cycles) && !received) begin
        @(posedge clk);
        case (id)
          0: if (ep0_rx_valid) begin val = ep0_rx_data; received = 1'b1; end
          1: if (ep1_rx_valid) begin val = ep1_rx_data; received = 1'b1; end
          default: if (hp_rx_valid) begin val = hp_rx_data; received = 1'b1; end
        endcase
        tcnt++;
      end

      if (!received) begin
        $display("[FAIL] id=%0d did not receive data within timeout", id);
        $finish;
      end

      $display("%0t: expect_rx_by_id got id=%0d val=0x%08x", $time, id, val);

      if (val !== exp) begin
        $display("[FAIL] id=%0d received 0x%08x (expected 0x%08x)", id, val, exp);
        $finish;
      end else begin
        $display("[PASS] id=%0d received expected 0x%08x", id, exp);
      end

      // allow one cycle for FIFO pop (rx_ready tied high)
      @(posedge clk);
    end
  endtask

  // -----------------------------
  // Wave dump + simple monitors
  // -----------------------------
  initial begin
    $dumpfile("mailbox_tb.vcd");
    $dumpvars(0, mailbox_tb);
  end

  // Small monitors to help debugging
  always @(posedge clk) begin
    if (ep0_tx_valid) $display("%0t: ep0_tx_valid asserted (tx_ready=%0b)", $time, ep0_tx_ready);
    if (ep0.tx_w_en) $display("%0t: ep0 tx_w_en (tx_w_full=%0b) wr_ptr=%0d rd_ptr=%0d tx_r_empty=%0b u_tx_fifo.w_en=%0b u_tx_fifo.w_full=%0b", $time, ep0.tx_w_full, ep0.u_tx_fifo.wr_ptr, ep0.u_tx_fifo.rd_ptr, ep0.tx_r_empty, ep0.u_tx_fifo.w_en, ep0.u_tx_fifo.w_full);
    if (ep0.tx_r_en) $display("%0t: ep0 tx_r_en (m_wb_ack=%0b) wr_ptr=%0d rd_ptr=%0d tx_r_empty=%0b u_tx_fifo.w_en=%0b u_tx_fifo.w_full=%0b", $time, ep0.m_wb_ack, ep0.u_tx_fifo.wr_ptr, ep0.u_tx_fifo.rd_ptr, ep0.tx_r_empty, ep0.u_tx_fifo.w_en, ep0.u_tx_fifo.w_full);
    if (ep0_m_wb_stb) $display("%0t: ep0 -> sw0 dl0 adr=%h dat=%h", $time, ep0_m_wb_adr, ep0_m_wb_dat);
    if (sw0.dl0_wb_stb) begin
      $display("%0t: sw0 got dl0 input adr=%h dat=%h", $time, ep0_m_wb_adr, ep0_m_wb_dat);
      $display("    sw0.sel_dl0=%0d sel_dl1=%0d grant_src=%b dl0_w_en=%0b dl1_w_en=%0b dl1_fifo_full=%0b", $time, sw0.sel_dl0, sw0.sel_dl1, sw0.grant_src, sw0.dl0_fifo_w_en, sw0.dl1_fifo_w_en, sw0.dl1_fifo_full);
    end
    if (sw0.m_up_wb_stb) $display("%0t: sw0 uplink -> center adr=%h dat=%h", $time, sw0.m_up_wb_adr, sw0.m_up_wb_dat);
    if (center.sw0_wb_stb) $display("%0t: center got sw0 uplink adr=%h dat=%h", $time, sw0.m_up_wb_adr, sw0.m_up_wb_dat);
    if (ep1_m_wb_stb) $display("%0t: ep1 -> sw0 dl1 adr=%h dat=%h", $time, ep1_m_wb_adr, ep1_m_wb_dat);
    if (sw0.m_dl1_wb_stb) $display("%0t: sw0 -> dl1 send adr=%h dat=%h", $time, m_dl1_wb_adr, m_dl1_wb_dat);
    if (sw0.m_dl0_wb_stb) $display("%0t: sw0 -> dl0 send adr=%h dat=%h", $time, m_dl0_wb_adr, m_dl0_wb_dat);
    if (center.m_sw0_wb_stb) $display("%0t: center -> sw0 adr=%h dat=%h", $time, center_m_sw0_wb_adr, center_m_sw0_wb_dat);
    if (ep1_rx_valid) $display("%0t: ep1 rx_valid data=%h", $time, ep1_rx_data);
    if (hp_rx_valid) $display("%0t: hp rx_valid data=%h", $time, hp_rx_data);
  end

  // -----------------------------
  // Test sequence
  // -----------------------------
  initial begin
    // Initialize inputs
    ep0_tx_valid = 1'b0; ep0_tx_prio = 1'b0; ep0_tx_eop = 1'b1; ep0_tx_dest = 16'h0; ep0_tx_data = 32'h0; ep0_tx_opcode = OPC_DATA;
    ep1_tx_valid = 1'b0; ep1_tx_prio = 1'b0; ep1_tx_eop = 1'b1; ep1_tx_dest = 16'h0; ep1_tx_data = 32'h0; ep1_tx_opcode = OPC_DATA;

    // Allow reset release
    @(posedge clk);
    @(posedge clk);

    // Test1: simple unicast from ep0 -> ep1 (local)
    $display("Test1: ep0 -> ep1 unicast");
    send_ep0({8'h11, 8'd1}, 32'hDEAD_BEEF);
    expect_rx_by_id(1, 32'hDEAD_BEEF);

    // Test2: ep0 -> HP (upstream) cluster 0x00
    $display("Test2: ep0 -> hp unicast");
    send_ep0({8'h00, 8'd0}, 32'hCAFE_BABE);
    expect_rx_by_id(2, 32'hCAFE_BABE);

    // Test3: broadcast from ep0 -> all (cluster == 0xFF)
    $display("Test3: ep0 -> broadcast");
    send_ep0({8'hFF, 8'h00}, 32'hF00D_F00D);
    expect_rx_by_id(1, 32'hF00D_F00D);
    expect_rx_by_id(2, 32'hF00D_F00D);

    $display("All tests passed.");
    $finish;
  end

endmodule
