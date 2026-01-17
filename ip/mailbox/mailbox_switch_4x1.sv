`timescale 1ns/1ps
// Mailbox Switch 4x1: four downlinks, one uplink with QoS + route-lock (AXI4-Lite full-duplex)
module mailbox_switch_4x1 #(
  parameter logic [7:0] MY_CLUSTER_ID = 8'h01,
  parameter int FIFO_DEPTH = 2
) (
  input  logic clk,
  input  logic rst_n,

  // Downlink AXI-Lite masters into this switch (from leaves)
  input  logic        dl0_awvalid,
  output logic        dl0_awready,
  input  logic [15:0] dl0_awaddr,
  input  logic        dl0_wvalid,
  output logic        dl0_wready,
  input  logic [31:0] dl0_wdata,
  input  logic [3:0]  dl0_wstrb,
  input  mailbox_pkg::mailbox_tag_t dl0_tag,
  output logic        dl0_bvalid,
  input  logic        dl0_bready,

  input  logic        dl0_arvalid,
  output logic        dl0_arready,
  input  logic [15:0] dl0_araddr,
  output logic        dl0_rvalid,
  input  logic        dl0_rready,
  output logic [31:0] dl0_rdata,

  input  logic        dl1_awvalid,
  output logic        dl1_awready,
  input  logic [15:0] dl1_awaddr,
  input  logic        dl1_wvalid,
  output logic        dl1_wready,
  input  logic [31:0] dl1_wdata,
  input  logic [3:0]  dl1_wstrb,
  input  mailbox_pkg::mailbox_tag_t dl1_tag,
  output logic        dl1_bvalid,
  input  logic        dl1_bready,

  input  logic        dl1_arvalid,
  output logic        dl1_arready,
  input  logic [15:0] dl1_araddr,
  output logic        dl1_rvalid,
  input  logic        dl1_rready,
  output logic [31:0] dl1_rdata,

  input  logic        dl2_awvalid,
  output logic        dl2_awready,
  input  logic [15:0] dl2_awaddr,
  input  logic        dl2_wvalid,
  output logic        dl2_wready,
  input  logic [31:0] dl2_wdata,
  input  logic [3:0]  dl2_wstrb,
  input  mailbox_pkg::mailbox_tag_t dl2_tag,
  output logic        dl2_bvalid,
  input  logic        dl2_bready,

  input  logic        dl2_arvalid,
  output logic        dl2_arready,
  input  logic [15:0] dl2_araddr,
  output logic        dl2_rvalid,
  input  logic        dl2_rready,
  output logic [31:0] dl2_rdata,

  input  logic        dl3_awvalid,
  output logic        dl3_awready,
  input  logic [15:0] dl3_awaddr,
  input  logic        dl3_wvalid,
  output logic        dl3_wready,
  input  logic [31:0] dl3_wdata,
  input  logic [3:0]  dl3_wstrb,
  input  mailbox_pkg::mailbox_tag_t dl3_tag,
  output logic        dl3_bvalid,
  input  logic        dl3_bready,

  input  logic        dl3_arvalid,
  output logic        dl3_arready,
  input  logic [15:0] dl3_araddr,
  output logic        dl3_rvalid,
  input  logic        dl3_rready,
  output logic [31:0] dl3_rdata,

  // Uplink AXI-Lite master into this switch (from center)
  input  logic        up_awvalid,
  output logic        up_awready,
  input  logic [15:0] up_awaddr,
  input  logic        up_wvalid,
  output logic        up_wready,
  input  logic [31:0] up_wdata,
  input  logic [3:0]  up_wstrb,
  input  mailbox_pkg::mailbox_tag_t up_tag,
  output logic        up_bvalid,
  input  logic        up_bready,

  input  logic        up_arvalid,
  output logic        up_arready,
  input  logic [15:0] up_araddr,
  output logic        up_rvalid,
  input  logic        up_rready,
  output logic [31:0] up_rdata,

  // Master out to uplink (toward center)
  output logic        m_up_awvalid,
  input  logic        m_up_awready,
  output logic [15:0] m_up_awaddr,
  output logic        m_up_wvalid,
  input  logic        m_up_wready,
  output logic [31:0] m_up_wdata,
  output logic [3:0]  m_up_wstrb,
  output mailbox_pkg::mailbox_tag_t m_up_tag,
  output logic        m_up_bready,
  input  logic        m_up_bvalid,

  output logic        m_up_arvalid,
  input  logic        m_up_arready,
  output logic [15:0] m_up_araddr,
  input  logic        m_up_rvalid,
  output logic        m_up_rready,
  input  logic [31:0] m_up_rdata,

  // Masters out to downlinks (toward leaves)
  output logic        m_dl0_awvalid,
  input  logic        m_dl0_awready,
  output logic [15:0] m_dl0_awaddr,
  output logic        m_dl0_wvalid,
  input  logic        m_dl0_wready,
  output logic [31:0] m_dl0_wdata,
  output logic [3:0]  m_dl0_wstrb,
  output mailbox_pkg::mailbox_tag_t m_dl0_tag,
  output logic        m_dl0_bready,
  input  logic        m_dl0_bvalid,

  output logic        m_dl0_arvalid,
  input  logic        m_dl0_arready,
  output logic [15:0] m_dl0_araddr,
  input  logic        m_dl0_rvalid,
  output logic        m_dl0_rready,
  input  logic [31:0] m_dl0_rdata,

  output logic        m_dl1_awvalid,
  input  logic        m_dl1_awready,
  output logic [15:0] m_dl1_awaddr,
  output logic        m_dl1_wvalid,
  input  logic        m_dl1_wready,
  output logic [31:0] m_dl1_wdata,
  output logic [3:0]  m_dl1_wstrb,
  output mailbox_pkg::mailbox_tag_t m_dl1_tag,
  output logic        m_dl1_bready,
  input  logic        m_dl1_bvalid,

  output logic        m_dl1_arvalid,
  input  logic        m_dl1_arready,
  output logic [15:0] m_dl1_araddr,
  input  logic        m_dl1_rvalid,
  output logic        m_dl1_rready,
  input  logic [31:0] m_dl1_rdata,

  output logic        m_dl2_awvalid,
  input  logic        m_dl2_awready,
  output logic [15:0] m_dl2_awaddr,
  output logic        m_dl2_wvalid,
  input  logic        m_dl2_wready,
  output logic [31:0] m_dl2_wdata,
  output logic [3:0]  m_dl2_wstrb,
  output mailbox_pkg::mailbox_tag_t m_dl2_tag,
  output logic        m_dl2_bready,
  input  logic        m_dl2_bvalid,

  output logic        m_dl2_arvalid,
  input  logic        m_dl2_arready,
  output logic [15:0] m_dl2_araddr,
  input  logic        m_dl2_rvalid,
  output logic        m_dl2_rready,
  input  logic [31:0] m_dl2_rdata,

  output logic        m_dl3_awvalid,
  input  logic        m_dl3_awready,
  output logic [15:0] m_dl3_awaddr,
  output logic        m_dl3_wvalid,
  input  logic        m_dl3_wready,
  output logic [31:0] m_dl3_wdata,
  output logic [3:0]  m_dl3_wstrb,
  output mailbox_pkg::mailbox_tag_t m_dl3_tag,
  output logic        m_dl3_bready,
  input  logic        m_dl3_bvalid,

  output logic        m_dl3_arvalid,
  input  logic        m_dl3_arready,
  output logic [15:0] m_dl3_araddr,
  input  logic        m_dl3_rvalid,
  output logic        m_dl3_rready,
  input  logic [31:0] m_dl3_rdata
);

  import mailbox_pkg::*;

  localparam int N_IN  = 5; // dl0..dl3, up
  localparam int N_OUT = 5; // dl0..dl3, up
  localparam int RR_W  = 3;

  typedef struct packed {
    logic [15:0] adr;
    logic [31:0] dat;
    logic [3:0]  strb;
    mailbox_tag_t tag;
  } flit_t;
  localparam int FLIT_W = $bits(flit_t);

  flit_t ingress_flit [N_IN];
  logic  ingress_req  [N_IN];
  logic  ingress_accept [N_IN];
  logic  resp_pending [N_IN];
  logic [N_OUT-1:0] ingress_tgt [N_IN];
  logic [N_IN-1:0] ingress_bready;

  assign ingress_bready = '{dl0_bready, dl1_bready, dl2_bready, dl3_bready, up_bready};

  typedef struct packed {
    logic lock_active;
    logic [$clog2(N_IN)-1:0] lock_src;
  } lock_t;

  lock_t lock_out [N_OUT];
  logic [RR_W-1:0] rr_out [N_OUT];
  logic [1:0] lat_ctr [N_OUT];

  // FIFO interfaces per output
  logic [N_OUT-1:0] fifo_w_en, fifo_full, fifo_r_en, fifo_empty;
  logic [N_OUT-1:0][FLIT_W-1:0] fifo_w_data, fifo_r_data;
  flit_t fifo_head [N_OUT];
  flit_t fifo_w_flit [N_OUT];

  // Bump hop count and parity before enqueue
  function automatic mailbox_tag_t update_tag(
    input mailbox_tag_t in_tag,
    input logic [31:0] data
  );
    mailbox_tag_t t;
    mailbox_tag_t t_np;
    begin
      t = in_tag;
      if (t.hops != 4'hF) t.hops = t.hops + 1'b1;
      t_np = t;
      t_np.parity = 1'b0;
      t.parity = compute_parity(data, t_np);
      update_tag = t;
    end
  endfunction

  function automatic logic [N_OUT-1:0] decode_targets(
    input logic [15:0] adr,
    input logic        from_uplink
  );
    logic [7:0] cluster;
    logic [7:0] dest_local;
    logic [N_OUT-1:0] tgt;
    begin
      cluster = adr[15:8];
      dest_local = adr[7:0];
      tgt = '0;
      if (cluster == 8'hFF) begin
        tgt = from_uplink ? 5'b01111 : 5'b11111; // avoid sending back up when already from uplink
      end else if (cluster == MY_CLUSTER_ID) begin
        if (dest_local == 8'hFF) tgt = 5'b01111; // cluster broadcast
        else if (dest_local < 8'd4) tgt[dest_local] = 1'b1;
      end else begin
        tgt = from_uplink ? '0 : 5'b10000; // locals send upstream only
      end
      decode_targets = tgt;
    end
  endfunction

  // QoS arbiter: BE min service (1-in-4), hop tiebreak, RR among equals
  function automatic int pick_src(
    input lock_t lock,
    input logic [RR_W-1:0] rr,
    input logic [1:0] lat_ctr_in,
    input logic [N_IN-1:0] req,
    input logic [N_IN-1:0] prio,
    input logic [N_IN-1:0][3:0] hops
  );
    int sel;
    logic any_lat, any_be, force_be;
    int idx;
    logic [3:0] best_hops;
    begin
      if (lock.lock_active) begin
        pick_src = lock.lock_src;
      end else begin
        any_lat = |(req & prio);
        any_be  = |(req & ~prio);
        force_be = (lat_ctr_in == 2'd3) && any_be;
        sel = -1;
        best_hops = 4'd0;
        for (int k = 0; k < N_IN; k++) begin
          idx = rr + k;
          if (idx >= N_IN) idx = idx - N_IN;
          if (!req[idx]) continue;
          if (force_be && prio[idx]) continue;
          if (!force_be && any_lat && !prio[idx]) continue;
          if ((sel == -1) || (hops[idx] > best_hops)) begin
            sel = idx;
            best_hops = hops[idx];
          end
        end
        pick_src = sel;
      end
    end
  endfunction

  // Ingress build
  always_comb begin
    ingress_req = '{default:1'b0};
    ingress_tgt = '{default:'0};
    ingress_flit = '{default:'0};

    ingress_req[0] = dl0_awvalid && dl0_wvalid && !resp_pending[0];
    ingress_req[1] = dl1_awvalid && dl1_wvalid && !resp_pending[1];
    ingress_req[2] = dl2_awvalid && dl2_wvalid && !resp_pending[2];
    ingress_req[3] = dl3_awvalid && dl3_wvalid && !resp_pending[3];
    ingress_req[4] = up_awvalid  && up_wvalid  && !resp_pending[4];

    ingress_tgt[0] = decode_targets(dl0_awaddr, 1'b0);
    ingress_tgt[1] = decode_targets(dl1_awaddr, 1'b0);
    ingress_tgt[2] = decode_targets(dl2_awaddr, 1'b0);
    ingress_tgt[3] = decode_targets(dl3_awaddr, 1'b0);
    ingress_tgt[4] = decode_targets(up_awaddr,  1'b1);

    ingress_flit[0] = '{adr:dl0_awaddr, dat:dl0_wdata, strb:dl0_wstrb, tag:update_tag(dl0_tag, dl0_wdata)};
    ingress_flit[1] = '{adr:dl1_awaddr, dat:dl1_wdata, strb:dl1_wstrb, tag:update_tag(dl1_tag, dl1_wdata)};
    ingress_flit[2] = '{adr:dl2_awaddr, dat:dl2_wdata, strb:dl2_wstrb, tag:update_tag(dl2_tag, dl2_wdata)};
    ingress_flit[3] = '{adr:dl3_awaddr, dat:dl3_wdata, strb:dl3_wstrb, tag:update_tag(dl3_tag, dl3_wdata)};
    ingress_flit[4] = '{adr:up_awaddr,  dat:up_wdata,  strb:up_wstrb,  tag:update_tag(up_tag,  up_wdata)};
  end

  // Per-output arbitration
  logic [$clog2(N_IN)-1:0] sel_out [N_OUT];
  logic [N_IN-1:0] req_matrix [N_OUT];
  logic [N_IN-1:0] prio_matrix [N_OUT];
  logic [N_IN-1:0][3:0] hops_matrix [N_OUT];

  always_comb begin
    // Default matrices
    for (int o = 0; o < N_OUT; o++) begin
      req_matrix[o]  = '0;
      prio_matrix[o] = '0;
      hops_matrix[o] = '{default:4'd0};
      sel_out[o]     = {$clog2(N_IN){1'b1}}; // none
    end

    // Build request/prio/hops per output, respecting FIFO space
    for (int i = 0; i < N_IN; i++) begin
      for (int o = 0; o < N_OUT; o++) begin
        if (ingress_req[i] && ingress_tgt[i][o] && !fifo_full[o]) begin
          req_matrix[o][i]  = 1'b1;
          prio_matrix[o][i] = ingress_flit[i].tag.prio;
          hops_matrix[o][i] = ingress_flit[i].tag.hops;
        end
      end
    end

    for (int o = 0; o < N_OUT; o++) begin
      sel_out[o] = pick_src(lock_out[o], rr_out[o], lat_ctr[o], req_matrix[o], prio_matrix[o], hops_matrix[o]);
    end
  end

  // Grant only if every targeted output selected this source
  logic grant_src [N_IN];
  always_comb begin
    logic has_all;
    for (int i = 0; i < N_IN; i++) grant_src[i] = 1'b0;
    for (int i = 0; i < N_IN; i++) begin
      if (!ingress_req[i]) continue;
      has_all = 1'b1;
      for (int o = 0; o < N_OUT; o++) begin
        if (ingress_tgt[i][o]) begin
          if (sel_out[o] != i[$clog2(N_IN)-1:0]) has_all = 1'b0;
        end
      end
      grant_src[i] = has_all && (ingress_tgt[i] != '0);
    end
  end

  // FIFO write enables; assert ready when all targets granted
  always_comb begin
    fifo_w_en     = '0;
    fifo_w_data   = '{default:'0};
    ingress_accept = '{default:1'b0};

    dl0_awready = 1'b0; dl0_wready = 1'b0; dl0_bvalid = resp_pending[0];
    dl1_awready = 1'b0; dl1_wready = 1'b0; dl1_bvalid = resp_pending[1];
    dl2_awready = 1'b0; dl2_wready = 1'b0; dl2_bvalid = resp_pending[2];
    dl3_awready = 1'b0; dl3_wready = 1'b0; dl3_bvalid = resp_pending[3];
    up_awready  = 1'b0; up_wready  = 1'b0; up_bvalid  = resp_pending[4];

    for (int i = 0; i < N_IN; i++) begin
      if (!grant_src[i] || !ingress_req[i]) continue;
      ingress_accept[i] = 1'b1;
      for (int o = 0; o < N_OUT; o++) begin
        if (ingress_tgt[i][o]) begin
          fifo_w_en[o]   = 1'b1;
          fifo_w_data[o] = ingress_flit[i];
        end
      end
      case (i)
        0: begin dl0_awready = 1'b1; dl0_wready = 1'b1; end
        1: begin dl1_awready = 1'b1; dl1_wready = 1'b1; end
        2: begin dl2_awready = 1'b1; dl2_wready = 1'b1; end
        3: begin dl3_awready = 1'b1; dl3_wready = 1'b1; end
        default: begin up_awready = 1'b1; up_wready = 1'b1; end
      endcase
    end
  end

  // Track outstanding B responses per ingress
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      resp_pending <= '{default:1'b0};
    end else begin
      for (int i = 0; i < N_IN; i++) begin
        if (resp_pending[i] && ingress_bready[i]) resp_pending[i] <= 1'b0;
        else if (ingress_accept[i]) resp_pending[i] <= 1'b1;
      end
    end
  end

  // FIFOs
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl0_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[0]), .w_data(fifo_w_data[0]), .w_full(fifo_full[0]),
    .r_en(fifo_r_en[0]), .r_data(fifo_r_data[0]), .r_empty(fifo_empty[0])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl1_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[1]), .w_data(fifo_w_data[1]), .w_full(fifo_full[1]),
    .r_en(fifo_r_en[1]), .r_data(fifo_r_data[1]), .r_empty(fifo_empty[1])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl2_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[2]), .w_data(fifo_w_data[2]), .w_full(fifo_full[2]),
    .r_en(fifo_r_en[2]), .r_data(fifo_r_data[2]), .r_empty(fifo_empty[2])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl3_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[3]), .w_data(fifo_w_data[3]), .w_full(fifo_full[3]),
    .r_en(fifo_r_en[3]), .r_data(fifo_r_data[3]), .r_empty(fifo_empty[3])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_up_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[4]), .w_data(fifo_w_data[4]), .w_full(fifo_full[4]),
    .r_en(fifo_r_en[4]), .r_data(fifo_r_data[4]), .r_empty(fifo_empty[4])
  );

  assign fifo_head[0] = flit_t'(fifo_r_data[0]);
  assign fifo_head[1] = flit_t'(fifo_r_data[1]);
  assign fifo_head[2] = flit_t'(fifo_r_data[2]);
  assign fifo_head[3] = flit_t'(fifo_r_data[3]);
  assign fifo_head[4] = flit_t'(fifo_r_data[4]);

  assign fifo_w_flit[0] = flit_t'(fifo_w_data[0]);
  assign fifo_w_flit[1] = flit_t'(fifo_w_data[1]);
  assign fifo_w_flit[2] = flit_t'(fifo_w_data[2]);
  assign fifo_w_flit[3] = flit_t'(fifo_w_data[3]);
  assign fifo_w_flit[4] = flit_t'(fifo_w_data[4]);

  // Drive outbound AXI-Lite from FIFO heads
  assign m_dl0_awvalid = !fifo_empty[0];
  assign m_dl0_wvalid  = !fifo_empty[0];
  assign m_dl0_awaddr  = fifo_head[0].adr;
  assign m_dl0_wdata   = fifo_head[0].dat;
  assign m_dl0_wstrb   = fifo_head[0].strb;
  assign m_dl0_tag     = fifo_head[0].tag;
  assign m_dl0_bready  = 1'b1;

  assign m_dl1_awvalid = !fifo_empty[1];
  assign m_dl1_wvalid  = !fifo_empty[1];
  assign m_dl1_awaddr  = fifo_head[1].adr;
  assign m_dl1_wdata   = fifo_head[1].dat;
  assign m_dl1_wstrb   = fifo_head[1].strb;
  assign m_dl1_tag     = fifo_head[1].tag;
  assign m_dl1_bready  = 1'b1;

  assign m_dl2_awvalid = !fifo_empty[2];
  assign m_dl2_wvalid  = !fifo_empty[2];
  assign m_dl2_awaddr  = fifo_head[2].adr;
  assign m_dl2_wdata   = fifo_head[2].dat;
  assign m_dl2_wstrb   = fifo_head[2].strb;
  assign m_dl2_tag     = fifo_head[2].tag;
  assign m_dl2_bready  = 1'b1;

  assign m_dl3_awvalid = !fifo_empty[3];
  assign m_dl3_wvalid  = !fifo_empty[3];
  assign m_dl3_awaddr  = fifo_head[3].adr;
  assign m_dl3_wdata   = fifo_head[3].dat;
  assign m_dl3_wstrb   = fifo_head[3].strb;
  assign m_dl3_tag     = fifo_head[3].tag;
  assign m_dl3_bready  = 1'b1;

  assign m_up_awvalid = !fifo_empty[4];
  assign m_up_wvalid  = !fifo_empty[4];
  assign m_up_awaddr  = fifo_head[4].adr;
  assign m_up_wdata   = fifo_head[4].dat;
  assign m_up_wstrb   = fifo_head[4].strb;
  assign m_up_tag     = fifo_head[4].tag;
  assign m_up_bready  = 1'b1;

  assign fifo_r_en[0] = !fifo_empty[0] && m_dl0_awready && m_dl0_wready;
  assign fifo_r_en[1] = !fifo_empty[1] && m_dl1_awready && m_dl1_wready;
  assign fifo_r_en[2] = !fifo_empty[2] && m_dl2_awready && m_dl2_wready;
  assign fifo_r_en[3] = !fifo_empty[3] && m_dl3_awready && m_dl3_wready;
  assign fifo_r_en[4] = !fifo_empty[4] && m_up_awready  && m_up_wready;

  // ---------------------------------------------------------------------------
  // AR/R channel (one outstanding per ingress; disallow broadcast reads)
  // ---------------------------------------------------------------------------
  logic [N_IN-1:0] ar_req;
  logic [N_IN-1:0][N_OUT-1:0] ar_tgt;
  logic [N_IN-1:0] ar_req_masked;
  logic [N_OUT-1:0] ar_busy_out;
  logic [N_OUT-1:0][$clog2(N_IN)-1:0] ar_pending_src;
  logic [N_IN-1:0] ingress_ar_busy;
  int ar_sel [N_OUT];
  logic [N_IN-1:0] ingress_rvalid;
  logic [N_IN-1:0][31:0] ingress_rdata;

  function automatic logic one_hot5(input logic [N_OUT-1:0] v);
    int c;
    begin
      c = 0;
      for (int k = 0; k < N_OUT; k++) c += v[k];
      one_hot5 = (c == 1);
    end
  endfunction

  assign ar_req[0] = dl0_arvalid && !ingress_ar_busy[0];
  assign ar_req[1] = dl1_arvalid && !ingress_ar_busy[1];
  assign ar_req[2] = dl2_arvalid && !ingress_ar_busy[2];
  assign ar_req[3] = dl3_arvalid && !ingress_ar_busy[3];
  assign ar_req[4] = up_arvalid  && !ingress_ar_busy[4];

  assign ar_tgt[0] = decode_targets(dl0_araddr, 1'b0);
  assign ar_tgt[1] = decode_targets(dl1_araddr, 1'b0);
  assign ar_tgt[2] = decode_targets(dl2_araddr, 1'b0);
  assign ar_tgt[3] = decode_targets(dl3_araddr, 1'b0);
  assign ar_tgt[4] = decode_targets(up_araddr,  1'b1);

  for (genvar i = 0; i < N_IN; i++) begin : g_ar_mask
    assign ar_req_masked[i] = ar_req[i] && one_hot5(ar_tgt[i]);
  end

  function automatic int pick_ar_src(
    input logic [N_IN-1:0] req_m,
    input logic [N_IN-1:0][N_OUT-1:0] tgt,
    input int o
  );
    begin
      pick_ar_src = -1;
      for (int i = 0; i < N_IN; i++) begin
        if (req_m[i] && tgt[i][o]) begin
          pick_ar_src = i;
          break;
        end
      end
    end
  endfunction

  always_comb begin
    for (int o = 0; o < N_OUT; o++) begin
      if (ar_busy_out[o]) ar_sel[o] = -1;
      else ar_sel[o] = pick_ar_src(ar_req_masked, ar_tgt, o);
    end
  end

  assign m_dl0_arvalid = (ar_sel[0] != -1);
  assign m_dl0_araddr  = (ar_sel[0] == 0) ? dl0_araddr : (ar_sel[0] == 1) ? dl1_araddr : (ar_sel[0] == 2) ? dl2_araddr : (ar_sel[0] == 3) ? dl3_araddr : up_araddr;
  assign m_dl1_arvalid = (ar_sel[1] != -1);
  assign m_dl1_araddr  = (ar_sel[1] == 0) ? dl0_araddr : (ar_sel[1] == 1) ? dl1_araddr : (ar_sel[1] == 2) ? dl2_araddr : (ar_sel[1] == 3) ? dl3_araddr : up_araddr;
  assign m_dl2_arvalid = (ar_sel[2] != -1);
  assign m_dl2_araddr  = (ar_sel[2] == 0) ? dl0_araddr : (ar_sel[2] == 1) ? dl1_araddr : (ar_sel[2] == 2) ? dl2_araddr : (ar_sel[2] == 3) ? dl3_araddr : up_araddr;
  assign m_dl3_arvalid = (ar_sel[3] != -1);
  assign m_dl3_araddr  = (ar_sel[3] == 0) ? dl0_araddr : (ar_sel[3] == 1) ? dl1_araddr : (ar_sel[3] == 2) ? dl2_araddr : (ar_sel[3] == 3) ? dl3_araddr : up_araddr;
  assign m_up_arvalid  = (ar_sel[4] != -1);
  assign m_up_araddr   = (ar_sel[4] == 0) ? dl0_araddr : (ar_sel[4] == 1) ? dl1_araddr : (ar_sel[4] == 2) ? dl2_araddr : (ar_sel[4] == 3) ? dl3_araddr : up_araddr;

  assign dl0_arready = (!ingress_ar_busy[0]) &&
                       ((ar_sel[0]==0 && m_dl0_arready) || (ar_sel[1]==0 && m_dl1_arready) || (ar_sel[2]==0 && m_dl2_arready) || (ar_sel[3]==0 && m_dl3_arready) || (ar_sel[4]==0 && m_up_arready));
  assign dl1_arready = (!ingress_ar_busy[1]) &&
                       ((ar_sel[0]==1 && m_dl0_arready) || (ar_sel[1]==1 && m_dl1_arready) || (ar_sel[2]==1 && m_dl2_arready) || (ar_sel[3]==1 && m_dl3_arready) || (ar_sel[4]==1 && m_up_arready));
  assign dl2_arready = (!ingress_ar_busy[2]) &&
                       ((ar_sel[0]==2 && m_dl0_arready) || (ar_sel[1]==2 && m_dl1_arready) || (ar_sel[2]==2 && m_dl2_arready) || (ar_sel[3]==2 && m_dl3_arready) || (ar_sel[4]==2 && m_up_arready));
  assign dl3_arready = (!ingress_ar_busy[3]) &&
                       ((ar_sel[0]==3 && m_dl0_arready) || (ar_sel[1]==3 && m_dl1_arready) || (ar_sel[2]==3 && m_dl2_arready) || (ar_sel[3]==3 && m_dl3_arready) || (ar_sel[4]==3 && m_up_arready));
  assign up_arready  = (!ingress_ar_busy[4]) &&
                       ((ar_sel[0]==4 && m_dl0_arready) || (ar_sel[1]==4 && m_dl1_arready) || (ar_sel[2]==4 && m_dl2_arready) || (ar_sel[3]==4 && m_dl3_arready) || (ar_sel[4]==4 && m_up_arready));

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      ar_busy_out <= '0;
      for (int o = 0; o < N_OUT; o++) ar_pending_src[o] <= '0;
      for (int i = 0; i < N_IN; i++) ingress_ar_busy[i] <= 1'b0;
    end else begin
      if (m_dl0_rvalid && m_dl0_rready) ar_busy_out[0] <= 1'b0;
      if (m_dl1_rvalid && m_dl1_rready) ar_busy_out[1] <= 1'b0;
      if (m_dl2_rvalid && m_dl2_rready) ar_busy_out[2] <= 1'b0;
      if (m_dl3_rvalid && m_dl3_rready) ar_busy_out[3] <= 1'b0;
      if (m_up_rvalid  && m_up_rready)  ar_busy_out[4] <= 1'b0;

      for (int i = 0; i < N_IN; i++) begin
        if (ingress_rvalid[i] && ((i==0 && dl0_rready) || (i==1 && dl1_rready) || (i==2 && dl2_rready) || (i==3 && dl3_rready) || (i==4 && up_rready))) begin
          ingress_ar_busy[i] <= 1'b0;
        end
      end

      if (m_dl0_arvalid && m_dl0_arready) begin
        ar_busy_out[0] <= 1'b1;
        ar_pending_src[0] <= ar_sel[0][$clog2(N_IN)-1:0];
        ingress_ar_busy[ar_sel[0]] <= 1'b1;
      end
      if (m_dl1_arvalid && m_dl1_arready) begin
        ar_busy_out[1] <= 1'b1;
        ar_pending_src[1] <= ar_sel[1][$clog2(N_IN)-1:0];
        ingress_ar_busy[ar_sel[1]] <= 1'b1;
      end
      if (m_dl2_arvalid && m_dl2_arready) begin
        ar_busy_out[2] <= 1'b1;
        ar_pending_src[2] <= ar_sel[2][$clog2(N_IN)-1:0];
        ingress_ar_busy[ar_sel[2]] <= 1'b1;
      end
      if (m_dl3_arvalid && m_dl3_arready) begin
        ar_busy_out[3] <= 1'b1;
        ar_pending_src[3] <= ar_sel[3][$clog2(N_IN)-1:0];
        ingress_ar_busy[ar_sel[3]] <= 1'b1;
      end
      if (m_up_arvalid && m_up_arready) begin
        ar_busy_out[4] <= 1'b1;
        ar_pending_src[4] <= ar_sel[4][$clog2(N_IN)-1:0];
        ingress_ar_busy[ar_sel[4]] <= 1'b1;
      end
    end
  end

  always_comb begin
    ingress_rvalid = '{default:1'b0};
    ingress_rdata  = '{default:32'h0};

    if (m_dl0_rvalid && ar_busy_out[0]) begin
      case (ar_pending_src[0])
        0: begin ingress_rvalid[0]=1'b1; ingress_rdata[0]=m_dl0_rdata; end
        1: begin ingress_rvalid[1]=1'b1; ingress_rdata[1]=m_dl0_rdata; end
        2: begin ingress_rvalid[2]=1'b1; ingress_rdata[2]=m_dl0_rdata; end
        3: begin ingress_rvalid[3]=1'b1; ingress_rdata[3]=m_dl0_rdata; end
        4: begin ingress_rvalid[4]=1'b1; ingress_rdata[4]=m_dl0_rdata; end
        default: ;
      endcase
    end
    if (m_dl1_rvalid && ar_busy_out[1]) begin
      case (ar_pending_src[1])
        0: begin ingress_rvalid[0]=1'b1; ingress_rdata[0]=m_dl1_rdata; end
        1: begin ingress_rvalid[1]=1'b1; ingress_rdata[1]=m_dl1_rdata; end
        2: begin ingress_rvalid[2]=1'b1; ingress_rdata[2]=m_dl1_rdata; end
        3: begin ingress_rvalid[3]=1'b1; ingress_rdata[3]=m_dl1_rdata; end
        4: begin ingress_rvalid[4]=1'b1; ingress_rdata[4]=m_dl1_rdata; end
        default: ;
      endcase
    end
    if (m_dl2_rvalid && ar_busy_out[2]) begin
      case (ar_pending_src[2])
        0: begin ingress_rvalid[0]=1'b1; ingress_rdata[0]=m_dl2_rdata; end
        1: begin ingress_rvalid[1]=1'b1; ingress_rdata[1]=m_dl2_rdata; end
        2: begin ingress_rvalid[2]=1'b1; ingress_rdata[2]=m_dl2_rdata; end
        3: begin ingress_rvalid[3]=1'b1; ingress_rdata[3]=m_dl2_rdata; end
        4: begin ingress_rvalid[4]=1'b1; ingress_rdata[4]=m_dl2_rdata; end
        default: ;
      endcase
    end
    if (m_dl3_rvalid && ar_busy_out[3]) begin
      case (ar_pending_src[3])
        0: begin ingress_rvalid[0]=1'b1; ingress_rdata[0]=m_dl3_rdata; end
        1: begin ingress_rvalid[1]=1'b1; ingress_rdata[1]=m_dl3_rdata; end
        2: begin ingress_rvalid[2]=1'b1; ingress_rdata[2]=m_dl3_rdata; end
        3: begin ingress_rvalid[3]=1'b1; ingress_rdata[3]=m_dl3_rdata; end
        4: begin ingress_rvalid[4]=1'b1; ingress_rdata[4]=m_dl3_rdata; end
        default: ;
      endcase
    end
    if (m_up_rvalid && ar_busy_out[4]) begin
      case (ar_pending_src[4])
        0: begin ingress_rvalid[0]=1'b1; ingress_rdata[0]=m_up_rdata; end
        1: begin ingress_rvalid[1]=1'b1; ingress_rdata[1]=m_up_rdata; end
        2: begin ingress_rvalid[2]=1'b1; ingress_rdata[2]=m_up_rdata; end
        3: begin ingress_rvalid[3]=1'b1; ingress_rdata[3]=m_up_rdata; end
        4: begin ingress_rvalid[4]=1'b1; ingress_rdata[4]=m_up_rdata; end
        default: ;
      endcase
    end
  end

  assign dl0_rvalid = ingress_rvalid[0];
  assign dl1_rvalid = ingress_rvalid[1];
  assign dl2_rvalid = ingress_rvalid[2];
  assign dl3_rvalid = ingress_rvalid[3];
  assign up_rvalid  = ingress_rvalid[4];

  assign dl0_rdata  = ingress_rdata[0];
  assign dl1_rdata  = ingress_rdata[1];
  assign dl2_rdata  = ingress_rdata[2];
  assign dl3_rdata  = ingress_rdata[3];
  assign up_rdata   = ingress_rdata[4];

  assign m_dl0_rready = ar_busy_out[0] && ((ar_pending_src[0]==0 && dl0_rready) || (ar_pending_src[0]==1 && dl1_rready) || (ar_pending_src[0]==2 && dl2_rready) || (ar_pending_src[0]==3 && dl3_rready) || (ar_pending_src[0]==4 && up_rready));
  assign m_dl1_rready = ar_busy_out[1] && ((ar_pending_src[1]==0 && dl0_rready) || (ar_pending_src[1]==1 && dl1_rready) || (ar_pending_src[1]==2 && dl2_rready) || (ar_pending_src[1]==3 && dl3_rready) || (ar_pending_src[1]==4 && up_rready));
  assign m_dl2_rready = ar_busy_out[2] && ((ar_pending_src[2]==0 && dl0_rready) || (ar_pending_src[2]==1 && dl1_rready) || (ar_pending_src[2]==2 && dl2_rready) || (ar_pending_src[2]==3 && dl3_rready) || (ar_pending_src[2]==4 && up_rready));
  assign m_dl3_rready = ar_busy_out[3] && ((ar_pending_src[3]==0 && dl0_rready) || (ar_pending_src[3]==1 && dl1_rready) || (ar_pending_src[3]==2 && dl2_rready) || (ar_pending_src[3]==3 && dl3_rready) || (ar_pending_src[3]==4 && up_rready));
  assign m_up_rready  = ar_busy_out[4] && ((ar_pending_src[4]==0 && dl0_rready) || (ar_pending_src[4]==1 && dl1_rready) || (ar_pending_src[4]==2 && dl2_rready) || (ar_pending_src[4]==3 && dl3_rready) || (ar_pending_src[4]==4 && up_rready));

  // Helpers
  function automatic logic is_latency(input mailbox_tag_t t);
    return t.prio;
  endfunction

  // Lock + RR + latency counters
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      for (int o = 0; o < N_OUT; o++) begin
        lock_out[o] <= '{default:'0};
        rr_out[o]   <= '0;
        lat_ctr[o]  <= 2'd0;
      end
    end else begin
      for (int o = 0; o < N_OUT; o++) begin
        // Route-lock per output
        if (fifo_w_en[o]) begin
          if (!lock_out[o].lock_active && !fifo_w_flit[o].tag.eop) begin
            lock_out[o].lock_active <= 1'b1;
            lock_out[o].lock_src    <= sel_out[o];
          end else if (lock_out[o].lock_active && (sel_out[o] == lock_out[o].lock_src) && fifo_w_flit[o].tag.eop) begin
            lock_out[o].lock_active <= 1'b0;
          end
        end

        // Advance RR on enqueue when not locked
        if (fifo_w_en[o] && !lock_out[o].lock_active) begin
          if (rr_out[o] == N_IN-1) rr_out[o] <= '0; else rr_out[o] <= rr_out[o] + 1'b1;
        end

        // Latency counters for BE min service
        if (fifo_w_en[o]) begin
          lat_ctr[o] <= is_latency(fifo_w_flit[o].tag) ? ((lat_ctr[o] == 2'd3) ? 2'd3 : lat_ctr[o] + 1'b1) : 2'd0;
        end
      end
    end
  end

endmodule
