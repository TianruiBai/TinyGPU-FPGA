// Mailbox Switch 4x1: four downlinks, one uplink with QoS + route-lock
module mailbox_switch_4x1 #(
  parameter logic [7:0] MY_CLUSTER_ID = 8'h01,
  parameter int FIFO_DEPTH = 2
) (
  input  logic clk,
  input  logic rst_n,

  // Downlink masters into this switch (from leaves)
  input  logic        dl0_wb_cyc,
  input  logic        dl0_wb_stb,
  input  logic        dl0_wb_we,
  input  logic [15:0] dl0_wb_adr,
  input  logic [31:0] dl0_wb_dat,
  input  logic [3:0]  dl0_wb_sel,
  input  mailbox_pkg::mailbox_tag_t dl0_tag,
  output logic        dl0_wb_ack,

  input  logic        dl1_wb_cyc,
  input  logic        dl1_wb_stb,
  input  logic        dl1_wb_we,
  input  logic [15:0] dl1_wb_adr,
  input  logic [31:0] dl1_wb_dat,
  input  logic [3:0]  dl1_wb_sel,
  input  mailbox_pkg::mailbox_tag_t dl1_tag,
  output logic        dl1_wb_ack,

  input  logic        dl2_wb_cyc,
  input  logic        dl2_wb_stb,
  input  logic        dl2_wb_we,
  input  logic [15:0] dl2_wb_adr,
  input  logic [31:0] dl2_wb_dat,
  input  logic [3:0]  dl2_wb_sel,
  input  mailbox_pkg::mailbox_tag_t dl2_tag,
  output logic        dl2_wb_ack,

  input  logic        dl3_wb_cyc,
  input  logic        dl3_wb_stb,
  input  logic        dl3_wb_we,
  input  logic [15:0] dl3_wb_adr,
  input  logic [31:0] dl3_wb_dat,
  input  logic [3:0]  dl3_wb_sel,
  input  mailbox_pkg::mailbox_tag_t dl3_tag,
  output logic        dl3_wb_ack,

  // Uplink master into this switch (from center)
  input  logic        up_wb_cyc,
  input  logic        up_wb_stb,
  input  logic        up_wb_we,
  input  logic [15:0] up_wb_adr,
  input  logic [31:0] up_wb_dat,
  input  logic [3:0]  up_wb_sel,
  input  mailbox_pkg::mailbox_tag_t up_tag,
  output logic        up_wb_ack,

  // Master out to uplink (toward center)
  output logic        m_up_wb_cyc,
  output logic        m_up_wb_stb,
  output logic        m_up_wb_we,
  output logic [15:0] m_up_wb_adr,
  output logic [31:0] m_up_wb_dat,
  output logic [3:0]  m_up_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_up_tag,
  input  logic        m_up_wb_ack,

  // Masters out to downlinks (toward leaves)
  output logic        m_dl0_wb_cyc,
  output logic        m_dl0_wb_stb,
  output logic        m_dl0_wb_we,
  output logic [15:0] m_dl0_wb_adr,
  output logic [31:0] m_dl0_wb_dat,
  output logic [3:0]  m_dl0_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_dl0_tag,
  input  logic        m_dl0_wb_ack,

  output logic        m_dl1_wb_cyc,
  output logic        m_dl1_wb_stb,
  output logic        m_dl1_wb_we,
  output logic [15:0] m_dl1_wb_adr,
  output logic [31:0] m_dl1_wb_dat,
  output logic [3:0]  m_dl1_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_dl1_tag,
  input  logic        m_dl1_wb_ack,

  output logic        m_dl2_wb_cyc,
  output logic        m_dl2_wb_stb,
  output logic        m_dl2_wb_we,
  output logic [15:0] m_dl2_wb_adr,
  output logic [31:0] m_dl2_wb_dat,
  output logic [3:0]  m_dl2_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_dl2_tag,
  input  logic        m_dl2_wb_ack,

  output logic        m_dl3_wb_cyc,
  output logic        m_dl3_wb_stb,
  output logic        m_dl3_wb_we,
  output logic [15:0] m_dl3_wb_adr,
  output logic [31:0] m_dl3_wb_dat,
  output logic [3:0]  m_dl3_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_dl3_tag,
  input  logic        m_dl3_wb_ack
);

  import mailbox_pkg::*;

  localparam int N_IN  = 5; // dl0..dl3, up
  localparam int N_OUT = 5; // dl0..dl3, up
  localparam int RR_W  = 3;

  typedef struct packed {
    logic [15:0] adr;
    logic [31:0] dat;
    logic [3:0]  sel;
    mailbox_tag_t tag;
  } flit_t;
  localparam int FLIT_W = $bits(flit_t);

  flit_t ingress_flit [N_IN];
  logic  ingress_req  [N_IN];
  logic [N_OUT-1:0] ingress_tgt [N_IN];

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

    ingress_req[0] = dl0_wb_cyc && dl0_wb_stb;
    ingress_req[1] = dl1_wb_cyc && dl1_wb_stb;
    ingress_req[2] = dl2_wb_cyc && dl2_wb_stb;
    ingress_req[3] = dl3_wb_cyc && dl3_wb_stb;
    ingress_req[4] = up_wb_cyc  && up_wb_stb;

    ingress_tgt[0] = decode_targets(dl0_wb_adr, 1'b0);
    ingress_tgt[1] = decode_targets(dl1_wb_adr, 1'b0);
    ingress_tgt[2] = decode_targets(dl2_wb_adr, 1'b0);
    ingress_tgt[3] = decode_targets(dl3_wb_adr, 1'b0);
    ingress_tgt[4] = decode_targets(up_wb_adr,  1'b1);

    ingress_flit[0] = '{adr:dl0_wb_adr, dat:dl0_wb_dat, sel:dl0_wb_sel, tag:update_tag(dl0_tag, dl0_wb_dat)};
    ingress_flit[1] = '{adr:dl1_wb_adr, dat:dl1_wb_dat, sel:dl1_wb_sel, tag:update_tag(dl1_tag, dl1_wb_dat)};
    ingress_flit[2] = '{adr:dl2_wb_adr, dat:dl2_wb_dat, sel:dl2_wb_sel, tag:update_tag(dl2_tag, dl2_wb_dat)};
    ingress_flit[3] = '{adr:dl3_wb_adr, dat:dl3_wb_dat, sel:dl3_wb_sel, tag:update_tag(dl3_tag, dl3_wb_dat)};
    ingress_flit[4] = '{adr:up_wb_adr,  dat:up_wb_dat,  sel:up_wb_sel,  tag:update_tag(up_tag,  up_wb_dat)};
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

  // FIFO write enables; ack back to selected source when all targets granted
  always_comb begin
    fifo_w_en   = '0;
    fifo_w_data = '{default:'0};
    dl0_wb_ack = 1'b0; dl1_wb_ack = 1'b0; dl2_wb_ack = 1'b0; dl3_wb_ack = 1'b0; up_wb_ack = 1'b0;

    for (int i = 0; i < N_IN; i++) begin
      if (!grant_src[i]) continue;
      for (int o = 0; o < N_OUT; o++) begin
        if (ingress_tgt[i][o]) begin
          fifo_w_en[o]   = 1'b1;
          fifo_w_data[o] = ingress_flit[i];
        end
      end
      case (i)
        0: dl0_wb_ack = 1'b1;
        1: dl1_wb_ack = 1'b1;
        2: dl2_wb_ack = 1'b1;
        3: dl3_wb_ack = 1'b1;
        default: up_wb_ack = 1'b1;
      endcase
    end
  end

  // FIFOs
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl0_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(fifo_w_en[0]), .w_data(fifo_w_data[0]), .w_full(fifo_full[0]),
    .r_en(fifo_r_en[0]), .r_data(fifo_r_data[0]), .r_empty(fifo_empty[0])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl1_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(fifo_w_en[1]), .w_data(fifo_w_data[1]), .w_full(fifo_full[1]),
    .r_en(fifo_r_en[1]), .r_data(fifo_r_data[1]), .r_empty(fifo_empty[1])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl2_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(fifo_w_en[2]), .w_data(fifo_w_data[2]), .w_full(fifo_full[2]),
    .r_en(fifo_r_en[2]), .r_data(fifo_r_data[2]), .r_empty(fifo_empty[2])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl3_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(fifo_w_en[3]), .w_data(fifo_w_data[3]), .w_full(fifo_full[3]),
    .r_en(fifo_r_en[3]), .r_data(fifo_r_data[3]), .r_empty(fifo_empty[3])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_up_fifo (
    .clk(clk), .rst_n(rst_n),
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

  // Drive outbound Wishbone from FIFO heads
  assign m_dl0_wb_cyc = !fifo_empty[0];
  assign m_dl0_wb_stb = !fifo_empty[0];
  assign m_dl0_wb_we  = 1'b1;
  assign m_dl0_wb_adr = fifo_head[0].adr;
  assign m_dl0_wb_dat = fifo_head[0].dat;
  assign m_dl0_wb_sel = fifo_head[0].sel;
  assign m_dl0_tag    = fifo_head[0].tag;

  assign m_dl1_wb_cyc = !fifo_empty[1];
  assign m_dl1_wb_stb = !fifo_empty[1];
  assign m_dl1_wb_we  = 1'b1;
  assign m_dl1_wb_adr = fifo_head[1].adr;
  assign m_dl1_wb_dat = fifo_head[1].dat;
  assign m_dl1_wb_sel = fifo_head[1].sel;
  assign m_dl1_tag    = fifo_head[1].tag;

  assign m_dl2_wb_cyc = !fifo_empty[2];
  assign m_dl2_wb_stb = !fifo_empty[2];
  assign m_dl2_wb_we  = 1'b1;
  assign m_dl2_wb_adr = fifo_head[2].adr;
  assign m_dl2_wb_dat = fifo_head[2].dat;
  assign m_dl2_wb_sel = fifo_head[2].sel;
  assign m_dl2_tag    = fifo_head[2].tag;

  assign m_dl3_wb_cyc = !fifo_empty[3];
  assign m_dl3_wb_stb = !fifo_empty[3];
  assign m_dl3_wb_we  = 1'b1;
  assign m_dl3_wb_adr = fifo_head[3].adr;
  assign m_dl3_wb_dat = fifo_head[3].dat;
  assign m_dl3_wb_sel = fifo_head[3].sel;
  assign m_dl3_tag    = fifo_head[3].tag;

  assign m_up_wb_cyc = !fifo_empty[4];
  assign m_up_wb_stb = !fifo_empty[4];
  assign m_up_wb_we  = 1'b1;
  assign m_up_wb_adr = fifo_head[4].adr;
  assign m_up_wb_dat = fifo_head[4].dat;
  assign m_up_wb_sel = fifo_head[4].sel;
  assign m_up_tag    = fifo_head[4].tag;

  assign fifo_r_en[0] = !fifo_empty[0] && m_dl0_wb_ack;
  assign fifo_r_en[1] = !fifo_empty[1] && m_dl1_wb_ack;
  assign fifo_r_en[2] = !fifo_empty[2] && m_dl2_wb_ack;
  assign fifo_r_en[3] = !fifo_empty[3] && m_dl3_wb_ack;
  assign fifo_r_en[4] = !fifo_empty[4] && m_up_wb_ack;

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
