// Mailbox Switch 2x1: one uplink, two downlinks with QoS + route-lock
module mailbox_switch_2x1 #(
  parameter logic [7:0] MY_CLUSTER_ID = 8'h01,
  parameter int FIFO_DEPTH = 2,
  parameter logic VERBOSE = 1'b0
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
  input  logic        m_dl1_wb_ack
);

  import mailbox_pkg::*;

  typedef struct packed {
    logic [15:0] adr;
    logic [31:0] dat;
    logic [3:0]  sel;
    mailbox_tag_t tag;
  } flit_t;

  localparam int FLIT_W = $bits(flit_t);

  // Ingress capture
  flit_t ingress_flit [3];
  logic  ingress_req  [3];

  // Routing targets per ingress: [2]=up, [1]=dl1, [0]=dl0
  logic [2:0] ingress_tgt [3];

  // FIFO interfaces
  logic                 up_fifo_w_en,  dl0_fifo_w_en,  dl1_fifo_w_en;
  logic [FLIT_W-1:0]    up_fifo_w_data, dl0_fifo_w_data, dl1_fifo_w_data;
  logic                 up_fifo_full,  dl0_fifo_full,  dl1_fifo_full;
  logic                 up_fifo_r_en,  dl0_fifo_r_en,  dl1_fifo_r_en;
  logic [FLIT_W-1:0]    up_fifo_r_data, dl0_fifo_r_data, dl1_fifo_r_data;
  logic                 up_fifo_empty, dl0_fifo_empty, dl1_fifo_empty;

  flit_t up_head, dl0_head, dl1_head;
  flit_t up_w_flit, dl0_w_flit, dl1_w_flit;

  typedef struct packed {
    logic lock_active;
    logic [1:0] lock_src;
  } lock_t;

  lock_t lock_up, lock_dl0, lock_dl1;
  logic [1:0] rr_up, rr_dl0, rr_dl1;
  logic [1:0] lat_ctr_up, lat_ctr_dl0, lat_ctr_dl1;

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

  function automatic logic [2:0] decode_targets(
    input logic [15:0] adr,
    input logic        from_uplink
  );
    logic [7:0] cluster;
    logic [7:0] dest_local;
    logic [2:0] tgt;
    begin
      cluster = adr[15:8];
      dest_local = adr[7:0];
      tgt = 3'b000;
      if (cluster == 8'hFF) begin
        tgt = from_uplink ? 3'b011 : 3'b111; // avoid looping global broadcast back up
      end else if (cluster == MY_CLUSTER_ID) begin
        if (dest_local == 8'hFF) tgt = 3'b011; // cluster broadcast -> both locals
        else if (dest_local == 8'd0) tgt = 3'b001;
        else if (dest_local == 8'd1) tgt = 3'b010;
        else tgt = 3'b000; // unsupported local id stalls
      end else begin
        tgt = from_uplink ? 3'b000 : 3'b100; // only locals send upstream
      end
      return tgt;
    end
  endfunction

  // Priority chooser with BE minimum service (1 in 4) and hops tiebreak
  function automatic logic [1:0] pick_src(
    input lock_t lock,
    input logic [1:0] rr,
    input logic [1:0] lat_ctr,
    input logic req0, input logic prio0, input logic [3:0] hops0,
    input logic req1, input logic prio1, input logic [3:0] hops1,
    input logic req2, input logic prio2, input logic [3:0] hops2
  );
    logic any_lat, any_be, force_be;
    logic [3:0] best_hops;
    begin
      if (lock.lock_active) begin
        pick_src = lock.lock_src;
      end else begin
        pick_src = 2'd3;
        best_hops = 4'd0;
        any_lat = (req0 && prio0) || (req1 && prio1) || (req2 && prio2);
        any_be  = (req0 && !prio0) || (req1 && !prio1) || (req2 && !prio2);
        force_be = (lat_ctr == 2'd3) && any_be;

        for (int i = 0; i < 3; i++) begin
          int idx = (rr + i) % 3; // ensure rotation over 3 sources
          logic idx_req; logic idx_prio; logic [3:0] idx_hops;
          case (idx)
            0: begin idx_req = req0; idx_prio = prio0; idx_hops = hops0; end
            1: begin idx_req = req1; idx_prio = prio1; idx_hops = hops1; end
            default: begin idx_req = req2; idx_prio = prio2; idx_hops = hops2; end
          endcase

          if (!idx_req) continue;
          if (force_be && idx_prio) continue;
          if (!force_be && any_lat && !idx_prio) continue;

          if ((pick_src == 2'd3) || (idx_hops > best_hops)) begin
            pick_src = idx[1:0];
            best_hops = idx_hops;
          end
        end
      end
    end
  endfunction

  // Build ingress flits and targets
  always_comb begin
    ingress_req[0]  = dl0_wb_cyc && dl0_wb_stb;
    ingress_req[1]  = dl1_wb_cyc && dl1_wb_stb;
    ingress_req[2]  = up_wb_cyc  && up_wb_stb;

    ingress_tgt[0]  = decode_targets(dl0_wb_adr, 1'b0);
    ingress_tgt[1]  = decode_targets(dl1_wb_adr, 1'b0);
    ingress_tgt[2]  = decode_targets(up_wb_adr,  1'b1);

    ingress_flit[0] = '{adr:dl0_wb_adr, dat:dl0_wb_dat, sel:dl0_wb_sel, tag:update_tag(dl0_tag, dl0_wb_dat)};
    ingress_flit[1] = '{adr:dl1_wb_adr, dat:dl1_wb_dat, sel:dl1_wb_sel, tag:update_tag(dl1_tag, dl1_wb_dat)};
    ingress_flit[2] = '{adr:up_wb_adr,  dat:up_wb_dat,  sel:up_wb_sel,  tag:update_tag(up_tag,  up_wb_dat)};
  end

  // Per-output arbitration
  logic [1:0] sel_up, sel_dl0, sel_dl1;

  always_comb begin
    logic req_up0  = ingress_req[0] && ingress_tgt[0][2] && !up_fifo_full;
    logic req_up1  = ingress_req[1] && ingress_tgt[1][2] && !up_fifo_full;
    logic req_up2  = ingress_req[2] && ingress_tgt[2][2] && !up_fifo_full;

    logic req_dl00 = ingress_req[0] && ingress_tgt[0][0] && !dl0_fifo_full;
    logic req_dl01 = ingress_req[1] && ingress_tgt[1][0] && !dl0_fifo_full;
    logic req_dl02 = ingress_req[2] && ingress_tgt[2][0] && !dl0_fifo_full;

    logic req_dl10 = ingress_req[0] && ingress_tgt[0][1] && !dl1_fifo_full;
    logic req_dl11 = ingress_req[1] && ingress_tgt[1][1] && !dl1_fifo_full;
    logic req_dl12 = ingress_req[2] && ingress_tgt[2][1] && !dl1_fifo_full;

    sel_up  = pick_src(lock_up,  rr_up,  lat_ctr_up,
                       req_up0,  ingress_flit[0].tag.prio, ingress_flit[0].tag.hops,
                       req_up1,  ingress_flit[1].tag.prio, ingress_flit[1].tag.hops,
                       req_up2,  ingress_flit[2].tag.prio, ingress_flit[2].tag.hops);

    sel_dl0 = pick_src(lock_dl0, rr_dl0, lat_ctr_dl0,
                       req_dl00, ingress_flit[0].tag.prio, ingress_flit[0].tag.hops,
                       req_dl01, ingress_flit[1].tag.prio, ingress_flit[1].tag.hops,
                       req_dl02, ingress_flit[2].tag.prio, ingress_flit[2].tag.hops);

    sel_dl1 = pick_src(lock_dl1, rr_dl1, lat_ctr_dl1,
                       req_dl10, ingress_flit[0].tag.prio, ingress_flit[0].tag.hops,
                       req_dl11, ingress_flit[1].tag.prio, ingress_flit[1].tag.hops,
                       req_dl12, ingress_flit[2].tag.prio, ingress_flit[2].tag.hops);

    // DEBUG: show per-output dl1 request state and selection inputs
    if (VERBOSE) $display("%0t: dl1 reqs req_dl10=%b req_dl11=%b req_dl12=%b rr_dl1=%0d lat_ctr_dl1=%0d lock_active=%b dl1_fifo_full=%b sel_dl1=%0d",
             $time, req_dl10, req_dl11, req_dl12, rr_dl1, lat_ctr_dl1, lock_dl1.lock_active, dl1_fifo_full, sel_dl1);
  end

  // Grant only if every targeted output selected this source (keeps broadcast atomic)
  logic grant_src[3];
  always_comb begin
    logic need_up, need_dl0, need_dl1, has_all;
    for (int i = 0; i < 3; i++) grant_src[i] = 1'b0;
    for (int i = 0; i < 3; i++) begin
      if (!ingress_req[i]) continue;
      need_up  = ingress_tgt[i][2];
      need_dl0 = ingress_tgt[i][0];
      need_dl1 = ingress_tgt[i][1];

      has_all = (!need_up  || (sel_up  == i)) &&
                (!need_dl0 || (sel_dl0 == i)) &&
                (!need_dl1 || (sel_dl1 == i));
      grant_src[i] = has_all && (ingress_tgt[i] != 3'b000);

      // DEBUG: show per-source arbitration decision and flit info
      if (VERBOSE) $display("%0t: sw arbitration src=%0d tgt=%b sel_up=%0d sel_dl0=%0d sel_dl1=%0d has_all=%0b grant=%0b adr=%h dat=%h prio=%0b eop=%0b hops=%0d",
               $time, i, ingress_tgt[i], sel_up, sel_dl0, sel_dl1, has_all, grant_src[i], ingress_flit[i].adr, ingress_flit[i].dat, ingress_flit[i].tag.prio, ingress_flit[i].tag.eop, ingress_flit[i].tag.hops);
    end
  end

  // FIFO write enables; suppressed if broadcast not fully granted
  always_comb begin
    up_fifo_w_en   = 1'b0; up_fifo_w_data   = '0;
    dl0_fifo_w_en  = 1'b0; dl0_fifo_w_data  = '0;
    dl1_fifo_w_en  = 1'b0; dl1_fifo_w_data  = '0;
    dl0_wb_ack     = 1'b0; dl1_wb_ack      = 1'b0; up_wb_ack = 1'b0;

    for (int i = 0; i < 3; i++) begin
      if (!grant_src[i]) continue;
      if (ingress_tgt[i][2]) begin
        up_fifo_w_en   = 1'b1;
        up_fifo_w_data = ingress_flit[i];
      end
      if (ingress_tgt[i][0]) begin
        dl0_fifo_w_en   = 1'b1;
        dl0_fifo_w_data = ingress_flit[i];
      end
      if (ingress_tgt[i][1]) begin
        dl1_fifo_w_en   = 1'b1;
        dl1_fifo_w_data = ingress_flit[i];
      end

      case (i)
        0: dl0_wb_ack = 1'b1;
        1: dl1_wb_ack = 1'b1;
        default: up_wb_ack = 1'b1;
      endcase

      // DEBUG: show enqueue decision for this source
      if (VERBOSE) $display("%0t: sw enqueue src=%0d up_w_en=%0b dl0_w_en=%0b dl1_w_en=%0b dl0_ack=%0b dl1_ack=%0b up_ack=%0b",
               $time, i, up_fifo_w_en, dl0_fifo_w_en, dl1_fifo_w_en, dl0_wb_ack, dl1_wb_ack, up_wb_ack);
    end
  end

  // Output FIFOs
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_up_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(up_fifo_w_en), .w_data(up_fifo_w_data), .w_full(up_fifo_full),
    .r_en(up_fifo_r_en), .r_data(up_fifo_r_data), .r_empty(up_fifo_empty)
  );

  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl0_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(dl0_fifo_w_en), .w_data(dl0_fifo_w_data), .w_full(dl0_fifo_full),
    .r_en(dl0_fifo_r_en), .r_data(dl0_fifo_r_data), .r_empty(dl0_fifo_empty)
  );

  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_dl1_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(dl1_fifo_w_en), .w_data(dl1_fifo_w_data), .w_full(dl1_fifo_full),
    .r_en(dl1_fifo_r_en), .r_data(dl1_fifo_r_data), .r_empty(dl1_fifo_empty)
  );

  assign up_head  = flit_t'(up_fifo_r_data);
  assign dl0_head = flit_t'(dl0_fifo_r_data);
  assign dl1_head = flit_t'(dl1_fifo_r_data);
  assign up_w_flit  = flit_t'(up_fifo_w_data);
  assign dl0_w_flit = flit_t'(dl0_fifo_w_data);
  assign dl1_w_flit = flit_t'(dl1_fifo_w_data);

  // Drive outbound Wishbone from FIFO heads
  assign m_up_wb_cyc  = !up_fifo_empty;
  assign m_up_wb_stb  = !up_fifo_empty;
  assign m_up_wb_we   = 1'b1;
  assign m_up_wb_adr  = up_head.adr;
  assign m_up_wb_dat  = up_head.dat;
  assign m_up_wb_sel  = up_head.sel;
  assign m_up_tag     = up_head.tag;

  assign m_dl0_wb_cyc = !dl0_fifo_empty;
  assign m_dl0_wb_stb = !dl0_fifo_empty;
  assign m_dl0_wb_we  = 1'b1;
  assign m_dl0_wb_adr = dl0_head.adr;
  assign m_dl0_wb_dat = dl0_head.dat;
  assign m_dl0_wb_sel = dl0_head.sel;
  assign m_dl0_tag    = dl0_head.tag;

  assign m_dl1_wb_cyc = !dl1_fifo_empty;
  assign m_dl1_wb_stb = !dl1_fifo_empty;
  assign m_dl1_wb_we  = 1'b1;
  assign m_dl1_wb_adr = dl1_head.adr;
  assign m_dl1_wb_dat = dl1_head.dat;
  assign m_dl1_wb_sel = dl1_head.sel;
  assign m_dl1_tag    = dl1_head.tag;

  assign up_fifo_r_en  = !up_fifo_empty  && m_up_wb_ack;
  assign dl0_fifo_r_en = !dl0_fifo_empty && m_dl0_wb_ack;
  assign dl1_fifo_r_en = !dl1_fifo_empty && m_dl1_wb_ack;

  // Lock + QoS bookkeeping
  function automatic logic is_latency(input mailbox_tag_t t);
    return t.prio;
  endfunction

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      lock_up   <= '0; lock_dl0 <= '0; lock_dl1 <= '0;
      rr_up     <= 2'd0; rr_dl0  <= 2'd0; rr_dl1 <= 2'd0;
      lat_ctr_up <= 2'd0; lat_ctr_dl0 <= 2'd0; lat_ctr_dl1 <= 2'd0;
    end else begin
      // Update locks on enqueue events (route-lock until eop seen)
      if (up_fifo_w_en) begin
        if (!lock_up.lock_active && !up_w_flit.tag.eop) begin
          lock_up.lock_active <= 1'b1;
          lock_up.lock_src    <= sel_up;
        end else if (lock_up.lock_active && (sel_up == lock_up.lock_src) && up_w_flit.tag.eop) begin
          lock_up.lock_active <= 1'b0;
        end
      end

      if (dl0_fifo_w_en) begin
        if (!lock_dl0.lock_active && !dl0_w_flit.tag.eop) begin
          lock_dl0.lock_active <= 1'b1;
          lock_dl0.lock_src    <= sel_dl0;
        end else if (lock_dl0.lock_active && (sel_dl0 == lock_dl0.lock_src) && dl0_w_flit.tag.eop) begin
          lock_dl0.lock_active <= 1'b0;
        end
      end

      if (dl1_fifo_w_en) begin
        if (!lock_dl1.lock_active && !dl1_w_flit.tag.eop) begin
          lock_dl1.lock_active <= 1'b1;
          lock_dl1.lock_src    <= sel_dl1;
        end else if (lock_dl1.lock_active && (sel_dl1 == lock_dl1.lock_src) && dl1_w_flit.tag.eop) begin
          lock_dl1.lock_active <= 1'b0;
        end
      end

      // Advance RR on successful enqueue when not locked
      if (up_fifo_w_en && !lock_up.lock_active)   rr_up  <= rr_up  + 2'd1;
      if (dl0_fifo_w_en && !lock_dl0.lock_active) rr_dl0 <= rr_dl0 + 2'd1;
      if (dl1_fifo_w_en && !lock_dl1.lock_active) rr_dl1 <= rr_dl1 + 2'd1;

      // Latency counters (saturate at 3) for BE minimum service
      if (up_fifo_w_en) begin
        lat_ctr_up <= is_latency(up_w_flit.tag) ? ((lat_ctr_up == 2'd3) ? 2'd3 : lat_ctr_up + 1'b1) : 2'd0;
      end
      if (dl0_fifo_w_en) begin
        lat_ctr_dl0 <= is_latency(dl0_w_flit.tag) ? ((lat_ctr_dl0 == 2'd3) ? 2'd3 : lat_ctr_dl0 + 1'b1) : 2'd0;
      end
      if (dl1_fifo_w_en) begin
        lat_ctr_dl1 <= is_latency(dl1_w_flit.tag) ? ((lat_ctr_dl1 == 2'd3) ? 2'd3 : lat_ctr_dl1 + 1'b1) : 2'd0;
      end
    end
  end

endmodule
