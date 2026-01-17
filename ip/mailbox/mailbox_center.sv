// Mailbox Center: root router connecting 4 switches + HP port
module mailbox_center #(
  parameter logic [7:0] CHILD_CLUSTER_ID [4] = '{8'h01, 8'h02, 8'h03, 8'h04},
  parameter int FIFO_DEPTH = 4
) (
  input  logic clk,
  input  logic rst_n,

  // Uplinks from switches (masters into center)
  input  logic        sw0_wb_cyc,
  input  logic        sw0_wb_stb,
  input  logic        sw0_wb_we,
  input  logic [15:0] sw0_wb_adr,
  input  logic [31:0] sw0_wb_dat,
  input  logic [3:0]  sw0_wb_sel,
  input  mailbox_pkg::mailbox_tag_t sw0_tag,
  output logic        sw0_wb_ack,

  input  logic        sw1_wb_cyc,
  input  logic        sw1_wb_stb,
  input  logic        sw1_wb_we,
  input  logic [15:0] sw1_wb_adr,
  input  logic [31:0] sw1_wb_dat,
  input  logic [3:0]  sw1_wb_sel,
  input  mailbox_pkg::mailbox_tag_t sw1_tag,
  output logic        sw1_wb_ack,

  input  logic        sw2_wb_cyc,
  input  logic        sw2_wb_stb,
  input  logic        sw2_wb_we,
  input  logic [15:0] sw2_wb_adr,
  input  logic [31:0] sw2_wb_dat,
  input  logic [3:0]  sw2_wb_sel,
  input  mailbox_pkg::mailbox_tag_t sw2_tag,
  output logic        sw2_wb_ack,

  input  logic        sw3_wb_cyc,
  input  logic        sw3_wb_stb,
  input  logic        sw3_wb_we,
  input  logic [15:0] sw3_wb_adr,
  input  logic [31:0] sw3_wb_dat,
  input  logic [3:0]  sw3_wb_sel,
  input  mailbox_pkg::mailbox_tag_t sw3_tag,
  output logic        sw3_wb_ack,

  // HP port master into center (e.g. MCU)
  input  logic        hp_wb_cyc,
  input  logic        hp_wb_stb,
  input  logic        hp_wb_we,
  input  logic [15:0] hp_wb_adr,
  input  logic [31:0] hp_wb_dat,
  input  logic [3:0]  hp_wb_sel,
  input  mailbox_pkg::mailbox_tag_t hp_tag,
  output logic        hp_wb_ack,

  // Masters out to switches (toward clusters)
  output logic        m_sw0_wb_cyc,
  output logic        m_sw0_wb_stb,
  output logic        m_sw0_wb_we,
  output logic [15:0] m_sw0_wb_adr,
  output logic [31:0] m_sw0_wb_dat,
  output logic [3:0]  m_sw0_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_sw0_tag,
  input  logic        m_sw0_wb_ack,

  output logic        m_sw1_wb_cyc,
  output logic        m_sw1_wb_stb,
  output logic        m_sw1_wb_we,
  output logic [15:0] m_sw1_wb_adr,
  output logic [31:0] m_sw1_wb_dat,
  output logic [3:0]  m_sw1_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_sw1_tag,
  input  logic        m_sw1_wb_ack,

  output logic        m_sw2_wb_cyc,
  output logic        m_sw2_wb_stb,
  output logic        m_sw2_wb_we,
  output logic [15:0] m_sw2_wb_adr,
  output logic [31:0] m_sw2_wb_dat,
  output logic [3:0]  m_sw2_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_sw2_tag,
  input  logic        m_sw2_wb_ack,

  output logic        m_sw3_wb_cyc,
  output logic        m_sw3_wb_stb,
  output logic        m_sw3_wb_we,
  output logic [15:0] m_sw3_wb_adr,
  output logic [31:0] m_sw3_wb_dat,
  output logic [3:0]  m_sw3_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_sw3_tag,
  input  logic        m_sw3_wb_ack,

  // Master out to HP endpoint
  output logic        m_hp_wb_cyc,
  output logic        m_hp_wb_stb,
  output logic        m_hp_wb_we,
  output logic [15:0] m_hp_wb_adr,
  output logic [31:0] m_hp_wb_dat,
  output logic [3:0]  m_hp_wb_sel,
  output mailbox_pkg::mailbox_tag_t m_hp_tag,
  input  logic        m_hp_wb_ack
);

  import mailbox_pkg::*;

  localparam int N_IN  = 5; // sw0..sw3, hp
  localparam int N_OUT = 5; // sw0..sw3, hp
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

  logic [N_OUT-1:0] fifo_w_en, fifo_full, fifo_r_en, fifo_empty;
  logic [N_OUT-1:0][FLIT_W-1:0] fifo_w_data, fifo_r_data;
  flit_t fifo_head [N_OUT];
  flit_t fifo_w_flit [N_OUT];

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
    input logic        from_hp
  );
    logic [7:0] cluster;
    logic [7:0] dest_local;
    logic [N_OUT-1:0] tgt;
    begin
      cluster = adr[15:8];
      dest_local = adr[7:0];
      tgt = '0;
      if (cluster == 8'hFF) begin
        tgt = from_hp ? 5'b11111 : 5'b11111; // broadcasts replicate everywhere
      end else if (cluster == 8'h00) begin
        tgt[4] = 1'b1; // HP port
      end else begin
        for (int i = 0; i < 4; i++) begin
          if (cluster == CHILD_CLUSTER_ID[i]) tgt[i] = 1'b1;
        end
      end
      decode_targets = tgt;
    end
  endfunction

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

  // Build ingress
  always_comb begin
    ingress_req = '{default:1'b0};
    ingress_tgt = '{default:'0};
    ingress_flit = '{default:'0};

    ingress_req[0] = sw0_wb_cyc && sw0_wb_stb;
    ingress_req[1] = sw1_wb_cyc && sw1_wb_stb;
    ingress_req[2] = sw2_wb_cyc && sw2_wb_stb;
    ingress_req[3] = sw3_wb_cyc && sw3_wb_stb;
    ingress_req[4] = hp_wb_cyc && hp_wb_stb;

    ingress_tgt[0] = decode_targets(sw0_wb_adr, 1'b0);
    ingress_tgt[1] = decode_targets(sw1_wb_adr, 1'b0);
    ingress_tgt[2] = decode_targets(sw2_wb_adr, 1'b0);
    ingress_tgt[3] = decode_targets(sw3_wb_adr, 1'b0);
    ingress_tgt[4] = decode_targets(hp_wb_adr, 1'b1);

    ingress_flit[0] = '{adr:sw0_wb_adr, dat:sw0_wb_dat, sel:sw0_wb_sel, tag:update_tag(sw0_tag, sw0_wb_dat)};
    ingress_flit[1] = '{adr:sw1_wb_adr, dat:sw1_wb_dat, sel:sw1_wb_sel, tag:update_tag(sw1_tag, sw1_wb_dat)};
    ingress_flit[2] = '{adr:sw2_wb_adr, dat:sw2_wb_dat, sel:sw2_wb_sel, tag:update_tag(sw2_tag, sw2_wb_dat)};
    ingress_flit[3] = '{adr:sw3_wb_adr, dat:sw3_wb_dat, sel:sw3_wb_sel, tag:update_tag(sw3_tag, sw3_wb_dat)};
    ingress_flit[4] = '{adr:hp_wb_adr, dat:hp_wb_dat, sel:hp_wb_sel, tag:update_tag(hp_tag, hp_wb_dat)};
  end

  // Arbitration matrices
  logic [$clog2(N_IN)-1:0] sel_out [N_OUT];
  logic [N_IN-1:0] req_matrix [N_OUT];
  logic [N_IN-1:0] prio_matrix [N_OUT];
  logic [N_IN-1:0][3:0] hops_matrix [N_OUT];

  always_comb begin
    for (int o = 0; o < N_OUT; o++) begin
      req_matrix[o]  = '0;
      prio_matrix[o] = '0;
      hops_matrix[o] = '{default:4'd0};
      sel_out[o]     = {$clog2(N_IN){1'b1}}; // none
    end

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

  // Broadcast atomicity check
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

  // FIFO write + ack
  always_comb begin
    fifo_w_en   = '0;
    fifo_w_data = '{default:'0};
    sw0_wb_ack = 1'b0; sw1_wb_ack = 1'b0; sw2_wb_ack = 1'b0; sw3_wb_ack = 1'b0; hp_wb_ack = 1'b0;

    for (int i = 0; i < N_IN; i++) begin
      if (!grant_src[i]) continue;
      for (int o = 0; o < N_OUT; o++) begin
        if (ingress_tgt[i][o]) begin
          fifo_w_en[o]   = 1'b1;
          fifo_w_data[o] = ingress_flit[i];
        end
      end
      case (i)
        0: sw0_wb_ack = 1'b1;
        1: sw1_wb_ack = 1'b1;
        2: sw2_wb_ack = 1'b1;
        3: sw3_wb_ack = 1'b1;
        default: hp_wb_ack = 1'b1;
      endcase
    end
  end

  // FIFOs
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_sw0_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(fifo_w_en[0]), .w_data(fifo_w_data[0]), .w_full(fifo_full[0]),
    .r_en(fifo_r_en[0]), .r_data(fifo_r_data[0]), .r_empty(fifo_empty[0])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_sw1_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(fifo_w_en[1]), .w_data(fifo_w_data[1]), .w_full(fifo_full[1]),
    .r_en(fifo_r_en[1]), .r_data(fifo_r_data[1]), .r_empty(fifo_empty[1])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_sw2_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(fifo_w_en[2]), .w_data(fifo_w_data[2]), .w_full(fifo_full[2]),
    .r_en(fifo_r_en[2]), .r_data(fifo_r_data[2]), .r_empty(fifo_empty[2])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_sw3_fifo (
    .clk(clk), .rst_n(rst_n),
    .w_en(fifo_w_en[3]), .w_data(fifo_w_data[3]), .w_full(fifo_full[3]),
    .r_en(fifo_r_en[3]), .r_data(fifo_r_data[3]), .r_empty(fifo_empty[3])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_hp_fifo (
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

  assign m_sw0_wb_cyc = !fifo_empty[0];
  assign m_sw0_wb_stb = !fifo_empty[0];
  assign m_sw0_wb_we  = 1'b1;
  assign m_sw0_wb_adr = fifo_head[0].adr;
  assign m_sw0_wb_dat = fifo_head[0].dat;
  assign m_sw0_wb_sel = fifo_head[0].sel;
  assign m_sw0_tag    = fifo_head[0].tag;

  assign m_sw1_wb_cyc = !fifo_empty[1];
  assign m_sw1_wb_stb = !fifo_empty[1];
  assign m_sw1_wb_we  = 1'b1;
  assign m_sw1_wb_adr = fifo_head[1].adr;
  assign m_sw1_wb_dat = fifo_head[1].dat;
  assign m_sw1_wb_sel = fifo_head[1].sel;
  assign m_sw1_tag    = fifo_head[1].tag;

  assign m_sw2_wb_cyc = !fifo_empty[2];
  assign m_sw2_wb_stb = !fifo_empty[2];
  assign m_sw2_wb_we  = 1'b1;
  assign m_sw2_wb_adr = fifo_head[2].adr;
  assign m_sw2_wb_dat = fifo_head[2].dat;
  assign m_sw2_wb_sel = fifo_head[2].sel;
  assign m_sw2_tag    = fifo_head[2].tag;

  assign m_sw3_wb_cyc = !fifo_empty[3];
  assign m_sw3_wb_stb = !fifo_empty[3];
  assign m_sw3_wb_we  = 1'b1;
  assign m_sw3_wb_adr = fifo_head[3].adr;
  assign m_sw3_wb_dat = fifo_head[3].dat;
  assign m_sw3_wb_sel = fifo_head[3].sel;
  assign m_sw3_tag    = fifo_head[3].tag;

  assign m_hp_wb_cyc = !fifo_empty[4];
  assign m_hp_wb_stb = !fifo_empty[4];
  assign m_hp_wb_we  = 1'b1;
  assign m_hp_wb_adr = fifo_head[4].adr;
  assign m_hp_wb_dat = fifo_head[4].dat;
  assign m_hp_wb_sel = fifo_head[4].sel;
  assign m_hp_tag    = fifo_head[4].tag;

  assign fifo_r_en[0] = !fifo_empty[0] && m_sw0_wb_ack;
  assign fifo_r_en[1] = !fifo_empty[1] && m_sw1_wb_ack;
  assign fifo_r_en[2] = !fifo_empty[2] && m_sw2_wb_ack;
  assign fifo_r_en[3] = !fifo_empty[3] && m_sw3_wb_ack;
  assign fifo_r_en[4] = !fifo_empty[4] && m_hp_wb_ack;

  function automatic logic is_latency(input mailbox_tag_t t);
    return t.prio;
  endfunction

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      for (int o = 0; o < N_OUT; o++) begin
        lock_out[o] <= '{default:'0};
        rr_out[o]   <= '0;
        lat_ctr[o]  <= 2'd0;
      end
    end else begin
      for (int o = 0; o < N_OUT; o++) begin
        if (fifo_w_en[o]) begin
          if (!lock_out[o].lock_active && !fifo_w_flit[o].tag.eop) begin
            lock_out[o].lock_active <= 1'b1;
            lock_out[o].lock_src    <= sel_out[o];
          end else if (lock_out[o].lock_active && (sel_out[o] == lock_out[o].lock_src) && fifo_w_flit[o].tag.eop) begin
            lock_out[o].lock_active <= 1'b0;
          end
        end

        if (fifo_w_en[o] && !lock_out[o].lock_active) begin
          if (rr_out[o] == N_IN-1) rr_out[o] <= '0; else rr_out[o] <= rr_out[o] + 1'b1;
        end

        if (fifo_w_en[o]) begin
          lat_ctr[o] <= is_latency(fifo_w_flit[o].tag) ? ((lat_ctr[o] == 2'd3) ? 2'd3 : lat_ctr[o] + 1'b1) : 2'd0;
        end
      end
    end
  end

endmodule
