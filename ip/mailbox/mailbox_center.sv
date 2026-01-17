`timescale 1ns/1ps
// Mailbox Center: root router connecting 4 switches + HP port (AXI4-Lite full-duplex)
module mailbox_center #(
  parameter logic [7:0] CHILD_CLUSTER_ID [4] = '{8'h01, 8'h02, 8'h03, 8'h04},
  parameter int FIFO_DEPTH = 4
) (
  input  logic clk,
  input  logic rst_n,

  // Uplinks from switches (masters into center)
  input  logic        sw0_awvalid,
  output logic        sw0_awready,
  input  logic [15:0] sw0_awaddr,
  input  logic        sw0_wvalid,
  output logic        sw0_wready,
  input  logic [31:0] sw0_wdata,
  input  logic [3:0]  sw0_wstrb,
  input  mailbox_pkg::mailbox_tag_t sw0_tag,
  output logic        sw0_bvalid,
  input  logic        sw0_bready,

  input  logic        sw0_arvalid,
  output logic        sw0_arready,
  input  logic [15:0] sw0_araddr,
  output logic        sw0_rvalid,
  input  logic        sw0_rready,
  output logic [31:0] sw0_rdata,

  input  logic        sw1_awvalid,
  output logic        sw1_awready,
  input  logic [15:0] sw1_awaddr,
  input  logic        sw1_wvalid,
  output logic        sw1_wready,
  input  logic [31:0] sw1_wdata,
  input  logic [3:0]  sw1_wstrb,
  input  mailbox_pkg::mailbox_tag_t sw1_tag,
  output logic        sw1_bvalid,
  input  logic        sw1_bready,

  input  logic        sw1_arvalid,
  output logic        sw1_arready,
  input  logic [15:0] sw1_araddr,
  output logic        sw1_rvalid,
  input  logic        sw1_rready,
  output logic [31:0] sw1_rdata,

  input  logic        sw2_awvalid,
  output logic        sw2_awready,
  input  logic [15:0] sw2_awaddr,
  input  logic        sw2_wvalid,
  output logic        sw2_wready,
  input  logic [31:0] sw2_wdata,
  input  logic [3:0]  sw2_wstrb,
  input  mailbox_pkg::mailbox_tag_t sw2_tag,
  output logic        sw2_bvalid,
  input  logic        sw2_bready,

  input  logic        sw2_arvalid,
  output logic        sw2_arready,
  input  logic [15:0] sw2_araddr,
  output logic        sw2_rvalid,
  input  logic        sw2_rready,
  output logic [31:0] sw2_rdata,

  input  logic        sw3_awvalid,
  output logic        sw3_awready,
  input  logic [15:0] sw3_awaddr,
  input  logic        sw3_wvalid,
  output logic        sw3_wready,
  input  logic [31:0] sw3_wdata,
  input  logic [3:0]  sw3_wstrb,
  input  mailbox_pkg::mailbox_tag_t sw3_tag,
  output logic        sw3_bvalid,
  input  logic        sw3_bready,

  input  logic        sw3_arvalid,
  output logic        sw3_arready,
  input  logic [15:0] sw3_araddr,
  output logic        sw3_rvalid,
  input  logic        sw3_rready,
  output logic [31:0] sw3_rdata,

  // HP port master into center (e.g. MCU)
  input  logic        hp_awvalid,
  output logic        hp_awready,
  input  logic [15:0] hp_awaddr,
  input  logic        hp_wvalid,
  output logic        hp_wready,
  input  logic [31:0] hp_wdata,
  input  logic [3:0]  hp_wstrb,
  input  mailbox_pkg::mailbox_tag_t hp_tag,
  output logic        hp_bvalid,
  input  logic        hp_bready,

  input  logic        hp_arvalid,
  output logic        hp_arready,
  input  logic [15:0] hp_araddr,
  output logic        hp_rvalid,
  input  logic        hp_rready,
  output logic [31:0] hp_rdata,

  // Masters out to switches (toward clusters)
  output logic        m_sw0_awvalid,
  input  logic        m_sw0_awready,
  output logic [15:0] m_sw0_awaddr,
  output logic        m_sw0_wvalid,
  input  logic        m_sw0_wready,
  output logic [31:0] m_sw0_wdata,
  output logic [3:0]  m_sw0_wstrb,
  output mailbox_pkg::mailbox_tag_t m_sw0_tag,
  output logic        m_sw0_bready,
  input  logic        m_sw0_bvalid,

  output logic        m_sw0_arvalid,
  input  logic        m_sw0_arready,
  output logic [15:0] m_sw0_araddr,
  input  logic        m_sw0_rvalid,
  output logic        m_sw0_rready,
  input  logic [31:0] m_sw0_rdata,

  output logic        m_sw1_awvalid,
  input  logic        m_sw1_awready,
  output logic [15:0] m_sw1_awaddr,
  output logic        m_sw1_wvalid,
  input  logic        m_sw1_wready,
  output logic [31:0] m_sw1_wdata,
  output logic [3:0]  m_sw1_wstrb,
  output mailbox_pkg::mailbox_tag_t m_sw1_tag,
  output logic        m_sw1_bready,
  input  logic        m_sw1_bvalid,

  output logic        m_sw1_arvalid,
  input  logic        m_sw1_arready,
  output logic [15:0] m_sw1_araddr,
  input  logic        m_sw1_rvalid,
  output logic        m_sw1_rready,
  input  logic [31:0] m_sw1_rdata,

  output logic        m_sw2_awvalid,
  input  logic        m_sw2_awready,
  output logic [15:0] m_sw2_awaddr,
  output logic        m_sw2_wvalid,
  input  logic        m_sw2_wready,
  output logic [31:0] m_sw2_wdata,
  output logic [3:0]  m_sw2_wstrb,
  output mailbox_pkg::mailbox_tag_t m_sw2_tag,
  output logic        m_sw2_bready,
  input  logic        m_sw2_bvalid,

  output logic        m_sw2_arvalid,
  input  logic        m_sw2_arready,
  output logic [15:0] m_sw2_araddr,
  input  logic        m_sw2_rvalid,
  output logic        m_sw2_rready,
  input  logic [31:0] m_sw2_rdata,

  output logic        m_sw3_awvalid,
  input  logic        m_sw3_awready,
  output logic [15:0] m_sw3_awaddr,
  output logic        m_sw3_wvalid,
  input  logic        m_sw3_wready,
  output logic [31:0] m_sw3_wdata,
  output logic [3:0]  m_sw3_wstrb,
  output mailbox_pkg::mailbox_tag_t m_sw3_tag,
  output logic        m_sw3_bready,
  input  logic        m_sw3_bvalid,

  output logic        m_sw3_arvalid,
  input  logic        m_sw3_arready,
  output logic [15:0] m_sw3_araddr,
  input  logic        m_sw3_rvalid,
  output logic        m_sw3_rready,
  input  logic [31:0] m_sw3_rdata,

  // Master out to HP endpoint
  output logic        m_hp_awvalid,
  input  logic        m_hp_awready,
  output logic [15:0] m_hp_awaddr,
  output logic        m_hp_wvalid,
  input  logic        m_hp_wready,
  output logic [31:0] m_hp_wdata,
  output logic [3:0]  m_hp_wstrb,
  output mailbox_pkg::mailbox_tag_t m_hp_tag,
  output logic        m_hp_bready,
  input  logic        m_hp_bvalid,

  output logic        m_hp_arvalid,
  input  logic        m_hp_arready,
  output logic [15:0] m_hp_araddr,
  input  logic        m_hp_rvalid,
  output logic        m_hp_rready,
  input  logic [31:0] m_hp_rdata
);

  import mailbox_pkg::*;

  localparam int N_IN  = 5; // sw0..sw3, hp
  localparam int N_OUT = 5; // sw0..sw3, hp
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

  assign ingress_bready = '{sw0_bready, sw1_bready, sw2_bready, sw3_bready, hp_bready};

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

  // Read path state
  logic [N_IN-1:0] ar_outstanding;
  logic [N_OUT-1:0] ar_pending;
  logic [$clog2(N_IN)-1:0] ar_pending_src [N_OUT];
  logic [RR_W-1:0] rr_out_ar [N_OUT];

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

  function automatic logic [N_OUT-1:0] decode_read_target(
    input logic [15:0] adr
  );
    logic [7:0] cluster;
    logic [N_OUT-1:0] tgt;
    begin
      cluster = adr[15:8];
      tgt = '0;
      if (cluster == 8'h00) begin
        tgt[4] = 1'b1;
      end else begin
        for (int i = 0; i < 4; i++) begin
          if (cluster == CHILD_CLUSTER_ID[i]) tgt[i] = 1'b1;
        end
      end
      decode_read_target = tgt;
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

  // AR/R routing helpers
  function automatic logic [15:0] araddr_for_idx(input int idx);
    case (idx)
      0: araddr_for_idx = sw0_araddr;
      1: araddr_for_idx = sw1_araddr;
      2: araddr_for_idx = sw2_araddr;
      3: araddr_for_idx = sw3_araddr;
      default: araddr_for_idx = hp_araddr;
    endcase
  endfunction

  function automatic logic source_rready(
    input int src
  );
    case (src)
      0: source_rready = sw0_rready;
      1: source_rready = sw1_rready;
      2: source_rready = sw2_rready;
      3: source_rready = sw3_rready;
      default: source_rready = hp_rready;
    endcase
  endfunction

  function automatic logic [31:0] target_rdata(
    input int tgt
  );
    case (tgt)
      0: target_rdata = m_sw0_rdata;
      1: target_rdata = m_sw1_rdata;
      2: target_rdata = m_sw2_rdata;
      3: target_rdata = m_sw3_rdata;
      default: target_rdata = m_hp_rdata;
    endcase
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
      idx = 0;
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

    ingress_req[0] = sw0_awvalid && sw0_wvalid && !resp_pending[0];
    ingress_req[1] = sw1_awvalid && sw1_wvalid && !resp_pending[1];
    ingress_req[2] = sw2_awvalid && sw2_wvalid && !resp_pending[2];
    ingress_req[3] = sw3_awvalid && sw3_wvalid && !resp_pending[3];
    ingress_req[4] = hp_awvalid  && hp_wvalid  && !resp_pending[4];

    ingress_tgt[0] = decode_targets(sw0_awaddr, 1'b0);
    ingress_tgt[1] = decode_targets(sw1_awaddr, 1'b0);
    ingress_tgt[2] = decode_targets(sw2_awaddr, 1'b0);
    ingress_tgt[3] = decode_targets(sw3_awaddr, 1'b0);
    ingress_tgt[4] = decode_targets(hp_awaddr, 1'b1);

    ingress_flit[0] = '{adr:sw0_awaddr, dat:sw0_wdata, strb:sw0_wstrb, tag:update_tag(sw0_tag, sw0_wdata)};
    ingress_flit[1] = '{adr:sw1_awaddr, dat:sw1_wdata, strb:sw1_wstrb, tag:update_tag(sw1_tag, sw1_wdata)};
    ingress_flit[2] = '{adr:sw2_awaddr, dat:sw2_wdata, strb:sw2_wstrb, tag:update_tag(sw2_tag, sw2_wdata)};
    ingress_flit[3] = '{adr:sw3_awaddr, dat:sw3_wdata, strb:sw3_wstrb, tag:update_tag(sw3_tag, sw3_wdata)};
    ingress_flit[4] = '{adr:hp_awaddr,  dat:hp_wdata,  strb:hp_wstrb,  tag:update_tag(hp_tag,  hp_wdata)};
  end

  // Read requests
  logic [N_IN-1:0] ar_req;
  logic [N_IN-1:0][N_OUT-1:0] ar_tgt;
  logic ar_sel_valid [N_OUT];
  logic [$clog2(N_IN)-1:0] ar_sel_idx [N_OUT];
  logic [N_OUT-1:0] target_arready;
  logic [N_OUT-1:0] target_rvalid;
  logic [N_OUT-1:0] ar_fire_out;
  logic [N_OUT-1:0] r_hs_out;
  logic [N_IN-1:0] ar_fire_i;
  logic [N_IN-1:0] r_hs_i;

  always_comb begin
    int idx;
    ar_req = '{default:1'b0};
    ar_tgt = '{default:'0};
    ar_sel_valid = '{default:1'b0};
    ar_sel_idx = '{default:'0};

    target_arready[0] = m_sw0_arready;
    target_arready[1] = m_sw1_arready;
    target_arready[2] = m_sw2_arready;
    target_arready[3] = m_sw3_arready;
    target_arready[4] = m_hp_arready;

    target_rvalid[0] = m_sw0_rvalid;
    target_rvalid[1] = m_sw1_rvalid;
    target_rvalid[2] = m_sw2_rvalid;
    target_rvalid[3] = m_sw3_rvalid;
    target_rvalid[4] = m_hp_rvalid;

    ar_req[0] = sw0_arvalid && !ar_outstanding[0];
    ar_req[1] = sw1_arvalid && !ar_outstanding[1];
    ar_req[2] = sw2_arvalid && !ar_outstanding[2];
    ar_req[3] = sw3_arvalid && !ar_outstanding[3];
    ar_req[4] = hp_arvalid  && !ar_outstanding[4];

    ar_tgt[0] = decode_read_target(sw0_araddr);
    ar_tgt[1] = decode_read_target(sw1_araddr);
    ar_tgt[2] = decode_read_target(sw2_araddr);
    ar_tgt[3] = decode_read_target(sw3_araddr);
    ar_tgt[4] = decode_read_target(hp_araddr);

    for (int o = 0; o < N_OUT; o++) begin
      int found;
      found = -1;
      idx = 0;
      if (!ar_pending[o]) begin
        for (int k = 0; k < N_IN; k++) begin
          idx = rr_out_ar[o] + k;
          if (idx >= N_IN) idx = idx - N_IN;
          if (ar_req[idx] && ar_tgt[idx][o]) begin
            found = idx;
            break;
          end
        end
      end
      if (found != -1) begin
        ar_sel_valid[o] = 1'b1;
        ar_sel_idx[o]   = found[$clog2(N_IN)-1:0];
      end
    end
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

  // FIFO write + AXI ready
  always_comb begin
    fifo_w_en     = '0;
    fifo_w_data   = '{default:'0};
    ingress_accept = '{default:1'b0};

    sw0_awready = 1'b0; sw0_wready = 1'b0; sw0_bvalid = resp_pending[0];
    sw1_awready = 1'b0; sw1_wready = 1'b0; sw1_bvalid = resp_pending[1];
    sw2_awready = 1'b0; sw2_wready = 1'b0; sw2_bvalid = resp_pending[2];
    sw3_awready = 1'b0; sw3_wready = 1'b0; sw3_bvalid = resp_pending[3];
    hp_awready  = 1'b0; hp_wready  = 1'b0; hp_bvalid  = resp_pending[4];

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
        0: begin sw0_awready = 1'b1; sw0_wready = 1'b1; end
        1: begin sw1_awready = 1'b1; sw1_wready = 1'b1; end
        2: begin sw2_awready = 1'b1; sw2_wready = 1'b1; end
        3: begin sw3_awready = 1'b1; sw3_wready = 1'b1; end
        default: begin hp_awready = 1'b1; hp_wready = 1'b1; end
      endcase
    end
  end

  // AR channel routing
  always_comb begin
    sw0_arready = 1'b0; sw1_arready = 1'b0; sw2_arready = 1'b0; sw3_arready = 1'b0; hp_arready = 1'b0;

    m_sw0_arvalid = ar_sel_valid[0];
    m_sw1_arvalid = ar_sel_valid[1];
    m_sw2_arvalid = ar_sel_valid[2];
    m_sw3_arvalid = ar_sel_valid[3];
    m_hp_arvalid  = ar_sel_valid[4];

    ar_fire_out[0] = m_sw0_arvalid && m_sw0_arready;
    ar_fire_out[1] = m_sw1_arvalid && m_sw1_arready;
    ar_fire_out[2] = m_sw2_arvalid && m_sw2_arready;
    ar_fire_out[3] = m_sw3_arvalid && m_sw3_arready;
    ar_fire_out[4] = m_hp_arvalid  && m_hp_arready;

    m_sw0_araddr = araddr_for_idx(ar_sel_idx[0]);
    m_sw1_araddr = araddr_for_idx(ar_sel_idx[1]);
    m_sw2_araddr = araddr_for_idx(ar_sel_idx[2]);
    m_sw3_araddr = araddr_for_idx(ar_sel_idx[3]);
    m_hp_araddr  = araddr_for_idx(ar_sel_idx[4]);

    for (int o = 0; o < N_OUT; o++) begin
      if (ar_sel_valid[o]) begin
        case (ar_sel_idx[o])
          0: sw0_arready = sw0_arready | target_arready[o];
          1: sw1_arready = sw1_arready | target_arready[o];
          2: sw2_arready = sw2_arready | target_arready[o];
          3: sw3_arready = sw3_arready | target_arready[o];
          default: hp_arready = hp_arready | target_arready[o];
        endcase
      end
    end
  end

  // R channel routing
  always_comb begin
    sw0_rvalid = 1'b0; sw1_rvalid = 1'b0; sw2_rvalid = 1'b0; sw3_rvalid = 1'b0; hp_rvalid = 1'b0;
    sw0_rdata  = '0;   sw1_rdata  = '0;   sw2_rdata  = '0;   sw3_rdata  = '0;   hp_rdata  = '0;

    m_sw0_rready = 1'b0; m_sw1_rready = 1'b0; m_sw2_rready = 1'b0; m_sw3_rready = 1'b0; m_hp_rready = 1'b0;

    for (int o = 0; o < N_OUT; o++) begin
      if (ar_pending[o]) begin
        case (ar_pending_src[o])
          0: begin sw0_rvalid = sw0_rvalid | target_rvalid[o]; sw0_rdata = target_rdata(o); end
          1: begin sw1_rvalid = sw1_rvalid | target_rvalid[o]; sw1_rdata = target_rdata(o); end
          2: begin sw2_rvalid = sw2_rvalid | target_rvalid[o]; sw2_rdata = target_rdata(o); end
          3: begin sw3_rvalid = sw3_rvalid | target_rvalid[o]; sw3_rdata = target_rdata(o); end
          default: begin hp_rvalid = hp_rvalid | target_rvalid[o]; hp_rdata = target_rdata(o); end
        endcase

        case (o)
          0: m_sw0_rready = source_rready(ar_pending_src[o]);
          1: m_sw1_rready = source_rready(ar_pending_src[o]);
          2: m_sw2_rready = source_rready(ar_pending_src[o]);
          3: m_sw3_rready = source_rready(ar_pending_src[o]);
          default: m_hp_rready  = source_rready(ar_pending_src[o]);
        endcase
      end
    end

    r_hs_out[0] = m_sw0_rvalid && m_sw0_rready;
    r_hs_out[1] = m_sw1_rvalid && m_sw1_rready;
    r_hs_out[2] = m_sw2_rvalid && m_sw2_rready;
    r_hs_out[3] = m_sw3_rvalid && m_sw3_rready;
    r_hs_out[4] = m_hp_rvalid  && m_hp_rready;
  end

  // Ingress handshakes for AR/R tracking
  always_comb begin
    ar_fire_i[0] = sw0_arvalid && sw0_arready;
    ar_fire_i[1] = sw1_arvalid && sw1_arready;
    ar_fire_i[2] = sw2_arvalid && sw2_arready;
    ar_fire_i[3] = sw3_arvalid && sw3_arready;
    ar_fire_i[4] = hp_arvalid  && hp_arready;

    r_hs_i[0] = sw0_rvalid && sw0_rready;
    r_hs_i[1] = sw1_rvalid && sw1_rready;
    r_hs_i[2] = sw2_rvalid && sw2_rready;
    r_hs_i[3] = sw3_rvalid && sw3_rready;
    r_hs_i[4] = hp_rvalid  && hp_rready;
  end

  // Track outstanding B responses per ingress
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      resp_pending <= '{default:1'b0};
      ar_outstanding <= '{default:1'b0};
      ar_pending <= '{default:1'b0};
      ar_pending_src <= '{default:'0};
      rr_out_ar <= '{default:'0};
    end else begin
      for (int i = 0; i < N_IN; i++) begin
        if (resp_pending[i] && ingress_bready[i]) resp_pending[i] <= 1'b0;
        else if (ingress_accept[i]) resp_pending[i] <= 1'b1;
      end

      // Track AR acceptance
      for (int o = 0; o < N_OUT; o++) begin
        if (ar_fire_out[o]) begin
          ar_pending[o] <= 1'b1;
          ar_pending_src[o] <= ar_sel_idx[o];
          rr_out_ar[o] <= (rr_out_ar[o] == N_IN-1) ? '0 : rr_out_ar[o] + 1'b1;
        end else if (r_hs_out[o]) begin
          ar_pending[o] <= 1'b0;
        end
      end

      for (int i = 0; i < N_IN; i++) begin
        if (r_hs_i[i]) ar_outstanding[i] <= 1'b0;
        else if (ar_fire_i[i]) ar_outstanding[i] <= 1'b1;
      end
    end
  end

  // FIFOs
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_north_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[0]), .w_data(fifo_w_data[0]), .w_full(fifo_full[0]),
    .r_en(fifo_r_en[0]), .r_data(fifo_r_data[0]), .r_empty(fifo_empty[0])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_south_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[1]), .w_data(fifo_w_data[1]), .w_full(fifo_full[1]),
    .r_en(fifo_r_en[1]), .r_data(fifo_r_data[1]), .r_empty(fifo_empty[1])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_west_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[2]), .w_data(fifo_w_data[2]), .w_full(fifo_full[2]),
    .r_en(fifo_r_en[2]), .r_data(fifo_r_data[2]), .r_empty(fifo_empty[2])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_east_fifo (
    .clk(clk), .rst_n(rst_n), .clr(1'b0),
    .w_en(fifo_w_en[3]), .w_data(fifo_w_data[3]), .w_full(fifo_full[3]),
    .r_en(fifo_r_en[3]), .r_data(fifo_r_data[3]), .r_empty(fifo_empty[3])
  );
  mailbox_fifo #(.WIDTH(FLIT_W), .DEPTH(FIFO_DEPTH)) u_local_fifo (
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

  assign m_sw0_awvalid = !fifo_empty[0];
  assign m_sw0_wvalid  = !fifo_empty[0];
  assign m_sw0_awaddr  = fifo_head[0].adr;
  assign m_sw0_wdata   = fifo_head[0].dat;
  assign m_sw0_wstrb   = fifo_head[0].strb;
  assign m_sw0_tag     = fifo_head[0].tag;
  assign m_sw0_bready  = 1'b1;

  assign m_sw1_awvalid = !fifo_empty[1];
  assign m_sw1_wvalid  = !fifo_empty[1];
  assign m_sw1_awaddr  = fifo_head[1].adr;
  assign m_sw1_wdata   = fifo_head[1].dat;
  assign m_sw1_wstrb   = fifo_head[1].strb;
  assign m_sw1_tag     = fifo_head[1].tag;
  assign m_sw1_bready  = 1'b1;

  assign m_sw2_awvalid = !fifo_empty[2];
  assign m_sw2_wvalid  = !fifo_empty[2];
  assign m_sw2_awaddr  = fifo_head[2].adr;
  assign m_sw2_wdata   = fifo_head[2].dat;
  assign m_sw2_wstrb   = fifo_head[2].strb;
  assign m_sw2_tag     = fifo_head[2].tag;
  assign m_sw2_bready  = 1'b1;

  assign m_sw3_awvalid = !fifo_empty[3];
  assign m_sw3_wvalid  = !fifo_empty[3];
  assign m_sw3_awaddr  = fifo_head[3].adr;
  assign m_sw3_wdata   = fifo_head[3].dat;
  assign m_sw3_wstrb   = fifo_head[3].strb;
  assign m_sw3_tag     = fifo_head[3].tag;
  assign m_sw3_bready  = 1'b1;

  assign m_hp_awvalid = !fifo_empty[4];
  assign m_hp_wvalid  = !fifo_empty[4];
  assign m_hp_awaddr  = fifo_head[4].adr;
  assign m_hp_wdata   = fifo_head[4].dat;
  assign m_hp_wstrb   = fifo_head[4].strb;
  assign m_hp_tag     = fifo_head[4].tag;
  assign m_hp_bready  = 1'b1;

  assign fifo_r_en[0] = !fifo_empty[0] && m_sw0_awready && m_sw0_wready;
  assign fifo_r_en[1] = !fifo_empty[1] && m_sw1_awready && m_sw1_wready;
  assign fifo_r_en[2] = !fifo_empty[2] && m_sw2_awready && m_sw2_wready;
  assign fifo_r_en[3] = !fifo_empty[3] && m_sw3_awready && m_sw3_wready;
  assign fifo_r_en[4] = !fifo_empty[4] && m_hp_awready  && m_hp_wready;

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
