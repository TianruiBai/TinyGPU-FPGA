`timescale 1ns/1ps
// AXI‑MailboxFabric Center (Root Router): stream-based
module mailbox_center_stream #(
  parameter logic [7:0] CHILD_CLUSTER_ID [0:3] = '{8'h01, 8'h02, 8'h03, 8'h04}
) (
  input  logic clk,
  input  logic rst_n,

  // Ingress from switches
  input  logic                           sw0_valid,
  output logic                           sw0_ready,
  input  mailbox_pkg::mailbox_flit_t     sw0_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] sw0_dest_id,

  input  logic                           sw1_valid,
  output logic                           sw1_ready,
  input  mailbox_pkg::mailbox_flit_t     sw1_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] sw1_dest_id,

  input  logic                           sw2_valid,
  output logic                           sw2_ready,
  input  mailbox_pkg::mailbox_flit_t     sw2_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] sw2_dest_id,

  input  logic                           sw3_valid,
  output logic                           sw3_ready,
  input  mailbox_pkg::mailbox_flit_t     sw3_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] sw3_dest_id,

  // Ingress from HP endpoint (MCU)
  input  logic                           hp_valid,
  output logic                           hp_ready,
  input  mailbox_pkg::mailbox_flit_t     hp_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] hp_dest_id,

  // Egress to switches
  output logic                           m_sw0_valid,
  input  logic                           m_sw0_ready,
  output mailbox_pkg::mailbox_flit_t     m_sw0_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_sw0_dest_id,

  output logic                           m_sw1_valid,
  input  logic                           m_sw1_ready,
  output mailbox_pkg::mailbox_flit_t     m_sw1_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_sw1_dest_id,

  output logic                           m_sw2_valid,
  input  logic                           m_sw2_ready,
  output mailbox_pkg::mailbox_flit_t     m_sw2_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_sw2_dest_id,

  output logic                           m_sw3_valid,
  input  logic                           m_sw3_ready,
  output mailbox_pkg::mailbox_flit_t     m_sw3_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_sw3_dest_id,

  // Egress to HP endpoint
  output logic                           m_hp_valid,
  input  logic                           m_hp_ready,
  output mailbox_pkg::mailbox_flit_t     m_hp_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_hp_dest_id
);
  import mailbox_pkg::*;

  function automatic logic is_global(input logic [NODE_ID_WIDTH-1:0] dest);
    return (dest[15:8] == 8'hFF);
  endfunction

  function automatic logic is_cluster(input logic [NODE_ID_WIDTH-1:0] dest, input logic [7:0] cid);
    return (dest[15:8] == cid);
  endfunction

  // Request signals per input
  logic sw0_req_sw0, sw0_req_sw1, sw0_req_sw2, sw0_req_sw3, sw0_req_hp, sw0_req_any;
  logic sw1_req_sw0, sw1_req_sw1, sw1_req_sw2, sw1_req_sw3, sw1_req_hp, sw1_req_any;
  logic sw2_req_sw0, sw2_req_sw1, sw2_req_sw2, sw2_req_sw3, sw2_req_hp, sw2_req_any;
  logic sw3_req_sw0, sw3_req_sw1, sw3_req_sw2, sw3_req_sw3, sw3_req_hp, sw3_req_any;
  logic hp_req_sw0,  hp_req_sw1,  hp_req_sw2,  hp_req_sw3,  hp_req_hp,  hp_req_any;

  always_comb begin
    // sw0
    sw0_req_hp  = sw0_valid && (is_global(sw0_dest_id) || is_cluster(sw0_dest_id, 8'h00));
    sw0_req_sw0 = sw0_valid && (is_global(sw0_dest_id) || is_cluster(sw0_dest_id, CHILD_CLUSTER_ID[0]));
    sw0_req_sw1 = sw0_valid && (is_global(sw0_dest_id) || is_cluster(sw0_dest_id, CHILD_CLUSTER_ID[1]));
    sw0_req_sw2 = sw0_valid && (is_global(sw0_dest_id) || is_cluster(sw0_dest_id, CHILD_CLUSTER_ID[2]));
    sw0_req_sw3 = sw0_valid && (is_global(sw0_dest_id) || is_cluster(sw0_dest_id, CHILD_CLUSTER_ID[3]));
    sw0_req_any = sw0_req_hp || sw0_req_sw0 || sw0_req_sw1 || sw0_req_sw2 || sw0_req_sw3;

    // sw1
    sw1_req_hp  = sw1_valid && (is_global(sw1_dest_id) || is_cluster(sw1_dest_id, 8'h00));
    sw1_req_sw0 = sw1_valid && (is_global(sw1_dest_id) || is_cluster(sw1_dest_id, CHILD_CLUSTER_ID[0]));
    sw1_req_sw1 = sw1_valid && (is_global(sw1_dest_id) || is_cluster(sw1_dest_id, CHILD_CLUSTER_ID[1]));
    sw1_req_sw2 = sw1_valid && (is_global(sw1_dest_id) || is_cluster(sw1_dest_id, CHILD_CLUSTER_ID[2]));
    sw1_req_sw3 = sw1_valid && (is_global(sw1_dest_id) || is_cluster(sw1_dest_id, CHILD_CLUSTER_ID[3]));
    sw1_req_any = sw1_req_hp || sw1_req_sw0 || sw1_req_sw1 || sw1_req_sw2 || sw1_req_sw3;

    // sw2
    sw2_req_hp  = sw2_valid && (is_global(sw2_dest_id) || is_cluster(sw2_dest_id, 8'h00));
    sw2_req_sw0 = sw2_valid && (is_global(sw2_dest_id) || is_cluster(sw2_dest_id, CHILD_CLUSTER_ID[0]));
    sw2_req_sw1 = sw2_valid && (is_global(sw2_dest_id) || is_cluster(sw2_dest_id, CHILD_CLUSTER_ID[1]));
    sw2_req_sw2 = sw2_valid && (is_global(sw2_dest_id) || is_cluster(sw2_dest_id, CHILD_CLUSTER_ID[2]));
    sw2_req_sw3 = sw2_valid && (is_global(sw2_dest_id) || is_cluster(sw2_dest_id, CHILD_CLUSTER_ID[3]));
    sw2_req_any = sw2_req_hp || sw2_req_sw0 || sw2_req_sw1 || sw2_req_sw2 || sw2_req_sw3;

    // sw3
    sw3_req_hp  = sw3_valid && (is_global(sw3_dest_id) || is_cluster(sw3_dest_id, 8'h00));
    sw3_req_sw0 = sw3_valid && (is_global(sw3_dest_id) || is_cluster(sw3_dest_id, CHILD_CLUSTER_ID[0]));
    sw3_req_sw1 = sw3_valid && (is_global(sw3_dest_id) || is_cluster(sw3_dest_id, CHILD_CLUSTER_ID[1]));
    sw3_req_sw2 = sw3_valid && (is_global(sw3_dest_id) || is_cluster(sw3_dest_id, CHILD_CLUSTER_ID[2]));
    sw3_req_sw3 = sw3_valid && (is_global(sw3_dest_id) || is_cluster(sw3_dest_id, CHILD_CLUSTER_ID[3]));
    sw3_req_any = sw3_req_hp || sw3_req_sw0 || sw3_req_sw1 || sw3_req_sw2 || sw3_req_sw3;

    // hp
    hp_req_hp  = hp_valid && (is_global(hp_dest_id) || is_cluster(hp_dest_id, 8'h00));
    hp_req_sw0 = hp_valid && (is_global(hp_dest_id) || is_cluster(hp_dest_id, CHILD_CLUSTER_ID[0]));
    hp_req_sw1 = hp_valid && (is_global(hp_dest_id) || is_cluster(hp_dest_id, CHILD_CLUSTER_ID[1]));
    hp_req_sw2 = hp_valid && (is_global(hp_dest_id) || is_cluster(hp_dest_id, CHILD_CLUSTER_ID[2]));
    hp_req_sw3 = hp_valid && (is_global(hp_dest_id) || is_cluster(hp_dest_id, CHILD_CLUSTER_ID[3]));
    hp_req_any = hp_req_hp || hp_req_sw0 || hp_req_sw1 || hp_req_sw2 || hp_req_sw3;
  end

  // Grants per output (priority: hp > sw0 > sw1 > sw2 > sw3)
  logic g_hp_from_hp, g_hp_from_sw0, g_hp_from_sw1, g_hp_from_sw2, g_hp_from_sw3;
  logic g_sw0_from_hp, g_sw0_from_sw0, g_sw0_from_sw1, g_sw0_from_sw2, g_sw0_from_sw3;
  logic g_sw1_from_hp, g_sw1_from_sw0, g_sw1_from_sw1, g_sw1_from_sw2, g_sw1_from_sw3;
  logic g_sw2_from_hp, g_sw2_from_sw0, g_sw2_from_sw1, g_sw2_from_sw2, g_sw2_from_sw3;
  logic g_sw3_from_hp, g_sw3_from_sw0, g_sw3_from_sw1, g_sw3_from_sw2, g_sw3_from_sw3;

  assign g_hp_from_hp  = hp_req_hp;
  assign g_hp_from_sw0 = !g_hp_from_hp && sw0_req_hp;
  assign g_hp_from_sw1 = !g_hp_from_hp && !g_hp_from_sw0 && sw1_req_hp;
  assign g_hp_from_sw2 = !g_hp_from_hp && !g_hp_from_sw0 && !g_hp_from_sw1 && sw2_req_hp;
  assign g_hp_from_sw3 = !g_hp_from_hp && !g_hp_from_sw0 && !g_hp_from_sw1 && !g_hp_from_sw2 && sw3_req_hp;

  assign g_sw0_from_hp  = hp_req_sw0;
  assign g_sw0_from_sw0 = !g_sw0_from_hp && sw0_req_sw0;
  assign g_sw0_from_sw1 = !g_sw0_from_hp && !g_sw0_from_sw0 && sw1_req_sw0;
  assign g_sw0_from_sw2 = !g_sw0_from_hp && !g_sw0_from_sw0 && !g_sw0_from_sw1 && sw2_req_sw0;
  assign g_sw0_from_sw3 = !g_sw0_from_hp && !g_sw0_from_sw0 && !g_sw0_from_sw1 && !g_sw0_from_sw2 && sw3_req_sw0;

  assign g_sw1_from_hp  = hp_req_sw1;
  assign g_sw1_from_sw0 = !g_sw1_from_hp && sw0_req_sw1;
  assign g_sw1_from_sw1 = !g_sw1_from_hp && !g_sw1_from_sw0 && sw1_req_sw1;
  assign g_sw1_from_sw2 = !g_sw1_from_hp && !g_sw1_from_sw0 && !g_sw1_from_sw1 && sw2_req_sw1;
  assign g_sw1_from_sw3 = !g_sw1_from_hp && !g_sw1_from_sw0 && !g_sw1_from_sw1 && !g_sw1_from_sw2 && sw3_req_sw1;

  assign g_sw2_from_hp  = hp_req_sw2;
  assign g_sw2_from_sw0 = !g_sw2_from_hp && sw0_req_sw2;
  assign g_sw2_from_sw1 = !g_sw2_from_hp && !g_sw2_from_sw0 && sw1_req_sw2;
  assign g_sw2_from_sw2 = !g_sw2_from_hp && !g_sw2_from_sw0 && !g_sw2_from_sw1 && sw2_req_sw2;
  assign g_sw2_from_sw3 = !g_sw2_from_hp && !g_sw2_from_sw0 && !g_sw2_from_sw1 && !g_sw2_from_sw2 && sw3_req_sw2;

  assign g_sw3_from_hp  = hp_req_sw3;
  assign g_sw3_from_sw0 = !g_sw3_from_hp && sw0_req_sw3;
  assign g_sw3_from_sw1 = !g_sw3_from_hp && !g_sw3_from_sw0 && sw1_req_sw3;
  assign g_sw3_from_sw2 = !g_sw3_from_hp && !g_sw3_from_sw0 && !g_sw3_from_sw1 && sw2_req_sw3;
  assign g_sw3_from_sw3 = !g_sw3_from_hp && !g_sw3_from_sw0 && !g_sw3_from_sw1 && !g_sw3_from_sw2 && sw3_req_sw3;

  // Output valids/data
  always_comb begin
    m_hp_valid = g_hp_from_hp || g_hp_from_sw0 || g_hp_from_sw1 || g_hp_from_sw2 || g_hp_from_sw3;
    if (g_hp_from_hp) begin
      m_hp_data    = hp_data;
      m_hp_dest_id = hp_dest_id;
    end else if (g_hp_from_sw0) begin
      m_hp_data    = sw0_data;
      m_hp_dest_id = sw0_dest_id;
    end else if (g_hp_from_sw1) begin
      m_hp_data    = sw1_data;
      m_hp_dest_id = sw1_dest_id;
    end else if (g_hp_from_sw2) begin
      m_hp_data    = sw2_data;
      m_hp_dest_id = sw2_dest_id;
    end else begin
      m_hp_data    = sw3_data;
      m_hp_dest_id = sw3_dest_id;
    end

    m_sw0_valid = g_sw0_from_hp || g_sw0_from_sw0 || g_sw0_from_sw1 || g_sw0_from_sw2 || g_sw0_from_sw3;
    if (g_sw0_from_hp) begin
      m_sw0_data   = hp_data;
      m_sw0_dest_id= hp_dest_id;
    end else if (g_sw0_from_sw0) begin
      m_sw0_data   = sw0_data;
      m_sw0_dest_id= sw0_dest_id;
    end else if (g_sw0_from_sw1) begin
      m_sw0_data   = sw1_data;
      m_sw0_dest_id= sw1_dest_id;
    end else if (g_sw0_from_sw2) begin
      m_sw0_data   = sw2_data;
      m_sw0_dest_id= sw2_dest_id;
    end else begin
      m_sw0_data   = sw3_data;
      m_sw0_dest_id= sw3_dest_id;
    end

    m_sw1_valid = g_sw1_from_hp || g_sw1_from_sw0 || g_sw1_from_sw1 || g_sw1_from_sw2 || g_sw1_from_sw3;
    if (g_sw1_from_hp) begin
      m_sw1_data   = hp_data;
      m_sw1_dest_id= hp_dest_id;
    end else if (g_sw1_from_sw0) begin
      m_sw1_data   = sw0_data;
      m_sw1_dest_id= sw0_dest_id;
    end else if (g_sw1_from_sw1) begin
      m_sw1_data   = sw1_data;
      m_sw1_dest_id= sw1_dest_id;
    end else if (g_sw1_from_sw2) begin
      m_sw1_data   = sw2_data;
      m_sw1_dest_id= sw2_dest_id;
    end else begin
      m_sw1_data   = sw3_data;
      m_sw1_dest_id= sw3_dest_id;
    end

    m_sw2_valid = g_sw2_from_hp || g_sw2_from_sw0 || g_sw2_from_sw1 || g_sw2_from_sw2 || g_sw2_from_sw3;
    if (g_sw2_from_hp) begin
      m_sw2_data   = hp_data;
      m_sw2_dest_id= hp_dest_id;
    end else if (g_sw2_from_sw0) begin
      m_sw2_data   = sw0_data;
      m_sw2_dest_id= sw0_dest_id;
    end else if (g_sw2_from_sw1) begin
      m_sw2_data   = sw1_data;
      m_sw2_dest_id= sw1_dest_id;
    end else if (g_sw2_from_sw2) begin
      m_sw2_data   = sw2_data;
      m_sw2_dest_id= sw2_dest_id;
    end else begin
      m_sw2_data   = sw3_data;
      m_sw2_dest_id= sw3_dest_id;
    end

    m_sw3_valid = g_sw3_from_hp || g_sw3_from_sw0 || g_sw3_from_sw1 || g_sw3_from_sw2 || g_sw3_from_sw3;
    if (g_sw3_from_hp) begin
      m_sw3_data   = hp_data;
      m_sw3_dest_id= hp_dest_id;
    end else if (g_sw3_from_sw0) begin
      m_sw3_data   = sw0_data;
      m_sw3_dest_id= sw0_dest_id;
    end else if (g_sw3_from_sw1) begin
      m_sw3_data   = sw1_data;
      m_sw3_dest_id= sw1_dest_id;
    end else if (g_sw3_from_sw2) begin
      m_sw3_data   = sw2_data;
      m_sw3_dest_id= sw2_dest_id;
    end else begin
      m_sw3_data   = sw3_data;
      m_sw3_dest_id= sw3_dest_id;
    end
  end

  // Input readies (must win all requested outputs)
  always_comb begin
    sw0_ready = (!sw0_req_any) ? 1'b1 :
                ((!sw0_req_hp  || (g_hp_from_sw0  && m_hp_ready)) &&
                 (!sw0_req_sw0 || (g_sw0_from_sw0 && m_sw0_ready)) &&
                 (!sw0_req_sw1 || (g_sw1_from_sw0 && m_sw1_ready)) &&
                 (!sw0_req_sw2 || (g_sw2_from_sw0 && m_sw2_ready)) &&
                 (!sw0_req_sw3 || (g_sw3_from_sw0 && m_sw3_ready)));

    sw1_ready = (!sw1_req_any) ? 1'b1 :
                ((!sw1_req_hp  || (g_hp_from_sw1  && m_hp_ready)) &&
                 (!sw1_req_sw0 || (g_sw0_from_sw1 && m_sw0_ready)) &&
                 (!sw1_req_sw1 || (g_sw1_from_sw1 && m_sw1_ready)) &&
                 (!sw1_req_sw2 || (g_sw2_from_sw1 && m_sw2_ready)) &&
                 (!sw1_req_sw3 || (g_sw3_from_sw1 && m_sw3_ready)));

    sw2_ready = (!sw2_req_any) ? 1'b1 :
                ((!sw2_req_hp  || (g_hp_from_sw2  && m_hp_ready)) &&
                 (!sw2_req_sw0 || (g_sw0_from_sw2 && m_sw0_ready)) &&
                 (!sw2_req_sw1 || (g_sw1_from_sw2 && m_sw1_ready)) &&
                 (!sw2_req_sw2 || (g_sw2_from_sw2 && m_sw2_ready)) &&
                 (!sw2_req_sw3 || (g_sw3_from_sw2 && m_sw3_ready)));

    sw3_ready = (!sw3_req_any) ? 1'b1 :
                ((!sw3_req_hp  || (g_hp_from_sw3  && m_hp_ready)) &&
                 (!sw3_req_sw0 || (g_sw0_from_sw3 && m_sw0_ready)) &&
                 (!sw3_req_sw1 || (g_sw1_from_sw3 && m_sw1_ready)) &&
                 (!sw3_req_sw2 || (g_sw2_from_sw3 && m_sw2_ready)) &&
                 (!sw3_req_sw3 || (g_sw3_from_sw3 && m_sw3_ready)));

    hp_ready  = (!hp_req_any) ? 1'b1 :
                ((!hp_req_hp  || (g_hp_from_hp  && m_hp_ready)) &&
                 (!hp_req_sw0 || (g_sw0_from_hp && m_sw0_ready)) &&
                 (!hp_req_sw1 || (g_sw1_from_hp && m_sw1_ready)) &&
                 (!hp_req_sw2 || (g_sw2_from_hp && m_sw2_ready)) &&
                 (!hp_req_sw3 || (g_sw3_from_hp && m_sw3_ready)));
  end

endmodule
