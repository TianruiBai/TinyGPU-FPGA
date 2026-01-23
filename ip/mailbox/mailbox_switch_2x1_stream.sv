`timescale 1ns/1ps
// AXI‑MailboxFabric Switch 2x1: two downlinks, one uplink (stream-based)
module mailbox_switch_2x1_stream #(
  parameter logic [7:0] MY_CLUSTER_ID = 8'h00
) (
  input  logic clk,
  input  logic rst_n,

  // Downlink ingress (from endpoints)
  input  logic                           dl0_valid,
  output logic                           dl0_ready,
  input  mailbox_pkg::mailbox_flit_t     dl0_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] dl0_dest_id,

  input  logic                           dl1_valid,
  output logic                           dl1_ready,
  input  mailbox_pkg::mailbox_flit_t     dl1_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] dl1_dest_id,

  // Uplink ingress (from center)
  input  logic                           up_valid,
  output logic                           up_ready,
  input  mailbox_pkg::mailbox_flit_t     up_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] up_dest_id,

  // Uplink egress (to center)
  output logic                           m_up_valid,
  input  logic                           m_up_ready,
  output mailbox_pkg::mailbox_flit_t     m_up_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_up_dest_id,

  // Downlink egress (to endpoints)
  output logic                           m_dl0_valid,
  input  logic                           m_dl0_ready,
  output mailbox_pkg::mailbox_flit_t     m_dl0_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_dl0_dest_id,

  output logic                           m_dl1_valid,
  input  logic                           m_dl1_ready,
  output mailbox_pkg::mailbox_flit_t     m_dl1_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_dl1_dest_id
);
  import mailbox_pkg::*;

  // Decode helpers
  function automatic logic is_global(input logic [NODE_ID_WIDTH-1:0] dest);
    return (dest[15:8] == 8'hFF);
  endfunction
  function automatic logic is_local(input logic [NODE_ID_WIDTH-1:0] dest);
    return (dest[15:8] == MY_CLUSTER_ID);
  endfunction
  function automatic logic is_local_bcast(input logic [NODE_ID_WIDTH-1:0] dest);
    return (dest[15:8] == MY_CLUSTER_ID) && (dest[7:4] == 4'hF);
  endfunction

  // Request signals per input
  logic dl0_req_up, dl0_req_dl0, dl0_req_dl1, dl0_req_any;
  logic dl1_req_up, dl1_req_dl0, dl1_req_dl1, dl1_req_any;
  logic up_req_dl0,  up_req_dl1,  up_req_any;

  always_comb begin
    // dl0
    dl0_req_up   = dl0_valid && (is_global(dl0_dest_id) || !is_local(dl0_dest_id));
    dl0_req_dl0  = dl0_valid && (is_global(dl0_dest_id) || is_local_bcast(dl0_dest_id) || (is_local(dl0_dest_id) && dl0_dest_id[7:4] == 4'h0));
    dl0_req_dl1  = dl0_valid && (is_global(dl0_dest_id) || is_local_bcast(dl0_dest_id) || (is_local(dl0_dest_id) && dl0_dest_id[7:4] == 4'h1));
    dl0_req_any  = dl0_req_up || dl0_req_dl0 || dl0_req_dl1;

    // dl1
    dl1_req_up   = dl1_valid && (is_global(dl1_dest_id) || !is_local(dl1_dest_id));
    dl1_req_dl0  = dl1_valid && (is_global(dl1_dest_id) || is_local_bcast(dl1_dest_id) || (is_local(dl1_dest_id) && dl1_dest_id[7:4] == 4'h0));
    dl1_req_dl1  = dl1_valid && (is_global(dl1_dest_id) || is_local_bcast(dl1_dest_id) || (is_local(dl1_dest_id) && dl1_dest_id[7:4] == 4'h1));
    dl1_req_any  = dl1_req_up || dl1_req_dl0 || dl1_req_dl1;

    // up (from center) only routes locally / global
    up_req_dl0   = up_valid && (is_global(up_dest_id) || is_local_bcast(up_dest_id) || (is_local(up_dest_id) && up_dest_id[7:4] == 4'h0));
    up_req_dl1   = up_valid && (is_global(up_dest_id) || is_local_bcast(up_dest_id) || (is_local(up_dest_id) && up_dest_id[7:4] == 4'h1));
    up_req_any   = up_req_dl0 || up_req_dl1;
  end

  // Grants (fixed priority)
  logic g_up_from_dl0, g_up_from_dl1;
  logic g_dl0_from_up, g_dl0_from_dl0, g_dl0_from_dl1;
  logic g_dl1_from_up, g_dl1_from_dl0, g_dl1_from_dl1;

  assign g_up_from_dl0 = dl0_req_up;
  assign g_up_from_dl1 = !g_up_from_dl0 && dl1_req_up;

  assign g_dl0_from_up  = up_req_dl0;
  assign g_dl0_from_dl0 = !g_dl0_from_up && dl0_req_dl0;
  assign g_dl0_from_dl1 = !g_dl0_from_up && !g_dl0_from_dl0 && dl1_req_dl0;

  assign g_dl1_from_up  = up_req_dl1;
  assign g_dl1_from_dl0 = !g_dl1_from_up && dl0_req_dl1;
  assign g_dl1_from_dl1 = !g_dl1_from_up && !g_dl1_from_dl0 && dl1_req_dl1;

  // Output valids/data
  always_comb begin
    m_up_valid     = g_up_from_dl0 || g_up_from_dl1;
    m_up_data      = g_up_from_dl0 ? dl0_data : dl1_data;
    m_up_dest_id   = g_up_from_dl0 ? dl0_dest_id : dl1_dest_id;

    m_dl0_valid    = g_dl0_from_up || g_dl0_from_dl0 || g_dl0_from_dl1;
    m_dl0_data     = g_dl0_from_up ? up_data : (g_dl0_from_dl0 ? dl0_data : dl1_data);
    m_dl0_dest_id  = g_dl0_from_up ? up_dest_id : (g_dl0_from_dl0 ? dl0_dest_id : dl1_dest_id);

    m_dl1_valid    = g_dl1_from_up || g_dl1_from_dl0 || g_dl1_from_dl1;
    m_dl1_data     = g_dl1_from_up ? up_data : (g_dl1_from_dl0 ? dl0_data : dl1_data);
    m_dl1_dest_id  = g_dl1_from_up ? up_dest_id : (g_dl1_from_dl0 ? dl0_dest_id : dl1_dest_id);
  end

  // Input readies (must win all requested outputs)
  always_comb begin
    dl0_ready = (!dl0_req_any) ? 1'b1 :
                ((!dl0_req_up  || (g_up_from_dl0  && m_up_ready)) &&
                 (!dl0_req_dl0 || (g_dl0_from_dl0 && m_dl0_ready)) &&
                 (!dl0_req_dl1 || (g_dl1_from_dl0 && m_dl1_ready)));

    dl1_ready = (!dl1_req_any) ? 1'b1 :
                ((!dl1_req_up  || (g_up_from_dl1  && m_up_ready)) &&
                 (!dl1_req_dl0 || (g_dl0_from_dl1 && m_dl0_ready)) &&
                 (!dl1_req_dl1 || (g_dl1_from_dl1 && m_dl1_ready)));

    up_ready  = (!up_req_any) ? 1'b1 :
                ((!up_req_dl0 || (g_dl0_from_up && m_dl0_ready)) &&
                 (!up_req_dl1 || (g_dl1_from_up && m_dl1_ready)));
  end

endmodule
