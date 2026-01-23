`timescale 1ns/1ps
// AXI‑MailboxFabric Switch 4x1: four downlinks, one uplink (stream-based)
module mailbox_switch_4x1_stream #(
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

  input  logic                           dl2_valid,
  output logic                           dl2_ready,
  input  mailbox_pkg::mailbox_flit_t     dl2_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] dl2_dest_id,

  input  logic                           dl3_valid,
  output logic                           dl3_ready,
  input  mailbox_pkg::mailbox_flit_t     dl3_data,
  input  logic [mailbox_pkg::NODE_ID_WIDTH-1:0] dl3_dest_id,

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
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_dl1_dest_id,

  output logic                           m_dl2_valid,
  input  logic                           m_dl2_ready,
  output mailbox_pkg::mailbox_flit_t     m_dl2_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_dl2_dest_id,

  output logic                           m_dl3_valid,
  input  logic                           m_dl3_ready,
  output mailbox_pkg::mailbox_flit_t     m_dl3_data,
  output logic [mailbox_pkg::NODE_ID_WIDTH-1:0] m_dl3_dest_id
);
  import mailbox_pkg::*;

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
  logic dl0_req_up, dl0_req_dl0, dl0_req_dl1, dl0_req_dl2, dl0_req_dl3, dl0_req_any;
  logic dl1_req_up, dl1_req_dl0, dl1_req_dl1, dl1_req_dl2, dl1_req_dl3, dl1_req_any;
  logic dl2_req_up, dl2_req_dl0, dl2_req_dl1, dl2_req_dl2, dl2_req_dl3, dl2_req_any;
  logic dl3_req_up, dl3_req_dl0, dl3_req_dl1, dl3_req_dl2, dl3_req_dl3, dl3_req_any;
  logic up_req_dl0,  up_req_dl1,  up_req_dl2,  up_req_dl3,  up_req_any;

  always_comb begin
    // dl0
    dl0_req_up   = dl0_valid && (is_global(dl0_dest_id) || !is_local(dl0_dest_id));
    dl0_req_dl0  = dl0_valid && (is_global(dl0_dest_id) || is_local_bcast(dl0_dest_id) || (is_local(dl0_dest_id) && dl0_dest_id[7:4] == 4'h0));
    dl0_req_dl1  = dl0_valid && (is_global(dl0_dest_id) || is_local_bcast(dl0_dest_id) || (is_local(dl0_dest_id) && dl0_dest_id[7:4] == 4'h1));
    dl0_req_dl2  = dl0_valid && (is_global(dl0_dest_id) || is_local_bcast(dl0_dest_id) || (is_local(dl0_dest_id) && dl0_dest_id[7:4] == 4'h2));
    dl0_req_dl3  = dl0_valid && (is_global(dl0_dest_id) || is_local_bcast(dl0_dest_id) || (is_local(dl0_dest_id) && dl0_dest_id[7:4] == 4'h3));
    dl0_req_any  = dl0_req_up || dl0_req_dl0 || dl0_req_dl1 || dl0_req_dl2 || dl0_req_dl3;

    // dl1
    dl1_req_up   = dl1_valid && (is_global(dl1_dest_id) || !is_local(dl1_dest_id));
    dl1_req_dl0  = dl1_valid && (is_global(dl1_dest_id) || is_local_bcast(dl1_dest_id) || (is_local(dl1_dest_id) && dl1_dest_id[7:4] == 4'h0));
    dl1_req_dl1  = dl1_valid && (is_global(dl1_dest_id) || is_local_bcast(dl1_dest_id) || (is_local(dl1_dest_id) && dl1_dest_id[7:4] == 4'h1));
    dl1_req_dl2  = dl1_valid && (is_global(dl1_dest_id) || is_local_bcast(dl1_dest_id) || (is_local(dl1_dest_id) && dl1_dest_id[7:4] == 4'h2));
    dl1_req_dl3  = dl1_valid && (is_global(dl1_dest_id) || is_local_bcast(dl1_dest_id) || (is_local(dl1_dest_id) && dl1_dest_id[7:4] == 4'h3));
    dl1_req_any  = dl1_req_up || dl1_req_dl0 || dl1_req_dl1 || dl1_req_dl2 || dl1_req_dl3;

    // dl2
    dl2_req_up   = dl2_valid && (is_global(dl2_dest_id) || !is_local(dl2_dest_id));
    dl2_req_dl0  = dl2_valid && (is_global(dl2_dest_id) || is_local_bcast(dl2_dest_id) || (is_local(dl2_dest_id) && dl2_dest_id[7:4] == 4'h0));
    dl2_req_dl1  = dl2_valid && (is_global(dl2_dest_id) || is_local_bcast(dl2_dest_id) || (is_local(dl2_dest_id) && dl2_dest_id[7:4] == 4'h1));
    dl2_req_dl2  = dl2_valid && (is_global(dl2_dest_id) || is_local_bcast(dl2_dest_id) || (is_local(dl2_dest_id) && dl2_dest_id[7:4] == 4'h2));
    dl2_req_dl3  = dl2_valid && (is_global(dl2_dest_id) || is_local_bcast(dl2_dest_id) || (is_local(dl2_dest_id) && dl2_dest_id[7:4] == 4'h3));
    dl2_req_any  = dl2_req_up || dl2_req_dl0 || dl2_req_dl1 || dl2_req_dl2 || dl2_req_dl3;

    // dl3
    dl3_req_up   = dl3_valid && (is_global(dl3_dest_id) || !is_local(dl3_dest_id));
    dl3_req_dl0  = dl3_valid && (is_global(dl3_dest_id) || is_local_bcast(dl3_dest_id) || (is_local(dl3_dest_id) && dl3_dest_id[7:4] == 4'h0));
    dl3_req_dl1  = dl3_valid && (is_global(dl3_dest_id) || is_local_bcast(dl3_dest_id) || (is_local(dl3_dest_id) && dl3_dest_id[7:4] == 4'h1));
    dl3_req_dl2  = dl3_valid && (is_global(dl3_dest_id) || is_local_bcast(dl3_dest_id) || (is_local(dl3_dest_id) && dl3_dest_id[7:4] == 4'h2));
    dl3_req_dl3  = dl3_valid && (is_global(dl3_dest_id) || is_local_bcast(dl3_dest_id) || (is_local(dl3_dest_id) && dl3_dest_id[7:4] == 4'h3));
    dl3_req_any  = dl3_req_up || dl3_req_dl0 || dl3_req_dl1 || dl3_req_dl2 || dl3_req_dl3;

    // up (from center) only routes locally / global
    up_req_dl0   = up_valid && (is_global(up_dest_id) || is_local_bcast(up_dest_id) || (is_local(up_dest_id) && up_dest_id[7:4] == 4'h0));
    up_req_dl1   = up_valid && (is_global(up_dest_id) || is_local_bcast(up_dest_id) || (is_local(up_dest_id) && up_dest_id[7:4] == 4'h1));
    up_req_dl2   = up_valid && (is_global(up_dest_id) || is_local_bcast(up_dest_id) || (is_local(up_dest_id) && up_dest_id[7:4] == 4'h2));
    up_req_dl3   = up_valid && (is_global(up_dest_id) || is_local_bcast(up_dest_id) || (is_local(up_dest_id) && up_dest_id[7:4] == 4'h3));
    up_req_any   = up_req_dl0 || up_req_dl1 || up_req_dl2 || up_req_dl3;
  end

  // Grants (fixed priority)
  logic g_up_from_dl0, g_up_from_dl1, g_up_from_dl2, g_up_from_dl3;
  logic g_dl0_from_up, g_dl0_from_dl0, g_dl0_from_dl1, g_dl0_from_dl2, g_dl0_from_dl3;
  logic g_dl1_from_up, g_dl1_from_dl0, g_dl1_from_dl1, g_dl1_from_dl2, g_dl1_from_dl3;
  logic g_dl2_from_up, g_dl2_from_dl0, g_dl2_from_dl1, g_dl2_from_dl2, g_dl2_from_dl3;
  logic g_dl3_from_up, g_dl3_from_dl0, g_dl3_from_dl1, g_dl3_from_dl2, g_dl3_from_dl3;

  assign g_up_from_dl0 = dl0_req_up;
  assign g_up_from_dl1 = !g_up_from_dl0 && dl1_req_up;
  assign g_up_from_dl2 = !g_up_from_dl0 && !g_up_from_dl1 && dl2_req_up;
  assign g_up_from_dl3 = !g_up_from_dl0 && !g_up_from_dl1 && !g_up_from_dl2 && dl3_req_up;

  assign g_dl0_from_up  = up_req_dl0;
  assign g_dl0_from_dl0 = !g_dl0_from_up && dl0_req_dl0;
  assign g_dl0_from_dl1 = !g_dl0_from_up && !g_dl0_from_dl0 && dl1_req_dl0;
  assign g_dl0_from_dl2 = !g_dl0_from_up && !g_dl0_from_dl0 && !g_dl0_from_dl1 && dl2_req_dl0;
  assign g_dl0_from_dl3 = !g_dl0_from_up && !g_dl0_from_dl0 && !g_dl0_from_dl1 && !g_dl0_from_dl2 && dl3_req_dl0;

  assign g_dl1_from_up  = up_req_dl1;
  assign g_dl1_from_dl0 = !g_dl1_from_up && dl0_req_dl1;
  assign g_dl1_from_dl1 = !g_dl1_from_up && !g_dl1_from_dl0 && dl1_req_dl1;
  assign g_dl1_from_dl2 = !g_dl1_from_up && !g_dl1_from_dl0 && !g_dl1_from_dl1 && dl2_req_dl1;
  assign g_dl1_from_dl3 = !g_dl1_from_up && !g_dl1_from_dl0 && !g_dl1_from_dl1 && !g_dl1_from_dl2 && dl3_req_dl1;

  assign g_dl2_from_up  = up_req_dl2;
  assign g_dl2_from_dl0 = !g_dl2_from_up && dl0_req_dl2;
  assign g_dl2_from_dl1 = !g_dl2_from_up && !g_dl2_from_dl0 && dl1_req_dl2;
  assign g_dl2_from_dl2 = !g_dl2_from_up && !g_dl2_from_dl0 && !g_dl2_from_dl1 && dl2_req_dl2;
  assign g_dl2_from_dl3 = !g_dl2_from_up && !g_dl2_from_dl0 && !g_dl2_from_dl1 && !g_dl2_from_dl2 && dl3_req_dl2;

  assign g_dl3_from_up  = up_req_dl3;
  assign g_dl3_from_dl0 = !g_dl3_from_up && dl0_req_dl3;
  assign g_dl3_from_dl1 = !g_dl3_from_up && !g_dl3_from_dl0 && dl1_req_dl3;
  assign g_dl3_from_dl2 = !g_dl3_from_up && !g_dl3_from_dl0 && !g_dl3_from_dl1 && dl2_req_dl3;
  assign g_dl3_from_dl3 = !g_dl3_from_up && !g_dl3_from_dl0 && !g_dl3_from_dl1 && !g_dl3_from_dl2 && dl3_req_dl3;

  // Output valids/data
  always_comb begin
    m_up_valid     = g_up_from_dl0 || g_up_from_dl1 || g_up_from_dl2 || g_up_from_dl3;
    if (g_up_from_dl0) begin
      m_up_data    = dl0_data;
      m_up_dest_id = dl0_dest_id;
    end else if (g_up_from_dl1) begin
      m_up_data    = dl1_data;
      m_up_dest_id = dl1_dest_id;
    end else if (g_up_from_dl2) begin
      m_up_data    = dl2_data;
      m_up_dest_id = dl2_dest_id;
    end else begin
      m_up_data    = dl3_data;
      m_up_dest_id = dl3_dest_id;
    end

    m_dl0_valid   = g_dl0_from_up || g_dl0_from_dl0 || g_dl0_from_dl1 || g_dl0_from_dl2 || g_dl0_from_dl3;
    if (g_dl0_from_up) begin
      m_dl0_data   = up_data;
      m_dl0_dest_id= up_dest_id;
    end else if (g_dl0_from_dl0) begin
      m_dl0_data   = dl0_data;
      m_dl0_dest_id= dl0_dest_id;
    end else if (g_dl0_from_dl1) begin
      m_dl0_data   = dl1_data;
      m_dl0_dest_id= dl1_dest_id;
    end else if (g_dl0_from_dl2) begin
      m_dl0_data   = dl2_data;
      m_dl0_dest_id= dl2_dest_id;
    end else begin
      m_dl0_data   = dl3_data;
      m_dl0_dest_id= dl3_dest_id;
    end

    m_dl1_valid   = g_dl1_from_up || g_dl1_from_dl0 || g_dl1_from_dl1 || g_dl1_from_dl2 || g_dl1_from_dl3;
    if (g_dl1_from_up) begin
      m_dl1_data   = up_data;
      m_dl1_dest_id= up_dest_id;
    end else if (g_dl1_from_dl0) begin
      m_dl1_data   = dl0_data;
      m_dl1_dest_id= dl0_dest_id;
    end else if (g_dl1_from_dl1) begin
      m_dl1_data   = dl1_data;
      m_dl1_dest_id= dl1_dest_id;
    end else if (g_dl1_from_dl2) begin
      m_dl1_data   = dl2_data;
      m_dl1_dest_id= dl2_dest_id;
    end else begin
      m_dl1_data   = dl3_data;
      m_dl1_dest_id= dl3_dest_id;
    end

    m_dl2_valid   = g_dl2_from_up || g_dl2_from_dl0 || g_dl2_from_dl1 || g_dl2_from_dl2 || g_dl2_from_dl3;
    if (g_dl2_from_up) begin
      m_dl2_data   = up_data;
      m_dl2_dest_id= up_dest_id;
    end else if (g_dl2_from_dl0) begin
      m_dl2_data   = dl0_data;
      m_dl2_dest_id= dl0_dest_id;
    end else if (g_dl2_from_dl1) begin
      m_dl2_data   = dl1_data;
      m_dl2_dest_id= dl1_dest_id;
    end else if (g_dl2_from_dl2) begin
      m_dl2_data   = dl2_data;
      m_dl2_dest_id= dl2_dest_id;
    end else begin
      m_dl2_data   = dl3_data;
      m_dl2_dest_id= dl3_dest_id;
    end

    m_dl3_valid   = g_dl3_from_up || g_dl3_from_dl0 || g_dl3_from_dl1 || g_dl3_from_dl2 || g_dl3_from_dl3;
    if (g_dl3_from_up) begin
      m_dl3_data   = up_data;
      m_dl3_dest_id= up_dest_id;
    end else if (g_dl3_from_dl0) begin
      m_dl3_data   = dl0_data;
      m_dl3_dest_id= dl0_dest_id;
    end else if (g_dl3_from_dl1) begin
      m_dl3_data   = dl1_data;
      m_dl3_dest_id= dl1_dest_id;
    end else if (g_dl3_from_dl2) begin
      m_dl3_data   = dl2_data;
      m_dl3_dest_id= dl2_dest_id;
    end else begin
      m_dl3_data   = dl3_data;
      m_dl3_dest_id= dl3_dest_id;
    end
  end

  // Input readies (must win all requested outputs)
  always_comb begin
    dl0_ready = (!dl0_req_any) ? 1'b1 :
                ((!dl0_req_up  || (g_up_from_dl0  && m_up_ready)) &&
                 (!dl0_req_dl0 || (g_dl0_from_dl0 && m_dl0_ready)) &&
                 (!dl0_req_dl1 || (g_dl1_from_dl0 && m_dl1_ready)) &&
                 (!dl0_req_dl2 || (g_dl2_from_dl0 && m_dl2_ready)) &&
                 (!dl0_req_dl3 || (g_dl3_from_dl0 && m_dl3_ready)));

    dl1_ready = (!dl1_req_any) ? 1'b1 :
                ((!dl1_req_up  || (g_up_from_dl1  && m_up_ready)) &&
                 (!dl1_req_dl0 || (g_dl0_from_dl1 && m_dl0_ready)) &&
                 (!dl1_req_dl1 || (g_dl1_from_dl1 && m_dl1_ready)) &&
                 (!dl1_req_dl2 || (g_dl2_from_dl1 && m_dl2_ready)) &&
                 (!dl1_req_dl3 || (g_dl3_from_dl1 && m_dl3_ready)));

    dl2_ready = (!dl2_req_any) ? 1'b1 :
                ((!dl2_req_up  || (g_up_from_dl2  && m_up_ready)) &&
                 (!dl2_req_dl0 || (g_dl0_from_dl2 && m_dl0_ready)) &&
                 (!dl2_req_dl1 || (g_dl1_from_dl2 && m_dl1_ready)) &&
                 (!dl2_req_dl2 || (g_dl2_from_dl2 && m_dl2_ready)) &&
                 (!dl2_req_dl3 || (g_dl3_from_dl2 && m_dl3_ready)));

    dl3_ready = (!dl3_req_any) ? 1'b1 :
                ((!dl3_req_up  || (g_up_from_dl3  && m_up_ready)) &&
                 (!dl3_req_dl0 || (g_dl0_from_dl3 && m_dl0_ready)) &&
                 (!dl3_req_dl1 || (g_dl1_from_dl3 && m_dl1_ready)) &&
                 (!dl3_req_dl2 || (g_dl2_from_dl3 && m_dl2_ready)) &&
                 (!dl3_req_dl3 || (g_dl3_from_dl3 && m_dl3_ready)));

    up_ready  = (!up_req_any) ? 1'b1 :
                ((!up_req_dl0 || (g_dl0_from_up && m_dl0_ready)) &&
                 (!up_req_dl1 || (g_dl1_from_up && m_dl1_ready)) &&
                 (!up_req_dl2 || (g_dl2_from_up && m_dl2_ready)) &&
                 (!up_req_dl3 || (g_dl3_from_up && m_dl3_ready)));
  end

endmodule
