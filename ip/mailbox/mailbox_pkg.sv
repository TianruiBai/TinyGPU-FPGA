`timescale 1ns/1ps
// Package for mailbox interconnect common types and helpers
package mailbox_pkg;

  // Opcode enumeration
  typedef enum logic [3:0] {
    OPC_DATA = 4'h0,
    OPC_IRQ  = 4'h1,
    OPC_ACK  = 4'h2,
    OPC_NACK = 4'h3,
    OPC_RSV  = 4'hF
  } mailbox_opcode_e;

  typedef struct packed {
    logic [7:0] src_id;    // Return address (Cluster[7:0])
    logic       eop;       // End of packet beat
    logic       prio;      // 1 = latency, 0 = best-effort
    logic [3:0] opcode;    // mailbox_opcode_e
    logic [3:0] hops;      // Saturating hop count (distance/age)
    logic       parity;    // Even parity over data|src_id|eop|prio (optional)
  } mailbox_tag_t;

  // Compute even parity when enabled
  function automatic logic compute_parity(
    input logic [31:0] data,
    input mailbox_tag_t tag_no_parity
  );
    mailbox_tag_t tag_zeroed;

    tag_zeroed = tag_no_parity;
    tag_zeroed.parity = 1'b0;

    compute_parity = ^{data, tag_zeroed};
  endfunction

endpackage : mailbox_pkg
