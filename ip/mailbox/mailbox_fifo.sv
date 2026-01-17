`timescale 1ns/1ps
// Simple synchronous FIFO with parameterizable depth and width
module mailbox_fifo #(
  parameter int WIDTH = 32,
  parameter int DEPTH = 4,
  localparam int PTR_W = $clog2(DEPTH)
) (
  input  logic             clk,
  input  logic             rst_n,
  input  logic             clr,
  // Write
  input  logic             w_en,
  input  logic [WIDTH-1:0] w_data,
  output logic             w_full,
  // Read
  input  logic             r_en,
  output logic [WIDTH-1:0] r_data,
  output logic             r_empty
);

  logic [WIDTH-1:0] mem [0:DEPTH-1];
  logic [PTR_W:0]   wr_ptr, rd_ptr;

  assign w_full  = (wr_ptr[PTR_W] != rd_ptr[PTR_W]) && (wr_ptr[PTR_W-1:0] == rd_ptr[PTR_W-1:0]);
  assign r_empty = (wr_ptr == rd_ptr);
  assign r_data  = mem[rd_ptr[PTR_W-1:0]];

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n || clr) begin
      wr_ptr <= '0;
    end else if (w_en && !w_full) begin
      mem[wr_ptr[PTR_W-1:0]] <= w_data;
      wr_ptr <= wr_ptr + 1'b1;
      // debug: show write events during simulation
      if ($test$plusargs("VERBOSE") || 1'b0) $display("%0t: FIFO write: wr_ptr(before)=%0d data=%0p", $time, wr_ptr, w_data);
    end
  end

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n || clr) begin
      rd_ptr <= '0;
    end else if (r_en && !r_empty) begin
      rd_ptr <= rd_ptr + 1'b1;
    end
  end
endmodule
