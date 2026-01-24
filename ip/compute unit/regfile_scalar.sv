`default_nettype none
// SPDX-License-Identifier: Apache-2.0
// Module: regfile_scalar
// Description: Scalar register file with 5 read ports and 3 write ports (x0 hardwired to zero).
//
// Maintainer: TinyGPGPU Team
// Created: 2024-01-01
// Modified: 2026-01-23 - Expand to 5R/3W and remove procedural loops per RTL policy
// Tags: RTL, SYNTH
module regfile_scalar (
    input  logic        clk,
    input  logic        rst_n,
    input  logic [4:0]  raddr_a,
    input  logic [4:0]  raddr_b,
    input  logic [4:0]  raddr_c,
    input  logic [4:0]  raddr_d,
    input  logic [4:0]  raddr_e,
    output logic [31:0] rdata_a,
    output logic [31:0] rdata_b,
    output logic [31:0] rdata_c,
    output logic [31:0] rdata_d,
    output logic [31:0] rdata_e,
    input  logic        we0,
    input  logic [4:0]  waddr0,
    input  logic [31:0] wdata0,
    input  logic        we1,
    input  logic [4:0]  waddr1,
    input  logic [31:0] wdata1,
    input  logic        we2,
    input  logic [4:0]  waddr2,
    input  logic [31:0] wdata2
);
    (* ram_style = "distributed" *) logic [31:0] mem [31:0];

    // Per-register update (no procedural loops). Priority: we2 > we1 > we0.
    generate
        genvar i;
        for (i = 0; i < 32; i++) begin : gen_scalar_rf
            localparam logic [4:0] IDX = i;
            wire hit0 = we0 && (waddr0 == IDX) && (waddr0 != 5'd0);
            wire hit1 = we1 && (waddr1 == IDX) && (waddr1 != 5'd0);
            wire hit2 = we2 && (waddr2 == IDX) && (waddr2 != 5'd0);

            wire [31:0] wr_data = hit2 ? wdata2 : hit1 ? wdata1 : hit0 ? wdata0 : mem[i];

            always_ff @(posedge clk or negedge rst_n) begin
                if (!rst_n) begin
                    mem[i] <= 32'h0;
                end else begin
                    mem[i] <= wr_data;
                end
            end
        end
    endgenerate

    // Combinational reads; x0 is hardwired to zero
    always_comb begin
        rdata_a = (raddr_a == 5'd0) ? 32'h0 : mem[raddr_a];
        rdata_b = (raddr_b == 5'd0) ? 32'h0 : mem[raddr_b];
        rdata_c = (raddr_c == 5'd0) ? 32'h0 : mem[raddr_c];
        rdata_d = (raddr_d == 5'd0) ? 32'h0 : mem[raddr_d];
        rdata_e = (raddr_e == 5'd0) ? 32'h0 : mem[raddr_e];
    end
endmodule
