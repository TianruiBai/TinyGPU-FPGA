`timescale 1ns/1ps

// Simple behavioral Octal SPI (OSPI) controller stub
// Bridges the compute-unit global 32-bit request/response interface to an
// internal memory model. External OSPI pins are exposed but not actively
// driven; this model is intended for functional simulation and interface bring-up.
module ospi_controller #(
    parameter int MEM_WORDS   = 16384,      // 64KB default backing store (32-bit words)
    parameter int RD_LATENCY  = 2,          // cycles from accept to response for loads
    parameter int FIFO_DEPTH  = 4           // outstanding load queue depth
)(
    input  logic        clk,
    input  logic        rst_n,
    // Internal request interface (matches compute_unit global bus)
    input  logic        req_valid,
    input  logic        req_is_load,
    input  logic [31:0] req_addr,
    input  logic [31:0] req_wdata,
    input  logic [4:0]  req_rd,
    output logic        req_ready,
    output logic        resp_valid,
    output logic [4:0]  resp_rd,
    output logic [31:0] resp_data,
    // External OSPI pins (stubbed)
    output logic        ospi_cs_n,
    output logic        ospi_sck,
    inout  wire  [7:0]  ospi_io,
    output logic        ospi_oe
);
    // Backing store for functional simulation
    logic [31:0] mem [0:MEM_WORDS-1];

    // Outstanding load queue
    typedef struct packed {
        logic [31:0] addr;
        logic [4:0]  rd;
        logic [3:0]  delay;
    } load_q_t;

    load_q_t load_q   [0:FIFO_DEPTH-1];
    logic   [FIFO_DEPTH-1:0] load_q_valid;

    // Simple address to word index helper
    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr >> 2) % MEM_WORDS;
    endfunction

    // Ready when queue not full
    assign req_ready = rst_n && !(&load_q_valid);

    // External pins are idle in this stub
    assign ospi_cs_n = 1'b1;
    assign ospi_sck  = 1'b0;
    assign ospi_oe   = 1'b0;
    assign ospi_io   = 8'hZZ;

    // Response outputs default
    assign resp_valid = |load_q_valid && (load_q[0].delay == 0);
    assign resp_rd    = resp_valid ? load_q[0].rd : 5'd0;
    assign resp_data  = resp_valid ? mem[mem_index(load_q[0].addr)] : 32'h0;

    // Queue management and memory operations
    integer i;
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            load_q_valid <= '0;
            for (i = 0; i < FIFO_DEPTH; i++) begin
                load_q[i] <= '0;
            end
        end else begin
            // Age delays
            for (i = 0; i < FIFO_DEPTH; i++) begin
                if (load_q_valid[i] && load_q[i].delay != 0)
                    load_q[i].delay <= load_q[i].delay - 1'b1;
            end

            // Pop head when responded
            if (resp_valid) begin
                // shift queue
                for (i = 0; i < FIFO_DEPTH-1; i++) begin
                    load_q[i]       <= load_q[i+1];
                    load_q_valid[i] <= load_q_valid[i+1];
                end
                load_q_valid[FIFO_DEPTH-1] <= 1'b0;
            end

            // Accept new request
            if (req_valid && req_ready) begin
                if (req_is_load) begin
                    // push to tail
                    integer tail;
                    tail = 0;
                    // find first empty slot
                    for (i = 0; i < FIFO_DEPTH; i++) begin
                        if (!load_q_valid[i]) begin
                            tail = i;
                            break;
                        end
                    end
                    load_q_valid[tail] <= 1'b1;
                    load_q[tail].addr  <= req_addr;
                    load_q[tail].rd    <= req_rd;
                    load_q[tail].delay <= RD_LATENCY[3:0];
                end else begin
                    mem[mem_index(req_addr)] <= req_wdata;
                end
            end
        end
    end
endmodule
