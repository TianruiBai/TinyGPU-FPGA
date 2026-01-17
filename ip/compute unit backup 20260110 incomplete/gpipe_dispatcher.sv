`include "isa_pkg.sv"
module gpipe_dispatcher (
    input  logic                clk,
    input  logic                rst_n,
    input  logic                valid,
    input  isa_pkg::decode_ctrl_t ctrl,
    input  logic [31:0]         op_a,
    input  logic [31:0]         op_b,
    // Raster handshake
    output logic                cmd_valid,
    input  logic                cmd_ready,
    output logic [2:0]          cmd_type,
    output logic [31:0]         cmd_data_x,
    output logic [31:0]         cmd_data_y,
    // Dispatcher ready back to queue
    output logic                ready
);
    // Decode funct3 for graphics macro-ops
    wire is_rsetup = ctrl.funct3 == 3'b001;
    wire is_rdraw  = ctrl.funct3 == 3'b010;

    // Use lowest 2 bits of immediate to select vertex index for setup
    wire [1:0] v_idx = ctrl.imm[1:0];

    always_comb begin
        cmd_valid = valid && (is_rsetup || is_rdraw);
        cmd_data_x = op_a;
        cmd_data_y = op_b;
        
        if (is_rsetup) begin
            case (v_idx)
                2'b00: cmd_type = 3'b001; // SET_V0
                2'b01: cmd_type = 3'b010; // SET_V1
                2'b10: cmd_type = 3'b011; // SET_V2
                default: cmd_type = 3'b000; // NOP
            endcase
        end else if (is_rdraw) begin
            cmd_type = 3'b100; // DRAW
        end else begin
            cmd_type = 3'b000;
        end

        ready = valid ? (cmd_valid ? cmd_ready : 1'b1) : 1'b1;
    end
endmodule
