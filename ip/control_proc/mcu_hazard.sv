// Simple hazard/forwarding unit
module mcu_hazard (
    input  logic        id_ex_mem_read,
    input  logic [4:0]  id_ex_rd,
    input  logic [4:0]  if_id_rs1,
    input  logic [4:0]  if_id_rs2,
    input  logic        if_id_uses_rs2,
    output logic        stall_decode
);
    always_comb begin
        stall_decode = 1'b0;
        if (id_ex_mem_read && (id_ex_rd != 5'd0)) begin
            if ((id_ex_rd == if_id_rs1) || (if_id_uses_rs2 && (id_ex_rd == if_id_rs2))) begin
                stall_decode = 1'b1; // load-use hazard
            end
        end
    end
endmodule
