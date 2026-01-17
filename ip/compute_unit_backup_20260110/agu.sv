module agu (
    input  logic [31:0] base_addr,
    input  logic [31:0] offset,
    output logic [31:0] effective_addr
);
    // Simple address generation: base + offset
    assign effective_addr = base_addr + offset;
endmodule
