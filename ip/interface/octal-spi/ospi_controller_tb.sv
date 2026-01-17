`timescale 1ns/1ps

module ospi_controller_tb;
    // Clock/reset
    logic clk;
    logic rst_n;

    // DUT interface
    logic        req_valid;
    logic        req_is_load;
    logic [31:0] req_addr;
    logic [31:0] req_wdata;
    logic [4:0]  req_rd;
    logic        req_ready;
    logic        resp_valid;
    logic [4:0]  resp_rd;
    logic [31:0] resp_data;

    // Unused external pins
    wire ospi_cs_n;
    wire ospi_sck;
    wire [7:0] ospi_io;
    wire ospi_oe;

    // DUT
    ospi_controller #(
        .MEM_WORDS(256),
        .RD_LATENCY(2),
        .FIFO_DEPTH(4)
    ) dut (
        .clk(clk),
        .rst_n(rst_n),
        .req_valid(req_valid),
        .req_is_load(req_is_load),
        .req_addr(req_addr),
        .req_wdata(req_wdata),
        .req_rd(req_rd),
        .req_ready(req_ready),
        .resp_valid(resp_valid),
        .resp_rd(resp_rd),
        .resp_data(resp_data),
        .ospi_cs_n(ospi_cs_n),
        .ospi_sck(ospi_sck),
        .ospi_io(ospi_io),
        .ospi_oe(ospi_oe)
    );

    // Clock
    initial begin
        clk = 0;
        forever #5 clk = ~clk; // 100MHz
    end

    // Reset
    initial begin
        rst_n = 0;
        req_valid = 0;
        req_is_load = 0;
        req_addr = 0;
        req_wdata = 0;
        req_rd = 0;
        repeat (4) @(posedge clk);
        rst_n = 1;
    end

    // Simple task to write a word
    task automatic do_store(input [31:0] addr, input [31:0] data);
        begin
            @(posedge clk);
            req_valid  <= 1'b1;
            req_is_load<= 1'b0;
            req_addr   <= addr;
            req_wdata  <= data;
            req_rd     <= 5'd0;
            wait (req_ready);
            @(posedge clk);
            req_valid  <= 1'b0;
        end
    endtask

    // Simple task to load a word
    task automatic do_load(input [31:0] addr, input [4:0] rd);
        begin
            @(posedge clk);
            req_valid  <= 1'b1;
            req_is_load<= 1'b1;
            req_addr   <= addr;
            req_wdata  <= 32'h0;
            req_rd     <= rd;
            wait (req_ready);
            @(posedge clk);
            req_valid  <= 1'b0;
        end
    endtask

    // Scoreboard
    logic [31:0] exp_data [0:31];
    initial begin
        for (int i = 0; i < 32; i++) exp_data[i] = 32'h0;
    end

    // Monitor responses
    always @(posedge clk) begin
        if (resp_valid) begin
            if (resp_data !== exp_data[resp_rd]) begin
                $fatal(1, "RD%0d mismatch: got %08x exp %08x", resp_rd, resp_data, exp_data[resp_rd]);
            end else begin
                $display("[OK] RD%0d = %08x", resp_rd, resp_data);
            end
        end
    end

    initial begin
        wait(rst_n);

        // Populate expected table for rds we will use
        exp_data[1] = 32'hA5A5_0001;
        exp_data[2] = 32'h5A5A_0002;
        exp_data[3] = 32'hDEAD_BEEF;

        // Stores
        do_store(32'h0000_0000, exp_data[1]);
        do_store(32'h0000_0004, exp_data[2]);
        do_store(32'h0000_0008, exp_data[3]);

        // Loads (will be queued)
        do_load(32'h0000_0000, 5'd1);
        do_load(32'h0000_0004, 5'd2);
        do_load(32'h0000_0008, 5'd3);

        // Wait for all responses
        repeat (20) @(posedge clk);

        $display("All OSPI controller tests passed.");
        $finish;
    end

    // Waveform dump
    initial begin
        $dumpfile("ospi_controller_tb.vcd");
        $dumpvars(0, ospi_controller_tb);
    end
endmodule
