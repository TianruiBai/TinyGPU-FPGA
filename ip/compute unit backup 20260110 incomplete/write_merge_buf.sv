module write_merge_buf #(
    parameter int ENTRIES = 8
) (
    input  logic        clk,
    input  logic        rst_n,

    // Incoming 32-bit store stream
    input  logic        in_valid,
    input  logic [31:0] in_addr,
    input  logic [31:0] in_wdata,
    input  logic [3:0]  in_wstrb,
    output logic        in_ready,

    // Flush request (e.g., MEMBAR/abort)
    input  logic        flush,

    // Outgoing 32-bit store stream
    output logic        out_valid,
    output logic [31:0] out_addr,
    output logic [31:0] out_wdata,
    output logic [3:0]  out_wstrb,
    input  logic        out_ready,

    output logic        busy
);
    typedef struct packed {
        logic        valid;
        logic [31:4] base;
        logic [127:0] data;
        logic [15:0]  strb; // 1 bit per byte
    } entry_t;

    entry_t entries [ENTRIES];

    logic [$clog2(ENTRIES)-1:0] rr_evict;

    typedef enum logic [1:0] {
        ST_ACCUM,
        ST_FLUSH_ENTRY
    } state_t;

    state_t state;

    logic [$clog2(ENTRIES)-1:0] flush_idx;
    logic [1:0]                 flush_word;

    function automatic logic [1:0] word_sel(input logic [31:0] addr);
        word_sel = addr[3:2];
    endfunction

    function automatic logic [31:0] base_align(input logic [31:0] addr);
        base_align = {addr[31:4], 4'b0000};
    endfunction

    function automatic logic has_any_bytes(input logic [15:0] s);
        has_any_bytes = |s;
    endfunction

    // Find hit / free entry
    logic hit;
    logic [$clog2(ENTRIES)-1:0] hit_idx;
    logic free_found;
    logic [$clog2(ENTRIES)-1:0] free_idx;

    always_comb begin
        hit = 1'b0;
        hit_idx = '0;
        free_found = 1'b0;
        free_idx = '0;

        for (int i = 0; i < ENTRIES; i++) begin
            if (entries[i].valid && (entries[i].base == in_addr[31:4])) begin
                hit = 1'b1;
                hit_idx = i[$clog2(ENTRIES)-1:0];
            end
            if (!entries[i].valid && !free_found) begin
                free_found = 1'b0;
                free_idx = i[$clog2(ENTRIES)-1:0];
                free_found = 1'b1;
            end
        end
    end

    // Busy if any entry valid or flushing
    always_comb begin
        busy = (state != ST_ACCUM);
        for (int i = 0; i < ENTRIES; i++) begin
            if (entries[i].valid) busy = 1'b1;
        end
    end

    // Input ready: accept stores only in accumulate state and when we are not forced to flush immediately.
    // When full and no hit/free, we must flush first.
    wire full_no_place = in_valid && !hit && !free_found;

    always_comb begin
        in_ready = (state == ST_ACCUM) && !flush && !full_no_place;
    end

    // Output defaults
    always_comb begin
        out_valid = 1'b0;
        out_addr  = 32'h0;
        out_wdata = 32'h0;
        out_wstrb = 4'h0;

        if (state == ST_FLUSH_ENTRY) begin
            out_valid = 1'b1;
            out_addr  = base_align({entries[flush_idx].base, 4'b0}) + {28'h0, flush_word, 2'b00};
            out_wdata = entries[flush_idx].data[flush_word*32 +: 32];
            out_wstrb = entries[flush_idx].strb[flush_word*4 +: 4];
            if (out_wstrb == 4'h0) out_valid = 1'b0;
        end
    end

    // Find next word-with-bytes during flush
    function automatic logic [1:0] next_word_with_bytes(
        input logic [15:0] strb,
        input logic [1:0]  start
    );
        logic [1:0] w;
        begin
            w = start;
            for (int k = 0; k < 4; k++) begin
                if (strb[w*4 +: 4] != 4'h0) begin
                    return w;
                end
                w = w + 2'd1;
            end
            return start;
        end
    endfunction

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state <= ST_ACCUM;
            rr_evict <= '0;
            flush_idx <= '0;
            flush_word <= 2'd0;
            for (int i = 0; i < ENTRIES; i++) begin
                entries[i].valid <= 1'b0;
                entries[i].base  <= '0;
                entries[i].data  <= '0;
                entries[i].strb  <= '0;
            end
        end else begin
            // Start flush on explicit flush request
            if (flush && (state == ST_ACCUM)) begin
                // pick the next valid entry in RR order
                logic found;
                found = 1'b0;
                for (int k = 0; k < ENTRIES; k++) begin
                    logic [$clog2(ENTRIES)-1:0] idx;
                    idx = (rr_evict + k[$clog2(ENTRIES)-1:0]);
                    if (entries[idx].valid && !found) begin
                        flush_idx <= idx;
                        flush_word <= next_word_with_bytes(entries[idx].strb, 2'd0);
                        state <= ST_FLUSH_ENTRY;
                        found = 1'b1;
                    end
                end
            end

            // Auto-flush when full and new store arrives
            if ((state == ST_ACCUM) && full_no_place) begin
                flush_idx <= rr_evict;
                flush_word <= next_word_with_bytes(entries[rr_evict].strb, 2'd0);
                state <= ST_FLUSH_ENTRY;
            end

            // Accumulate writes
            if (in_valid && in_ready) begin
                logic [$clog2(ENTRIES)-1:0] widx;
                logic [1:0] wsel;
                wsel = word_sel(in_addr);

                if (hit) widx = hit_idx;
                else widx = free_idx;

                if (!entries[widx].valid) begin
                    entries[widx].valid <= 1'b1;
                    entries[widx].base  <= in_addr[31:4];
                    entries[widx].data  <= '0;
                    entries[widx].strb  <= '0;
                end

                entries[widx].data[wsel*32 +: 32] <= in_wdata;
                for (int b = 0; b < 4; b++) begin
                    if (in_wstrb[b]) entries[widx].strb[wsel*4 + b] <= 1'b1;
                end
            end

            // Flush progression
            if (state == ST_FLUSH_ENTRY) begin
                // If current word has no bytes, advance
                if (entries[flush_idx].strb[flush_word*4 +: 4] == 4'h0) begin
                    flush_word <= flush_word + 2'd1;
                end else if (out_valid && out_ready) begin
                    // Clear bytes we just wrote
                    for (int b = 0; b < 4; b++) begin
                        entries[flush_idx].strb[flush_word*4 + b] <= 1'b0;
                    end
                    flush_word <= flush_word + 2'd1;
                end

                // If entry drained, clear it and return to accumulate (or continue draining others if flush still asserted)
                if (!has_any_bytes(entries[flush_idx].strb)) begin
                    entries[flush_idx].valid <= 1'b0;
                    entries[flush_idx].base  <= '0;
                    entries[flush_idx].data  <= '0;
                    entries[flush_idx].strb  <= '0;

                    rr_evict <= rr_evict + 1'b1;
                    state <= ST_ACCUM;
                end
            end
        end
    end
endmodule
