// Skeleton L1 Data Cache for TinyGPGPU
// Note: this is an initial skeleton/module stub with parameters and top-level interface.
// Intended for Phase A (write-through L1, small MSHRs, PLRU replacement).

module l1_data_cache #(
    parameter int L1_ENABLED = 1,
    parameter int L1_SIZE_BYTES = 4096,
    parameter int LINE_BYTES = 64,
    parameter int ASSOCIATIVITY = 4,
    parameter int NUM_MSHR = 4,
    parameter int WB_DEPTH = 8,
    parameter int AXI_DATA_BITS = 64,
    parameter int MAX_RDATA_WIDTH = 32,
    parameter int VEC_WORDS = 4,
    parameter bit WRITEBACK = 0 // 0 = write-through (default), 1 = write-back
)(
    input  logic clk,
    input  logic rst_n,

    // CU side interface (three LSU ports: LSU0, LSU1, LSU_TEX)
    // LSU0 interface
    input  logic                 lsu0_req_valid,
    input  logic [1:0]           lsu0_req_type, // 0=LOAD,1=STORE,2=ATOM
    input  logic [31:0]          lsu0_req_addr,
    input  logic [(MAX_RDATA_WIDTH*VEC_WORDS)-1:0] lsu0_req_wdata,
    input  logic [LINE_BYTES/8-1:0] lsu0_req_wstrb,
    input  logic                 lsu0_req_is_vector,
    input  logic [VEC_WORDS-1:0]  lsu0_req_vec_wmask, // per-vector-word mask, LSB covers lowest word
    input  logic [7:0]           lsu0_req_id,
    output logic                 lsu0_req_ready,

    output logic                 lsu0_resp_valid,
    output logic [(MAX_RDATA_WIDTH*VEC_WORDS)-1:0] lsu0_resp_data,
    output logic [7:0]           lsu0_resp_id,
    output logic                 lsu0_resp_err,

    // LSU1 interface
    input  logic                 lsu1_req_valid,
    input  logic [1:0]           lsu1_req_type,
    input  logic [31:0]          lsu1_req_addr,
    input  logic [(MAX_RDATA_WIDTH*VEC_WORDS)-1:0] lsu1_req_wdata,
    input  logic [LINE_BYTES/8-1:0] lsu1_req_wstrb,
    input  logic                 lsu1_req_is_vector,
    input  logic [VEC_WORDS-1:0]  lsu1_req_vec_wmask,
    input  logic [7:0]           lsu1_req_id,
    output logic                 lsu1_req_ready,

    output logic                 lsu1_resp_valid,
    output logic [(MAX_RDATA_WIDTH*VEC_WORDS)-1:0] lsu1_resp_data,
    output logic [7:0]           lsu1_resp_id,
    output logic                 lsu1_resp_err,

    // LSU_TEXTURE interface (texture accesses are typically handled by separate
    // texture cache (`ip/compute unit/texture_cache.sv`) and may operate at
    // different granularity/line sizes. This interface is kept for integration
    // but texture path may bypass or translate through the dedicated texture cache.
    input  logic                 lsu_tex_req_valid,
    input  logic [1:0]           lsu_tex_req_type,
    input  logic [31:0]          lsu_tex_req_addr,
    input  logic [MAX_RDATA_WIDTH-1:0] lsu_tex_req_wdata,
    input  logic [LINE_BYTES/8-1:0] lsu_tex_req_wstrb,
    input  logic [7:0]           lsu_tex_req_id,
    output logic                 lsu_tex_req_ready,

    output logic                 lsu_tex_resp_valid,
    output logic [MAX_RDATA_WIDTH-1:0] lsu_tex_resp_data,
    output logic [7:0]           lsu_tex_resp_id,
    output logic                 lsu_tex_resp_err,

    // Config / control
    input  logic                 cfg_flush,
    input  logic                 cfg_invalidate,

    // Memory/MIU side interface (simple request/response)
    output logic                 mem_req_valid,
    output logic                 mem_req_rw,   // 0=read, 1=write
    output logic [31:0]         mem_req_addr,
    output logic [7:0]          mem_req_size,
    output logic [3:0]          mem_req_qos,
    output logic [7:0]          mem_req_id,
    output logic [LINE_BYTES*8-1:0] mem_req_wdata,
    output logic [LINE_BYTES/8-1:0] mem_req_wstrb,
    input  logic                 mem_req_ready,

    input  logic                 mem_resp_valid,
    input  logic [AXI_DATA_BITS-1:0] mem_resp_data,
    input  logic [7:0]          mem_resp_id
);

    // Local parameters & derived values
    localparam int NUM_LINES = L1_SIZE_BYTES / LINE_BYTES;
    // Note: associative mapping is planned; Phase A uses simple direct-mapped lines.
    localparam int OFFSET_BITS = $clog2(LINE_BYTES);
    localparam int INDEX_BITS  = $clog2(NUM_LINES);
    localparam int TAG_BITS    = 32 - INDEX_BITS - OFFSET_BITS;

    // Number of lines (direct-mapped view)
    // localparam int NUM_SETS  = NUM_LINES / ASSOCIATIVITY; (for future assoc implementation)


    // Counters (simple perf counters)
    logic [31:0] cnt_hits, cnt_misses, cnt_refills;

    // Tag and data RAM (direct-mapped for Phase A)
    typedef struct packed {
        logic               valid;
        logic [TAG_BITS-1:0] tag;
        logic               dirty; // used when WRITEBACK==1
    } tag_entry_t;

    tag_entry_t tag_ram   [0:NUM_LINES-1];
    logic [LINE_BYTES*8-1:0] data_ram  [0:NUM_LINES-1];

    // Simple inflight mem_req tracking
    typedef enum logic [2:0] {
        MEM_SRC_NONE       = 3'd0,
        MEM_SRC_L1_REFILL  = 3'd1,
        MEM_SRC_WB_BUFFER  = 3'd2,
        MEM_SRC_TEX_REFILL = 3'd3,
        MEM_SRC_L1_WT      = 3'd4
    } mem_src_t;

    logic mem_req_valid_r;
    logic mem_req_rw_r;
    logic [31:0] mem_req_addr_r;
    logic [7:0]  mem_req_size_r;
    logic [3:0]  mem_req_qos_r;
    logic [7:0]  mem_req_id_r;
    logic [LINE_BYTES*8-1:0] mem_req_wdata_r;
    logic [LINE_BYTES/8-1:0] mem_req_wstrb_r;
    mem_src_t mem_req_src_r;
    mem_src_t mem_outstanding_src;
    logic mem_busy; // indicates mem_req is inflight

    // Write-back buffer (small FIFO) to avoid stalling misses on dirty evictions
    typedef struct packed {
        logic [31:0]             addr;
        logic [LINE_BYTES*8-1:0] data;
    } wb_entry_t;

    localparam int WB_PTR_BITS = (WB_DEPTH <= 1) ? 1 : $clog2(WB_DEPTH);
    localparam int WB_COUNT_BITS = WB_PTR_BITS + 1;
    wb_entry_t wb_fifo   [0:WB_DEPTH-1];
    logic [WB_PTR_BITS-1:0] wb_rd_ptr;
    logic [WB_PTR_BITS-1:0] wb_wr_ptr;
    logic [WB_PTR_BITS:0]   wb_count;
    wire wb_full  = (wb_count == WB_COUNT_BITS'(WB_DEPTH));
    wire wb_empty = (wb_count == 0);

    // WB buffer push control
    logic wb_push;
    logic [31:0] wb_push_addr;
    logic [LINE_BYTES*8-1:0] wb_push_data;
    logic wb_push_dirty_flag;

    // Capture pending eviction when WB buffer is temporarily full
    logic [31:0] evict_addr_pending;
    logic [LINE_BYTES*8-1:0] evict_data_pending;

    // Handy decode helpers for pending miss
    wire [INDEX_BITS-1:0] pending_idx_calc = addr_idx(pending_addr);
    wire [TAG_BITS-1:0]   pending_tag_calc = addr_tag(pending_addr);

    // Flush scanner state for write-back mode
    logic flush_active;
    logic [INDEX_BITS-1:0] flush_index;

    // TODO: implement full MSHR table with store coalescing; Phase A uses conservative behaviors.

    // Request arbitration (LSU0 has priority over LSU1; LSU_TEX handled separately)
    logic        req_sel_lsu0;
    logic        req_sel_lsu1;
    logic        req_fire;
    logic [1:0]  req_type_s;
    logic [31:0] req_addr_s;
    logic [(MAX_RDATA_WIDTH*VEC_WORDS)-1:0] req_wdata_s;
    logic        req_is_vector_s;
    logic [VEC_WORDS-1:0] req_vec_wmask_s;
    logic [7:0]  req_id_s;

    // Pending miss handling (single outstanding)
    logic        pending_valid;
    logic        pending_is_store;
    logic        pending_is_vector;
    logic [31:0] pending_addr;
    logic [(MAX_RDATA_WIDTH*VEC_WORDS)-1:0] pending_wdata;
    logic [VEC_WORDS-1:0] pending_vec_wmask;
    logic [7:0]  pending_id;
    logic        pending_from_lsu0;

    typedef enum logic [1:0] {ST_IDLE, ST_WB_EVICT, ST_MISS_REQ, ST_MISS_WAIT} l1_state_t;
    l1_state_t state;

    // Refill assembly (multi-beat when AXI_DATA_BITS < LINE_BYTES*8)
    localparam int BEAT_BYTES = AXI_DATA_BITS / 8;
    localparam int LINE_BEATS = (LINE_BYTES + BEAT_BYTES - 1) / BEAT_BYTES;
    localparam int REFILL_BITS = LINE_BYTES * 8;
    localparam int REFILL_CNT_BITS = (LINE_BEATS <= 1) ? 1 : $clog2(LINE_BEATS);
    localparam int LINE_BEATS_M1 = (LINE_BEATS > 0) ? (LINE_BEATS - 1) : 0;
    logic [REFILL_BITS-1:0] refill_buf;
    logic [REFILL_CNT_BITS-1:0] refill_cnt;

    // Safe mem_resp line extraction (avoids out-of-range part-select warnings)
    wire [REFILL_BITS-1:0] mem_resp_line;
    generate
        if (AXI_DATA_BITS >= REFILL_BITS) begin : gen_mem_resp_full
            assign mem_resp_line = mem_resp_data[REFILL_BITS-1:0];
        end else begin : gen_mem_resp_extend
            assign mem_resp_line = {{(REFILL_BITS-AXI_DATA_BITS){1'b0}}, mem_resp_data};
        end
    endgenerate

    // Helpers for address decode
    function automatic [TAG_BITS-1:0] addr_tag(input logic [31:0] a);
        addr_tag = a[31 -: TAG_BITS];
    endfunction
    function automatic [INDEX_BITS-1:0] addr_idx(input logic [31:0] a);
        addr_idx = a[OFFSET_BITS +: INDEX_BITS];
    endfunction
    function automatic [OFFSET_BITS-1:0] addr_off(input logic [31:0] a);
        addr_off = a[OFFSET_BITS-1:0];
    endfunction

    // Texture cache wiring (bypass L1)
    logic tex_req_ready;
    logic tex_resp_valid;
    logic [31:0] tex_resp_data;
    logic [4:0]  tex_resp_id;
    logic tex_miss_req_valid;
    logic [31:0] tex_miss_req_addr;
    logic tex_miss_req_ready;
    logic tex_miss_resp_valid;
    logic [LINE_BYTES*8-1:0] tex_miss_resp_data;

    texture_cache #(
        .LINE_BYTES(LINE_BYTES)
    ) tex_cache (
        .clk(clk),
        .rst_n(rst_n),
        .req_valid(lsu_tex_req_valid && (lsu_tex_req_type == 2'b00)),
        .req_addr(lsu_tex_req_addr),
        .req_rd(lsu_tex_req_id[4:0]),
        .req_ready(tex_req_ready),
        .resp_valid(tex_resp_valid),
        .resp_data(tex_resp_data),
        .resp_rd(tex_resp_id),
        .miss_req_valid(tex_miss_req_valid),
        .miss_req_addr(tex_miss_req_addr),
        .miss_req_ready(tex_miss_req_ready),
        .miss_resp_valid(tex_miss_resp_valid),
        .miss_resp_data(tex_miss_resp_data)
    );

    // Default ready/resp signals
    assign lsu0_req_ready = (state == ST_IDLE) && !mem_busy && !mem_req_valid_r;
    assign lsu1_req_ready = (state == ST_IDLE) && !mem_busy && !mem_req_valid_r && !lsu0_req_valid;
    assign lsu_tex_req_ready = tex_req_ready && (lsu_tex_req_type == 2'b00);

    // Response registers
    logic lsu0_resp_valid_r;
    logic lsu1_resp_valid_r;
    logic [(MAX_RDATA_WIDTH*VEC_WORDS)-1:0] lsu0_resp_data_r;
    logic [(MAX_RDATA_WIDTH*VEC_WORDS)-1:0] lsu1_resp_data_r;
    logic [7:0] lsu0_resp_id_r;
    logic [7:0] lsu1_resp_id_r;

    assign lsu0_resp_valid = lsu0_resp_valid_r;
    assign lsu0_resp_data  = lsu0_resp_data_r;
    assign lsu0_resp_err   = 1'b0;
    assign lsu0_resp_id    = lsu0_resp_id_r;

    assign lsu1_resp_valid = lsu1_resp_valid_r;
    assign lsu1_resp_data  = lsu1_resp_data_r;
    assign lsu1_resp_err   = 1'b0;
    assign lsu1_resp_id    = lsu1_resp_id_r;

    assign lsu_tex_resp_valid = tex_resp_valid;
    assign lsu_tex_resp_data  = tex_resp_data;
    assign lsu_tex_resp_err   = 1'b0;
    assign lsu_tex_resp_id    = {3'b000, tex_resp_id};

    // mem_req plumbing
    assign mem_req_valid = mem_req_valid_r;
    assign mem_req_rw    = mem_req_rw_r;
    assign mem_req_addr  = mem_req_addr_r;
    assign mem_req_size  = mem_req_size_r;
    assign mem_req_qos   = mem_req_qos_r;
    assign mem_req_id    = mem_req_id_r;
    assign mem_req_wdata = mem_req_wdata_r;
    assign mem_req_wstrb = mem_req_wstrb_r;

    // Texture miss ready follows global bus availability
    assign tex_miss_req_ready = (!mem_busy && !mem_req_valid_r);

    // Select request
    assign req_sel_lsu0 = lsu0_req_valid && lsu0_req_ready;
    assign req_sel_lsu1 = lsu1_req_valid && lsu1_req_ready;
    assign req_fire = req_sel_lsu0 || req_sel_lsu1;
    assign req_type_s = req_sel_lsu0 ? lsu0_req_type : lsu1_req_type;
    assign req_addr_s = req_sel_lsu0 ? lsu0_req_addr : lsu1_req_addr;
    assign req_wdata_s = req_sel_lsu0 ? lsu0_req_wdata : lsu1_req_wdata;
    assign req_is_vector_s = req_sel_lsu0 ? lsu0_req_is_vector : lsu1_req_is_vector;
    assign req_vec_wmask_s = req_sel_lsu0 ? lsu0_req_vec_wmask : lsu1_req_vec_wmask;
    assign req_id_s = req_sel_lsu0 ? lsu0_req_id : lsu1_req_id;

    // Core state machine
    integer i;
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            cnt_hits <= 0;
            cnt_misses <= 0;
            cnt_refills <= 0;
            mem_req_valid_r <= 1'b0;
            mem_req_rw_r <= 1'b0;
            mem_req_addr_r <= 32'b0;
            mem_req_size_r <= 8'b0;
            mem_req_qos_r <= 4'b0;
            mem_req_id_r <= 8'b0;
            mem_req_wdata_r <= '0;
            mem_req_wstrb_r <= '0;
            mem_busy <= 1'b0;
            mem_outstanding_src <= MEM_SRC_NONE;
            state <= ST_IDLE;
            pending_valid <= 1'b0;
            pending_is_store <= 1'b0;
            pending_is_vector <= 1'b0;
            pending_addr <= 32'b0;
            pending_wdata <= '0;
            pending_vec_wmask <= '0;
            pending_id <= 8'b0;
            pending_from_lsu0 <= 1'b0;
            lsu0_resp_valid_r <= 1'b0;
            lsu1_resp_valid_r <= 1'b0;
            lsu0_resp_data_r <= '0;
            lsu1_resp_data_r <= '0;
            lsu0_resp_id_r <= 8'b0;
            lsu1_resp_id_r <= 8'b0;
            flush_active <= 1'b0;
            flush_index <= '0;
            refill_buf <= '0;
            refill_cnt <= '0;
            mem_req_src_r <= MEM_SRC_NONE;
            wb_rd_ptr <= '0;
            wb_wr_ptr <= '0;
            wb_count <= '0;
            evict_addr_pending <= 32'b0;
            evict_data_pending <= '0;
            tex_miss_resp_valid <= 1'b0;
            tex_miss_resp_data <= '0;
            for (i = 0; i < NUM_LINES; i = i + 1) begin
                tag_ram[i].valid <= 1'b0;
                tag_ram[i].tag   <= '0;
                tag_ram[i].dirty <= 1'b0;
                data_ram[i]      <= '0;
            end
        end else begin
            // default pulses
            lsu0_resp_valid_r <= 1'b0;
            lsu1_resp_valid_r <= 1'b0;
            tex_miss_resp_valid <= 1'b0;

            // write-back buffer push/pop defaults
            wb_push <= 1'b0;
            wb_push_addr <= 32'b0;
            wb_push_data <= '0;
            wb_push_dirty_flag <= 1'b0;

            // Hold mem_req_valid_r until accepted
            if (mem_req_valid_r && mem_req_ready) begin
                mem_req_valid_r <= 1'b0;
                mem_busy <= 1'b1;
                mem_outstanding_src <= mem_req_src_r;
                if (mem_req_src_r == MEM_SRC_WB_BUFFER) begin
                    wb_rd_ptr <= wb_rd_ptr + 1'b1;
                    wb_count <= wb_count - 1'b1;
                end
                if (mem_req_rw_r == 1'b0) begin
                    refill_cnt <= '0;
                    refill_buf <= '0;
                end
            end
            if (mem_resp_valid) begin
                mem_busy <= 1'b0;
            end

            // Handle flush request for write-back
            if (WRITEBACK && cfg_flush && !flush_active) begin
                flush_active <= 1'b1;
                flush_index <= '0;
            end

            // Flush scan: one line per cycle (synthesizable, no break)
            if (flush_active && !mem_busy && !mem_req_valid_r && state == ST_IDLE) begin
                if (tag_ram[flush_index].valid && tag_ram[flush_index].dirty && !wb_full) begin
                    wb_push <= 1'b1;
                    wb_push_addr <= {tag_ram[flush_index].tag, flush_index, {OFFSET_BITS{1'b0}}};
                    wb_push_data <= data_ram[flush_index];
                    wb_push_dirty_flag <= 1'b1;
                    tag_ram[flush_index].dirty <= 1'b0;
                end
                if (!wb_full) begin
                    if (flush_index == INDEX_BITS'(NUM_LINES-1)) begin
                        flush_active <= 1'b0;
                    end else begin
                        flush_index <= flush_index + 1'b1;
                    end
                end
            end

            // Texture cache miss handling: issue read when bus idle and L1 not asserting mem_req
            if (tex_miss_req_valid && tex_miss_req_ready && !mem_busy && !mem_req_valid_r) begin
                mem_req_valid_r <= 1'b1;
                mem_req_rw_r <= 1'b0;
                mem_req_addr_r <= tex_miss_req_addr;
                mem_req_size_r <= LINE_BYTES[7:0];
                mem_req_qos_r <= 4'b0000;
                mem_req_id_r <= 8'hfe; // reserved for TEX path
                mem_req_wdata_r <= '0;
                mem_req_wstrb_r <= '0;
                mem_req_src_r <= MEM_SRC_TEX_REFILL;
            end

            case (state)
                ST_IDLE: begin
                    if (req_fire) begin
                        logic [INDEX_BITS-1:0] idx;
                        logic [TAG_BITS-1:0]   tag;
                        logic [OFFSET_BITS-1:0] off;
                        idx = addr_idx(req_addr_s);
                        tag = addr_tag(req_addr_s);
                        off = addr_off(req_addr_s);
                        if (tag_ram[idx].valid && (tag_ram[idx].tag == tag)) begin
                            // HIT
                            cnt_hits <= cnt_hits + 1;
                            if (req_type_s == 2'b00) begin
                                // LOAD
                                integer lane;
                                integer base_word;
                                base_word = int'(off[OFFSET_BITS-1:2]);
                                if (req_sel_lsu0) begin
                                    lsu0_resp_id_r <= req_id_s;
                                    lsu0_resp_data_r <= '0;
                                    if (req_is_vector_s) begin
                                        for (lane = 0; lane < VEC_WORDS; lane = lane + 1) begin
                                            lsu0_resp_data_r[lane*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH]
                                                <= data_ram[idx][(base_word+lane)*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                                        end
                                    end else begin
                                        lsu0_resp_data_r[0 +: MAX_RDATA_WIDTH]
                                            <= data_ram[idx][base_word*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                                    end
                                    lsu0_resp_valid_r <= 1'b1;
                                end else begin
                                    lsu1_resp_id_r <= req_id_s;
                                    lsu1_resp_data_r <= '0;
                                    if (req_is_vector_s) begin
                                        for (lane = 0; lane < VEC_WORDS; lane = lane + 1) begin
                                            lsu1_resp_data_r[lane*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH]
                                                <= data_ram[idx][(base_word+lane)*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                                        end
                                    end else begin
                                        lsu1_resp_data_r[0 +: MAX_RDATA_WIDTH]
                                            <= data_ram[idx][base_word*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                                    end
                                    lsu1_resp_valid_r <= 1'b1;
                                end
                            end else if (req_type_s == 2'b01) begin
                                // STORE
                                integer lane;
                                integer base_word;
                                logic [LINE_BYTES*8-1:0] line_new;
                                base_word = int'(off[OFFSET_BITS-1:2]);
                                line_new = data_ram[idx];
                                if (!req_is_vector_s) begin
                                    line_new[base_word*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH]
                                        = req_wdata_s[0 +: MAX_RDATA_WIDTH];
                                end else begin
                                    for (lane = 0; lane < VEC_WORDS; lane = lane + 1) begin
                                        if (req_vec_wmask_s[lane]) begin
                                            line_new[(base_word+lane)*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH]
                                                = req_wdata_s[lane*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                                        end
                                    end
                                end
                                data_ram[idx] <= line_new;
                                if (WRITEBACK) begin
                                    tag_ram[idx].dirty <= 1'b1;
                                end else if (!mem_busy && mem_req_ready) begin
                                    mem_req_valid_r <= 1'b1;
                                    mem_req_rw_r <= 1'b1;
                                    mem_req_addr_r <= {tag, idx, {OFFSET_BITS{1'b0}}};
                                    mem_req_size_r <= LINE_BYTES[7:0];
                                    mem_req_qos_r <= 4'b0001;
                                    mem_req_id_r <= req_id_s;
                                    mem_req_wdata_r <= line_new;
                                    mem_req_wstrb_r <= {LINE_BYTES/8{1'b1}};
                                    mem_req_src_r <= MEM_SRC_L1_WT;
                                end
                            end
                        end else begin
                            // MISS
                            cnt_misses <= cnt_misses + 1;
                            pending_valid <= 1'b1;
                            pending_is_store <= (req_type_s == 2'b01);
                            pending_is_vector <= req_is_vector_s;
                            pending_addr <= req_addr_s;
                            pending_wdata <= req_wdata_s;
                            pending_vec_wmask <= req_vec_wmask_s;
                            pending_id <= req_id_s;
                            pending_from_lsu0 <= req_sel_lsu0;

                            if (WRITEBACK && tag_ram[idx].valid && tag_ram[idx].dirty) begin
                                // Evict dirty line via WB buffer
                                if (!wb_full) begin
                                    wb_push <= 1'b1;
                                    wb_push_addr <= {tag_ram[idx].tag, idx, {OFFSET_BITS{1'b0}}};
                                    wb_push_data <= data_ram[idx];
                                    wb_push_dirty_flag <= 1'b1;
                                    tag_ram[idx].dirty <= 1'b0;
                                end else begin
                                    evict_addr_pending <= {tag_ram[idx].tag, idx, {OFFSET_BITS{1'b0}}};
                                    evict_data_pending <= data_ram[idx];
                                    state <= ST_WB_EVICT;
                                end
                            end
                            // Request line refill (can proceed even while WB buffer drains)
                            if (!mem_req_valid_r && !mem_busy) begin
                                mem_req_valid_r <= 1'b1;
                                mem_req_rw_r <= 1'b0;
                                mem_req_addr_r <= {tag, idx, {OFFSET_BITS{1'b0}}};
                                mem_req_size_r <= LINE_BYTES[7:0];
                                mem_req_qos_r <= 4'b0000;
                                mem_req_id_r <= req_id_s;
                                mem_req_wdata_r <= '0;
                                mem_req_wstrb_r <= '0;
                                mem_req_src_r <= MEM_SRC_L1_REFILL;
                                state <= ST_MISS_WAIT;
                            end
                        end
                    end
                end

                ST_WB_EVICT: begin
                    if (!wb_full) begin
                        wb_push <= 1'b1;
                        wb_push_addr <= evict_addr_pending;
                        wb_push_data <= evict_data_pending;
                        wb_push_dirty_flag <= 1'b1;
                        // issue miss refill after buffering eviction
                        if (!mem_req_valid_r && !mem_busy) begin
                            mem_req_valid_r <= 1'b1;
                            mem_req_rw_r <= 1'b0;
                            mem_req_addr_r <= {pending_tag_calc, pending_idx_calc, {OFFSET_BITS{1'b0}}};
                            mem_req_size_r <= LINE_BYTES[7:0];
                            mem_req_qos_r <= 4'b0000;
                            mem_req_id_r <= pending_id;
                            mem_req_wdata_r <= '0;
                            mem_req_wstrb_r <= '0;
                            mem_req_src_r <= MEM_SRC_L1_REFILL;
                            state <= ST_MISS_WAIT;
                        end
                    end
                end

                ST_MISS_WAIT: begin
                    // wait for refill response handled below
                end

                default: begin
                    state <= ST_IDLE;
                end
            endcase

            // Drain WB buffer with lowest priority (after core/text requests in this cycle)
            if (!mem_busy && !mem_req_valid_r && !wb_empty) begin
                mem_req_valid_r <= 1'b1;
                mem_req_rw_r <= 1'b1;
                mem_req_addr_r <= wb_fifo[wb_rd_ptr].addr;
                mem_req_size_r <= LINE_BYTES[7:0];
                mem_req_qos_r <= 4'b0010;
                mem_req_id_r <= 8'd255;
                mem_req_wdata_r <= wb_fifo[wb_rd_ptr].data;
                mem_req_wstrb_r <= {LINE_BYTES/8{1'b1}};
                mem_req_src_r <= MEM_SRC_WB_BUFFER;
                $display("L1 WB drain addr=%08h @%0t", mem_req_addr_r, $time);
            end

            // Handle memory responses (read refills or WB acks)
            if (mem_resp_valid) begin
                if (mem_outstanding_src == MEM_SRC_L1_REFILL) begin
                    logic [INDEX_BITS-1:0] idx;
                    logic [TAG_BITS-1:0]   tag;
                    logic [OFFSET_BITS-1:0] off;
                    integer base_word;
                    logic [REFILL_BITS-1:0] refill_buf_next;
                    logic refill_last;
                    idx = addr_idx(pending_addr);
                    tag = addr_tag(pending_addr);
                    off = addr_off(pending_addr);
                    base_word = int'(off[OFFSET_BITS-1:2]);

                    if (LINE_BEATS == 1) begin
                        data_ram[idx] <= mem_resp_line;
                        refill_last = 1'b1;
                    end else begin
                        refill_buf_next = refill_buf;
                        refill_buf_next[refill_cnt*AXI_DATA_BITS +: AXI_DATA_BITS] = mem_resp_data;
                        refill_last = (refill_cnt == REFILL_CNT_BITS'(LINE_BEATS_M1));
                        if (refill_last) begin
                            data_ram[idx] <= refill_buf_next;
                            refill_cnt <= '0;
                            $display("L1 refill line addr=%08h last=1 @%0t", {tag, idx, {OFFSET_BITS{1'b0}}}, $time);
                        end else begin
                            refill_buf <= refill_buf_next;
                            refill_cnt <= refill_cnt + 1'b1;
                            $display("L1 refill beat %0d/%0d addr=%08h @%0t", refill_cnt, LINE_BEATS, {tag, idx, {OFFSET_BITS{1'b0}}}, $time);
                        end
                    end
                    if (refill_last) begin
                        tag_ram[idx].valid <= 1'b1;
                        tag_ram[idx].tag   <= tag;
                        tag_ram[idx].dirty <= 1'b0;
                        cnt_refills <= cnt_refills + 1;
                        $display("L1 refill done addr=%08h pending_store=%0b @%0t", {tag, idx, {OFFSET_BITS{1'b0}}}, pending_is_store, $time);
                    end

                    if (refill_last && pending_is_store) begin
                        integer lane;
                        logic [LINE_BYTES*8-1:0] line_new2;
                        line_new2 = data_ram[idx];
                        if (!pending_is_vector) begin
                            line_new2[base_word*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH]
                                = pending_wdata[0 +: MAX_RDATA_WIDTH];
                        end else begin
                            for (lane = 0; lane < VEC_WORDS; lane = lane + 1) begin
                                if (pending_vec_wmask[lane]) begin
                                    line_new2[(base_word+lane)*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH]
                                        = pending_wdata[lane*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                                end
                            end
                        end
                        data_ram[idx] <= line_new2;
                        if (WRITEBACK) begin
                            tag_ram[idx].dirty <= 1'b1;
                        end else if (!mem_busy && mem_req_ready) begin
                            mem_req_valid_r <= 1'b1;
                            mem_req_rw_r <= 1'b1;
                            mem_req_addr_r <= {tag, idx, {OFFSET_BITS{1'b0}}};
                            mem_req_size_r <= LINE_BYTES[7:0];
                            mem_req_qos_r <= 4'b0001;
                            mem_req_id_r <= pending_id;
                            mem_req_wdata_r <= line_new2;
                            mem_req_wstrb_r <= {LINE_BYTES/8{1'b1}};
                            mem_req_src_r <= MEM_SRC_L1_WT;
                        end
                    end

                    if (refill_last && !pending_is_store) begin
                        integer lane;
                        if (pending_from_lsu0) begin
                            lsu0_resp_id_r <= pending_id;
                            lsu0_resp_data_r <= '0;
                            if (pending_is_vector) begin
                                for (lane = 0; lane < VEC_WORDS; lane = lane + 1) begin
                                    lsu0_resp_data_r[lane*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH]
                                        <= data_ram[idx][(base_word+lane)*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                                end
                            end else begin
                                lsu0_resp_data_r[0 +: MAX_RDATA_WIDTH]
                                    <= data_ram[idx][base_word*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                            end
                            lsu0_resp_valid_r <= 1'b1;
                        end else begin
                            lsu1_resp_id_r <= pending_id;
                            lsu1_resp_data_r <= '0;
                            if (pending_is_vector) begin
                                for (lane = 0; lane < VEC_WORDS; lane = lane + 1) begin
                                    lsu1_resp_data_r[lane*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH]
                                        <= data_ram[idx][(base_word+lane)*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                                end
                            end else begin
                                lsu1_resp_data_r[0 +: MAX_RDATA_WIDTH]
                                    <= data_ram[idx][base_word*MAX_RDATA_WIDTH +: MAX_RDATA_WIDTH];
                            end
                            lsu1_resp_valid_r <= 1'b1;
                        end
                    end

                    if (refill_last) begin
                        pending_valid <= 1'b0;
                        state <= ST_IDLE;
                        mem_outstanding_src <= MEM_SRC_NONE;
                    end
                end else if (mem_outstanding_src == MEM_SRC_TEX_REFILL) begin
                    logic [REFILL_BITS-1:0] refill_buf_next_tex;
                    logic refill_last_tex;
                    if (LINE_BEATS == 1) begin
                        tex_miss_resp_data <= mem_resp_line;
                        refill_last_tex = 1'b1;
                    end else begin
                        refill_buf_next_tex = refill_buf;
                        refill_buf_next_tex[refill_cnt*AXI_DATA_BITS +: AXI_DATA_BITS] = mem_resp_data;
                        refill_last_tex = (refill_cnt == REFILL_CNT_BITS'(LINE_BEATS_M1));
                        if (refill_last_tex) begin
                            tex_miss_resp_data <= refill_buf_next_tex;
                            refill_cnt <= '0;
                        end else begin
                            refill_buf <= refill_buf_next_tex;
                            refill_cnt <= refill_cnt + 1'b1;
                        end
                    end
                    if (refill_last_tex) begin
                        tex_miss_resp_valid <= 1'b1;
                        mem_outstanding_src <= MEM_SRC_NONE;
                    end
                end else begin
                    mem_outstanding_src <= MEM_SRC_NONE;
                end
            end

            // WB buffer push happens after all combinational decisions to keep ordering clear
            if (wb_push) begin
                wb_fifo[wb_wr_ptr] <= '{addr: wb_push_addr, data: wb_push_data};
                wb_wr_ptr <= wb_wr_ptr + 1'b1;
                wb_count <= wb_count + 1'b1;
                assert (wb_count < WB_COUNT_BITS'(WB_DEPTH)) else $error("WB buffer overflow");
                assert (wb_push_dirty_flag) else $error("WB push without dirty line");
            end

            // Assertions for refill ordering and dirty eviction correctness
            if (mem_resp_valid && (mem_outstanding_src == MEM_SRC_L1_REFILL || mem_outstanding_src == MEM_SRC_TEX_REFILL)) begin
                assert ((LINE_BEATS == 1) || (refill_cnt < REFILL_CNT_BITS'(LINE_BEATS))) else $error("Refill beat ordering violation");
            end
            if (wb_push) begin
                assert (^wb_push_addr !== 1'bx) else $error("WB push address X");
            end
        end
    end

endmodule
