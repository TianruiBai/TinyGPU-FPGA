module lsu #(
    parameter bit MAILBOX_ENABLE = 1'b0,
    parameter int WMB_ENTRIES = 8
) (
    input  logic         clk,
    input  logic         rst_n,
    // Pipeline side
    input  logic         valid_in,
    input  logic         is_vector,
    input  logic         is_store,
    // Scalar load/store size (RISC-V style): 000=byte, 001=half, 010=word
    input  logic [2:0]   scalar_funct3,
    input  logic         is_atomic,
    input  logic [2:0]   atomic_op,
    input  logic [1:0]   vec_mode,      // 0=unit,1=stride,2=indexed (for vector ops)
    input  logic [31:0]  vec_stride,    // scalar stride bytes (funct3=001)
    input  logic [127:0] vec_index,     // index vector (funct3=010)
    input  logic [31:0]  addr,
    input  logic [127:0] write_data,
    input  logic         flush_wmb,
    input  logic [4:0]   dest_reg_idx,
    output logic         stall_pipeline, // now "internal_stall" concept
    output logic         busy,           // asserted when any transaction in flight

    // Graphics/ROP store interface (write-only, global space)
    input  logic         gfx_st_valid,
    input  logic [31:0]  gfx_st_addr,
    input  logic [31:0]  gfx_st_wdata,
    input  logic [3:0]   gfx_st_wstrb,
    output logic         gfx_st_ready,

    // Texture Cache Miss Interface (Refill Request)
    input  logic         tex_req_valid,
    input  logic [31:0]  tex_req_addr,
    output logic         tex_req_ready,
    output logic         tex_resp_valid,
    output logic [127:0] tex_resp_data, // Always 128-bit vector

    // GFX Descriptor Cache Miss Interface (Refill Request)
    input  logic         gfx_req_valid,
    input  logic [31:0]  gfx_req_addr,
    output logic         gfx_req_ready,
    output logic         gfx_resp_valid,
    output logic [127:0] gfx_resp_data, // Always 128-bit vector

    // Local memory (shared BRAM)
    output logic         local_req_valid,
    output logic         local_we,
    output logic         local_req_is_vector,
    output logic [31:0]  local_addr,
    output logic [127:0] local_wdata,
    output logic [1:0]   local_bank_sel,
    input  logic [127:0] local_rdata,

    // Global memory (OSPI) Unified 32-bit Interface
    output logic         global_req_valid,
    output logic         global_req_is_load,
    output logic [31:0]  global_req_addr,
    output logic [31:0]  global_req_wdata, // Low 32 bits for scalar, sequential for vector
    output logic [4:0]   global_req_rd,    // Only relevant for LSU scalar tracking
    input  logic         global_req_ready,

    input  logic         global_resp_valid,
    input  logic [4:0]   global_resp_rd,
    input  logic [31:0]  global_resp_data,

    // Mailbox sideband toward endpoint (internal)
    output logic         mailbox_tx_valid,
    output logic [15:0]  mailbox_tx_dest,
    output logic [31:0]  mailbox_tx_data,
    output logic         mailbox_tx_prio,
    output logic         mailbox_tx_eop,
    output logic [3:0]   mailbox_tx_opcode,
    input  logic         mailbox_tx_ready,
    output logic [3:0]   mailbox_tx_strb,
    output logic         mailbox_rd_valid,
    input  logic         mailbox_rd_ready,
    output logic [15:0]  mailbox_rd_dest,
    output logic         mailbox_rd_prio,
    output logic [3:0]   mailbox_rd_opcode,
    input  logic         mailbox_rd_resp_valid,
    output logic         mailbox_rd_resp_ready,
    input  logic [31:0]  mailbox_rd_resp_data,
    input  mailbox_pkg::mailbox_tag_t mailbox_rd_resp_tag,

    // Writeback toward register files
    output logic         wb_valid,
    output logic         wb_is_vector,
    output logic [4:0]   wb_reg_idx,
    output logic [127:0] wb_data
);

    import mailbox_pkg::*;
    // ------------------------------------------------------------------------
    // CORE LSU INTERNAL DEFINITIONS
    // ------------------------------------------------------------------------
    typedef enum logic [1:0] {
        LSU_IDLE,
        LSU_WAIT_SCALAR_RESP
    } lsu_state_e;

    lsu_state_e state;
    logic       is_global;
    logic [31:0] addr_eff;

    // Scalar subword load tracking (LB/LH implemented as aligned LW + extract)
    logic        pending_scalar_subword;
    logic [2:0]  pending_scalar_funct3;
    logic [1:0]  pending_scalar_byte_off;

    // Scalar subword store fixup (SB/SH implemented as aligned LW + merge + SW)
    typedef enum logic [1:0] {
        RMW_IDLE,
        RMW_WAIT_LOAD,
        RMW_ISSUE_STORE
    } rmw_state_e;
    rmw_state_e  rmw_state;
    logic [31:0] rmw_addr;
    logic [2:0]  rmw_funct3;
    logic [31:0] rmw_store_data;
    logic [31:0] rmw_old_word;
    logic [31:0] rmw_new_word;
    logic        rmw_load_issued;
    logic [1:0]  r_vec_mode;
    logic [31:0] r_vec_stride;
    logic [127:0] r_vec_index;
    logic [127:0] r_vec_oprnd;
    logic [127:0] atom_oprnd;
    logic [31:0]  atom_oprnd_s;
    logic [2:0]   r_atomic_op;
    logic         atomic_pending;
    logic         atom_is_vector;
    typedef enum logic [1:0] {ATOM_IDLE, ATOM_WAIT_LOAD, ATOM_WAIT_WRITE} atom_state_e;
    atom_state_e  atom_state;
    logic [4:0]   atom_rd;
    logic [31:0]  atom_base;
    logic [127:0] atom_old;
    logic [127:0] atom_new;
    logic [31:0]  atom_old_s;
    logic [31:0]  atom_new_s;

    // Track blocking scalar global load outstanding
    logic       scalar_global_wait;
    logic [4:0] pending_scalar_rd;

    // Mailbox tracking
    wire        mailbox_store;
    wire        mailbox_load_hit;
    logic       mailbox_pending;
    logic       mailbox_load_pending;
    logic [4:0] mailbox_load_rd;
    logic [2:0] mailbox_load_funct3;
    logic [15:0] mailbox_dest_r;
    logic [31:0] mailbox_data_r;
    logic [15:0] mailbox_load_dest;
    logic        mailbox_rd_outstanding;
    wire        mailbox_ready_int;

    // Helpers to detect if the current MIU response is a cache refill (not an LSU response)
    wire tex_resp_valid_internal_wire;
    wire gfx_resp_valid_internal_wire;

    // Write-merge buffer (global scalar stores)
    logic        wmb_in_valid;
    logic [31:0] wmb_in_addr;
    logic [31:0] wmb_in_wdata;
    logic [3:0]  wmb_in_wstrb;
    logic        wmb_in_ready;
    logic        wmb_out_valid;
    logic [31:0] wmb_out_addr;
    logic [31:0] wmb_out_wdata;
    logic [3:0]  wmb_out_wstrb;
    logic        wmb_out_ready;
    logic        wmb_busy;

    // Track local read latency (1-cycle BRAM)
    logic       r_local_read_pending;
    logic       r_local_is_vector;
    logic [1:0] r_local_bank_sel;
    logic [4:0] r_local_rd_idx;

    assign is_global = addr[31];
    assign addr_eff  = is_vector ? {addr[31:4], 4'b0} : addr; // enforce 16-byte alignment

    assign mailbox_store     = MAILBOX_ENABLE && valid_in && is_store && !is_vector && !is_atomic && (addr[31:16] == 16'h7000);
    assign mailbox_load_hit  = MAILBOX_ENABLE && valid_in && !is_store && !is_vector && !is_atomic && (addr[31:16] == 16'h7000);
    assign mailbox_ready_int = MAILBOX_ENABLE ? mailbox_tx_ready : 1'b1;

    wire [31:0] scalar_word_addr = {addr[31:2], 2'b00};
    // Only scalar subword STORES (SB/SH) are blocked from directly issuing to the
    // global interface; they are implemented via the RMW path below.
    // Scalar subword LOADS (LB/LH) issue an aligned word read and extract in WB.
    wire        scalar_is_subword_store = valid_in && is_global && is_store && !is_vector && !is_atomic && (scalar_funct3 != 3'b010);

    // Scalar global stores bypass the arbiter and go through the write-merge buffer.
    wire is_scalar_global_store = valid_in && is_global && is_store && !is_vector && !is_atomic && !scalar_global_wait
                               && (scalar_funct3 == 3'b010) && (rmw_state == RMW_IDLE);

    // Priority: gfx/rop stores > scalar pipeline stores
    // Mask unknowns on gfx valid to avoid propagating X into WMB selection/stall.
    wire gfx_st_valid_masked = (gfx_st_valid === 1'b1);
    wire wmb_sel_gfx = gfx_st_valid_masked;

    assign wmb_in_valid = wmb_sel_gfx || is_scalar_global_store;
    assign wmb_in_addr  = wmb_sel_gfx ? gfx_st_addr  : addr;
    assign wmb_in_wdata = wmb_sel_gfx ? gfx_st_wdata : write_data[31:0];
    assign wmb_in_wstrb = wmb_sel_gfx ? gfx_st_wstrb : 4'hF;

    assign gfx_st_ready = wmb_sel_gfx ? wmb_in_ready : 1'b0;

    write_merge_buf #(
        .ENTRIES(WMB_ENTRIES)
    ) u_wmb (
        .clk(clk),
        .rst_n(rst_n),

        .in_valid(wmb_in_valid),
        .in_addr(wmb_in_addr),
        .in_wdata(wmb_in_wdata),
        .in_wstrb(wmb_in_wstrb),
        .in_ready(wmb_in_ready),

        .flush(flush_wmb),

        .out_valid(wmb_out_valid),
        .out_addr(wmb_out_addr),
        .out_wdata(wmb_out_wdata),
        .out_wstrb(wmb_out_wstrb),
        .out_ready(wmb_out_ready),

        .busy(wmb_busy)
    );

    // ------------------------------------------------------------------------
    // MIU ARBITER RESPONSE BUS (from external miu_arbiter)
    // ------------------------------------------------------------------------
    logic        arb_resp_valid;
    logic [4:0]  arb_resp_rd;
    logic [31:0] arb_resp_data_scalar;
    logic [127:0] arb_resp_data_vector;
    logic        arb_resp_is_vector;
    logic        arb_resp_is_rmw;
    logic        miu_busy;
    logic        miu_resp_is_tex;
    logic        miu_resp_is_gfx;

    // LSU sub-block internal request signals (to Arbiter)
    logic        lsu_internal_req_valid;
    logic        lsu_internal_req_is_load;
    logic        lsu_internal_req_is_vector;
    logic [31:0] lsu_internal_req_addr;
    logic [127:0] lsu_internal_req_wdata;
    logic [4:0]  lsu_internal_req_rd;
    logic        lsu_internal_req_ready;

    // ------------------------------------------------------------------------
    // LOCAL MEMORY LOGIC & LSU REQUEST GENERATION
    // ------------------------------------------------------------------------
    logic        next_lsu_req_valid;
    logic        next_lsu_req_is_load;
    logic        next_lsu_req_is_vector;
    logic [31:0] next_lsu_req_addr;
    logic [127:0] next_lsu_req_wdata;
    logic [4:0]  next_lsu_req_rd;

    always_comb begin
        // Local path
        local_req_valid     = valid_in && !is_global && !mailbox_store && !mailbox_pending && !mailbox_load_hit && !mailbox_load_pending;
        local_req_is_vector = valid_in && !is_global && is_vector && !mailbox_store && !mailbox_pending && !mailbox_load_hit && !mailbox_load_pending;
        local_we            = valid_in && !is_global && is_store && !mailbox_store && !mailbox_pending && !mailbox_load_hit && !mailbox_load_pending;
        local_addr          = addr_eff;
        local_wdata         = is_vector ? write_data : {4{write_data[31:0]}};
        local_bank_sel      = is_vector ? 2'b00 : addr[1:0];

        // Defaults for global request path
        next_lsu_req_valid     = valid_in && is_global && !scalar_global_wait && !is_scalar_global_store && !scalar_is_subword_store && !mailbox_store && !mailbox_pending && !mailbox_load_hit && !mailbox_load_pending;
        next_lsu_req_is_load   = !is_store;
        next_lsu_req_is_vector = is_vector;
        // For scalar subword loads (LB/LH), issue an aligned word read and extract in WB.
        next_lsu_req_addr      = (!is_store && !is_vector && (scalar_funct3 != 3'b010)) ? scalar_word_addr : addr_eff;
        next_lsu_req_wdata     = is_vector ? write_data : {96'h0, write_data[31:0]};
        next_lsu_req_rd        = dest_reg_idx;

        // Subword store fixup: SB/SH are implemented as RMW on an aligned word.
        if (rmw_state != RMW_IDLE) begin
            next_lsu_req_valid     = !scalar_global_wait;
            next_lsu_req_is_vector = 1'b0;
            next_lsu_req_addr      = {rmw_addr[31:2], 2'b00};
            next_lsu_req_rd        = 5'd0;
            if (rmw_state == RMW_WAIT_LOAD) begin
                next_lsu_req_valid   = !scalar_global_wait && !rmw_load_issued;
                next_lsu_req_is_load = 1'b1;
                next_lsu_req_wdata   = 128'h0;
            end else begin
                next_lsu_req_is_load = 1'b0;
                next_lsu_req_wdata   = {96'h0, rmw_new_word};
            end
        end

        // Atomic overrides: issue read then write using stored operands
        if (atomic_pending) begin
            next_lsu_req_valid     = !scalar_global_wait;
            next_lsu_req_is_vector = atom_is_vector;
            next_lsu_req_addr      = atom_base;
            next_lsu_req_rd        = atom_rd;
            if (atom_state == ATOM_WAIT_LOAD) begin
                next_lsu_req_is_load   = 1'b1;
                next_lsu_req_wdata     = 128'h0;
            end else begin
                next_lsu_req_is_load   = 1'b0;
                next_lsu_req_wdata     = atom_is_vector ? atom_new : {96'h0, atom_new_s};
            end
        end

        lsu_internal_req_valid     = next_lsu_req_valid;
        lsu_internal_req_is_load   = next_lsu_req_is_load;
        lsu_internal_req_is_vector = next_lsu_req_is_vector;
        lsu_internal_req_addr      = next_lsu_req_addr;
        lsu_internal_req_wdata     = next_lsu_req_wdata;
        lsu_internal_req_rd        = next_lsu_req_rd;

                /* verilator lint_off UNOPTFLAT */
                // Stall pipeline if:
                // 1. Waiting for scalar load response
                // 2. Issuing a new global request but arbiter is not ready (backpressure)
                // 3. Issuing a scalar load (blocking by ISA def)
                 stall_pipeline = mailbox_store
                         || mailbox_pending
                         || mailbox_load_hit
                         || mailbox_load_pending
                             || scalar_global_wait 
                                     || (rmw_state != RMW_IDLE)
                                     || atomic_pending
                                     || (flush_wmb && wmb_busy)
                                     || (is_scalar_global_store && (gfx_st_valid_masked || !wmb_in_ready))
                                     // Conservative ordering: don't let scalar loads pass buffered stores
                                     // (no store-to-load forwarding implemented)
                                     || (next_lsu_req_valid && !next_lsu_req_is_vector && !is_store && wmb_busy)
                                     || (next_lsu_req_valid && !lsu_internal_req_ready)
                                     || (next_lsu_req_valid && !next_lsu_req_is_vector && !is_store); // Block on scalar load issue
                /* verilator lint_on UNOPTFLAT */
    end

    function automatic logic [31:0] sext8(input logic [7:0] b);
        sext8 = {{24{b[7]}}, b};
    endfunction

    function automatic logic [31:0] sext16(input logic [15:0] h);
        sext16 = {{16{h[15]}}, h};
    endfunction

    function automatic logic [31:0] extract_subword_load(
        input logic [31:0] word,
        input logic [2:0]  f3,
        input logic [1:0]  byte_off
    );
        logic [7:0]  b;
        logic [15:0] h;
        begin
            unique case (byte_off)
                2'd0: b = word[7:0];
                2'd1: b = word[15:8];
                2'd2: b = word[23:16];
                default: b = word[31:24];
            endcase
            h = byte_off[1] ? word[31:16] : word[15:0];

            unique case (f3)
                3'b000: extract_subword_load = sext8(b);           // LB
                3'b001: extract_subword_load = sext16(h);          // LH
                3'b100: extract_subword_load = {24'h0, b};         // LBU
                3'b101: extract_subword_load = {16'h0, h};         // LHU
                default: extract_subword_load = word;              // LW / fallback
            endcase
        end
    endfunction

    function automatic logic [31:0] merge_subword_store(
        input logic [31:0] old_word,
        input logic [2:0]  f3,
        input logic [1:0]  byte_off,
        input logic [31:0] store_data
    );
        logic [31:0] mask;
        logic [31:0] ins;
        int unsigned sh;
        begin
            unique case (f3)
                3'b000: begin // SB
                    sh   = {30'd0, byte_off} * 8;
                    mask = 32'h0000_00FF << sh;
                    ins  = (store_data & 32'hFF) << sh;
                end
                3'b001: begin // SH
                    sh   = {31'd0, byte_off[1]} * 16;
                    mask = 32'h0000_FFFF << sh;
                    ins  = (store_data & 32'hFFFF) << sh;
                end
                default: begin
                    mask = 32'hFFFF_FFFF;
                    ins  = store_data;
                end
            endcase
            merge_subword_store = (old_word & ~mask) | (ins & mask);
        end
    endfunction

    // ------------------------------------------------------------------------
    // LSU STATE & WRITEBACK
    // ------------------------------------------------------------------------
    function automatic [31:0] lane_addr(
        input logic [31:0] base,
        input logic [1:0]  mode,
        input logic [31:0] stride,
        input logic [127:0] idx_vec,
        input int          lane
    );
        case (mode)
            2'b01: lane_addr = base + stride * lane; // strided
            2'b10: lane_addr = base + idx_vec[lane*32 +: 32]; // indexed
            default: lane_addr = base + lane*4; // unit (4 bytes per lane)
        endcase
    endfunction

    always_ff @(posedge clk or negedge rst_n) begin
        integer li;
        logic signed [31:0] atom_a;
        logic signed [31:0] atom_b;
        if (!rst_n) begin
            state                <= LSU_IDLE;
            scalar_global_wait   <= 1'b0;
            pending_scalar_rd    <= '0;
            pending_scalar_subword <= 1'b0;
            pending_scalar_funct3  <= 3'b010;
            pending_scalar_byte_off<= 2'b00;

            rmw_state            <= RMW_IDLE;
            rmw_addr             <= 32'h0;
            rmw_funct3           <= 3'b010;
            rmw_store_data       <= 32'h0;
            rmw_old_word         <= 32'h0;
            rmw_new_word         <= 32'h0;
            rmw_load_issued      <= 1'b0;
            r_local_read_pending <= 1'b0;
            r_local_is_vector    <= 1'b0;
            r_local_bank_sel     <= 2'b0;
            r_local_rd_idx       <= '0;
            r_vec_mode           <= 2'b0;
            r_vec_stride         <= 32'h0;
            r_vec_index          <= 128'h0;
            r_vec_oprnd          <= 128'h0;
            atom_oprnd           <= 128'h0;
            atom_oprnd_s         <= 32'h0;
            r_atomic_op          <= 3'b0;
            atomic_pending       <= 1'b0;
            atom_is_vector       <= 1'b0;
            atom_state           <= ATOM_IDLE;
            atom_rd              <= '0;
            atom_base            <= 32'h0;
            atom_old             <= 128'h0;
            atom_new             <= 128'h0;
            atom_old_s           <= 32'h0;
            atom_new_s           <= 32'h0;
            mailbox_pending      <= 1'b0;
            mailbox_load_pending <= 1'b0;
            mailbox_dest_r       <= 16'h0;
            mailbox_data_r       <= 32'h0;
            mailbox_load_dest    <= 16'h0;
            mailbox_rd_outstanding <= 1'b0;
        end else begin
            // Local read tracking
            if (valid_in && !is_global && !is_store) begin
                r_local_read_pending <= 1'b1;
                r_local_is_vector    <= is_vector;
                r_local_bank_sel     <= is_vector ? 2'b00 : addr[1:0];
                r_local_rd_idx       <= dest_reg_idx;
            end else begin
                r_local_read_pending <= 1'b0;
            end

            // Latch vector addressing info on issue; freeze while atomic in flight
            if (valid_in && is_vector && !atomic_pending) begin
                r_vec_mode   <= vec_mode;
                r_vec_stride <= vec_stride;
                r_vec_index  <= vec_index;
                r_vec_oprnd  <= write_data;
                r_atomic_op  <= atomic_op;
            end

            // Latch scalar atomic op code as well (scalar atomics are RMW on 32-bit)
            if (valid_in && !is_vector && is_atomic && !atomic_pending) begin
                r_atomic_op <= atomic_op;
            end

            // Scalar global load blocking logic
            case (state)
                LSU_IDLE: begin
                    // Only block on real pipeline-issued scalar global LOADs.
                    // Do not let internal RMW/atomic override traffic accidentally
                    // assert scalar_global_wait using unrelated decode-stage signals.
                    if (
                        valid_in
                        && is_global
                        && !is_vector
                        && !is_store
                        && !is_atomic
                        && (rmw_state == RMW_IDLE)
                        && !atomic_pending
                        && lsu_internal_req_valid
                        && lsu_internal_req_is_load
                        && !lsu_internal_req_is_vector
                        && lsu_internal_req_ready
                    ) begin
                        scalar_global_wait <= 1'b1;
                        pending_scalar_rd  <= lsu_internal_req_rd;
                        pending_scalar_subword  <= (scalar_funct3 != 3'b010);
                        pending_scalar_funct3   <= scalar_funct3;
                        pending_scalar_byte_off <= addr[1:0];
                        state              <= LSU_WAIT_SCALAR_RESP;
                    end
                end
                LSU_WAIT_SCALAR_RESP: begin
                    // Check arbiter response (routed to LSU)
                    // Scalar loads are blocking; while waiting, the next non-vector LSU
                    // response is the completion for the in-flight scalar load.
                    if (
                        arb_resp_valid
                        && !arb_resp_is_vector
                        && !tex_resp_valid_internal_wire
                        && !gfx_resp_valid_internal_wire
                        && (arb_resp_rd == pending_scalar_rd)
                    ) begin
                        scalar_global_wait <= 1'b0;
                        pending_scalar_subword <= 1'b0;
                        state              <= LSU_IDLE;
                    end
                end
                default: state <= LSU_IDLE;
            endcase

            // SB/SH fixup: do aligned word load then merged word store.
            if (rmw_state == RMW_IDLE) begin
                if (valid_in && is_global && !is_vector && is_store && !is_atomic && (scalar_funct3 != 3'b010) && !scalar_global_wait) begin
                    rmw_state      <= RMW_WAIT_LOAD;
                    rmw_addr       <= addr;
                    rmw_funct3     <= scalar_funct3;
                    rmw_store_data <= write_data[31:0];
                    rmw_load_issued <= 1'b0;
                end
            end else if (rmw_state == RMW_WAIT_LOAD) begin
                // Mark the aligned-word load as issued once accepted.
                if (lsu_internal_req_valid && lsu_internal_req_ready && lsu_internal_req_is_load) begin
                    rmw_load_issued <= 1'b1;
                end

                // Only consume the response corresponding to the internal RMW load.
                // This avoids accidentally grabbing an unrelated scalar response
                // (including software loads to x0 / rd==0).
                if (rmw_load_issued && arb_resp_valid && !arb_resp_is_vector && !tex_resp_valid_internal_wire && !gfx_resp_valid_internal_wire && arb_resp_is_rmw) begin
                    rmw_old_word <= arb_resp_data_scalar;
                    rmw_new_word <= merge_subword_store(arb_resp_data_scalar, rmw_funct3, rmw_addr[1:0], rmw_store_data);
                    rmw_state    <= RMW_ISSUE_STORE;
                    rmw_load_issued <= 1'b0;
                end
            end else begin // RMW_ISSUE_STORE
                if (lsu_internal_req_valid && lsu_internal_req_ready && !lsu_internal_req_is_load) begin
                    rmw_state <= RMW_IDLE;
                end
            end

            // Atomic flow (vector + scalar)
            if (valid_in && is_atomic && !scalar_global_wait && !atomic_pending) begin
                atomic_pending <= 1'b1;
                atom_state     <= ATOM_WAIT_LOAD;
                atom_rd        <= dest_reg_idx;
                atom_base      <= addr_eff;
                atom_is_vector <= is_vector;
                if (is_vector) begin
                    atom_oprnd <= write_data; // capture source vector for atomic op
                end else begin
                    atom_oprnd_s <= write_data[31:0];
                end
            end else if (atomic_pending && atom_state == ATOM_WAIT_LOAD && arb_resp_valid && !tex_resp_valid_internal_wire && !gfx_resp_valid_internal_wire) begin
                if (atom_is_vector) begin
                    atom_old <= arb_resp_data_vector;
                    for (li = 0; li < 4; li++) begin
                        atom_a = arb_resp_data_vector[li*32 +: 32];
                        atom_b = atom_oprnd[li*32 +: 32];
                        case (r_atomic_op)
                            3'b000: atom_new[li*32 +: 32] <= atom_a + atom_b; // ADD
                            3'b001: atom_new[li*32 +: 32] <= (atom_a < atom_b) ? atom_a : atom_b; // MIN
                            3'b010: atom_new[li*32 +: 32] <= (atom_a > atom_b) ? atom_a : atom_b; // MAX
                            3'b011: atom_new[li*32 +: 32] <= atom_b; // XCHG
                            3'b101: atom_new[li*32 +: 32] <= atom_a & atom_b; // AND
                            3'b110: atom_new[li*32 +: 32] <= atom_a | atom_b; // OR
                            3'b111: atom_new[li*32 +: 32] <= atom_a ^ atom_b; // XOR
                            default: atom_new[li*32 +: 32] <= atom_a; // default keep
                        endcase
                    end
                end else begin
                    atom_old_s <= arb_resp_data_scalar;
                    atom_a = arb_resp_data_scalar;
                    atom_b = atom_oprnd_s;
                    case (r_atomic_op)
                        3'b000: atom_new_s <= atom_a + atom_b; // ADD
                        3'b001: atom_new_s <= (atom_a < atom_b) ? atom_a : atom_b; // MIN
                        3'b010: atom_new_s <= (atom_a > atom_b) ? atom_a : atom_b; // MAX
                        3'b011: atom_new_s <= atom_b;           // XCHG
                        3'b101: atom_new_s <= atom_a & atom_b;  // AND
                        3'b110: atom_new_s <= atom_a | atom_b;  // OR
                        3'b111: atom_new_s <= atom_a ^ atom_b;  // XOR
                        default: atom_new_s <= atom_a;
                    endcase
                end
                atom_state <= ATOM_WAIT_WRITE;
            end else if (atomic_pending && atom_state == ATOM_WAIT_WRITE && lsu_internal_req_valid && lsu_internal_req_ready && !lsu_internal_req_is_load) begin
                atomic_pending <= 1'b0;
                atom_state     <= ATOM_IDLE;
            end

            // Mailbox capture/handshake (optional)
            if (MAILBOX_ENABLE) begin
                if (!mailbox_pending && mailbox_store) begin
                    mailbox_pending <= 1'b1;
                    mailbox_dest_r  <= addr[15:0];
                    mailbox_data_r  <= write_data[31:0];
                end else if (mailbox_pending && mailbox_ready_int) begin
                    mailbox_pending <= 1'b0;
                end

                if (!mailbox_load_pending && mailbox_load_hit) begin
                    mailbox_load_pending <= 1'b1;
                    mailbox_load_rd      <= dest_reg_idx;
                    mailbox_load_funct3  <= scalar_funct3;
                    mailbox_load_dest    <= addr[15:0];
                end else if (mailbox_load_pending && mailbox_rd_resp_valid && mailbox_rd_resp_ready) begin
                    mailbox_load_pending <= 1'b0;
                end
            end else begin
                mailbox_pending <= 1'b0;
                mailbox_load_pending <= 1'b0;
            end

            if (mailbox_rd_valid && mailbox_rd_ready) begin
                mailbox_rd_outstanding <= 1'b1;
            end else if (mailbox_rd_resp_valid && mailbox_rd_resp_ready) begin
                mailbox_rd_outstanding <= 1'b0;
            end
        end
    end

    // LSU Writeback Mux (Local vs Global)
    always_comb begin
        wb_valid     = 1'b0;
        wb_is_vector = 1'b0;
        wb_reg_idx   = '0;
        wb_data      = '0;

        // Mailbox load completion takes priority
        if (MAILBOX_ENABLE && mailbox_load_pending && mailbox_rd_resp_valid) begin
            wb_valid     = 1'b1;
            wb_is_vector = 1'b0;
            wb_reg_idx   = mailbox_load_rd;
            wb_data      = {96'h0, extract_subword_load(mailbox_rd_resp_data, mailbox_load_funct3, 2'b00)};
        end else if (atomic_pending && atom_state == ATOM_WAIT_WRITE && lsu_internal_req_valid && lsu_internal_req_ready && !lsu_internal_req_is_load) begin
            wb_valid     = 1'b1;
            wb_is_vector = atom_is_vector;
            wb_reg_idx   = atom_rd;
            wb_data      = atom_is_vector ? atom_old : {96'h0, atom_old_s}; // return old value
        end else if (arb_resp_valid && !tex_resp_valid_internal_wire && !gfx_resp_valid_internal_wire) begin // If response is for LSU
             // Use explicit scalar/vector tagging from the response router.
             if (arb_resp_is_vector) begin
                 wb_valid     = 1'b1;
                 wb_is_vector = 1'b1;
                 wb_reg_idx   = arb_resp_rd;
                 wb_data      = arb_resp_data_vector;
             end else begin
                 // Suppress writeback for internal SB/SH fixup loads.
                 if (!arb_resp_is_rmw) begin
                     wb_valid     = 1'b1;
                     wb_is_vector = 1'b0;
                     wb_reg_idx   = arb_resp_rd;
                     wb_data      = {96'h0, pending_scalar_subword ? extract_subword_load(arb_resp_data_scalar, pending_scalar_funct3, pending_scalar_byte_off)
                                                              : arb_resp_data_scalar};
                 end
             end
        end else if (r_local_read_pending) begin
            wb_valid     = 1'b1;
            wb_is_vector = r_local_is_vector;
            wb_reg_idx   = r_local_rd_idx;
            if (r_local_is_vector) begin
                wb_data = local_rdata;
            end else begin
                case (r_local_bank_sel)
                    2'b00: wb_data = {96'h0, local_rdata[31:0]};
                    2'b01: wb_data = {96'h0, local_rdata[63:32]};
                    2'b10: wb_data = {96'h0, local_rdata[95:64]};
                    default: wb_data = {96'h0, local_rdata[127:96]};
                endcase
            end
        end
    end

    wire lsu_req_is_rmw_load = (rmw_state == RMW_WAIT_LOAD) && !rmw_load_issued;

    miu_arbiter u_miu (
        .clk(clk),
        .rst_n(rst_n),

        .tex_req_valid(tex_req_valid),
        .tex_req_addr(tex_req_addr),
        .tex_req_ready(tex_req_ready),
        .tex_resp_valid(tex_resp_valid),
        .tex_resp_data(tex_resp_data),

        .gfx_req_valid(gfx_req_valid),
        .gfx_req_addr(gfx_req_addr),
        .gfx_req_ready(gfx_req_ready),
        .gfx_resp_valid(gfx_resp_valid),
        .gfx_resp_data(gfx_resp_data),

        .wmb_out_valid(wmb_out_valid),
        .wmb_out_addr(wmb_out_addr),
        .wmb_out_wdata(wmb_out_wdata),
        .wmb_out_wstrb(wmb_out_wstrb),
        .wmb_out_ready(wmb_out_ready),

        .lsu_req_valid(lsu_internal_req_valid),
        .lsu_req_is_load(lsu_internal_req_is_load),
        .lsu_req_is_vector(lsu_internal_req_is_vector),
        .lsu_req_addr(lsu_internal_req_addr),
        .lsu_req_wdata(lsu_internal_req_wdata),
        .lsu_req_rd(lsu_internal_req_rd),
        .lsu_req_is_rmw_load(lsu_req_is_rmw_load),
        .lsu_req_ready(lsu_internal_req_ready),

        .vec_mode(r_vec_mode),
        .vec_stride(r_vec_stride),
        .vec_index(r_vec_index),

        .global_req_valid(global_req_valid),
        .global_req_is_load(global_req_is_load),
        .global_req_addr(global_req_addr),
        .global_req_wdata(global_req_wdata),
        .global_req_rd(global_req_rd),
        .global_req_ready(global_req_ready),

        .global_resp_valid(global_resp_valid),
        .global_resp_rd(global_resp_rd),
        .global_resp_data(global_resp_data),

        .arb_resp_valid(arb_resp_valid),
        .arb_resp_rd(arb_resp_rd),
        .arb_resp_data_scalar(arb_resp_data_scalar),
        .arb_resp_data_vector(arb_resp_data_vector),
        .arb_resp_is_vector(arb_resp_is_vector),
        .arb_resp_is_rmw(arb_resp_is_rmw),
        .arb_resp_is_tex(miu_resp_is_tex),
        .arb_resp_is_gfx(miu_resp_is_gfx),

        .busy(miu_busy)
    );

    // Maintain legacy internal naming for gating LSU writeback vs texture refill.
    assign tex_resp_valid_internal_wire = tex_resp_valid;
    assign gfx_resp_valid_internal_wire = gfx_resp_valid;

    // Busy when any request or response is outstanding (scalar/vector/atomic/tex)
    assign busy = scalar_global_wait
               || atomic_pending
               || wmb_busy
               || r_local_read_pending
               || miu_busy
               || mailbox_pending
               || mailbox_load_pending
               || mailbox_rd_outstanding;

    // Mailbox outputs
    assign mailbox_rd_valid   = MAILBOX_ENABLE ? (mailbox_load_pending && !mailbox_rd_outstanding) : 1'b0;
    assign mailbox_rd_dest    = MAILBOX_ENABLE ? mailbox_load_dest : 16'h0;
    assign mailbox_rd_prio    = 1'b0;
    assign mailbox_rd_opcode  = 4'h0;
    assign mailbox_rd_resp_ready = MAILBOX_ENABLE ? 1'b1 : 1'b0;
    assign mailbox_tx_valid  = MAILBOX_ENABLE ? mailbox_pending : 1'b0;
    assign mailbox_tx_dest   = MAILBOX_ENABLE ? mailbox_dest_r  : 16'h0;
    assign mailbox_tx_data   = MAILBOX_ENABLE ? mailbox_data_r  : 32'h0;
    assign mailbox_tx_prio   = MAILBOX_ENABLE ? 1'b0 : 1'b0;
    assign mailbox_tx_eop    = MAILBOX_ENABLE ? 1'b1 : 1'b0;
    assign mailbox_tx_opcode = MAILBOX_ENABLE ? 4'h0 : 4'h0;
    assign mailbox_tx_strb   = MAILBOX_ENABLE ? (mailbox_dest_r[1:0] == 2'b00 ? 4'b1111:
                                                    mailbox_dest_r[1:0] == 2'b01 ? 4'b1110 :
                                                    mailbox_dest_r[1:0] == 2'b10 ? 4'b1100 :
                                                    mailbox_dest_r[1:0] == 2'b11 ? 4'b1000 : 4'h0) : 4'h0;
    assign mailbox_rx_ready  = MAILBOX_ENABLE && mailbox_load_pending;

endmodule