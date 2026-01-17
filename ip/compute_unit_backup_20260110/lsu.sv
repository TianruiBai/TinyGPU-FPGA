module lsu (
    input  logic         clk,
    input  logic         rst_n,
    // Pipeline side
    input  logic         valid_in,
    input  logic         is_vector,
    input  logic         is_store,
    input  logic         is_atomic,
    input  logic [2:0]   atomic_op,
    input  logic [1:0]   vec_mode,      // 0=unit,1=stride,2=indexed (for vector ops)
    input  logic [31:0]  vec_stride,    // scalar stride bytes (funct3=001)
    input  logic [127:0] vec_index,     // index vector (funct3=010)
    input  logic [31:0]  addr,
    input  logic [127:0] write_data,
    input  logic [4:0]   dest_reg_idx,
    output logic         stall_pipeline, // now "internal_stall" concept
    output logic         busy,           // asserted when any transaction in flight

    // Texture Cache Miss Interface (Refill Request)
    input  logic         tex_req_valid,
    input  logic [31:0]  tex_req_addr,
    output logic         tex_req_ready,
    output logic         tex_resp_valid,
    output logic [127:0] tex_resp_data, // Always 128-bit vector

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

    // Writeback toward register files
    output logic         wb_valid,
    output logic         wb_is_vector,
    output logic [4:0]   wb_reg_idx,
    output logic [127:0] wb_data
);
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
    logic [1:0]  r_vec_mode;
    logic [31:0] r_vec_stride;
    logic [127:0] r_vec_index;
    logic [127:0] r_vec_oprnd;
    logic [127:0] atom_oprnd;
    logic [2:0]   r_atomic_op;
    logic         atomic_pending;
    typedef enum logic [1:0] {ATOM_IDLE, ATOM_WAIT_LOAD, ATOM_WAIT_WRITE} atom_state_e;
    atom_state_e  atom_state;
    logic [4:0]   atom_rd;
    logic [31:0]  atom_base;
    logic [127:0] atom_old;
    logic [127:0] atom_new;

    // Track blocking scalar global load outstanding
    logic       scalar_global_wait;
    logic [4:0] pending_scalar_rd;

    // Helper to detect if we routed request to Tex
    wire tex_resp_valid_internal_wire; 

    // Track local read latency (1-cycle BRAM)
    logic       r_local_read_pending;
    logic       r_local_is_vector;
    logic [1:0] r_local_bank_sel;
    logic [4:0] r_local_rd_idx;

    assign is_global = addr[31];
    assign addr_eff  = is_vector ? {addr[31:4], 4'b0} : addr; // enforce 16-byte alignment

    // ------------------------------------------------------------------------
    // ARBITER & ADAPTER SIGNALS
    // ------------------------------------------------------------------------
    // Internal standardized request bus (128-bit)
    logic        arb_req_valid;
    logic        arb_req_is_load;
    logic        arb_req_is_vector;
    logic [31:0] arb_req_addr;
    logic [127:0] arb_req_wdata;
    logic [4:0]  arb_req_rd;
    logic        arb_req_ready;

    // Internal standardized response bus (128-bit/Scalar split)
    logic        arb_resp_valid;
    logic [4:0]  arb_resp_rd;
    logic [31:0] arb_resp_data_scalar;
    logic [127:0] arb_resp_data_vector;

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
    always_comb begin
        // Local path
        local_req_valid     = valid_in && !is_global;
        local_req_is_vector = valid_in && !is_global && is_vector;
        local_we            = valid_in && !is_global && is_store;
        local_addr          = addr_eff;
        local_wdata         = is_vector ? write_data : {4{write_data[31:0]}};
        local_bank_sel      = is_vector ? 2'b00 : addr[1:0];

        // Global request generation (LSU contribution)
        lsu_internal_req_valid     = valid_in && is_global && !scalar_global_wait;
        lsu_internal_req_is_load   = !is_store;
        lsu_internal_req_is_vector = is_vector;
        lsu_internal_req_addr      = addr_eff;
        lsu_internal_req_wdata     = is_vector ? write_data : {96'h0, write_data[31:0]};
        lsu_internal_req_rd        = dest_reg_idx;

        // Atomic overrides: issue read then write using stored operands
        if (atomic_pending) begin
            lsu_internal_req_valid     = !scalar_global_wait;
            lsu_internal_req_is_vector = 1'b1;
            lsu_internal_req_addr      = atom_base;
            lsu_internal_req_rd        = atom_rd;
            if (atom_state == ATOM_WAIT_LOAD) begin
                lsu_internal_req_is_load   = 1'b1;
                lsu_internal_req_wdata     = 128'h0;
            end else begin
                lsu_internal_req_is_load   = 1'b0;
                lsu_internal_req_wdata     = atom_new;
            end
        end

        // Stall pipeline if:
        // 1. Waiting for scalar load response
        // 2. Issuing a new global request but arbiter is not ready (backpressure)
        // 3. Issuing a scalar load (blocking by ISA def)
        stall_pipeline = scalar_global_wait 
                   || atomic_pending
                   || (lsu_internal_req_valid && !lsu_internal_req_ready)
                   || (lsu_internal_req_valid && !is_vector && !is_store); // Block on scalar load issue
    end

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
            r_local_read_pending <= 1'b0;
            r_local_is_vector    <= 1'b0;
            r_local_bank_sel     <= 2'b0;
            r_local_rd_idx       <= '0;
            r_vec_mode           <= 2'b0;
            r_vec_stride         <= 32'h0;
            r_vec_index          <= 128'h0;
            r_vec_oprnd          <= 128'h0;
            atom_oprnd           <= 128'h0;
            r_atomic_op          <= 3'b0;
            atomic_pending       <= 1'b0;
            atom_state           <= ATOM_IDLE;
            atom_rd              <= '0;
            atom_base            <= 32'h0;
            atom_old             <= 128'h0;
            atom_new             <= 128'h0;
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

            // Scalar global load blocking logic
            case (state)
                LSU_IDLE: begin
                    if (lsu_internal_req_valid && !is_vector && !is_store && lsu_internal_req_ready) begin
                        scalar_global_wait <= 1'b1;
                        pending_scalar_rd  <= dest_reg_idx;
                        state              <= LSU_WAIT_SCALAR_RESP;
                    end
                end
                LSU_WAIT_SCALAR_RESP: begin
                    // Check arbiter response (routed to LSU)
                    if (arb_resp_valid && (arb_resp_rd == pending_scalar_rd)) begin // RD check simplistic
                        scalar_global_wait <= 1'b0;
                        state              <= LSU_IDLE;
                    end
                end
                default: state <= LSU_IDLE;
            endcase

            // Atomic vector flow
            if (valid_in && is_vector && is_atomic && !scalar_global_wait && !atomic_pending) begin
                atomic_pending <= 1'b1;
                atom_state     <= ATOM_WAIT_LOAD;
                atom_rd        <= dest_reg_idx;
                atom_base      <= addr_eff;
                atom_oprnd     <= write_data; // capture source vector for atomic op
            end else if (atomic_pending && atom_state == ATOM_WAIT_LOAD && arb_resp_valid && !tex_resp_valid_internal_wire) begin
                atom_old   <= arb_resp_data_vector;
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
                atom_state <= ATOM_WAIT_WRITE;
            end else if (atomic_pending && atom_state == ATOM_WAIT_WRITE && arb_req_valid && arb_req_ready && !arb_req_is_load) begin
                atomic_pending <= 1'b0;
                atom_state     <= ATOM_IDLE;
            end
        end
    end

    // LSU Writeback Mux (Local vs Global)
    always_comb begin
        wb_valid     = 1'b0;
        wb_is_vector = 1'b0;
        wb_reg_idx   = '0;
        wb_data      = '0;

        // Global responses (from arbiter)
        // Note: vector loads are non-blocking, so we accept them anytime.
        // Scalar loads unblock the state machine.
        if (atomic_pending && atom_state == ATOM_WAIT_WRITE && arb_req_valid && arb_req_ready && !arb_req_is_load) begin
            wb_valid     = 1'b1;
            wb_is_vector = 1'b1;
            wb_reg_idx   = atom_rd;
            wb_data      = atom_old; // return old value
        end else if (arb_resp_valid && !tex_resp_valid_internal_wire) begin // If response is for LSU
             // Distinguish based on scalar_global_wait? Or allow vector WB?
             // Since scalar load blocks, if we are waiting, it MUST be the scalar response.
             // If we are NOT waiting, any response must be vector load WB.
             wb_valid = 1'b1;
             if (scalar_global_wait) begin
                 wb_is_vector = 1'b0;
                 wb_reg_idx   = arb_resp_rd;
                 wb_data      = {96'h0, arb_resp_data_scalar};
             end else begin
                 wb_is_vector = 1'b1;
                 wb_reg_idx   = arb_resp_rd;
                 wb_data      = arb_resp_data_vector;
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

    // ------------------------------------------------------------------------
    // ARBITER (Texture vs LSU)
    // Priority: Texture > LSU
    // ------------------------------------------------------------------------
    logic arb_grant_tex;
    // Mask unknowns on texture valid to avoid propagating X into stall/busy
    wire tex_req_valid_masked = (tex_req_valid === 1'b1);

    // Grant Texture if valid via internal wire check or direct
    assign arb_grant_tex = tex_req_valid_masked; // Strict priority

    assign arb_req_valid      = tex_req_valid_masked || lsu_internal_req_valid;
    assign arb_req_is_load    = arb_grant_tex ? 1'b1 : lsu_internal_req_is_load; // Tex always loads
    assign arb_req_is_vector  = arb_grant_tex ? 1'b1 : lsu_internal_req_is_vector;
    assign arb_req_addr       = arb_grant_tex ? tex_req_addr : lsu_internal_req_addr;
    assign arb_req_wdata      = arb_grant_tex ? 128'h0 : lsu_internal_req_wdata;
    assign arb_req_rd         = arb_grant_tex ? 5'd0 : lsu_internal_req_rd;

    // Acknowledge routing
    assign tex_req_ready          = arb_grant_tex && arb_req_ready;
    assign lsu_internal_req_ready = (!arb_grant_tex) && arb_req_ready;

    // ------------------------------------------------------------------------
    // MEMORY ADAPTER (128-bit Internal <-> 32-bit External)
    // ------------------------------------------------------------------------
    typedef enum logic [2:0] {
        ADAPT_IDLE,
        ADAPT_VWR_1, ADAPT_VWR_2, ADAPT_VWR_3,
        ADAPT_VRD_1, ADAPT_VRD_2, ADAPT_VRD_3
    } adapt_state_t;
    
    adapt_state_t adapt_state;
    logic [127:0] req_wdata_hold;
    logic [31:0]  req_addr_hold;
    logic         req_is_vec_hold;
    
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            adapt_state <= ADAPT_IDLE;
            req_wdata_hold <= '0;
            req_addr_hold  <= '0;
            req_is_vec_hold <= 1'b0;
        end else begin
            case (adapt_state)
                ADAPT_IDLE: begin
                    if (arb_req_valid) begin
                        if (arb_req_is_vector) begin
                            // Only accept if external ready for first beat
                            if (global_req_ready) begin
                                req_addr_hold   <= arb_req_addr;
                                req_wdata_hold  <= arb_req_wdata;
                                req_is_vec_hold <= 1'b1;
                                adapt_state     <= arb_req_is_load ? ADAPT_VRD_1 : ADAPT_VWR_1;
                            end
                        end
                        // Scalar requests pass through immediately, no state
                    end
                end
                ADAPT_VWR_1: if (global_req_ready) adapt_state <= ADAPT_VWR_2;
                ADAPT_VWR_2: if (global_req_ready) adapt_state <= ADAPT_VWR_3;
                ADAPT_VWR_3: if (global_req_ready) adapt_state <= ADAPT_IDLE;
                ADAPT_VRD_1: if (global_req_ready) adapt_state <= ADAPT_VRD_2;
                ADAPT_VRD_2: if (global_req_ready) adapt_state <= ADAPT_VRD_3;
                ADAPT_VRD_3: if (global_req_ready) adapt_state <= ADAPT_IDLE;
            endcase
        end
    end

    // External Request Drive
    always_comb begin
        global_req_valid = 1'b0;
        global_req_is_load = arb_req_is_load;
        global_req_addr  = arb_req_addr;
        global_req_wdata = arb_req_wdata[31:0];
        global_req_rd    = arb_req_rd;
        arb_req_ready    = 1'b0;

        case (adapt_state)
            ADAPT_IDLE: begin
                if (arb_req_valid) begin
                    if (arb_req_is_vector) begin
                        // Pass beat 0
                        global_req_valid = 1'b1;
                        global_req_addr  = lane_addr(arb_req_addr, r_vec_mode, r_vec_stride, r_vec_index, 0);
                        global_req_wdata = arb_req_wdata[31:0];
                        if (global_req_ready) arb_req_ready = 1'b1;
                    end else begin
                        // Scalar pass through
                        global_req_valid = 1'b1;
                        global_req_addr  = arb_req_addr;
                        global_req_wdata = arb_req_wdata[31:0];
                        if (global_req_ready) arb_req_ready = 1'b1;
                    end
                end
            end
            ADAPT_VWR_1: begin
                global_req_valid = 1'b1; global_req_is_load = 1'b0;
                global_req_addr  = lane_addr(req_addr_hold, r_vec_mode, r_vec_stride, r_vec_index, 1);
                global_req_wdata = req_wdata_hold[63:32];
            end
            ADAPT_VWR_2: begin
                global_req_valid = 1'b1; global_req_is_load = 1'b0;
                global_req_addr  = lane_addr(req_addr_hold, r_vec_mode, r_vec_stride, r_vec_index, 2);
                global_req_wdata = req_wdata_hold[95:64];
            end
            ADAPT_VWR_3: begin
                global_req_valid = 1'b1; global_req_is_load = 1'b0;
                global_req_addr  = lane_addr(req_addr_hold, r_vec_mode, r_vec_stride, r_vec_index, 3);
                global_req_wdata = req_wdata_hold[127:96];
            end
            ADAPT_VRD_1: begin
                global_req_valid = 1'b1; global_req_is_load = 1'b1;
                global_req_addr  = lane_addr(req_addr_hold, r_vec_mode, r_vec_stride, r_vec_index, 1);
            end
            ADAPT_VRD_2: begin
                global_req_valid = 1'b1; global_req_is_load = 1'b1;
                global_req_addr  = lane_addr(req_addr_hold, r_vec_mode, r_vec_stride, r_vec_index, 2);
            end
            ADAPT_VRD_3: begin
                global_req_valid = 1'b1; global_req_is_load = 1'b1;
                global_req_addr  = lane_addr(req_addr_hold, r_vec_mode, r_vec_stride, r_vec_index, 3);
            end
        endcase
    end

    // ------------------------------------------------------------------------
    // RESPONSE RECONSTRUCTION & ROUTING
    // ------------------------------------------------------------------------
    logic [1:0]   rx_collect_cnt;
    logic [127:0] rx_collect_buf;
    logic [4:0]   rx_pending_rd;
    logic         rx_fifo_pop;

    // FIFO: Transaction Type (1=Vec, 0=Scalar)
    logic [3:0] expect_fifo_type [0:7];
    // FIFO: Source (1=Tex, 0=LSU)
    logic [3:0] expect_fifo_src  [0:7]; 
    logic [2:0] expect_wr, expect_rd;
    logic       expect_empty;

    assign expect_empty = (expect_wr == expect_rd);

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            expect_wr <= 0;
            expect_rd <= 0;
            rx_collect_cnt <= 0;
            rx_collect_buf <= 0;
            rx_pending_rd  <= 0;
        end else begin
            // Push only load transactions (stores have no response)
            if (arb_req_valid && arb_req_ready && arb_req_is_load) begin
                expect_fifo_type[expect_wr] <= arb_req_is_vector;
                expect_fifo_src[expect_wr]  <= arb_grant_tex;
                expect_wr <= expect_wr + 1;
            end
            
            if (rx_fifo_pop) expect_rd <= expect_rd + 1;

            if (global_resp_valid && !expect_empty) begin
                if (expect_fifo_type[expect_rd]) begin // Vector
                    case (rx_collect_cnt)
                        0: begin rx_collect_buf[31:0]   <= global_resp_data; rx_pending_rd <= global_resp_rd; end
                        1: rx_collect_buf[63:32]  <= global_resp_data;
                        2: rx_collect_buf[95:64]  <= global_resp_data;
                        3: rx_collect_buf[127:96] <= global_resp_data;
                    endcase
                    rx_collect_cnt <= rx_collect_cnt + 1;
                end
                // Scalar: Just passes through, handled combinatorially, no state update needed (pop trigger below)
            end
            
            if (rx_fifo_pop && expect_fifo_type[expect_rd]) rx_collect_cnt <= 0;
        end
    end

    // Response Routing Logic
    wire       current_is_tex = expect_fifo_src[expect_rd];
    
    always_comb begin
        arb_resp_valid = 1'b0;
        arb_resp_data_scalar = 32'b0;
        arb_resp_data_vector = 128'b0;
        arb_resp_rd = 5'b0;
        rx_fifo_pop = 1'b0;
        
        if (global_resp_valid && !expect_empty) begin
            if (expect_fifo_type[expect_rd] == 0) begin // Scalar
                arb_resp_valid = 1'b1;
                arb_resp_data_scalar = global_resp_data;
                arb_resp_rd = global_resp_rd;
                rx_fifo_pop = 1'b1;
            end else begin // Vector
                if (rx_collect_cnt == 2'd3) begin
                    arb_resp_valid = 1'b1;
                    arb_resp_data_vector = {global_resp_data, rx_collect_buf[95:0]};
                    arb_resp_rd = rx_pending_rd;
                    rx_fifo_pop = 1'b1;
                end
            end
        end
    end

    assign tex_resp_valid_internal_wire = arb_resp_valid && current_is_tex;
    
    assign tex_resp_valid = tex_resp_valid_internal_wire;
    assign tex_resp_data  = arb_resp_data_vector;

    // Busy when any request or response is outstanding (scalar/vector/atomic/tex)
    assign busy = scalar_global_wait
               || atomic_pending
               || r_local_read_pending
               || (adapt_state != ADAPT_IDLE)
               || (arb_req_valid && !arb_req_ready)
               || !expect_empty;

endmodule
