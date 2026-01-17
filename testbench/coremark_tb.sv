`timescale 1ns/1ps

module coremark_tb(
    input logic clk,
    input logic rst_n
);
    import isa_pkg::*;

    // -------------------------
    // Clock/reset
    // -------------------------

    // -------------------------
    // DUT interfaces
    // -------------------------
    logic [63:0] inst_rdata;
    logic [31:0] inst_addr;

    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;

    logic        data_req_ready;
    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    logic        err_fp_overflow;
    logic        err_fp_invalid;
    logic        err_vec_overflow;
    logic        err_vec_invalid;

    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    compute_unit_top #(
        .CORE_ID(32'h2),
        .TILE_OFFSET(32'h0002_0003)
    ) dut (
        .clk(clk),
        .rst_n(rst_n),
        .inst_rdata(inst_rdata),
        .inst_addr(inst_addr),
        .data_req_valid(data_req_valid),
        .data_req_is_load(data_req_is_load),
        .data_req_addr(data_req_addr),
        .data_req_wdata(data_req_wdata),
        .data_req_rd(data_req_rd),
        .err_fp_overflow(err_fp_overflow),
        .err_fp_invalid(err_fp_invalid),
        .err_vec_overflow(err_vec_overflow),
        .err_vec_invalid(err_vec_invalid),
        .csr_status(csr_status),
        .csr_fstatus(csr_fstatus),
        .csr_vstatus(csr_vstatus),
        .data_req_ready(data_req_ready),
        .data_resp_valid(data_resp_valid),
        .data_resp_rd(data_resp_rd),
        .data_resp_data(data_resp_data)
    );

    // Always-ready memory model
    initial begin
        data_req_ready = 1'b1;
    end



    // -------------------------
    // ROM (instruction memory)
    // -------------------------
    localparam int ROM_WORDS = 65536; // 256KB of 32-bit instructions
    localparam int ROM_AW    = $clog2(ROM_WORDS);
    logic [31:0] rom [0:ROM_WORDS-1];

    // 64-bit fetch aligned to 8 bytes: low word at +0, high word at +4
    localparam int ROM_BUNDLES = (ROM_WORDS / 2);
    localparam int BUNDLE_AW   = $clog2(ROM_BUNDLES);
    logic [BUNDLE_AW-1:0] bundle_idx;
    assign bundle_idx = inst_addr[BUNDLE_AW+2:3];
    assign inst_rdata = {rom[{bundle_idx, 1'b1}], rom[{bundle_idx, 1'b0}]};

    // -------------------------
    // Global memory model (32-bit words)
    // -------------------------
    localparam int MEM_WORDS = 262144; // 1MB

    logic [31:0] base_addr;
    logic [31:0] done_off;
    logic [31:0] max_cycles;

    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - base_addr) >> 2;
    endfunction

    // Simple response FIFO (in-order)
    localparam int RESP_DEPTH = 32;
    logic              resp_valid   [0:RESP_DEPTH-1];
    logic [4:0]        resp_rd      [0:RESP_DEPTH-1];
    logic [31:0]       resp_data_q  [0:RESP_DEPTH-1];
    logic [4:0]        resp_wp;
    logic [4:0]        resp_rp;
    logic              resp_empty;

    assign resp_empty = (resp_wp == resp_rp);

    // DONE tracking
    logic        done_seen;
    logic [31:0] done_value;
    int unsigned cycle_count;

    // Debug controls
    bit          debug_en;
    bit          debug_regs;
    bit          debug_mem;
    bit          debug_status;
    bit          debug_x0;
    bit          debug_pipe;
    bit          debug_br;
    bit          debug_watch;
    bit          check_bounds;
    int unsigned debug_every;
    int unsigned debug_from;
    int unsigned debug_until;

    logic [31:0] watch_ex_pc;
    int          watch_rd;

    function automatic bit dbg_in_window();
        if ((debug_from != 0) && (cycle_count < debug_from)) return 1'b0;
        if ((debug_until != 0) && (cycle_count > debug_until)) return 1'b0;
        return 1'b1;
    endfunction

    function automatic logic [31:0] rom_word_at(input logic [31:0] addr);
        int unsigned idx;
        idx = addr >> 2;
        if (idx < ROM_WORDS) rom_word_at = rom[idx];
        else rom_word_at = 32'hDEAD_BEEF;
    endfunction

    task automatic dump_status();
        $display(
            "%0t COREMARK_TB: STATUS cyc=%0d if_pc=%08h rr_v=%0d rr_pc=%08h ex_v=%0d ex_pc=%08h mem_v=%0d wb_v=%0d stall_any=%0d (sb=%0d lsu_busy=%0d lsu_stall=%0d membar=%0d) vq=%0d gfxq=%0d",
            $time,
            cycle_count,
            dut.if_pc,
            dut.rr_valid,
            dut.rr_pc,
            dut.ex_valid,
            dut.ex_pc,
            dut.mem_valid,
            dut.wb_valid,
            dut.stall_any,
            dut.stall_scoreboard,
            dut.lsu_busy,
            dut.lsu_stall,
            dut.stall_membar,
            dut.vq_count,
            dut.gfx_queue_count
        );

        $display(
            "%0t COREMARK_TB: IF inst0_v=%0d inst0=%08h inst1_v=%0d inst1=%08h branch_taken=%0d branch_tgt=%08h",
            $time,
            dut.if_inst0_valid,
            dut.if_inst0,
            dut.if_inst1_valid,
            dut.if_inst1,
            dut.if_pred_taken,
            dut.if_pred_target
        );

        $display(
            "%0t COREMARK_TB: CSR status=%08h fstatus=%08h vstatus=%08h err_fp(of=%0d inv=%0d) err_vec(of=%0d inv=%0d)",
            $time,
            csr_status,
            csr_fstatus,
            csr_vstatus,
            err_fp_overflow,
            err_fp_invalid,
            err_vec_overflow,
            err_vec_invalid
        );
    endtask

    always @(posedge clk) begin
        if (!rst_n) begin
            resp_wp         <= '0;
            resp_rp         <= '0;
            data_resp_valid <= 1'b0;
            data_resp_rd    <= '0;
            data_resp_data  <= 32'h0;
            for (int i = 0; i < RESP_DEPTH; i++) begin
                resp_valid[i]  <= 1'b0;
                resp_rd[i]     <= '0;
                resp_data_q[i] <= 32'h0;
            end
            done_seen   <= 1'b0;
            done_value  <= 32'h0;
            cycle_count <= 0;
        end else begin
            cycle_count <= cycle_count + 1;

            if (debug_en && debug_status) begin
                if (dbg_in_window()) begin
                    if ((debug_every != 0) && ((cycle_count % debug_every) == 0)) begin
                        dump_status();
                    end
                end
            end

            if (data_req_valid && data_req_ready) begin
                int idx;
                idx = mem_index(data_req_addr);

                if (check_bounds && ((idx < 0) || (idx >= MEM_WORDS))) begin
                    $display(
                        "%0t COREMARK_TB: OOB MEM access addr=%08h base=%08h idx=%0d words=%0d is_load=%0d",
                        $time,
                        data_req_addr,
                        base_addr,
                        idx,
                        MEM_WORDS,
                        data_req_is_load
                    );
                    dump_status();
                    $fatal(1);
                end

                if (data_req_is_load) begin
                    resp_valid[resp_wp]  <= 1'b1;
                    resp_rd[resp_wp]     <= data_req_rd;
                    resp_data_q[resp_wp] <= mem[idx];
                    resp_wp <= resp_wp + 1'b1;
                end else begin
                    mem[idx] <= data_req_wdata;
                    if (data_req_addr == (base_addr + done_off)) begin
                        done_seen  <= 1'b1;
                        done_value <= data_req_wdata;
                        $display("%0t COREMARK_TB: DONE write value=%08h", $time, data_req_wdata);
                    end
                end

                if (debug_en && debug_mem && dbg_in_window()) begin
                    $display(
                        "%0t COREMARK_TB: MEM %s addr=%08h wdata=%08h rd=%0d",
                        $time,
                        data_req_is_load ? "LD" : "ST",
                        data_req_addr,
                        data_req_wdata,
                        data_req_rd
                    );
                end
            end

            if (!resp_empty && resp_valid[resp_rp]) begin
                data_resp_valid <= 1'b1;
                data_resp_rd    <= resp_rd[resp_rp];
                data_resp_data  <= resp_data_q[resp_rp];
                resp_valid[resp_rp] <= 1'b0;
                resp_rp <= resp_rp + 1'b1;

                if (debug_en && debug_mem && dbg_in_window()) begin
                    $display(
                        "%0t COREMARK_TB: RESP rd=%0d data=%08h",
                        $time,
                        resp_rd[resp_rp],
                        resp_data_q[resp_rp]
                    );
                end
            end else begin
                data_resp_valid <= 1'b0;
            end

            if (!done_seen && (cycle_count >= max_cycles)) begin
                $display("%0t COREMARK_TB: TIMEOUT max_cycles=%0d", $time, max_cycles);
                if (debug_en) begin
                    dump_status();
                end
                $fatal(1);
            end
        end
    end

    // -------------------------
    // Mini assembler helpers (for default ROM)
    // -------------------------
    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] j_type(input integer imm, input [4:0] rd, input [6:0] opcode);
        // J-type: imm[20|10:1|11|19:12] << 1
        j_type = {imm[20], imm[10:1], imm[11], imm[19:12], rd, opcode};
    endfunction


    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_INT_IMM);
    endfunction

    // -------------------------
    // Init (ROM + RAM) from +args
    // -------------------------
    string rom_file;
    string mem_file;
    initial begin
        base_addr  = 32'hFFFF_F800;
        done_off   = 32'h0000_01F0;
        max_cycles = 5_000_000;

        debug_en     = 1'b0;
        debug_regs   = 1'b0;
        debug_mem    = 1'b0;
        debug_status = 1'b0;
        debug_x0     = 1'b0;
        debug_pipe   = 1'b0;
        debug_br     = 1'b0;
        debug_watch  = 1'b0;
        check_bounds = 1'b0;
        debug_every  = 0;
        debug_from   = 0;
        debug_until  = 0;

        watch_ex_pc  = 32'h0;
        watch_rd     = -1;

        void'($value$plusargs("base=%h", base_addr));
        void'($value$plusargs("done_off=%h", done_off));
        void'($value$plusargs("max_cycles=%d", max_cycles));

        rom_file = "";
        mem_file = "";
        void'($value$plusargs("rom=%s", rom_file));
        void'($value$plusargs("mem_init=%s", mem_file));
        void'($value$plusargs("debug=%d", debug_en));
        void'($value$plusargs("debug_regs=%d", debug_regs));
        void'($value$plusargs("debug_mem=%d", debug_mem));
        void'($value$plusargs("debug_status=%d", debug_status));
        void'($value$plusargs("debug_x0=%d", debug_x0));
        void'($value$plusargs("debug_pipe=%d", debug_pipe));
        void'($value$plusargs("debug_br=%d", debug_br));
        void'($value$plusargs("debug_watch=%d", debug_watch));
        void'($value$plusargs("watch_ex_pc=%h", watch_ex_pc));
        void'($value$plusargs("watch_rd=%d", watch_rd));
        void'($value$plusargs("check_bounds=%d", check_bounds));
        void'($value$plusargs("debug_every=%d", debug_every));
        void'($value$plusargs("debug_from=%d", debug_from));
        void'($value$plusargs("debug_until=%d", debug_until));

        for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
        for (int i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;

        if (rom_file != "") begin
            $display("COREMARK_TB: loading ROM from %s", rom_file);
            $readmemh(rom_file, rom);
        end else begin
            // Default smoke: exercise JAL/JALR and then write DONE=1 (followed by MEMBAR).
            int pc;
            int jal_slot;
            int jal_delay_slot;
            int j_slot;
            int jalr_delay_slot;
            int sub_pc;
            int after_pc;
            pc = 0;

            // x1 = 0xFFFFF800 (BASE)
            rom[pc >> 2] = i_type(-2048, 5'd0, 3'b000, 5'd1, OP_INT_IMM); pc += 4;

            // Reserve: JAL x9, sub
            jal_slot = (pc >> 2);
            rom[jal_slot] = nop();
            pc += 4;

            // Delay slot (executed before the jump takes effect)
            jal_delay_slot = (pc >> 2);
            rom[jal_delay_slot] = nop();
            pc += 4;

            // Reserve: J after_sub (lands at the JAL return address)
            j_slot = (pc >> 2);
            rom[j_slot] = nop();
            pc += 4;

            // sub:
            sub_pc = pc;
            // x2 = 1
            rom[pc >> 2] = i_type(1, 5'd0, 3'b000, 5'd2, OP_INT_IMM); pc += 4;
            // SW x2 -> [x1+done_off]
            rom[pc >> 2] = s_type(done_off, 5'd1, 5'd2, 3'b010, OP_STORE); pc += 4;
            // MEMBAR (flush WMB so DONE store becomes externally visible)
            rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            // Return: JALR x0, 0(x9)
            rom[pc >> 2] = i_type(0, 5'd9, 3'b010, 5'd0, OP_BRANCH); pc += 4;

            // JALR delay slot (executed before return takes effect)
            jalr_delay_slot = (pc >> 2);
            rom[jalr_delay_slot] = nop();
            pc += 4;

            after_pc = pc;
            // Spin (j .)
            rom[pc >> 2] = j_type(0, 5'd0, OP_JAL);

            // Patch the call and the post-return jump
            rom[jal_slot] = j_type(sub_pc - (jal_slot << 2), 5'd9, OP_JAL);
            rom[j_slot]   = j_type(after_pc - (j_slot << 2), 5'd0, OP_JAL);
        end

        if (mem_file != "") begin
            $display("COREMARK_TB: loading MEM from %s", mem_file);
            $readmemh(mem_file, mem);
        end

        $display("COREMARK_TB: base=%08h done_off=%08h max_cycles=%0d", base_addr, done_off, max_cycles);
        $display(
            "COREMARK_TB: debug=%0d regs=%0d mem=%0d status=%0d pipe=%0d br=%0d watch=%0d watch_ex_pc=%08h watch_rd=%0d x0=%0d every=%0d from=%0d until=%0d check_bounds=%0d",
            debug_en,
            debug_regs,
            debug_mem,
            debug_status,
            debug_pipe,
            debug_br,
            debug_watch,
            watch_ex_pc,
            watch_rd,
            debug_x0,
            debug_every,
            debug_from,
            debug_until,
            check_bounds
        );
    end

    // End simulation on DONE
    always @(posedge clk) begin
        if (rst_n && done_seen) begin
            $display("COREMARK_TB: PASS (done=%08h cycles=%0d)", done_value, cycle_count);
            if (debug_en) begin
                dump_status();
            end
            $finish;
        end
    end

    // Minimal bring-up debug (auto-disables after a few cycles)
    always @(posedge clk) begin
        if (debug_en && rst_n && dbg_in_window() && (cycle_count < 40)) begin
            $display(
                "%0t COREMARK_TB: pc=%08h hw=%0d inst_addr=%08h inst_rdata=%016h",
                $time,
                dut.if_pc,
                dut.u_fetch.pc_halfword,
                inst_addr,
                inst_rdata
            );
        end

        if (debug_en && debug_mem && rst_n && dbg_in_window() && dut.local_req_valid) begin
            $display(
                "%0t COREMARK_TB: LOCAL %s addr=%08h bank=%0d vec=%0d wdata0=%08h rdata0=%08h",
                $time,
                dut.local_we ? "ST" : "LD",
                dut.local_addr,
                dut.local_bank_sel,
                dut.local_req_is_vector,
                dut.local_wdata[31:0],
                dut.local_rdata[31:0]
            );
        end

        if (debug_en && debug_regs && rst_n && dut.s_we) begin
            if (dbg_in_window()) begin
                if (debug_x0 || (dut.s_waddr != 5'd0)) begin
                $display(
                    "%0t COREMARK_TB: REGW cyc=%0d if_pc=%08h rr_pc=%08h ex_pc=%08h x%0d=%08h src(pend=%0d lsu=%0d fp=%0d valu=%0d alu=%0d)",
                    $time,
                    cycle_count,
                    dut.if_pc,
                    dut.rr_pc,
                    dut.ex_pc,
                    dut.s_waddr,
                    dut.s_wdata,
                    dut.scalar_wb_from_pending,
                    dut.scalar_wb_from_lsu,
                    dut.scalar_wb_from_fp,
                    dut.scalar_wb_from_valu,
                    dut.scalar_wb_from_alu
                );
            end
            end
        end

        if (debug_en && debug_br && rst_n && dut.if_pred_taken) begin
            if (dbg_in_window()) begin
                $display(
                    "%0t COREMARK_TB: BR cyc=%0d ex_pc=%08h inst=%08h target=%08h rs1=%0d rd=%0d op_a=%08h op_b=%08h imm=%08h funct3=%0d",
                    $time,
                    cycle_count,
                    dut.ex_pc,
                    rom_word_at(dut.ex_pc),
                    dut.if_pred_target,
                    dut.ex_ctrl.rs1,
                    dut.ex_ctrl.rd,
                    dut.ex_op_a,
                    dut.ex_op_b,
                    dut.ex_ctrl.imm,
                    dut.ex_ctrl.funct3
                );
            end
        end

        if (debug_en && debug_pipe && rst_n && dut.ex_valid) begin
            if (dbg_in_window()) begin
                if (dut.ex_ctrl.is_load || dut.ex_ctrl.is_store || dut.ex_ctrl.is_system) begin
                    $display(
                        "%0t COREMARK_TB: EX cyc=%0d pc=%08h inst=%08h load=%0d store=%0d sys=%0d addr=%08h global=%0d rd=%0d rs1=%0d rs2=%0d imm=%08h",
                        $time,
                        cycle_count,
                        dut.ex_pc,
                        rom_word_at(dut.ex_pc),
                        dut.ex_ctrl.is_load,
                        dut.ex_ctrl.is_store,
                        dut.ex_ctrl.is_system,
                        dut.ex_addr,
                        dut.ex_addr[31],
                        dut.ex_ctrl.rd,
                        dut.ex_ctrl.rs1,
                        dut.ex_ctrl.rs2,
                        dut.ex_ctrl.imm
                    );
                end
            end
        end

        if (debug_en && debug_watch && rst_n && dbg_in_window() && dut.ex_valid) begin
            if ((watch_ex_pc != 32'h0) && (dut.ex_pc == watch_ex_pc)) begin
                $display(
                    "%0t COREMARK_TB: WATCH_EX cyc=%0d pc=%08h inst=%08h load=%0d store=%0d br=%0d sys=%0d uses_rd=%0d rd=%0d rs1=%0d rs2=%0d funct3=%0d funct7=%0d imm=%08h op_a=%08h op_b=%08h addr=%08h scalar_res=%08h",
                    $time,
                    cycle_count,
                    dut.ex_pc,
                    rom_word_at(dut.ex_pc),
                    dut.ex_ctrl.is_load,
                    dut.ex_ctrl.is_store,
                    dut.ex_ctrl.is_branch,
                    dut.ex_ctrl.is_system,
                    dut.ex_ctrl.uses_rd,
                    dut.ex_ctrl.rd,
                    dut.ex_ctrl.rs1,
                    dut.ex_ctrl.rs2,
                    dut.ex_ctrl.funct3,
                    dut.ex_ctrl.funct7,
                    dut.ex_ctrl.imm,
                    dut.ex_op_a,
                    dut.ex_op_b,
                    dut.ex_addr,
                    dut.ex_scalar_res
                );
            end

            if ((watch_rd >= 0) && dut.ex_ctrl.uses_rd && (dut.ex_ctrl.rd == watch_rd[4:0])) begin
                $display(
                    "%0t COREMARK_TB: WATCH_RD cyc=%0d pc=%08h inst=%08h rd=%0d rs1=%0d rs2=%0d funct3=%0d funct7=%0d imm=%08h op_a=%08h op_b=%08h addr=%08h scalar_res=%08h",
                    $time,
                    cycle_count,
                    dut.ex_pc,
                    rom_word_at(dut.ex_pc),
                    dut.ex_ctrl.rd,
                    dut.ex_ctrl.rs1,
                    dut.ex_ctrl.rs2,
                    dut.ex_ctrl.funct3,
                    dut.ex_ctrl.funct7,
                    dut.ex_ctrl.imm,
                    dut.ex_op_a,
                    dut.ex_op_b,
                    dut.ex_addr,
                    dut.ex_scalar_res
                );
            end
        end

        if (debug_en && rst_n && dbg_in_window() && (cycle_count == 20 || cycle_count == 40)) begin
            $display(
                "%0t COREMARK_TB: x1=%08h x2=%08h",
                $time,
                dut.u_regfile_scalar.mem[1],
                dut.u_regfile_scalar.mem[2]
            );
        end

        if (debug_en && rst_n && (err_fp_overflow || err_fp_invalid || err_vec_overflow || err_vec_invalid)) begin
            $display(
                "%0t COREMARK_TB: ERR fp(of=%0d inv=%0d) vec(of=%0d inv=%0d)",
                $time,
                err_fp_overflow,
                err_fp_invalid,
                err_vec_overflow,
                err_vec_invalid
            );
            dump_status();
        end
    end

endmodule
