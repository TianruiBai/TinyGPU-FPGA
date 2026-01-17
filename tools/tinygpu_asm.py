import sys
import re
import struct
import argparse

# =================================================================================
# 1. ISA Definitions & Constants
# =================================================================================

# --- 7-bit Opcodes ---
OPCODES = {
    'OP_INT':       0b0001011, # Scalar Integer ALU (R-Type)
    'OP_FP':        0b0001011, # Scalar FP uses same Major Op as INT (distinguished by funct7)
    'OP_INT_IMM':   0b0010111, # Scalar Integer Imm (I-Type)
    'LUI':          0b0110111, # Upper Imm (U-Type)
    'LOAD':         0b0001100, # Scalar Load
    'STORE':        0b0001101, # Scalar Store
    'BRANCH':       0b0001110, # Branch / Jump
    'OP_JAL':       0b1101111, # Dedicated JAL opcode
    'SYSTEM':       0b0001111, # CSR / WFI
    'OP_VEC':       0b0101111, # Vector ALU
    'VLOAD':        0b0010001, # Vector Load
    'VSTORE':       0b0010010, # Vector Store
    'TEXTURE':      0b0010011, # Texture
    'GRAPHICS':     0b0010100, # Raster/Geom/Atomic (Scalar)
    'VTRANS':       0b0010000, # Vector Transfer (Mov/Bcast)
    'VATOM':        0b0010101, # Vector Atomic
}

# --- Vector Element Type Encodings (funct3) ---
VEC_TYPES = {
    'I32': 0b000, 'I16': 0b001, 'I8':  0b010,
    'F32': 0b011, 'F16': 0b100, 'F8':  0b101
}

# --- Instruction Table ---
# Key: Mnemonic
# Value: (OpcodeKey, Format, funct3, funct7_or_funct6)
# Formats: R, I, S, B, U, J, V (Vector), G (Graphics)
INSTRUCTIONS = {
    # === 3. Scalar Integer ===
    'ADD':    ('OP_INT', 'R', 0b000, 0b0000000),
    'SUB':    ('OP_INT', 'R', 0b000, 0b0100000),
    'MUL':    ('OP_INT', 'R', 0b000, 0b0000001),
    'AND':    ('OP_INT', 'R', 0b111, 0b0000000),
    'OR':     ('OP_INT', 'R', 0b110, 0b0000000),
    'XOR':    ('OP_INT', 'R', 0b100, 0b0000000),
    'SHL':    ('OP_INT', 'R', 0b001, 0b0000000),
    'LSR':    ('OP_INT', 'R', 0b101, 0b0000000),
    'ASR':    ('OP_INT', 'R', 0b101, 0b0100000),
    'MIN':    ('OP_INT', 'R', 0b010, 0b0000000), # funct7 inferred 0
    'MAX':    ('OP_INT', 'R', 0b011, 0b0000000),
    'ABS':    ('OP_INT', 'R', 0b001, 0b0000000), # Requires rs2=0
    
    'ADDI':   ('OP_INT_IMM', 'I', 0b000, 0),
    'ANDI':   ('OP_INT_IMM', 'I', 0b111, 0),
    'ORI':    ('OP_INT_IMM', 'I', 0b110, 0),
    'XORI':   ('OP_INT_IMM', 'I', 0b100, 0),

    'LUI':    ('LUI', 'U', 0, 0),

    'CMP':    ('OP_INT', 'R', 0b000, 0b0000010), # Signed
    'CMPU':   ('OP_INT', 'R', 0b001, 0b0000010), # Unsigned
    'CMPEQ':  ('OP_INT', 'R', 0b010, 0b0000010), # Equality
    'CLZ':    ('OP_INT', 'R', 0b000, 0b0000011),
    'CTZ':    ('OP_INT', 'R', 0b001, 0b0000011),

    'BEQ':    ('BRANCH', 'B', 0b000, 0),
    'BNE':    ('BRANCH', 'B', 0b001, 0),
    'BLT':    ('BRANCH', 'B', 0b100, 0),
    'BGE':    ('BRANCH', 'B', 0b101, 0),
    'BLTU':   ('BRANCH', 'B', 0b110, 0),
    'BGEU':   ('BRANCH', 'B', 0b111, 0),
    'JAL':    ('OP_JAL', 'J', 0, 0),
    'JALR':   ('BRANCH', 'I', 0b010, 0),

    'MEMBAR': ('SYSTEM', 'I', 0b000, 0),
    'CSRRW':  ('SYSTEM', 'I', 0b001, 0),
    'CSRRS':  ('SYSTEM', 'I', 0b010, 0),
    'SETMISC':('SYSTEM', 'I', 0b011, 0),
    'WFI':    ('SYSTEM', 'I', 0b111, 0),

    # === 4. Scalar Floating-Point (FP16) ===
    # All use OP_INT (0001011) but funct7=0001000
    'FADD':   ('OP_FP', 'R', 0b000, 0b0001000),
    'FSUB':   ('OP_FP', 'R', 0b001, 0b0001000),
    'FMUL':   ('OP_FP', 'R', 0b010, 0b0001000),
    'FMA':    ('OP_FP', 'R', 0b011, 0b0001000),
    'FMIN':   ('OP_FP', 'R', 0b100, 0b0001000),
    'FMAX':   ('OP_FP', 'R', 0b101, 0b0001000),
    'FCVT.I2F': ('OP_FP', 'R', 0b110, 0b0001000),
    'FCVT.F2I': ('OP_FP', 'R', 0b111, 0b0001000),
    'FRECIP': ('OP_FP', 'R', 0b000, 0b0001001), # Special funct7 block
    'FRSQRT': ('OP_FP', 'R', 0b001, 0b0001001),

    # === 5. Memory ===
    'LW':     ('LOAD',  'I', 0b010, 0),
    'LH':     ('LOAD',  'I', 0b001, 0),
    'LB':     ('LOAD',  'I', 0b000, 0),
    'SW':     ('STORE', 'S', 0b010, 0),
    'SH':     ('STORE', 'S', 0b001, 0),
    'SB':     ('STORE', 'S', 0b000, 0),
    
    # Scalar Atomics (Opcode 0010100)
    'AADD':   ('GRAPHICS', 'R_ATOM', 0b000, 0), # Atomic ADD
    'AMIN':   ('GRAPHICS', 'R_ATOM', 0b001, 0),
    'AMAX':   ('GRAPHICS', 'R_ATOM', 0b010, 0),
    'AXCHG':  ('GRAPHICS', 'R_ATOM', 0b011, 0),
    'ACAS':   ('GRAPHICS', 'R_ATOM', 0b100, 0),
    'AAND':   ('GRAPHICS', 'R_ATOM', 0b101, 0),
    'AOR':    ('GRAPHICS', 'R_ATOM', 0b110, 0),
    'AXOR':   ('GRAPHICS', 'R_ATOM', 0b111, 0),

    # === 6. Vector ALU ===
    # Mapped to OP_VEC. Suffix (.F32 etc) determines funct3 (Type).
    'VADD':   ('OP_VEC', 'V', 0, 0b000000),
    'VSUB':   ('OP_VEC', 'V', 0, 0b000001),
    'VMIN':   ('OP_VEC', 'V', 0, 0b000010),
    
    # Special Overload: 000011 is VCMP (Int) or VMAX (FP)
    'VCMP':   ('OP_VEC', 'V', 0, 0b000011), 
    'VMAX':   ('OP_VEC', 'V', 0, 0b000011),

    'VDOT':   ('OP_VEC', 'V', 0, 0b000100),
    'VCROSS': ('OP_VEC', 'V', 0, 0b000101),
    'VSEL':   ('OP_VEC', 'V', 0, 0b000110),
    'VSWIZ':  ('OP_VEC', 'V', 0, 0b000111),
    'VPERM':  ('OP_VEC', 'V', 0, 0b000111), # Alias
    'VPACK':  ('OP_VEC', 'V', 0, 0b001000),
    'VXOR':   ('OP_VEC', 'V', 0, 0b001001),
    'VAND':   ('OP_VEC', 'V', 0, 0b001010),
    'VOR':    ('OP_VEC', 'V', 0, 0b001011),
    'VMUL':   ('OP_VEC', 'V', 0, 0b001100),
    'VRCP':   ('OP_VEC', 'V', 0, 0b001101),
    'VRSQRT': ('OP_VEC', 'V', 0, 0b001110),
    'VUNPACK':('OP_VEC', 'V', 0, 0b001111),

    # === 7. Vector Transfer/Misc ===
    'VBCAST': ('VTRANS', 'V', 0b000, 0),
    'VINS':   ('VTRANS', 'V', 0b001, 0),
    'VEXTR':  ('VTRANS', 'V', 0b010, 0),

    # === Vector Memory ===
    # Vector memory ops (aliases without suffix for default .V)
    'VLD':    ('VLOAD',  'V_MEM', 0b000, 0),
    'VST':    ('VSTORE', 'V_MEM', 0b000, 0),
    'VLD.V':  ('VLOAD',  'V_MEM', 0b000, 0),
    'VLD.S':  ('VLOAD',  'V_MEM', 0b001, 0),
    'VLDX':   ('VLOAD',  'V_MEM', 0b010, 0),
    'VLD.C':  ('VLOAD',  'V_MEM', 0b011, 0),
    'VST.V':  ('VSTORE', 'V_MEM', 0b000, 0),
    'VST.S':  ('VSTORE', 'V_MEM', 0b001, 0),
    'VSTX':   ('VSTORE', 'V_MEM', 0b010, 0),

    # === Vector Atomics ===
    'VATOM.ADD': ('VATOM', 'V_ATOM', 0b000, 0), # Uses funct3 for Op
    # (Other VATOMs would be added similarly if needed)

    # === 8. Texture ===
    'TEX2D':  ('TEXTURE','V', 0b000, 0), # funct3=0 (Nearest)

    # === 9. Graphics ===
    'RSTATE': ('GRAPHICS', 'I', 0b000, 0),
    'RSETUP': ('GRAPHICS', 'I', 0b001, 0),
    'RDRAW':  ('GRAPHICS', 'I', 0b010, 0),
    'RRECT':  ('GRAPHICS', 'I', 0b011, 0),
    'GSTATE': ('GRAPHICS', 'I', 0b100, 0),
    'GPARAM': ('GRAPHICS', 'I', 0b101, 0),
    'GDRAW':  ('GRAPHICS', 'I', 0b110, 0),
}

# =================================================================================
# 2. Helpers
# =================================================================================

def parse_int(s, labels={}, pc=0):
    s = s.strip()
    scale = 1
    if s.startswith('-'):
        scale = -1
        s = s[1:]
    val = 0
    try:
        if s in labels: return labels[s] * scale
        if s.lower().startswith('0x'): val = int(s, 16)
        elif s.lower().startswith('0b'): val = int(s, 2)
        else: val = int(s)
        return val * scale
    except:
        return 0

def reg_idx(r_str):
    r_str = r_str.strip().lower().replace(',', '')
    if r_str == 'pc': return 0
    if not r_str: return 0
    # Handles s0-31, f0-31, v0-31, c0-31
    if r_str[0] in ['s', 'f', 'v', 'c']:
        return int(r_str[1:])
    return 0

def split_imm32(val):
    lo = val & 0xFFF
    hi = (val >> 12) & 0xFFFFF
    if lo & 0x800: hi += 1
    return hi, lo

def pack(args):
    res = 0
    for val, bits, shift in args:
        mask = (1 << bits) - 1
        res |= (int(val) & mask) << shift
    return res

# --- Encoders ---
def enc_r(op, f3, f7, rd, rs1, rs2):
    return pack([(op,7,0),(rd,5,7),(f3,3,12),(rs1,5,15),(rs2,5,20),(f7,7,25)])

def enc_i(op, f3, rd, rs1, imm):
    return pack([(op,7,0),(rd,5,7),(f3,3,12),(rs1,5,15),(imm,12,20)])

def enc_s(op, f3, rs1, rs2, imm):
    return pack([(op,7,0),(imm&0x1F,5,7),(f3,3,12),(rs1,5,15),(rs2,5,20),((imm>>5)&0x7F,7,25)])

def enc_b(op, f3, rs1, rs2, imm):
    imm >>= 1
    return pack([(op,7,0),((imm>>10)&1,1,7),(imm&0xF,4,8),(f3,3,12),(rs1,5,15),(rs2,5,20),((imm>>4)&0x3F,6,25),((imm>>11)&1,1,31)])

def enc_j(op, rd, imm):
    imm >>= 1
    return pack([(op,7,0),(rd,5,7),((imm>>11)&0xFF,8,12),((imm>>10)&1,1,20),(imm&0x3FF,10,21),((imm>>19)&1,1,31)])

def enc_u(op, rd, imm):
    return pack([(op,7,0),(rd,5,7),(imm,20,12)])

def enc_v(op, f3, f6, vm, rd, rs1, rs2):
    # Vector Format: funct6 | vm | rs2 | rs1 | funct3 | rd | opcode
    return pack([(op,7,0),(rd,5,7),(f3,3,12),(rs1,5,15),(rs2,5,20),(vm,1,25),(f6,6,26)])

# =================================================================================
# 3. Assembler Class
# =================================================================================

class Assembler:
    def __init__(self, text_base=0, data_base=0x80000000):
        self.labels = {}
        self.text_base = text_base
        self.data_base = data_base
        self.pc = 0
        self.data_ptr = 0
        self.section = 'text'
        self.inst_words = []
        self.data_bytes = bytearray()
        self.source_lines = []

    def set_section(self, name):
        name = name.lower()
        if name in ['.text', '.code']: self.section = 'text'
        elif name in ['.data', '.rodata', '.bss']: self.section = 'data'

    def align_data(self, b):
        pad = (b - (self.data_ptr % b)) % b
        self.data_ptr += pad
        if self.section == 'data': self.data_bytes.extend(b'\x00' * pad)

    # --- Pass 1: Symbols ---
    def pass_one(self, text):
        self.pc = self.text_base
        self.data_ptr = self.data_base
        self.section = 'text'
        
        for line in text.splitlines():
            line = line.split('#')[0].split(';')[0].strip()
            if not line: continue
            
            if ':' in line:
                lbl, rest = line.split(':', 1)
                self.labels[lbl.strip()] = self.pc if self.section=='text' else self.data_ptr
                line = rest.strip()
            
            if not line: continue
            self.source_lines.append(line)
            
            toks = re.split(r'[,\s]+', line)
            cmd = toks[0].lower()
            
            # Directives
            if cmd.startswith('.'):
                if cmd in ['.text','.data','.rodata','.bss']: self.set_section(cmd)
                elif cmd == '.org':
                    addr = int(toks[1], 0)
                    if self.section=='text': self.pc = addr
                    else: self.data_ptr = addr
                elif cmd == '.byte': self.data_ptr += len(toks)-1
                elif cmd in ['.word','.long']: 
                    self.align_data(4)
                    self.data_ptr += 4*(len(toks)-1)
                elif cmd == '.string': self.data_ptr += len(line[line.find('"')+1:line.rfind('"')]) + 1
                elif cmd == '.zero': self.data_ptr += int(toks[1],0)
                continue
            
            # Instructions
            if self.section == 'text':
                if cmd.upper() in ['LA', 'LI']: self.pc += 8
                else: self.pc += 4

    # --- Pass 2: Codegen ---
    def pass_two(self):
        self.pc = self.text_base
        self.data_ptr = self.data_base
        self.section = 'text'
        self.inst_words = []
        self.data_bytes = bytearray()
        
        for line in self.source_lines:
            toks = [x for x in re.split(r'[,\s]+', line) if x]
            cmd = toks[0].lower()
            
            if cmd.startswith('.'):
                # ... (Handle directives same as pass 1 but emit data) ...
                if cmd in ['.text','.data']: self.set_section(cmd)
                elif cmd == '.byte':
                    for v in toks[1:]: self.data_bytes.append(parse_int(v,self.labels)&0xFF)
                elif cmd == '.word':
                    self.align_data(4)
                    for v in toks[1:]: 
                        val = parse_int(v,self.labels)
                        self.data_bytes.extend(struct.pack('<I', val&0xFFFFFFFF))
                continue
                
            if self.section == 'text':
                self.assemble_inst(toks, line)

    def assemble_inst(self, toks, line):
        mnem_full = toks[0].upper()
        
        # 1. Pseudo-Ops
        if mnem_full == 'LA' or mnem_full == 'LI':
            rd = reg_idx(toks[1])
            imm = parse_int(toks[2], self.labels)
            hi, lo = split_imm32(imm)
            self.inst_words.append(enc_u(OPCODES['LUI'], rd, hi))
            self.inst_words.append(enc_i(OPCODES['OP_INT_IMM'], 0, rd, rd, lo))
            self.pc += 8
            return

        if mnem_full == 'MOV': # Alias for ADDI rd, rs1, 0
            rd = reg_idx(toks[1])
            rs1 = reg_idx(toks[2])
            self.inst_words.append(enc_i(OPCODES['OP_INT_IMM'], 0, rd, rs1, 0))
            self.pc += 4
            return

        # 2. Parse Mnemonic parts (BASE.SUFFIX)
        base = mnem_full
        suffix = None
        vm_mask = 0
        
        # Handle FCVT exceptions (dots in base name)
        if base.startswith('FCVT'):
            # FCVT.I2F or FCVT.F2I are the keys in table
            pass 
        elif '.' in base:
            parts = base.split('.')
            base = parts[0]
            for p in parts[1:]:
                if p == 'M': vm_mask = 1
                elif p in VEC_TYPES: suffix = p

        if base not in INSTRUCTIONS:
            print(f"Error: Unknown op {base} in line: {line}")
            sys.exit(1)

        op_key, fmt, def_f3, def_f7 = INSTRUCTIONS[base]
        opcode = OPCODES[op_key]
        
        # 3. Determine Final funct3 / funct7
        funct3 = def_f3
        funct7 = def_f7 # In vectors this is funct6
        
        # Vector Suffix Logic
        if fmt == 'V':
            if suffix:
                funct3 = VEC_TYPES[suffix]
            
            # Special Overload Check (VCMP vs VMAX)
            # If opcode is VCMP (000011) but type is float -> It's VMAX
            if def_f7 == 0b000011:
                is_float = (funct3 >= 0b011) # F32, F16, F8
                if base == 'VCMP' and is_float:
                    print(f"Warning: VCMP used with Float type at {hex(self.pc)}. Hardware treats this as VMAX.")
                if base == 'VMAX' and not is_float:
                    print(f"Warning: VMAX used with Int type at {hex(self.pc)}. Hardware treats this as VCMP.")

        args = toks[1:]
        
        # 4. Encoding
        iw = 0
        try:
            if fmt == 'R':
                iw = enc_r(opcode, funct3, funct7, reg_idx(args[0]), reg_idx(args[1]), reg_idx(args[2]))
            elif fmt == 'I':
                if base == 'JALR': # JALR rd, rs1, imm
                    iw = enc_i(opcode, funct3, reg_idx(args[0]), reg_idx(args[1]), parse_int(args[2], self.labels))
                else: # ADDI rd, rs1, imm
                    iw = enc_i(opcode, funct3, reg_idx(args[0]), reg_idx(args[1]), parse_int(args[2], self.labels))
            elif fmt == 'S':
                # SW rs2, imm(rs1)
                rs2 = reg_idx(args[0])
                if '(' in args[1]:
                    off, b = args[1].split('(')
                    rs1 = reg_idx(b.strip(')'))
                    imm = parse_int(off, self.labels)
                else:
                    rs1 = reg_idx(args[1])
                    imm = parse_int(args[2], self.labels)
                iw = enc_s(opcode, funct3, rs1, rs2, imm)
            elif fmt == 'B': # BEQ rs1, rs2, label
                off = parse_int(args[2], self.labels) - self.pc
                iw = enc_b(opcode, funct3, reg_idx(args[0]), reg_idx(args[1]), off)
            elif fmt == 'J': # JAL rd, label
                off = parse_int(args[1], self.labels) - self.pc
                iw = enc_j(opcode, reg_idx(args[0]), off)
            elif fmt == 'U': # LUI rd, imm
                iw = enc_u(opcode, reg_idx(args[0]), parse_int(args[1], self.labels))
            elif fmt == 'V': # VADD.F32 rd, rs1, rs2
                rs2_idx = reg_idx(args[2]) if len(args)>2 else 0
                iw = enc_v(opcode, funct3, funct7, vm_mask, reg_idx(args[0]), reg_idx(args[1]), rs2_idx)
            elif fmt == 'V_MEM': # VLD.V rd, off(rs1)
                rd = reg_idx(args[0])
                if '(' in args[1]:
                    off, b = args[1].split('(')
                    rs1 = reg_idx(b.strip(')'))
                    imm = parse_int(off, self.labels)
                else:
                    rs1 = reg_idx(args[1])
                    imm = parse_int(args[2], self.labels)
                # VLOAD uses I-Type-ish shape, VSTORE uses S-Type shape
                if 'VLD' in base:
                    iw = enc_i(opcode, funct3, rd, rs1, imm)
                else:
                    iw = enc_s(opcode, funct3, rs1, rd, imm)
            elif fmt == 'R_ATOM': # Scalar Atomic
                iw = enc_r(opcode, funct3, 0, reg_idx(args[0]), reg_idx(args[1]), reg_idx(args[2]))

            self.inst_words.append(iw)
            self.pc += 4
        except Exception as e:
            print(f"Assembly Error at {hex(self.pc)}: {e}")
            sys.exit(1)

    def save(self, name):
        with open(f"{name}_inst.hex", 'w') as f:
            for w in self.inst_words: f.write(f"{w:08x}\n")
        
        # Pad data to 4 bytes
        while len(self.data_bytes) % 4 != 0: self.data_bytes.append(0)
        
        with open(f"{name}_data.hex", 'w') as f:
            for i in range(0, len(self.data_bytes), 4):
                w = struct.unpack('<I', self.data_bytes[i:i+4])[0]
                f.write(f"{w:08x}\n")
        print(f"Built {name}: Code={len(self.inst_words)*4}B, Data={len(self.data_bytes)}B")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('input')
    parser.add_argument('-o', default='out')
    args = parser.parse_args()
    
    with open(args.input) as f: src = f.read()
    asm = Assembler()
    asm.pass_one(src)
    asm.pass_two()
    asm.save(args.o)