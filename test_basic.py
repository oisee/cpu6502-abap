#!/usr/bin/env python3
"""
6502 emulator for MS-BASIC with two I/O interception modes:

1. PORT MODE (--port): Memory-mapped I/O at $FFF0-$FFF3
   - Character-by-character I/O via ports
   - Emulator handles echo
   - Good for raw terminal interaction

2. PC MODE (--pc): Function interception at RDKEY/COUT addresses
   - Halt CPU when PC hits RDKEY_ABAP ($28CE)
   - Request FULL LINE from user (Python handles line editing/echo)
   - Feed line to CPU character-by-character until buffer empty
   - Then halt again - perfect for ABAP PBO/PAI model!
   - No echo in port reads (BASIC or user handles it)

Usage:
  python3 test_basic.py --port      # Port-based I/O (default)
  python3 test_basic.py --pc        # PC-based function interception
  python3 test_basic.py --batch     # Batch test mode
"""

import sys
import select
import tty
import termios

# Memory
memory = bytearray(65536)

# Registers
A = 0    # Accumulator
X = 0    # X index
Y = 0    # Y index
SP = 0xFF  # Stack pointer
PC = 0   # Program counter
P = 0x24  # Status: --1--I-- (IRQ disabled, bit 5 always 1)

# Status flag bits
N_FLAG = 0x80  # Negative
V_FLAG = 0x40  # Overflow
B_FLAG = 0x10  # Break
D_FLAG = 0x08  # Decimal
I_FLAG = 0x04  # IRQ disable
Z_FLAG = 0x02  # Zero
C_FLAG = 0x01  # Carry

# I/O port addresses
IO_CHAROUT = 0xFFF0
IO_CHARIN = 0xFFF1
IO_STATUS = 0xFFF2
IO_PEEK = 0xFFF3

# PC interception addresses (from msbasic label file)
ADDR_COUT = 0x28CA      # COUT_ABAP
ADDR_RDKEY = 0x28CE     # RDKEY_ABAP
ADDR_COLD_START = 0x2730

# Buffers and state
input_buffer = []
output_buffer = []
running = True
cycle_count = 0
halted_for_input = False  # PC mode: waiting for line input

# Mode settings
io_mode = 'port'  # 'port' or 'pc'
batch_mode = False
io_debug = False


def set_nz(value):
    """Set N and Z flags based on value."""
    global P
    P = P & ~(N_FLAG | Z_FLAG)
    if value == 0:
        P |= Z_FLAG
    if value & 0x80:
        P |= N_FLAG
    return value & 0xFF


def push(value):
    global SP
    memory[0x100 + SP] = value & 0xFF
    SP = (SP - 1) & 0xFF


def pull():
    global SP
    SP = (SP + 1) & 0xFF
    return memory[0x100 + SP]


def push_word(value):
    push((value >> 8) & 0xFF)
    push(value & 0xFF)


def pull_word():
    lo = pull()
    hi = pull()
    return (hi << 8) | lo


def read_byte(addr):
    """Read byte from memory with I/O handling."""
    global io_debug
    addr &= 0xFFFF

    if io_mode == 'port':
        # Port-based I/O
        if addr == IO_CHARIN:
            if input_buffer:
                ch = input_buffer.pop(0)
                if io_debug:
                    print(f"[IN:{chr(ch) if 32 <= ch < 127 else f'${ch:02X}'}]", end='', flush=True)
                # Echo in port mode
                write_byte(IO_CHAROUT, ch)
                return ch
            return 0
        elif addr == IO_STATUS:
            return 1 if input_buffer else 0
        elif addr == IO_PEEK:
            return input_buffer[0] if input_buffer else 0
    else:
        # PC mode - ports still exist but no echo on read
        if addr == IO_CHARIN:
            if input_buffer:
                ch = input_buffer.pop(0)
                if io_debug:
                    print(f"[IN:{chr(ch) if 32 <= ch < 127 else f'${ch:02X}'}]", end='', flush=True)
                # NO echo in PC mode - line was already echoed during input
                return ch
            return 0
        elif addr == IO_STATUS:
            return 1 if input_buffer else 0
        elif addr == IO_PEEK:
            return input_buffer[0] if input_buffer else 0

    return memory[addr]


def write_byte(addr, value):
    """Write byte to memory with I/O handling."""
    global running, batch_mode
    addr &= 0xFFFF
    value &= 0xFF

    if addr == IO_CHAROUT:
        if not batch_mode:
            ch = chr(value) if 32 <= value < 127 or value in (10, 13) else f'[{value:02X}]'
            sys.stdout.write(ch)
            sys.stdout.flush()
        output_buffer.append(value)
    elif addr >= 0x0800 and addr < 0xFFF0:
        # ROM area - writes ignored (memory detection stops here)
        pass
    else:
        memory[addr] = value


def read_word(addr):
    return read_byte(addr) | (read_byte(addr + 1) << 8)


def fetch():
    global PC
    value = read_byte(PC)
    PC = (PC + 1) & 0xFFFF
    return value


def fetch_word():
    lo = fetch()
    hi = fetch()
    return (hi << 8) | lo


# Addressing modes
def addr_imm(): return fetch()
def addr_zp(): return fetch()
def addr_zpx(): return (fetch() + X) & 0xFF
def addr_zpy(): return (fetch() + Y) & 0xFF
def addr_abs(): return fetch_word()
def addr_absx(): return (fetch_word() + X) & 0xFFFF
def addr_absy(): return (fetch_word() + Y) & 0xFFFF
def addr_indx():
    zp = (fetch() + X) & 0xFF
    return read_byte(zp) | (read_byte((zp + 1) & 0xFF) << 8)
def addr_indy():
    zp = fetch()
    base = read_byte(zp) | (read_byte((zp + 1) & 0xFF) << 8)
    return (base + Y) & 0xFFFF


def branch(condition):
    global PC
    offset = fetch()
    if offset & 0x80:
        offset -= 256
    if condition:
        PC = (PC + offset) & 0xFFFF


def execute():
    """Execute one instruction. Returns True if should continue, False if halted."""
    global A, X, Y, SP, PC, P, running, cycle_count, halted_for_input

    # PC mode: check for function interception BEFORE fetch
    if io_mode == 'pc':
        if PC == ADDR_RDKEY:
            # RDKEY_ABAP - check if we have input
            if not input_buffer:
                # No input - halt and wait for line
                halted_for_input = True
                return False
            # Have input - let RDKEY execute normally (will read from IO_CHARIN)
        # COUT_ABAP - just let it execute normally (writes to IO_CHAROUT)

    opcode = fetch()
    cycle_count += 1

    # LDA
    if opcode == 0xA9: A = set_nz(fetch())
    elif opcode == 0xA5: A = set_nz(read_byte(addr_zp()))
    elif opcode == 0xB5: A = set_nz(read_byte(addr_zpx()))
    elif opcode == 0xAD: A = set_nz(read_byte(addr_abs()))
    elif opcode == 0xBD: A = set_nz(read_byte(addr_absx()))
    elif opcode == 0xB9: A = set_nz(read_byte(addr_absy()))
    elif opcode == 0xA1: A = set_nz(read_byte(addr_indx()))
    elif opcode == 0xB1: A = set_nz(read_byte(addr_indy()))

    # LDX
    elif opcode == 0xA2: X = set_nz(fetch())
    elif opcode == 0xA6: X = set_nz(read_byte(addr_zp()))
    elif opcode == 0xB6: X = set_nz(read_byte(addr_zpy()))
    elif opcode == 0xAE: X = set_nz(read_byte(addr_abs()))
    elif opcode == 0xBE: X = set_nz(read_byte(addr_absy()))

    # LDY
    elif opcode == 0xA0: Y = set_nz(fetch())
    elif opcode == 0xA4: Y = set_nz(read_byte(addr_zp()))
    elif opcode == 0xB4: Y = set_nz(read_byte(addr_zpx()))
    elif opcode == 0xAC: Y = set_nz(read_byte(addr_abs()))
    elif opcode == 0xBC: Y = set_nz(read_byte(addr_absx()))

    # STA
    elif opcode == 0x85: write_byte(addr_zp(), A)
    elif opcode == 0x95: write_byte(addr_zpx(), A)
    elif opcode == 0x8D: write_byte(addr_abs(), A)
    elif opcode == 0x9D: write_byte(addr_absx(), A)
    elif opcode == 0x99: write_byte(addr_absy(), A)
    elif opcode == 0x81: write_byte(addr_indx(), A)
    elif opcode == 0x91: write_byte(addr_indy(), A)

    # STX
    elif opcode == 0x86: write_byte(addr_zp(), X)
    elif opcode == 0x96: write_byte(addr_zpy(), X)
    elif opcode == 0x8E: write_byte(addr_abs(), X)

    # STY
    elif opcode == 0x84: write_byte(addr_zp(), Y)
    elif opcode == 0x94: write_byte(addr_zpx(), Y)
    elif opcode == 0x8C: write_byte(addr_abs(), Y)

    # Transfers
    elif opcode == 0xAA: X = set_nz(A)
    elif opcode == 0x8A: A = set_nz(X)
    elif opcode == 0xA8: Y = set_nz(A)
    elif opcode == 0x98: A = set_nz(Y)
    elif opcode == 0xBA: X = set_nz(SP)
    elif opcode == 0x9A: SP = X

    # Stack
    elif opcode == 0x48: push(A)
    elif opcode == 0x68: A = set_nz(pull())
    elif opcode == 0x08: push(P | 0x30)
    elif opcode == 0x28: P = (pull() & 0xEF) | 0x20

    # ADC
    elif opcode in (0x69, 0x65, 0x75, 0x6D, 0x7D, 0x79, 0x61, 0x71):
        if opcode == 0x69: val = fetch()
        elif opcode == 0x65: val = read_byte(addr_zp())
        elif opcode == 0x75: val = read_byte(addr_zpx())
        elif opcode == 0x6D: val = read_byte(addr_abs())
        elif opcode == 0x7D: val = read_byte(addr_absx())
        elif opcode == 0x79: val = read_byte(addr_absy())
        elif opcode == 0x61: val = read_byte(addr_indx())
        elif opcode == 0x71: val = read_byte(addr_indy())
        carry = 1 if (P & C_FLAG) else 0
        result = A + val + carry
        P = P & ~(C_FLAG | V_FLAG)
        if result > 255: P |= C_FLAG
        if ((A ^ result) & (val ^ result) & 0x80): P |= V_FLAG
        A = set_nz(result & 0xFF)

    # SBC
    elif opcode in (0xE9, 0xE5, 0xF5, 0xED, 0xFD, 0xF9, 0xE1, 0xF1):
        if opcode == 0xE9: val = fetch()
        elif opcode == 0xE5: val = read_byte(addr_zp())
        elif opcode == 0xF5: val = read_byte(addr_zpx())
        elif opcode == 0xED: val = read_byte(addr_abs())
        elif opcode == 0xFD: val = read_byte(addr_absx())
        elif opcode == 0xF9: val = read_byte(addr_absy())
        elif opcode == 0xE1: val = read_byte(addr_indx())
        elif opcode == 0xF1: val = read_byte(addr_indy())
        carry = 1 if (P & C_FLAG) else 0
        result = A - val - (1 - carry)
        P = P & ~(C_FLAG | V_FLAG)
        if result >= 0: P |= C_FLAG
        if ((A ^ val) & (A ^ result) & 0x80): P |= V_FLAG
        A = set_nz(result & 0xFF)

    # AND
    elif opcode == 0x29: A = set_nz(A & fetch())
    elif opcode == 0x25: A = set_nz(A & read_byte(addr_zp()))
    elif opcode == 0x35: A = set_nz(A & read_byte(addr_zpx()))
    elif opcode == 0x2D: A = set_nz(A & read_byte(addr_abs()))
    elif opcode == 0x3D: A = set_nz(A & read_byte(addr_absx()))
    elif opcode == 0x39: A = set_nz(A & read_byte(addr_absy()))
    elif opcode == 0x21: A = set_nz(A & read_byte(addr_indx()))
    elif opcode == 0x31: A = set_nz(A & read_byte(addr_indy()))

    # ORA
    elif opcode == 0x09: A = set_nz(A | fetch())
    elif opcode == 0x05: A = set_nz(A | read_byte(addr_zp()))
    elif opcode == 0x15: A = set_nz(A | read_byte(addr_zpx()))
    elif opcode == 0x0D: A = set_nz(A | read_byte(addr_abs()))
    elif opcode == 0x1D: A = set_nz(A | read_byte(addr_absx()))
    elif opcode == 0x19: A = set_nz(A | read_byte(addr_absy()))
    elif opcode == 0x01: A = set_nz(A | read_byte(addr_indx()))
    elif opcode == 0x11: A = set_nz(A | read_byte(addr_indy()))

    # EOR
    elif opcode == 0x49: A = set_nz(A ^ fetch())
    elif opcode == 0x45: A = set_nz(A ^ read_byte(addr_zp()))
    elif opcode == 0x55: A = set_nz(A ^ read_byte(addr_zpx()))
    elif opcode == 0x4D: A = set_nz(A ^ read_byte(addr_abs()))
    elif opcode == 0x5D: A = set_nz(A ^ read_byte(addr_absx()))
    elif opcode == 0x59: A = set_nz(A ^ read_byte(addr_absy()))
    elif opcode == 0x41: A = set_nz(A ^ read_byte(addr_indx()))
    elif opcode == 0x51: A = set_nz(A ^ read_byte(addr_indy()))

    # CMP
    elif opcode in (0xC9, 0xC5, 0xD5, 0xCD, 0xDD, 0xD9, 0xC1, 0xD1):
        if opcode == 0xC9: val = fetch()
        elif opcode == 0xC5: val = read_byte(addr_zp())
        elif opcode == 0xD5: val = read_byte(addr_zpx())
        elif opcode == 0xCD: val = read_byte(addr_abs())
        elif opcode == 0xDD: val = read_byte(addr_absx())
        elif opcode == 0xD9: val = read_byte(addr_absy())
        elif opcode == 0xC1: val = read_byte(addr_indx())
        elif opcode == 0xD1: val = read_byte(addr_indy())
        P = P & ~C_FLAG
        if A >= val: P |= C_FLAG
        set_nz((A - val) & 0xFF)

    # CPX
    elif opcode == 0xE0:
        val = fetch(); P = P & ~C_FLAG
        if X >= val: P |= C_FLAG
        set_nz((X - val) & 0xFF)
    elif opcode == 0xE4:
        val = read_byte(addr_zp()); P = P & ~C_FLAG
        if X >= val: P |= C_FLAG
        set_nz((X - val) & 0xFF)
    elif opcode == 0xEC:
        val = read_byte(addr_abs()); P = P & ~C_FLAG
        if X >= val: P |= C_FLAG
        set_nz((X - val) & 0xFF)

    # CPY
    elif opcode == 0xC0:
        val = fetch(); P = P & ~C_FLAG
        if Y >= val: P |= C_FLAG
        set_nz((Y - val) & 0xFF)
    elif opcode == 0xC4:
        val = read_byte(addr_zp()); P = P & ~C_FLAG
        if Y >= val: P |= C_FLAG
        set_nz((Y - val) & 0xFF)
    elif opcode == 0xCC:
        val = read_byte(addr_abs()); P = P & ~C_FLAG
        if Y >= val: P |= C_FLAG
        set_nz((Y - val) & 0xFF)

    # INC
    elif opcode == 0xE6: addr = addr_zp(); write_byte(addr, set_nz((read_byte(addr) + 1) & 0xFF))
    elif opcode == 0xF6: addr = addr_zpx(); write_byte(addr, set_nz((read_byte(addr) + 1) & 0xFF))
    elif opcode == 0xEE: addr = addr_abs(); write_byte(addr, set_nz((read_byte(addr) + 1) & 0xFF))
    elif opcode == 0xFE: addr = addr_absx(); write_byte(addr, set_nz((read_byte(addr) + 1) & 0xFF))

    # DEC
    elif opcode == 0xC6: addr = addr_zp(); write_byte(addr, set_nz((read_byte(addr) - 1) & 0xFF))
    elif opcode == 0xD6: addr = addr_zpx(); write_byte(addr, set_nz((read_byte(addr) - 1) & 0xFF))
    elif opcode == 0xCE: addr = addr_abs(); write_byte(addr, set_nz((read_byte(addr) - 1) & 0xFF))
    elif opcode == 0xDE: addr = addr_absx(); write_byte(addr, set_nz((read_byte(addr) - 1) & 0xFF))

    elif opcode == 0xE8: X = set_nz((X + 1) & 0xFF)
    elif opcode == 0xCA: X = set_nz((X - 1) & 0xFF)
    elif opcode == 0xC8: Y = set_nz((Y + 1) & 0xFF)
    elif opcode == 0x88: Y = set_nz((Y - 1) & 0xFF)

    # ASL
    elif opcode == 0x0A:
        P = P & ~C_FLAG
        if A & 0x80: P |= C_FLAG
        A = set_nz((A << 1) & 0xFF)
    elif opcode == 0x06: addr = addr_zp(); val = read_byte(addr); P = P & ~C_FLAG; P |= C_FLAG if val & 0x80 else 0; write_byte(addr, set_nz((val << 1) & 0xFF))
    elif opcode == 0x16: addr = addr_zpx(); val = read_byte(addr); P = P & ~C_FLAG; P |= C_FLAG if val & 0x80 else 0; write_byte(addr, set_nz((val << 1) & 0xFF))
    elif opcode == 0x0E: addr = addr_abs(); val = read_byte(addr); P = P & ~C_FLAG; P |= C_FLAG if val & 0x80 else 0; write_byte(addr, set_nz((val << 1) & 0xFF))
    elif opcode == 0x1E: addr = addr_absx(); val = read_byte(addr); P = P & ~C_FLAG; P |= C_FLAG if val & 0x80 else 0; write_byte(addr, set_nz((val << 1) & 0xFF))

    # LSR
    elif opcode == 0x4A:
        P = P & ~C_FLAG
        if A & 0x01: P |= C_FLAG
        A = set_nz(A >> 1)
    elif opcode == 0x46: addr = addr_zp(); val = read_byte(addr); P = P & ~C_FLAG; P |= C_FLAG if val & 0x01 else 0; write_byte(addr, set_nz(val >> 1))
    elif opcode == 0x56: addr = addr_zpx(); val = read_byte(addr); P = P & ~C_FLAG; P |= C_FLAG if val & 0x01 else 0; write_byte(addr, set_nz(val >> 1))
    elif opcode == 0x4E: addr = addr_abs(); val = read_byte(addr); P = P & ~C_FLAG; P |= C_FLAG if val & 0x01 else 0; write_byte(addr, set_nz(val >> 1))
    elif opcode == 0x5E: addr = addr_absx(); val = read_byte(addr); P = P & ~C_FLAG; P |= C_FLAG if val & 0x01 else 0; write_byte(addr, set_nz(val >> 1))

    # ROL
    elif opcode == 0x2A:
        carry = 1 if (P & C_FLAG) else 0; P = P & ~C_FLAG
        if A & 0x80: P |= C_FLAG
        A = set_nz(((A << 1) | carry) & 0xFF)
    elif opcode == 0x26: addr = addr_zp(); val = read_byte(addr); carry = 1 if (P & C_FLAG) else 0; P = P & ~C_FLAG; P |= C_FLAG if val & 0x80 else 0; write_byte(addr, set_nz(((val << 1) | carry) & 0xFF))
    elif opcode == 0x36: addr = addr_zpx(); val = read_byte(addr); carry = 1 if (P & C_FLAG) else 0; P = P & ~C_FLAG; P |= C_FLAG if val & 0x80 else 0; write_byte(addr, set_nz(((val << 1) | carry) & 0xFF))
    elif opcode == 0x2E: addr = addr_abs(); val = read_byte(addr); carry = 1 if (P & C_FLAG) else 0; P = P & ~C_FLAG; P |= C_FLAG if val & 0x80 else 0; write_byte(addr, set_nz(((val << 1) | carry) & 0xFF))
    elif opcode == 0x3E: addr = addr_absx(); val = read_byte(addr); carry = 1 if (P & C_FLAG) else 0; P = P & ~C_FLAG; P |= C_FLAG if val & 0x80 else 0; write_byte(addr, set_nz(((val << 1) | carry) & 0xFF))

    # ROR
    elif opcode == 0x6A:
        carry = 0x80 if (P & C_FLAG) else 0; P = P & ~C_FLAG
        if A & 0x01: P |= C_FLAG
        A = set_nz((A >> 1) | carry)
    elif opcode == 0x66: addr = addr_zp(); val = read_byte(addr); carry = 0x80 if (P & C_FLAG) else 0; P = P & ~C_FLAG; P |= C_FLAG if val & 0x01 else 0; write_byte(addr, set_nz((val >> 1) | carry))
    elif opcode == 0x76: addr = addr_zpx(); val = read_byte(addr); carry = 0x80 if (P & C_FLAG) else 0; P = P & ~C_FLAG; P |= C_FLAG if val & 0x01 else 0; write_byte(addr, set_nz((val >> 1) | carry))
    elif opcode == 0x6E: addr = addr_abs(); val = read_byte(addr); carry = 0x80 if (P & C_FLAG) else 0; P = P & ~C_FLAG; P |= C_FLAG if val & 0x01 else 0; write_byte(addr, set_nz((val >> 1) | carry))
    elif opcode == 0x7E: addr = addr_absx(); val = read_byte(addr); carry = 0x80 if (P & C_FLAG) else 0; P = P & ~C_FLAG; P |= C_FLAG if val & 0x01 else 0; write_byte(addr, set_nz((val >> 1) | carry))

    # BIT
    elif opcode == 0x24:
        val = read_byte(addr_zp()); P = P & ~(N_FLAG | V_FLAG | Z_FLAG)
        if val & 0x80: P |= N_FLAG
        if val & 0x40: P |= V_FLAG
        if (A & val) == 0: P |= Z_FLAG
    elif opcode == 0x2C:
        val = read_byte(addr_abs()); P = P & ~(N_FLAG | V_FLAG | Z_FLAG)
        if val & 0x80: P |= N_FLAG
        if val & 0x40: P |= V_FLAG
        if (A & val) == 0: P |= Z_FLAG

    # Branches
    elif opcode == 0x10: branch(not (P & N_FLAG))
    elif opcode == 0x30: branch(P & N_FLAG)
    elif opcode == 0x50: branch(not (P & V_FLAG))
    elif opcode == 0x70: branch(P & V_FLAG)
    elif opcode == 0x90: branch(not (P & C_FLAG))
    elif opcode == 0xB0: branch(P & C_FLAG)
    elif opcode == 0xD0: branch(not (P & Z_FLAG))
    elif opcode == 0xF0: branch(P & Z_FLAG)

    # JMP
    elif opcode == 0x4C: PC = fetch_word()
    elif opcode == 0x6C:
        addr = fetch_word()
        lo = read_byte(addr)
        hi = read_byte((addr & 0xFF00) | ((addr + 1) & 0xFF))
        PC = (hi << 8) | lo

    # JSR/RTS
    elif opcode == 0x20:
        addr = fetch_word()
        push_word(PC - 1)
        PC = addr
    elif opcode == 0x60: PC = (pull_word() + 1) & 0xFFFF

    # RTI
    elif opcode == 0x40:
        P = (pull() & 0xEF) | 0x20
        PC = pull_word()

    # BRK
    elif opcode == 0x00:
        PC = (PC + 1) & 0xFFFF
        push_word(PC)
        push(P | B_FLAG)
        P |= I_FLAG
        PC = read_word(0xFFFE)

    # Flags
    elif opcode == 0x18: P &= ~C_FLAG
    elif opcode == 0x38: P |= C_FLAG
    elif opcode == 0x58: P &= ~I_FLAG
    elif opcode == 0x78: P |= I_FLAG
    elif opcode == 0xB8: P &= ~V_FLAG
    elif opcode == 0xD8: P &= ~D_FLAG
    elif opcode == 0xF8: P |= D_FLAG

    # NOP
    elif opcode == 0xEA: pass

    else:
        print(f"\nUnknown opcode: ${opcode:02X} at ${PC-1:04X}")
        running = False
        return False

    return True


def load_binary(filename, address):
    with open(filename, 'rb') as f:
        data = f.read()
    for i, byte in enumerate(data):
        memory[address + i] = byte
    return len(data)


def check_stdin():
    return select.select([sys.stdin], [], [], 0)[0]


def run_pc_mode():
    """
    PC interception mode - halt on RDKEY, get full line, feed to CPU.
    This maps to ABAP PBO/PAI model:
    - PBO: CPU runs until it needs input (halts at RDKEY)
    - PAI: User enters line, line fed to CPU, CPU runs until next input needed
    """
    global running, input_buffer, halted_for_input

    print("MS-BASIC (PC interception mode)")
    print("Line editing handled by Python - type commands and press Enter")
    print("Type 'quit' to exit\n")

    while running:
        # Run CPU until it halts for input
        halted_for_input = False
        while running and not halted_for_input:
            if not execute():
                if not halted_for_input:
                    break  # Error or other stop

        if not running:
            break

        if halted_for_input:
            # CPU is waiting at RDKEY - get a line from user
            try:
                line = input()  # Python handles line editing and echo
            except EOFError:
                print("\n[EOF]")
                break

            if line.lower() == 'quit':
                print("[Exiting]")
                break

            # Feed the line + CR to the input buffer
            for ch in line:
                input_buffer.append(ord(ch))
            input_buffer.append(13)  # CR

            # Resume CPU - it will now read from buffer
            halted_for_input = False


def run_port_mode():
    """Port-based I/O with raw terminal mode."""
    global running, input_buffer

    old_settings = termios.tcgetattr(sys.stdin)
    try:
        tty.setraw(sys.stdin.fileno())
        print("\r\nMS-BASIC (Port I/O mode)\r")
        print("Press Ctrl-C to exit\r\n")

        while running:
            if check_stdin():
                ch = sys.stdin.read(1)
                if ch == '\x03':  # Ctrl-C
                    print("\r\n^C - Exiting\r")
                    break
                if ch == '\r' or ch == '\n':
                    input_buffer.append(13)
                else:
                    input_buffer.append(ord(ch))

            for _ in range(1000):
                if not running:
                    break
                execute()

    except KeyboardInterrupt:
        print("\r\nInterrupted\r")
    finally:
        termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old_settings)


def run_batch(commands, max_cycles=10000000):
    """Batch mode for testing."""
    global running, input_buffer, cycle_count, batch_mode, halted_for_input

    batch_mode = True

    # Queue all commands
    for cmd in commands:
        for ch in cmd:
            input_buffer.append(ord(ch))
        input_buffer.append(13)

    # Run until done
    last_output_len = 0
    stable_count = 0

    while running and cycle_count < max_cycles:
        if not execute():
            if halted_for_input and not input_buffer:
                # Waiting for input but none left - done
                break

        if cycle_count % 10000 == 0:
            if not input_buffer and len(output_buffer) == last_output_len:
                stable_count += 1
                if stable_count > 10:
                    break
            else:
                stable_count = 0
                last_output_len = len(output_buffer)

    return ''.join(chr(b) if 32 <= b < 127 or b in (10, 13) else '' for b in output_buffer)


def main():
    global PC, io_mode

    # Parse args
    mode = 'port'  # default
    if len(sys.argv) > 1:
        if sys.argv[1] == '--pc':
            mode = 'pc'
        elif sys.argv[1] == '--port':
            mode = 'port'
        elif sys.argv[1] == '--batch':
            mode = 'batch'
        else:
            print(f"Unknown mode: {sys.argv[1]}")
            print("Usage: test_basic.py [--port|--pc|--batch]")
            return 1

    io_mode = mode if mode != 'batch' else 'port'

    # Load BASIC
    binary_path = "bin/msbasic.bin"
    try:
        size = load_binary(binary_path, 0x0800)
        print(f"Loaded {size} bytes at $0800")
    except FileNotFoundError:
        print(f"Error: {binary_path} not found")
        return 1

    PC = ADDR_COLD_START
    print(f"Starting at ${PC:04X}")

    if mode == 'batch':
        commands = [
            '',  # Memory size
            '',  # Terminal width
            'PRINT "HELLO WORLD"',
            'PRINT 2+2',
            '10 PRINT "TEST"',
            '20 PRINT 3*4',
            'RUN',
        ]
        print(f"Running batch: {commands}\n")
        output = run_batch(commands)
        print("--- Output ---")
        print(output)
        print("--- End ---")

        if "HELLO WORLD" in output and "4" in output and "TEST" in output and "12" in output:
            print("\nSUCCESS: MS-BASIC is working!")
            return 0
        else:
            print("\nFAILURE: Expected output not found")
            return 1

    elif mode == 'pc':
        run_pc_mode()

    else:  # port mode
        run_port_mode()

    return 0


if __name__ == '__main__':
    sys.exit(main())
