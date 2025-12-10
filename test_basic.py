#!/usr/bin/env python3
"""
6502 emulator for MS-BASIC with configurable hooks for PC and port interception.

Hook Types:
  - PC hooks: Trigger when PC reaches specific address (before instruction fetch)
  - Read hooks: Trigger on memory/port read
  - Write hooks: Trigger on memory/port write

Hook Actions:
  - CONTINUE: Continue execution normally
  - HALT: Stop CPU, return to caller (for PBO/PAI style)
  - SKIP: Skip the operation (for read/write hooks)
  - OVERRIDE: Override with custom value (for read hooks)

Both hook types work together:
  - PC hooks intercept at function level (e.g., halt at RDKEY for line input)
  - Port hooks handle character I/O ($FFF0-$FFF3)
  - If PC hook halts before port read, port hook isn't triggered
  - Custom code reading ports directly still works via port hooks

Usage:
  python3 test_basic.py             # Interactive mode (both hooks enabled)
  python3 test_basic.py --batch     # Batch test mode
"""

import sys
import select
import tty
import termios
from enum import Enum
from typing import Callable, Dict, Optional, Tuple

# Hook action results
class HookAction(Enum):
    CONTINUE = 0    # Continue normal execution
    HALT = 1        # Halt CPU, return to caller
    SKIP = 2        # Skip this read/write operation
    OVERRIDE = 3    # Use override value (read hooks only)

# Hook result tuple: (action, override_value)
HookResult = Tuple[HookAction, Optional[int]]

# Hook function signatures
# PC hook: fn(pc: int) -> HookResult
# Read hook: fn(addr: int) -> HookResult
# Write hook: fn(addr: int, value: int) -> HookResult

class CPU6502:
    """6502 CPU emulator with hook support."""

    def __init__(self):
        # Memory
        self.memory = bytearray(65536)

        # Registers
        self.A = 0      # Accumulator
        self.X = 0      # X index
        self.Y = 0      # Y index
        self.SP = 0xFF  # Stack pointer
        self.PC = 0     # Program counter
        self.P = 0x24   # Status flags

        # Status flag bits
        self.N_FLAG = 0x80
        self.V_FLAG = 0x40
        self.B_FLAG = 0x10
        self.D_FLAG = 0x08
        self.I_FLAG = 0x04
        self.Z_FLAG = 0x02
        self.C_FLAG = 0x01

        # State
        self.running = True
        self.halted = False
        self.cycle_count = 0

        # Hooks
        self.pc_hooks: Dict[int, Callable] = {}      # addr -> hook_fn
        self.read_hooks: Dict[int, Callable] = {}    # addr -> hook_fn
        self.write_hooks: Dict[int, Callable] = {}   # addr -> hook_fn

        # I/O buffers (for convenience)
        self.input_buffer = []
        self.output_buffer = []

    # --- Hook Management ---

    def on_pc(self, addr: int, hook_fn: Callable):
        """Register a PC hook - called when PC reaches addr."""
        self.pc_hooks[addr] = hook_fn

    def on_read(self, addr: int, hook_fn: Callable):
        """Register a read hook - called on memory read at addr."""
        self.read_hooks[addr] = hook_fn

    def on_write(self, addr: int, hook_fn: Callable):
        """Register a write hook - called on memory write at addr."""
        self.write_hooks[addr] = hook_fn

    def remove_pc_hook(self, addr: int):
        self.pc_hooks.pop(addr, None)

    def remove_read_hook(self, addr: int):
        self.read_hooks.pop(addr, None)

    def remove_write_hook(self, addr: int):
        self.write_hooks.pop(addr, None)

    # --- Flag helpers ---

    def set_nz(self, value: int) -> int:
        self.P = self.P & ~(self.N_FLAG | self.Z_FLAG)
        if value == 0:
            self.P |= self.Z_FLAG
        if value & 0x80:
            self.P |= self.N_FLAG
        return value & 0xFF

    # --- Stack operations ---

    def push(self, value: int):
        self.memory[0x100 + self.SP] = value & 0xFF
        self.SP = (self.SP - 1) & 0xFF

    def pull(self) -> int:
        self.SP = (self.SP + 1) & 0xFF
        return self.memory[0x100 + self.SP]

    def push_word(self, value: int):
        self.push((value >> 8) & 0xFF)
        self.push(value & 0xFF)

    def pull_word(self) -> int:
        lo = self.pull()
        hi = self.pull()
        return (hi << 8) | lo

    # --- Memory access with hooks ---

    def read_byte(self, addr: int) -> int:
        addr &= 0xFFFF

        # Check for read hook
        if addr in self.read_hooks:
            action, value = self.read_hooks[addr](addr)
            if action == HookAction.OVERRIDE:
                return value if value is not None else 0
            elif action == HookAction.HALT:
                self.halted = True
                return 0
            elif action == HookAction.SKIP:
                return 0
            # CONTINUE falls through to normal read

        return self.memory[addr]

    def write_byte(self, addr: int, value: int):
        addr &= 0xFFFF
        value &= 0xFF

        # Check for write hook
        if addr in self.write_hooks:
            action, _ = self.write_hooks[addr](addr, value)
            if action == HookAction.HALT:
                self.halted = True
                return
            elif action == HookAction.SKIP:
                return
            # CONTINUE falls through to normal write

        self.memory[addr] = value

    def read_word(self, addr: int) -> int:
        return self.read_byte(addr) | (self.read_byte(addr + 1) << 8)

    # --- Instruction fetch ---

    def fetch(self) -> int:
        value = self.read_byte(self.PC)
        self.PC = (self.PC + 1) & 0xFFFF
        return value

    def fetch_word(self) -> int:
        lo = self.fetch()
        hi = self.fetch()
        return (hi << 8) | lo

    # --- Addressing modes ---

    def addr_imm(self): return self.fetch()
    def addr_zp(self): return self.fetch()
    def addr_zpx(self): return (self.fetch() + self.X) & 0xFF
    def addr_zpy(self): return (self.fetch() + self.Y) & 0xFF
    def addr_abs(self): return self.fetch_word()
    def addr_absx(self): return (self.fetch_word() + self.X) & 0xFFFF
    def addr_absy(self): return (self.fetch_word() + self.Y) & 0xFFFF

    def addr_indx(self):
        zp = (self.fetch() + self.X) & 0xFF
        return self.read_byte(zp) | (self.read_byte((zp + 1) & 0xFF) << 8)

    def addr_indy(self):
        zp = self.fetch()
        base = self.read_byte(zp) | (self.read_byte((zp + 1) & 0xFF) << 8)
        return (base + self.Y) & 0xFFFF

    def branch(self, condition: bool):
        offset = self.fetch()
        if offset & 0x80:
            offset -= 256
        if condition:
            self.PC = (self.PC + offset) & 0xFFFF

    # --- Main execution ---

    def step(self) -> bool:
        """Execute one instruction. Returns False if halted."""
        if self.halted:
            return False

        # Check PC hooks BEFORE fetch
        if self.PC in self.pc_hooks:
            action, _ = self.pc_hooks[self.PC](self.PC)
            if action == HookAction.HALT:
                self.halted = True
                return False
            elif action == HookAction.SKIP:
                return True  # Skip this instruction entirely

        opcode = self.fetch()
        self.cycle_count += 1

        # Decode and execute
        self._execute_opcode(opcode)

        return not self.halted and self.running

    def run(self, max_cycles: int = 0) -> int:
        """Run until halted or max_cycles. Returns cycles executed."""
        start_cycles = self.cycle_count
        while self.running and not self.halted:
            if max_cycles > 0 and (self.cycle_count - start_cycles) >= max_cycles:
                break
            self.step()
        return self.cycle_count - start_cycles

    def resume(self):
        """Resume after halt."""
        self.halted = False

    def load(self, data: bytes, addr: int):
        """Load binary data into memory."""
        for i, byte in enumerate(data):
            self.memory[addr + i] = byte

    def _execute_opcode(self, opcode: int):
        """Execute a single opcode."""
        # LDA
        if opcode == 0xA9: self.A = self.set_nz(self.fetch())
        elif opcode == 0xA5: self.A = self.set_nz(self.read_byte(self.addr_zp()))
        elif opcode == 0xB5: self.A = self.set_nz(self.read_byte(self.addr_zpx()))
        elif opcode == 0xAD: self.A = self.set_nz(self.read_byte(self.addr_abs()))
        elif opcode == 0xBD: self.A = self.set_nz(self.read_byte(self.addr_absx()))
        elif opcode == 0xB9: self.A = self.set_nz(self.read_byte(self.addr_absy()))
        elif opcode == 0xA1: self.A = self.set_nz(self.read_byte(self.addr_indx()))
        elif opcode == 0xB1: self.A = self.set_nz(self.read_byte(self.addr_indy()))

        # LDX
        elif opcode == 0xA2: self.X = self.set_nz(self.fetch())
        elif opcode == 0xA6: self.X = self.set_nz(self.read_byte(self.addr_zp()))
        elif opcode == 0xB6: self.X = self.set_nz(self.read_byte(self.addr_zpy()))
        elif opcode == 0xAE: self.X = self.set_nz(self.read_byte(self.addr_abs()))
        elif opcode == 0xBE: self.X = self.set_nz(self.read_byte(self.addr_absy()))

        # LDY
        elif opcode == 0xA0: self.Y = self.set_nz(self.fetch())
        elif opcode == 0xA4: self.Y = self.set_nz(self.read_byte(self.addr_zp()))
        elif opcode == 0xB4: self.Y = self.set_nz(self.read_byte(self.addr_zpx()))
        elif opcode == 0xAC: self.Y = self.set_nz(self.read_byte(self.addr_abs()))
        elif opcode == 0xBC: self.Y = self.set_nz(self.read_byte(self.addr_absx()))

        # STA
        elif opcode == 0x85: self.write_byte(self.addr_zp(), self.A)
        elif opcode == 0x95: self.write_byte(self.addr_zpx(), self.A)
        elif opcode == 0x8D: self.write_byte(self.addr_abs(), self.A)
        elif opcode == 0x9D: self.write_byte(self.addr_absx(), self.A)
        elif opcode == 0x99: self.write_byte(self.addr_absy(), self.A)
        elif opcode == 0x81: self.write_byte(self.addr_indx(), self.A)
        elif opcode == 0x91: self.write_byte(self.addr_indy(), self.A)

        # STX
        elif opcode == 0x86: self.write_byte(self.addr_zp(), self.X)
        elif opcode == 0x96: self.write_byte(self.addr_zpy(), self.X)
        elif opcode == 0x8E: self.write_byte(self.addr_abs(), self.X)

        # STY
        elif opcode == 0x84: self.write_byte(self.addr_zp(), self.Y)
        elif opcode == 0x94: self.write_byte(self.addr_zpx(), self.Y)
        elif opcode == 0x8C: self.write_byte(self.addr_abs(), self.Y)

        # Transfers
        elif opcode == 0xAA: self.X = self.set_nz(self.A)
        elif opcode == 0x8A: self.A = self.set_nz(self.X)
        elif opcode == 0xA8: self.Y = self.set_nz(self.A)
        elif opcode == 0x98: self.A = self.set_nz(self.Y)
        elif opcode == 0xBA: self.X = self.set_nz(self.SP)
        elif opcode == 0x9A: self.SP = self.X

        # Stack
        elif opcode == 0x48: self.push(self.A)
        elif opcode == 0x68: self.A = self.set_nz(self.pull())
        elif opcode == 0x08: self.push(self.P | 0x30)
        elif opcode == 0x28: self.P = (self.pull() & 0xEF) | 0x20

        # ADC
        elif opcode in (0x69, 0x65, 0x75, 0x6D, 0x7D, 0x79, 0x61, 0x71):
            if opcode == 0x69: val = self.fetch()
            elif opcode == 0x65: val = self.read_byte(self.addr_zp())
            elif opcode == 0x75: val = self.read_byte(self.addr_zpx())
            elif opcode == 0x6D: val = self.read_byte(self.addr_abs())
            elif opcode == 0x7D: val = self.read_byte(self.addr_absx())
            elif opcode == 0x79: val = self.read_byte(self.addr_absy())
            elif opcode == 0x61: val = self.read_byte(self.addr_indx())
            elif opcode == 0x71: val = self.read_byte(self.addr_indy())
            carry = 1 if (self.P & self.C_FLAG) else 0
            result = self.A + val + carry
            self.P = self.P & ~(self.C_FLAG | self.V_FLAG)
            if result > 255: self.P |= self.C_FLAG
            if ((self.A ^ result) & (val ^ result) & 0x80): self.P |= self.V_FLAG
            self.A = self.set_nz(result & 0xFF)

        # SBC
        elif opcode in (0xE9, 0xE5, 0xF5, 0xED, 0xFD, 0xF9, 0xE1, 0xF1):
            if opcode == 0xE9: val = self.fetch()
            elif opcode == 0xE5: val = self.read_byte(self.addr_zp())
            elif opcode == 0xF5: val = self.read_byte(self.addr_zpx())
            elif opcode == 0xED: val = self.read_byte(self.addr_abs())
            elif opcode == 0xFD: val = self.read_byte(self.addr_absx())
            elif opcode == 0xF9: val = self.read_byte(self.addr_absy())
            elif opcode == 0xE1: val = self.read_byte(self.addr_indx())
            elif opcode == 0xF1: val = self.read_byte(self.addr_indy())
            carry = 1 if (self.P & self.C_FLAG) else 0
            result = self.A - val - (1 - carry)
            self.P = self.P & ~(self.C_FLAG | self.V_FLAG)
            if result >= 0: self.P |= self.C_FLAG
            if ((self.A ^ val) & (self.A ^ result) & 0x80): self.P |= self.V_FLAG
            self.A = self.set_nz(result & 0xFF)

        # AND
        elif opcode == 0x29: self.A = self.set_nz(self.A & self.fetch())
        elif opcode == 0x25: self.A = self.set_nz(self.A & self.read_byte(self.addr_zp()))
        elif opcode == 0x35: self.A = self.set_nz(self.A & self.read_byte(self.addr_zpx()))
        elif opcode == 0x2D: self.A = self.set_nz(self.A & self.read_byte(self.addr_abs()))
        elif opcode == 0x3D: self.A = self.set_nz(self.A & self.read_byte(self.addr_absx()))
        elif opcode == 0x39: self.A = self.set_nz(self.A & self.read_byte(self.addr_absy()))
        elif opcode == 0x21: self.A = self.set_nz(self.A & self.read_byte(self.addr_indx()))
        elif opcode == 0x31: self.A = self.set_nz(self.A & self.read_byte(self.addr_indy()))

        # ORA
        elif opcode == 0x09: self.A = self.set_nz(self.A | self.fetch())
        elif opcode == 0x05: self.A = self.set_nz(self.A | self.read_byte(self.addr_zp()))
        elif opcode == 0x15: self.A = self.set_nz(self.A | self.read_byte(self.addr_zpx()))
        elif opcode == 0x0D: self.A = self.set_nz(self.A | self.read_byte(self.addr_abs()))
        elif opcode == 0x1D: self.A = self.set_nz(self.A | self.read_byte(self.addr_absx()))
        elif opcode == 0x19: self.A = self.set_nz(self.A | self.read_byte(self.addr_absy()))
        elif opcode == 0x01: self.A = self.set_nz(self.A | self.read_byte(self.addr_indx()))
        elif opcode == 0x11: self.A = self.set_nz(self.A | self.read_byte(self.addr_indy()))

        # EOR
        elif opcode == 0x49: self.A = self.set_nz(self.A ^ self.fetch())
        elif opcode == 0x45: self.A = self.set_nz(self.A ^ self.read_byte(self.addr_zp()))
        elif opcode == 0x55: self.A = self.set_nz(self.A ^ self.read_byte(self.addr_zpx()))
        elif opcode == 0x4D: self.A = self.set_nz(self.A ^ self.read_byte(self.addr_abs()))
        elif opcode == 0x5D: self.A = self.set_nz(self.A ^ self.read_byte(self.addr_absx()))
        elif opcode == 0x59: self.A = self.set_nz(self.A ^ self.read_byte(self.addr_absy()))
        elif opcode == 0x41: self.A = self.set_nz(self.A ^ self.read_byte(self.addr_indx()))
        elif opcode == 0x51: self.A = self.set_nz(self.A ^ self.read_byte(self.addr_indy()))

        # CMP
        elif opcode in (0xC9, 0xC5, 0xD5, 0xCD, 0xDD, 0xD9, 0xC1, 0xD1):
            if opcode == 0xC9: val = self.fetch()
            elif opcode == 0xC5: val = self.read_byte(self.addr_zp())
            elif opcode == 0xD5: val = self.read_byte(self.addr_zpx())
            elif opcode == 0xCD: val = self.read_byte(self.addr_abs())
            elif opcode == 0xDD: val = self.read_byte(self.addr_absx())
            elif opcode == 0xD9: val = self.read_byte(self.addr_absy())
            elif opcode == 0xC1: val = self.read_byte(self.addr_indx())
            elif opcode == 0xD1: val = self.read_byte(self.addr_indy())
            self.P = self.P & ~self.C_FLAG
            if self.A >= val: self.P |= self.C_FLAG
            self.set_nz((self.A - val) & 0xFF)

        # CPX
        elif opcode == 0xE0:
            val = self.fetch(); self.P &= ~self.C_FLAG
            if self.X >= val: self.P |= self.C_FLAG
            self.set_nz((self.X - val) & 0xFF)
        elif opcode == 0xE4:
            val = self.read_byte(self.addr_zp()); self.P &= ~self.C_FLAG
            if self.X >= val: self.P |= self.C_FLAG
            self.set_nz((self.X - val) & 0xFF)
        elif opcode == 0xEC:
            val = self.read_byte(self.addr_abs()); self.P &= ~self.C_FLAG
            if self.X >= val: self.P |= self.C_FLAG
            self.set_nz((self.X - val) & 0xFF)

        # CPY
        elif opcode == 0xC0:
            val = self.fetch(); self.P &= ~self.C_FLAG
            if self.Y >= val: self.P |= self.C_FLAG
            self.set_nz((self.Y - val) & 0xFF)
        elif opcode == 0xC4:
            val = self.read_byte(self.addr_zp()); self.P &= ~self.C_FLAG
            if self.Y >= val: self.P |= self.C_FLAG
            self.set_nz((self.Y - val) & 0xFF)
        elif opcode == 0xCC:
            val = self.read_byte(self.addr_abs()); self.P &= ~self.C_FLAG
            if self.Y >= val: self.P |= self.C_FLAG
            self.set_nz((self.Y - val) & 0xFF)

        # INC
        elif opcode == 0xE6:
            addr = self.addr_zp(); self.write_byte(addr, self.set_nz((self.read_byte(addr) + 1) & 0xFF))
        elif opcode == 0xF6:
            addr = self.addr_zpx(); self.write_byte(addr, self.set_nz((self.read_byte(addr) + 1) & 0xFF))
        elif opcode == 0xEE:
            addr = self.addr_abs(); self.write_byte(addr, self.set_nz((self.read_byte(addr) + 1) & 0xFF))
        elif opcode == 0xFE:
            addr = self.addr_absx(); self.write_byte(addr, self.set_nz((self.read_byte(addr) + 1) & 0xFF))

        # DEC
        elif opcode == 0xC6:
            addr = self.addr_zp(); self.write_byte(addr, self.set_nz((self.read_byte(addr) - 1) & 0xFF))
        elif opcode == 0xD6:
            addr = self.addr_zpx(); self.write_byte(addr, self.set_nz((self.read_byte(addr) - 1) & 0xFF))
        elif opcode == 0xCE:
            addr = self.addr_abs(); self.write_byte(addr, self.set_nz((self.read_byte(addr) - 1) & 0xFF))
        elif opcode == 0xDE:
            addr = self.addr_absx(); self.write_byte(addr, self.set_nz((self.read_byte(addr) - 1) & 0xFF))

        elif opcode == 0xE8: self.X = self.set_nz((self.X + 1) & 0xFF)
        elif opcode == 0xCA: self.X = self.set_nz((self.X - 1) & 0xFF)
        elif opcode == 0xC8: self.Y = self.set_nz((self.Y + 1) & 0xFF)
        elif opcode == 0x88: self.Y = self.set_nz((self.Y - 1) & 0xFF)

        # ASL
        elif opcode == 0x0A:
            self.P &= ~self.C_FLAG
            if self.A & 0x80: self.P |= self.C_FLAG
            self.A = self.set_nz((self.A << 1) & 0xFF)
        elif opcode in (0x06, 0x16, 0x0E, 0x1E):
            if opcode == 0x06: addr = self.addr_zp()
            elif opcode == 0x16: addr = self.addr_zpx()
            elif opcode == 0x0E: addr = self.addr_abs()
            else: addr = self.addr_absx()
            val = self.read_byte(addr)
            self.P &= ~self.C_FLAG
            if val & 0x80: self.P |= self.C_FLAG
            self.write_byte(addr, self.set_nz((val << 1) & 0xFF))

        # LSR
        elif opcode == 0x4A:
            self.P &= ~self.C_FLAG
            if self.A & 0x01: self.P |= self.C_FLAG
            self.A = self.set_nz(self.A >> 1)
        elif opcode in (0x46, 0x56, 0x4E, 0x5E):
            if opcode == 0x46: addr = self.addr_zp()
            elif opcode == 0x56: addr = self.addr_zpx()
            elif opcode == 0x4E: addr = self.addr_abs()
            else: addr = self.addr_absx()
            val = self.read_byte(addr)
            self.P &= ~self.C_FLAG
            if val & 0x01: self.P |= self.C_FLAG
            self.write_byte(addr, self.set_nz(val >> 1))

        # ROL
        elif opcode == 0x2A:
            carry = 1 if (self.P & self.C_FLAG) else 0
            self.P &= ~self.C_FLAG
            if self.A & 0x80: self.P |= self.C_FLAG
            self.A = self.set_nz(((self.A << 1) | carry) & 0xFF)
        elif opcode in (0x26, 0x36, 0x2E, 0x3E):
            if opcode == 0x26: addr = self.addr_zp()
            elif opcode == 0x36: addr = self.addr_zpx()
            elif opcode == 0x2E: addr = self.addr_abs()
            else: addr = self.addr_absx()
            val = self.read_byte(addr)
            carry = 1 if (self.P & self.C_FLAG) else 0
            self.P &= ~self.C_FLAG
            if val & 0x80: self.P |= self.C_FLAG
            self.write_byte(addr, self.set_nz(((val << 1) | carry) & 0xFF))

        # ROR
        elif opcode == 0x6A:
            carry = 0x80 if (self.P & self.C_FLAG) else 0
            self.P &= ~self.C_FLAG
            if self.A & 0x01: self.P |= self.C_FLAG
            self.A = self.set_nz((self.A >> 1) | carry)
        elif opcode in (0x66, 0x76, 0x6E, 0x7E):
            if opcode == 0x66: addr = self.addr_zp()
            elif opcode == 0x76: addr = self.addr_zpx()
            elif opcode == 0x6E: addr = self.addr_abs()
            else: addr = self.addr_absx()
            val = self.read_byte(addr)
            carry = 0x80 if (self.P & self.C_FLAG) else 0
            self.P &= ~self.C_FLAG
            if val & 0x01: self.P |= self.C_FLAG
            self.write_byte(addr, self.set_nz((val >> 1) | carry))

        # BIT
        elif opcode == 0x24:
            val = self.read_byte(self.addr_zp())
            self.P &= ~(self.N_FLAG | self.V_FLAG | self.Z_FLAG)
            if val & 0x80: self.P |= self.N_FLAG
            if val & 0x40: self.P |= self.V_FLAG
            if (self.A & val) == 0: self.P |= self.Z_FLAG
        elif opcode == 0x2C:
            val = self.read_byte(self.addr_abs())
            self.P &= ~(self.N_FLAG | self.V_FLAG | self.Z_FLAG)
            if val & 0x80: self.P |= self.N_FLAG
            if val & 0x40: self.P |= self.V_FLAG
            if (self.A & val) == 0: self.P |= self.Z_FLAG

        # Branches
        elif opcode == 0x10: self.branch(not (self.P & self.N_FLAG))
        elif opcode == 0x30: self.branch(self.P & self.N_FLAG)
        elif opcode == 0x50: self.branch(not (self.P & self.V_FLAG))
        elif opcode == 0x70: self.branch(self.P & self.V_FLAG)
        elif opcode == 0x90: self.branch(not (self.P & self.C_FLAG))
        elif opcode == 0xB0: self.branch(self.P & self.C_FLAG)
        elif opcode == 0xD0: self.branch(not (self.P & self.Z_FLAG))
        elif opcode == 0xF0: self.branch(self.P & self.Z_FLAG)

        # JMP
        elif opcode == 0x4C:
            self.PC = self.fetch_word()
        elif opcode == 0x6C:
            addr = self.fetch_word()
            lo = self.read_byte(addr)
            hi = self.read_byte((addr & 0xFF00) | ((addr + 1) & 0xFF))
            self.PC = (hi << 8) | lo

        # JSR/RTS
        elif opcode == 0x20:
            addr = self.fetch_word()
            self.push_word(self.PC - 1)
            self.PC = addr
        elif opcode == 0x60:
            self.PC = (self.pull_word() + 1) & 0xFFFF

        # RTI
        elif opcode == 0x40:
            self.P = (self.pull() & 0xEF) | 0x20
            self.PC = self.pull_word()

        # BRK
        elif opcode == 0x00:
            self.PC = (self.PC + 1) & 0xFFFF
            self.push_word(self.PC)
            self.push(self.P | self.B_FLAG)
            self.P |= self.I_FLAG
            self.PC = self.read_word(0xFFFE)

        # Flags
        elif opcode == 0x18: self.P &= ~self.C_FLAG
        elif opcode == 0x38: self.P |= self.C_FLAG
        elif opcode == 0x58: self.P &= ~self.I_FLAG
        elif opcode == 0x78: self.P |= self.I_FLAG
        elif opcode == 0xB8: self.P &= ~self.V_FLAG
        elif opcode == 0xD8: self.P &= ~self.D_FLAG
        elif opcode == 0xF8: self.P |= self.D_FLAG

        # NOP
        elif opcode == 0xEA:
            pass

        else:
            print(f"\nUnknown opcode: ${opcode:02X} at ${self.PC-1:04X}")
            self.running = False


# --- I/O Port addresses ---
IO_CHAROUT = 0xFFF0
IO_CHARIN = 0xFFF1
IO_STATUS = 0xFFF2
IO_PEEK = 0xFFF3

# MS-BASIC addresses
ADDR_COLD_START = 0x2730
ADDR_RDKEY = 0x28CE
ADDR_COUT = 0x28CA


def setup_basic_hooks(cpu: CPU6502, use_pc_halt: bool = True):
    """Set up I/O hooks for MS-BASIC.

    Both port hooks and PC hooks are always enabled:
    - Port hooks: Handle $FFF0-$FFF3 I/O (charout, charin, status, peek)
    - PC hooks: Optionally halt at RDKEY for line-based input

    Args:
        cpu: The CPU6502 instance
        use_pc_halt: If True, halt at RDKEY when input buffer empty (for ABAP PBO/PAI)
                     If False, port-based I/O only (reads return 0 when empty)
    """

    # Shared state
    state = {
        'use_pc_halt': use_pc_halt,
        'batch': False,
    }

    # --- Read hooks (Port interception) ---

    def on_read_charin(addr):
        """Read and consume character from input buffer."""
        if cpu.input_buffer:
            ch = cpu.input_buffer.pop(0)
            # Echo to output (port-level echo)
            if not state['batch']:
                cpu.write_byte(IO_CHAROUT, ch)
            return (HookAction.OVERRIDE, ch)
        return (HookAction.OVERRIDE, 0)

    def on_read_status(addr):
        """Return 1 if character available, 0 otherwise."""
        return (HookAction.OVERRIDE, 1 if cpu.input_buffer else 0)

    def on_read_peek(addr):
        """Peek at next character without consuming."""
        return (HookAction.OVERRIDE, cpu.input_buffer[0] if cpu.input_buffer else 0)

    # --- Write hooks (Port interception) ---

    def on_write_charout(addr, value):
        """Output character to console."""
        if not state['batch']:
            ch = chr(value) if 32 <= value < 127 or value in (10, 13) else f'[{value:02X}]'
            sys.stdout.write(ch)
            sys.stdout.flush()
        cpu.output_buffer.append(value)
        return (HookAction.SKIP, None)  # Don't write to memory

    def on_write_rom(addr, value):
        """Protect ROM from writes."""
        return (HookAction.SKIP, None)

    # --- PC hooks (Function interception) ---

    def on_pc_rdkey(pc):
        """Halt CPU at RDKEY if input buffer is empty (for line-based input)."""
        if state['use_pc_halt'] and not cpu.input_buffer:
            return (HookAction.HALT, None)  # Halt for line input
        return (HookAction.CONTINUE, None)

    # Register all port hooks
    cpu.on_read(IO_CHARIN, on_read_charin)
    cpu.on_read(IO_STATUS, on_read_status)
    cpu.on_read(IO_PEEK, on_read_peek)
    cpu.on_write(IO_CHAROUT, on_write_charout)

    # ROM protection ($0800+)
    for addr in range(0x0800, 0xFFF0):
        cpu.on_write(addr, on_write_rom)

    # Always register PC hook (controlled by use_pc_halt flag)
    cpu.on_pc(ADDR_RDKEY, on_pc_rdkey)

    return state


def check_stdin():
    return select.select([sys.stdin], [], [], 0)[0]


def run_interactive(cpu: CPU6502):
    """Run interactively with line-based input (PC hook halts at RDKEY)."""
    print("MS-BASIC (PC + Port hooks)")
    print("Type 'quit' to exit\n")

    while cpu.running:
        cpu.resume()
        cpu.run()

        if cpu.halted:
            # CPU halted at RDKEY - get line input
            try:
                line = input()
            except EOFError:
                break

            if line.lower() == 'quit':
                break

            # Feed line into input buffer (port hooks will handle the rest)
            for ch in line:
                cpu.input_buffer.append(ord(ch))
            cpu.input_buffer.append(13)  # CR


def run_batch(cpu: CPU6502, commands: list, state: dict):
    """Run batch commands."""
    state['batch'] = True

    for cmd in commands:
        for ch in cmd:
            cpu.input_buffer.append(ord(ch))
        cpu.input_buffer.append(13)

    last_len = 0
    stable = 0

    while cpu.running and cpu.cycle_count < 10000000:
        cpu.resume()
        cpu.run(10000)

        if not cpu.input_buffer and len(cpu.output_buffer) == last_len:
            stable += 1
            if stable > 10:
                break
        else:
            stable = 0
            last_len = len(cpu.output_buffer)

    return ''.join(chr(b) if 32 <= b < 127 or b in (10, 13) else ''
                   for b in cpu.output_buffer)


def main():
    batch_mode = '--batch' in sys.argv

    if len(sys.argv) > 1 and sys.argv[1] not in ('--batch',):
        print(f"Usage: {sys.argv[0]} [--batch]")
        print("  (no args)  Interactive mode with line input")
        print("  --batch    Run test commands and verify output")
        return 1

    # Create CPU and load BASIC
    cpu = CPU6502()

    try:
        with open("bin/msbasic.bin", "rb") as f:
            cpu.load(f.read(), 0x0800)
        print(f"Loaded MS-BASIC at $0800")
    except FileNotFoundError:
        print("Error: bin/msbasic.bin not found")
        return 1

    cpu.PC = ADDR_COLD_START
    print(f"Starting at ${cpu.PC:04X}")

    # Set up hooks (both PC and port hooks enabled)
    # use_pc_halt=False for batch (don't halt, let port hooks handle I/O)
    # use_pc_halt=True for interactive (halt at RDKEY for line input)
    state = setup_basic_hooks(cpu, use_pc_halt=not batch_mode)

    if batch_mode:
        commands = ['', '', 'PRINT "HELLO WORLD"', 'PRINT 2+2',
                    '10 PRINT "TEST"', '20 PRINT 3*4', 'RUN']
        print(f"Batch: {commands}\n")
        output = run_batch(cpu, commands, state)
        print("--- Output ---")
        print(output)
        print("--- End ---")

        if all(x in output for x in ["HELLO WORLD", "4", "TEST", "12"]):
            print("\nSUCCESS!")
            return 0
        print("\nFAILURE")
        return 1
    else:
        run_interactive(cpu)

    return 0


if __name__ == '__main__':
    sys.exit(main())
