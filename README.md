# CPU6502-ABAP

A MOS 6502 CPU emulator written in ABAP. Run 6502 machine code on SAP systems.

![Vibecoded](https://img.shields.io/badge/vibecoded-with%20Claude%20Code-blueviolet)
![License](https://img.shields.io/badge/license-MIT-green)
![Platform](https://img.shields.io/badge/platform-SAP%20ABAP-blue)

## Status

**Working!** All 44 unit tests pass. The emulator correctly executes 6502 machine code including:

- All arithmetic operations (ADC, SBC) with proper carry/overflow
- Logical operations (AND, ORA, EOR)
- Shifts and rotates (ASL, LSR, ROL, ROR)
- Comparisons (CMP, CPX, CPY)
- Branches (BEQ, BNE, BCS, BCC, BMI, BPL, BVS, BVC)
- Jumps (JMP absolute/indirect, JSR/RTS)
- Stack operations (PHA, PLA, PHP, PLP)
- All addressing modes (immediate, zero page, absolute, indexed, indirect)

## MS-BASIC Running!

**Microsoft BASIC (1977) runs on this emulator!**

```
MEMORY SIZE?
TERMINAL WIDTH?

 1279 BYTES FREE

COPYRIGHT 1977 BY MICROSOFT CO.

OK
PRINT "HELLO WORLD"
HELLO WORLD

OK
PRINT 2+2
 4

OK
10 PRINT "TEST"
20 PRINT 3*4
RUN
TEST
 12

OK
```

### Test Harness

A Python 6502 emulator (`test_basic.py`) is included for local testing:

```bash
# Batch mode - run pre-defined commands
python3 test_basic.py --batch

# Interactive mode - terminal session
python3 test_basic.py
```

Interactive mode uses raw terminal input with cursor position queries to seamlessly integrate with BASIC's line editing - type naturally, and BASIC echoes your input as it processes each character.

### Hook System

The emulator supports two levels of interception that work together:

**PC Hooks** - Trigger when PC reaches specific addresses:
- Halt CPU at function entry (e.g., RDKEY at `$28CE`)
- Perfect for ABAP PBO/PAI integration (halt → get line → resume)

**Port Hooks** - Trigger on memory-mapped I/O reads/writes:
- Character I/O at `$FFF0-$FFF3`
- Custom code reading ports directly still works

Hook actions:
| Action | Description |
|--------|-------------|
| CONTINUE | Continue normal execution |
| HALT | Stop CPU, return to caller |
| SKIP | Skip read/write operation |
| OVERRIDE | Return custom value (reads) |

### Memory-Mapped I/O

| Address | Name | Purpose |
|---------|------|---------|
| `$FFF0` | CHAROUT | Write character to output |
| `$FFF1` | CHARIN | Read character from input (consuming) |
| `$FFF2` | STATUS | I/O status (bit 0 = char available) |
| `$FFF3` | PEEK | Peek next char without consuming |

### MS-BASIC Entry Points

| Address | Name | Purpose |
|---------|------|---------|
| `$2730` | COLD_START | Cold boot entry |
| `$28CA` | COUT | Character output routine |
| `$28CE` | RDKEY | Read key routine (halt here for line input) |

The ABAP emulator can intercept these addresses to connect BASIC to SAP I/O.

## What is This?

The MOS 6502 is the legendary 8-bit CPU that powered the Apple II, Commodore 64, NES, Atari 2600, and many other classic systems. This ABAP implementation emulates the 6502 instruction set, allowing you to run 6502 programs inside SAP.

## Vibecoded with Claude Code

This project was **vibecoded** using [Claude Code](https://claude.ai/code) - Anthropic's AI coding assistant.

**Vibecoded in ABAP, directly in the SAP system.** No additional tools needed on SAP - just the [vibing-steampunk](https://github.com/oisee/vibing-steampunk) MCP server running locally, which gives Claude direct access to SAP ADT APIs.

## Architecture

```
zcl_cpu_00_cpu           - Main CPU emulator (registers, fetch-decode-execute)
zcl_cpu_00_bus_simple    - Simple bus implementation (64KB RAM)
zcl_cpu_00_test          - Unit tests (44 tests covering all operations)
zcl_cpu_00_speedrun      - Automated test execution
zif_cpu_00_bus           - Bus interface for memory/IO
```

### CPU Registers

| Register | Size | Description |
|----------|------|-------------|
| A | 8-bit | Accumulator |
| X | 8-bit | Index register X |
| Y | 8-bit | Index register Y |
| SP | 8-bit | Stack pointer (page $01) |
| PC | 16-bit | Program counter |
| P | 8-bit | Status flags (N V - B D I Z C) |

### Addressing Modes

- Immediate (`#$nn`)
- Zero Page (`$nn`)
- Zero Page,X (`$nn,X`)
- Zero Page,Y (`$nn,Y`)
- Absolute (`$nnnn`)
- Absolute,X (`$nnnn,X`)
- Absolute,Y (`$nnnn,Y`)
- Indirect (`($nnnn)`)
- Indexed Indirect (`($nn,X)`)
- Indirect Indexed (`($nn),Y`)

## Running

### Unit Tests

Run the full test suite via MCP:

```
RunUnitTests(object_url="/sap/bc/adt/oo/classes/ZCL_CPU_00_TEST")
```

Or in SAP GUI: `SE38 -> ZCPU6502_SPEEDRUN`

### Console Mode

```
SE38 -> ZCPU6502_CONSOLE
```

Load a ROM file and step through execution.

## Installation

1. Create package `$CPU6502` in your SAP system
2. Import this repo via [abapGit](https://abapgit.org/)
3. Activate all objects
4. Run unit tests to verify

## Files

```
src/cpu6502/           - ABAP 6502 emulator classes
bin/msbasic.bin        - Compiled MS-BASIC (30KB)
test_basic.py          - Python test harness
msbasic/               - MS-BASIC source (gitignored, from mist64/msbasic)
```

## Development

This project uses [vibing-steampunk](https://github.com/oisee/vibing-steampunk) for AI-assisted development:

```json
{
  "mcpServers": {
    "a4h-abap-adt": {
      "command": "/path/to/vsp",
      "env": {
        "SAP_URL": "http://your-sap-host:50000",
        "SAP_USER": "your-user",
        "SAP_PASSWORD": "your-password"
      }
    }
  }
}
```

See [CLAUDE.md](CLAUDE.md) for detailed development guidelines.

## Key Implementation Detail

ABAP's `/` operator performs decimal division, not integer division. All bit manipulation operations use `DIV` instead:

```abap
" LSR - Logical Shift Right
lv_result = mv_a DIV 2.  " NOT mv_a / 2!

" ROR - Rotate Right
lv_result = lv_value DIV 2 + lv_carry * 128.
```

## References

### 6502 Resources
- [6502.org](http://www.6502.org/) - The 6502 information hub
- [Easy 6502](https://skilldrick.github.io/easy6502/) - Interactive 6502 tutorial
- [6502 Instruction Set](https://www.masswerk.at/6502/6502_instruction_set.html) - Complete opcode reference

### MS-BASIC
- [mist64/msbasic](https://github.com/mist64/msbasic) - MS-BASIC for 6502 (ca65 compatible)
- [Microsoft BASIC History](https://www.pagetable.com/?p=774) - The story of MS-BASIC

### Development Tools
- [vibing-steampunk](https://github.com/oisee/vibing-steampunk) - MCP server for SAP ADT
- [Claude Code](https://claude.ai/code) - AI coding assistant

## License

MIT License - see [LICENSE](LICENSE) file.

## Credits

- **MOS Technology** - Original 6502 design (1975)
- **Microsoft** - BASIC interpreter (1975-1977)
- **Anthropic** - Claude Code, the AI that wrote this
- **vibing-steampunk** - MCP bridge that made it possible

---

*"The 6502 - powering dreams since 1975, now in your SAP system."*

*"READY." - Microsoft BASIC, running on ABAP, 2024*
