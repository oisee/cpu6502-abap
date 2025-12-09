# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ZORK-ABAP is a Z-Machine V3 interpreter written in ABAP, ported from a Python reference implementation (`z3_minimal.py`). It executes classic text adventure games like Zork on SAP systems.

**Status:** Working! MiniZork runs successfully.

## Development Environment

### MCP Server (vibing-steampunk / vsp)

This project uses [vibing-steampunk](https://github.com/oisee/vibing-steampunk) (`a4h-abap-adt` MCP server) for SAP ADT access. Key MCP tools:

| Tool | Purpose |
|------|---------|
| `SearchObject` | Find ABAP objects (`SearchObject(query="ZORK*")`) |
| `GetSource` | Read source code (`GetSource(object_type="CLAS", name="ZCL_ORK_00_ZMACHINE")`) |
| `WriteSource` | Create/update source (`WriteSource(object_type="CLAS", name="...", source="...")`) |
| `EditSource` | Surgical edits (`EditSource(object_url="...", old_string="...", new_string="...")`) |
| `RunUnitTests` | Execute tests (`RunUnitTests(object_url="/sap/bc/adt/oo/classes/ZCL_ORK_00_SPEEDRUN")`) |
| `Activate` | Activate objects |
| `SyntaxCheck` | Validate syntax before activation |

### SAP Resources

- **Package:** `$ZORK_00`
- **SMW0 Storage:** Game files (`ZORK-MINI.Z3`) and scripts (`ZORK-MINI-SPEEDRUN.TXT`, `ZORK-MINI-TEST.TXT`)

## Architecture

### Core Z-Machine Components (src/zork_00/)

| Class | Purpose |
|-------|---------|
| `zcl_ork_00_zmachine` | Main interpreter: fetch-decode-execute cycle, `step()` and `run()` methods |
| `zcl_ork_00_memory` | Memory management (big-endian), V3 header parsing, globals (16-255) |
| `zcl_ork_00_stack` | Call frames with locals (1-15), evaluation stack per frame |
| `zcl_ork_00_objects` | Object tree: parent/child/sibling, 32 attributes, properties |
| `zcl_ork_00_text` | ZSCII text decoding with abbreviations |
| `zcl_ork_00_dict` | Dictionary lookup and input tokenization |

### Game/Script Loading

| Object | Purpose |
|--------|---------|
| `zif_ork_00_game_loader` | Interface for story file loading |
| `zcl_ork_00_game_loader_smw0` | Load from SAP Web Repository (SMW0) |
| `zcl_ork_00_game_loader_file` | Load from filesystem |
| `zif_ork_00_script_loader` | Interface for command scripts |
| `zcl_ork_00_speedrun` | Automated execution with `#ASSERT` directives |

### Entry Points (Programs)

| Program | Purpose |
|---------|---------|
| `zork_00_console` | Interactive HTML console (24-line display) |
| `zork_00_speedrun` | Batch execution with verification |
| `zork_00_step` | Step-by-step debugging |

## Running Tests

```
RunUnitTests(object_url="/sap/bc/adt/oo/classes/ZCL_ORK_00_SPEEDRUN")
```

Test classes validate against known-good playthroughs. Assertions in test scripts check for expected output text.

## Critical Implementation Details

### Alphabet Indexing

Z-characters 6-31 map to alphabet indices 0-25:
```abap
lv_idx = lv_zc - 6.  " Subtract 6!
lv_char = c_a0+lv_idx(1).
```

### 10-bit ZSCII State Machine

```abap
" States: -1 = normal, -2 = waiting for high, >= 0 = have high bits
IF lv_zscii_hi >= 0.
  lv_zscii = lv_zscii_hi * 32 + lv_zc.
ELSEIF lv_zscii_hi = -2.
  lv_zscii_hi = lv_zc.
ELSEIF lv_zc = 6 AND lv_alphabet = 2.
  lv_zscii_hi = -2.
ENDIF.
```

### Memory Byte Conversion

```abap
DATA lv_byte TYPE x LENGTH 1.
lv_byte = iv_data+lv_i(1).
lv_val = lv_byte.  " Automatic hex-to-int conversion
```

### Object Table Layout (V3)

- Base + 0: Property defaults (31 words = 62 bytes)
- Base + 62: First object entry
- Each object: 9 bytes (4 attr + parent + sibling + child + 2-byte prop ptr)
- Attributes are big-endian within bytes (attr 0 = bit 7 of byte 0)

## ABAP Naming Conventions

| Prefix | Usage | Example |
|--------|-------|---------|
| `ts_` | Structure types | `ts_game_info` |
| `tt_` | Table types | `tt_game_list` |
| `lv_` | Local variables | `lv_count` |
| `lt_` | Local tables | `lt_lines` |
| `mv_` | Instance attributes | `mv_running` |
| `mt_` | Instance tables | `mt_frames` |
| `mo_` | Instance objects | `mo_zmachine` |
| `iv_` | Import parameters | `iv_story` |
| `ev_` | Export parameters | `ev_text` |
| `rv_` | Return values | `rv_result` |

**Note:** Use `ts_` for structures, NOT `ty_`.

## File Naming (abapGit)

- `.clas.abap` - Class main source
- `.clas.testclasses.abap` - Unit test classes
- `.intf.abap` - Interfaces
- `.prog.abap` - Reports/programs

## Reference Materials

Related repository `../cpm-abap` contains:
- `z3-reference/z3_minimal.py` - Python reference implementation (~1100 lines)
- `z3-reference/PORTING_GUIDE.md` - Detailed porting notes and opcode reference
- `test-games/minizork.z3` - Test story file (52,216 bytes)
- `ZMACHINE_STATUS.md` - Implementation status and bug fixes

## V3 Opcode Quick Reference

### 0OP
`rtrue`, `rfalse`, `print`, `print_ret`, `save`, `restore`, `ret_popped`, `quit`, `new_line`

### 1OP
`jz`, `get_sibling`, `get_child`, `get_parent`, `inc`, `dec`, `remove_obj`, `print_obj`, `ret`, `jump`, `print_paddr`

### 2OP
`je`, `jl`, `jg`, `jin`, `test`, `or`, `and`, `test_attr`, `set_attr`, `clear_attr`, `store`, `insert_obj`, `loadw`, `loadb`, `get_prop`, `add`, `sub`, `mul`, `div`, `mod`

### VAR
`call`, `storew`, `storeb`, `put_prop`, `sread`, `print_char`, `print_num`, `random`, `push`, `pull`
