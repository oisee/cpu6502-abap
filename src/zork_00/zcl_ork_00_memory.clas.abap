*&---------------------------------------------------------------------*
*& Z-Machine Memory - Direct port from z3_minimal.py
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_memory DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    " Header fields (parsed from story file)
    DATA version      TYPE i READ-ONLY.
    DATA flags1       TYPE i READ-ONLY.
    DATA high_mem     TYPE i READ-ONLY.
    DATA initial_pc   TYPE i READ-ONLY.
    DATA dict_addr    TYPE i READ-ONLY.
    DATA obj_addr     TYPE i READ-ONLY.
    DATA globals_addr TYPE i READ-ONLY.
    DATA static_mem   TYPE i READ-ONLY.
    DATA abbrev_addr  TYPE i READ-ONLY.
    DATA file_len     TYPE i READ-ONLY.
    DATA checksum     TYPE i READ-ONLY.
    DATA serial       TYPE string READ-ONLY.
    DATA mem_size     TYPE i READ-ONLY.

    METHODS constructor IMPORTING iv_data TYPE xstring.

    " Byte operations
    METHODS u8 IMPORTING iv_addr TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS w8 IMPORTING iv_addr TYPE i iv_val TYPE i.

    " Word operations (big-endian)
    METHODS u16 IMPORTING iv_addr TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS w16 IMPORTING iv_addr TYPE i iv_val TYPE i.

    " Global variables (16-255)
    METHODS get_global IMPORTING iv_var TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS set_global IMPORTING iv_var TYPE i iv_val TYPE i.

  PRIVATE SECTION.
    DATA mt_mem TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS parse_header.
ENDCLASS.

CLASS zcl_ork_00_memory IMPLEMENTATION.

  METHOD constructor.
    " Load story file bytes into memory table
    DATA(lv_len) = xstrlen( iv_data ).
    mem_size = lv_len.

    " Convert xstring to table of bytes using proper ABAP type conversion
    DATA lv_i TYPE i.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_val TYPE i.

    lv_i = 0.
    WHILE lv_i < lv_len.
      " Extract single byte from xstring
      lv_byte = iv_data+lv_i(1).
      " Direct assignment from x to i works in ABAP
      lv_val = lv_byte.
      APPEND lv_val TO mt_mem.
      lv_i = lv_i + 1.
    ENDWHILE.

    parse_header( ).
  ENDMETHOD.

  METHOD parse_header.
    " Parse Z-machine header (first 64 bytes)
    IF mem_size < 64.
      RETURN.
    ENDIF.

    version = u8( 0 ).
    IF version <> 3.
      " Only V3 supported
      RETURN.
    ENDIF.

    flags1 = u8( 1 ).
    high_mem = u16( 4 ).
    initial_pc = u16( 6 ).
    dict_addr = u16( 8 ).
    obj_addr = u16( 10 ).
    globals_addr = u16( 12 ).
    static_mem = u16( 14 ).
    abbrev_addr = u16( 24 ).
    file_len = u16( 26 ) * 2.  " V3: multiply by 2
    checksum = u16( 28 ).

    " Serial number (6 ASCII chars at 0x12-0x17)
    serial = ''.
    DATA lv_i TYPE i.
    lv_i = 18.
    WHILE lv_i < 24.
      DATA(lv_ch) = u8( lv_i ).
      IF lv_ch >= 32 AND lv_ch <= 126.
        serial = serial && cl_abap_conv_in_ce=>uccpi( lv_ch ).
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD u8.
    " Read unsigned byte
    IF iv_addr < 0 OR iv_addr >= mem_size.
      rv_val = 0.
      RETURN.
    ENDIF.
    READ TABLE mt_mem INDEX iv_addr + 1 INTO rv_val.
    IF sy-subrc <> 0.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.

  METHOD w8.
    " Write unsigned byte
    IF iv_addr < 0 OR iv_addr >= mem_size.
      RETURN.
    ENDIF.
    DATA(lv_val) = iv_val MOD 256.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.
    MODIFY mt_mem INDEX iv_addr + 1 FROM lv_val.
  ENDMETHOD.

  METHOD u16.
    " Read unsigned 16-bit word (big-endian)
    rv_val = u8( iv_addr ) * 256 + u8( iv_addr + 1 ).
  ENDMETHOD.

  METHOD w16.
    " Write unsigned 16-bit word (big-endian)
    DATA(lv_val) = iv_val MOD 65536.
    IF lv_val < 0.
      lv_val = lv_val + 65536.
    ENDIF.
    w8( iv_addr = iv_addr iv_val = lv_val DIV 256 ).
    w8( iv_addr = iv_addr + 1 iv_val = lv_val MOD 256 ).
  ENDMETHOD.

  METHOD get_global.
    " Get global variable (16-239 -> globals table)
    DATA(lv_addr) = globals_addr + ( iv_var - 16 ) * 2.
    rv_val = u16( lv_addr ).
  ENDMETHOD.

  METHOD set_global.
    " Set global variable
    DATA(lv_addr) = globals_addr + ( iv_var - 16 ) * 2.
    w16( iv_addr = lv_addr iv_val = iv_val MOD 65536 ).
  ENDMETHOD.

ENDCLASS.
