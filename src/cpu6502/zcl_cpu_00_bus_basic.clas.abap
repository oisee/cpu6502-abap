CLASS zcl_cpu_00_bus_basic DEFINITION
  PUBLIC
  CREATE PUBLIC.
************************************************************************
* MS-BASIC Bus Implementation for 6502 Emulator
* - 64KB RAM with ROM protection ($0800+)
* - Memory-mapped I/O at $FFF0-$FFF3:
*   $FFF0 - CHAROUT: Write character to output
*   $FFF1 - CHARIN:  Read character from input (consuming)
*   $FFF2 - STATUS:  I/O status (bit 0 = char available)
*   $FFF3 - PEEK:    Peek next char without consuming
************************************************************************

  PUBLIC SECTION.
    INTERFACES zif_cpu_00_bus.

    " I/O addresses for MS-BASIC
    CONSTANTS: c_io_charout TYPE i VALUE 65520,  " $FFF0
               c_io_charin  TYPE i VALUE 65521,  " $FFF1
               c_io_status  TYPE i VALUE 65522,  " $FFF2
               c_io_peek    TYPE i VALUE 65523.  " $FFF3

    " ROM start address (writes ignored above this)
    CONSTANTS: c_rom_start TYPE i VALUE 2048.    " $0800

    METHODS constructor.

    " Set start PC (for MS-BASIC cold start at $2730)
    METHODS set_start_pc
      IMPORTING iv_pc TYPE i.

    " Get start PC
    METHODS get_start_pc
      RETURNING VALUE(rv_pc) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mt_mem         TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
          mv_input_buf   TYPE string,     " Input queue
          mv_input_pos   TYPE i,          " Position in input
          mv_output_buf  TYPE string,     " Output accumulator
          mv_start_pc    TYPE i.          " Start PC (not using reset vector)

ENDCLASS.



CLASS zcl_cpu_00_bus_basic IMPLEMENTATION.

  METHOD constructor.
    " Initialize 64KB of zero-filled RAM
    DO 65536 TIMES.
      APPEND 0 TO mt_mem.
    ENDDO.
    mv_input_pos = 0.
    mv_start_pc = 10032.  " $2730 - MS-BASIC cold start
  ENDMETHOD.

  METHOD set_start_pc.
    mv_start_pc = iv_pc.
  ENDMETHOD.

  METHOD get_start_pc.
    rv_pc = mv_start_pc.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~read.
    " Bounds check
    IF iv_addr < 0 OR iv_addr > 65535.
      rv_val = 0.
      RETURN.
    ENDIF.

    " I/O: Read character (consuming)
    IF iv_addr = c_io_charin.
      IF mv_input_pos < strlen( mv_input_buf ).
        DATA lv_char TYPE c LENGTH 1.
        lv_char = mv_input_buf+mv_input_pos(1).
        mv_input_pos = mv_input_pos + 1.
        DATA lv_hex TYPE x LENGTH 2.
        lv_hex = cl_abap_conv_out_ce=>uccp( lv_char ).
        rv_val = lv_hex.
      ELSE.
        rv_val = 0.
      ENDIF.
      RETURN.
    ENDIF.

    " I/O: Status (bit 0 = char available)
    IF iv_addr = c_io_status.
      IF mv_input_pos < strlen( mv_input_buf ).
        rv_val = 1.
      ELSE.
        rv_val = 0.
      ENDIF.
      RETURN.
    ENDIF.

    " I/O: Peek (non-consuming read)
    IF iv_addr = c_io_peek.
      IF mv_input_pos < strlen( mv_input_buf ).
        DATA lv_peek_char TYPE c LENGTH 1.
        lv_peek_char = mv_input_buf+mv_input_pos(1).
        DATA lv_peek_hex TYPE x LENGTH 2.
        lv_peek_hex = cl_abap_conv_out_ce=>uccp( lv_peek_char ).
        rv_val = lv_peek_hex.
      ELSE.
        rv_val = 0.
      ENDIF.
      RETURN.
    ENDIF.

    " Regular memory read
    DATA lv_idx TYPE i.
    lv_idx = iv_addr + 1.  " ABAP tables are 1-indexed
    READ TABLE mt_mem INDEX lv_idx INTO rv_val.
    IF sy-subrc <> 0.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~write.
    " Bounds check
    IF iv_addr < 0 OR iv_addr > 65535.
      RETURN.
    ENDIF.

    " Ensure value is 8-bit
    DATA lv_val TYPE i.
    lv_val = iv_val MOD 256.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.

    " I/O: Output character
    IF iv_addr = c_io_charout.
      DATA lv_char TYPE c LENGTH 1.
      DATA lv_hex2 TYPE x LENGTH 2.
      lv_hex2 = lv_val.
      lv_char = cl_abap_conv_in_ce=>uccp( lv_hex2 ).
      mv_output_buf = mv_output_buf && lv_char.
      RETURN.
    ENDIF.

    " ROM protection - ignore writes to $0800 and above (except I/O)
    IF iv_addr >= c_rom_start AND iv_addr < c_io_charout.
      RETURN.
    ENDIF.

    " Regular memory write
    DATA lv_idx TYPE i.
    lv_idx = iv_addr + 1.
    MODIFY mt_mem INDEX lv_idx FROM lv_val.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~load.
    " Load binary data into memory (bypasses ROM protection)
    DATA: lv_len  TYPE i,
          lv_i    TYPE i,
          lv_byte TYPE x LENGTH 1,
          lv_val  TYPE i,
          lv_idx  TYPE i.

    lv_len = xstrlen( iv_data ).
    DO lv_len TIMES.
      lv_i = sy-index - 1.
      lv_byte = iv_data+lv_i(1).
      lv_val = lv_byte.
      lv_idx = iv_addr + lv_i + 1.  " +1 for ABAP indexing
      MODIFY mt_mem INDEX lv_idx FROM lv_val.
    ENDDO.

    " Set reset vector to point to start PC (for CPU reset)
    " $FFFC/$FFFD = 65532/65533
    DATA: lv_lo TYPE i,
          lv_hi TYPE i.
    lv_lo = mv_start_pc MOD 256.
    lv_hi = mv_start_pc DIV 256.
    MODIFY mt_mem INDEX 65533 FROM lv_lo.  " +1 for ABAP indexing
    MODIFY mt_mem INDEX 65534 FROM lv_hi.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~is_input_ready.
    rv_ready = xsdbool( mv_input_pos < strlen( mv_input_buf ) ).
  ENDMETHOD.

  METHOD zif_cpu_00_bus~get_output.
    rv_output = mv_output_buf.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~clear_output.
    CLEAR mv_output_buf.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~provide_input.
    " Append to input buffer with CR (not CRLF - 6502 uses CR)
    DATA lv_cr TYPE c LENGTH 1.
    lv_cr = cl_abap_char_utilities=>cr_lf(1).  " Just CR
    mv_input_buf = mv_input_buf && iv_text && lv_cr.
  ENDMETHOD.

ENDCLASS.