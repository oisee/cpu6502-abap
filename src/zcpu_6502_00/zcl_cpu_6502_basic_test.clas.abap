CLASS zcl_cpu_6502_basic_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
************************************************************************
* MS-BASIC Test Runner for 6502 Emulator
* Requires ZMSBASIC.BIN uploaded to SMW0
************************************************************************

  PUBLIC SECTION.
    CLASS-METHODS run_test
      RETURNING VALUE(rs_result) TYPE zcl_cpu_6502_speedrun_basic=>ts_result.

    CLASS-METHODS get_log
      RETURNING VALUE(rt_log) TYPE zcl_cpu_6502_speedrun_basic=>tt_log.

  PRIVATE SECTION.
    CLASS-DATA go_speedrun TYPE REF TO zcl_cpu_6502_speedrun_basic.

ENDCLASS.



CLASS ZCL_CPU_6502_BASIC_TEST IMPLEMENTATION.


  METHOD run_test.
    DATA: lo_loader   TYPE REF TO zcl_cpu_6502_rom_loader_smw0,
          lv_rom      TYPE xstring,
          lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands.

    " Load ROM from SMW0
    lo_loader = NEW zcl_cpu_6502_rom_loader_smw0( iv_pattern = '*BASIC*' ).
    lv_rom = lo_loader->zif_cpu_6502_rom_loader~load( 'ZMSBASIC.BIN' ).

    IF lv_rom IS INITIAL.
      rs_result-success = abap_false.
      rs_result-error_message = 'Could not load ZMSBASIC.BIN from SMW0'.
      RETURN.
    ENDIF.

    " Create test script
    " Use $CR for empty input (just sends carriage return)
    " First: MEMORY SIZE? prompt - send empty (accept default)
    APPEND '$CR' TO lt_commands.
    APPEND '%*MEMORY' TO lt_commands.
    " Second: TERMINAL WIDTH? prompt - send empty (accept default)
    APPEND '$CR' TO lt_commands.
    APPEND '%*WIDTH' TO lt_commands.
    " Should now be at READY prompt
    " Test commands
    APPEND 'PRINT "HELLO WORLD"' TO lt_commands.
    APPEND '%*HELLO WORLD' TO lt_commands.
    APPEND 'PRINT 2+2' TO lt_commands.
    APPEND '%*4' TO lt_commands.

    " Run speedrun
    go_speedrun = NEW zcl_cpu_6502_speedrun_basic(
      iv_rom      = lv_rom
      it_commands = lt_commands ).

    rs_result = go_speedrun->run( ).
  ENDMETHOD.


  METHOD get_log.
    IF go_speedrun IS BOUND.
      rt_log = go_speedrun->get_log( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
