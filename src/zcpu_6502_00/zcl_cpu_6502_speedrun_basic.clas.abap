CLASS zcl_cpu_6502_speedrun_basic DEFINITION
  PUBLIC
  CREATE PUBLIC.
************************************************************************
* MS-BASIC Speedrun Test Class
* Runs MS-BASIC with scripted input and verifies expected output
* Uses zcl_cpu_6502_bus_basic with I/O at $FFF0-$FFF3
************************************************************************

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_log_entry,
             seq  TYPE i,
             role TYPE c LENGTH 1,
             text TYPE string,
           END OF ts_log_entry.

    TYPES tt_log TYPE STANDARD TABLE OF ts_log_entry WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_result,
             success           TYPE abap_bool,
             commands_total    TYPE i,
             commands_run      TYPE i,
             assertions_total  TYPE i,
             assertions_pass   TYPE i,
             assertions_fail   TYPE i,
             cpu_halted        TYPE abap_bool,
             error_message     TYPE string,
           END OF ts_result.

    " Load address for MS-BASIC ROM
    CONSTANTS c_load_addr TYPE i VALUE 2048.    " $0800
    " Cold start address
    CONSTANTS c_start_pc  TYPE i VALUE 10032.   " $2730

    METHODS constructor
      IMPORTING iv_rom      TYPE xstring
                it_commands TYPE zif_cpu_6502_script_ldr=>tt_commands.

    METHODS run
      RETURNING VALUE(rs_result) TYPE ts_result.

    METHODS get_log
      RETURNING VALUE(rt_log) TYPE tt_log.

    METHODS get_output
      RETURNING VALUE(rv_output) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_bus TYPE REF TO zcl_cpu_6502_bus_basic.
    DATA mo_cpu TYPE REF TO zcl_cpu_6502_cpu.
    DATA mt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands.
    DATA mt_log TYPE tt_log.
    DATA mv_seq TYPE i.
    DATA mv_full_output TYPE string.

    METHODS log
      IMPORTING iv_role TYPE c
                iv_text TYPE string.

    METHODS check_assertion
      IMPORTING iv_assertion   TYPE string
                iv_output      TYPE string
      RETURNING VALUE(rv_pass) TYPE abap_bool.

ENDCLASS.



CLASS ZCL_CPU_6502_SPEEDRUN_BASIC IMPLEMENTATION.


  METHOD constructor.
    " Create bus with MS-BASIC I/O ports
    mo_bus = NEW zcl_cpu_6502_bus_basic( ).
    mo_bus->set_start_pc( c_start_pc ).

    " Load ROM at $0800
    mo_bus->zif_cpu_6502_bus~load( iv_addr = c_load_addr iv_data = iv_rom ).

    " Create CPU (will read reset vector set by load)
    mo_cpu = NEW zcl_cpu_6502_cpu( mo_bus ).

    mt_commands = it_commands.
    mv_seq = 0.
  ENDMETHOD.


  METHOD run.
    DATA: lv_line       TYPE string,
          lv_output     TYPE string,
          lv_pass       TYPE abap_bool,
          lv_first_char TYPE c LENGTH 1.

    rs_result-commands_total = 0.
    rs_result-commands_run = 0.
    rs_result-assertions_total = 0.
    rs_result-assertions_pass = 0.
    rs_result-assertions_fail = 0.

    log( iv_role = 'S' iv_text = |MS-BASIC Speedrun starting at { c_start_pc }| ).

    " Initial run - execute until first input is needed
    " This produces the initial output (e.g., MEMORY SIZE? prompt)
    mo_cpu->run( iv_max_cycles = 10000000 ).
    lv_output = mo_bus->zif_cpu_6502_bus~get_output( ).
    mv_full_output = mv_full_output && lv_output.
    IF lv_output IS NOT INITIAL.
      log( iv_role = 'O' iv_text = lv_output ).
    ENDIF.

    LOOP AT mt_commands INTO lv_line.
      " Get first character (empty string if line is empty)
      IF strlen( lv_line ) > 0.
        lv_first_char = lv_line(1).
      ELSE.
        CLEAR lv_first_char.
      ENDIF.

      " Skip comment lines (starting with #)
      IF lv_first_char = '#'.
        CONTINUE.
      ENDIF.

      " Skip empty lines
      IF lv_line IS INITIAL.
        CONTINUE.
      ENDIF.

      " Process assertions immediately (lines starting with %)
      " Check against current output buffer
      IF lv_first_char = '%'.
        rs_result-assertions_total = rs_result-assertions_total + 1.
        lv_output = mo_bus->zif_cpu_6502_bus~get_output( ).
        lv_pass = check_assertion( iv_assertion = lv_line iv_output = lv_output ).
        IF lv_pass = abap_true.
          rs_result-assertions_pass = rs_result-assertions_pass + 1.
          log( iv_role = '+' iv_text = lv_line ).
        ELSE.
          rs_result-assertions_fail = rs_result-assertions_fail + 1.
          log( iv_role = '-' iv_text = lv_line && | [got: { lv_output }]| ).
        ENDIF.
        CONTINUE.
      ENDIF.

      " Handle $ prefix for special sequences
      " $CR = send empty line (just carriage return) - DON'T clear output buffer
      DATA lv_is_cr TYPE abap_bool VALUE abap_false.
      IF lv_first_char = '$'.
        IF lv_line = '$CR' OR lv_line = '$cr'.
          CLEAR lv_line.  " Empty input, will send just CR
          lv_is_cr = abap_true.
        ELSE.
          " Unknown special sequence, skip
          CONTINUE.
        ENDIF.
      ENDIF.

      " Everything else is a command
      rs_result-commands_total = rs_result-commands_total + 1.

      " Send command
      log( iv_role = 'I' iv_text = lv_line ).
      " Only clear output for regular commands, not $CR (which is used to flush output)
      IF lv_is_cr = abap_false.
        mo_bus->zif_cpu_6502_bus~clear_output( ).
      ENDIF.
      " Use CPU's provide_input to clear waiting state AND provide input
      mo_cpu->provide_input( lv_line ).

      " Run CPU until it needs more input or halts
      mo_cpu->run( iv_max_cycles = 10000000 ).
      rs_result-commands_run = rs_result-commands_run + 1.

      " Capture output
      lv_output = mo_bus->zif_cpu_6502_bus~get_output( ).
      mv_full_output = mv_full_output && lv_output.
      IF lv_output IS NOT INITIAL.
        log( iv_role = 'O' iv_text = lv_output ).
      ENDIF.

      " Check if CPU halted
      IF mo_cpu->get_status( )-running = abap_false.
        rs_result-cpu_halted = abap_true.
        log( iv_role = 'S' iv_text = 'CPU halted' ).
        EXIT.
      ENDIF.
    ENDLOOP.

    rs_result-success = xsdbool( rs_result-assertions_fail = 0 ).

    log( iv_role = 'S' iv_text = |Result: { rs_result-assertions_pass }/{ rs_result-assertions_total } passed| ).
  ENDMETHOD.


  METHOD get_log.
    rt_log = mt_log.
  ENDMETHOD.


  METHOD get_output.
    rv_output = mv_full_output.
  ENDMETHOD.


  METHOD log.
    DATA ls_entry TYPE ts_log_entry.
    mv_seq = mv_seq + 1.
    ls_entry-seq = mv_seq.
    ls_entry-role = iv_role.
    ls_entry-text = iv_text.
    APPEND ls_entry TO mt_log.
  ENDMETHOD.


  METHOD check_assertion.
    DATA: lv_pattern TYPE string,
          lv_check   TYPE string,
          lv_upper   TYPE string.

    IF strlen( iv_assertion ) < 3.
      rv_pass = abap_false.
      RETURN.
    ENDIF.

    " Extract pattern after %* / %= / %!
    lv_check = iv_assertion+2.

    CASE iv_assertion(2).
      WHEN '%*'.
        " Case-insensitive contains
        lv_pattern = lv_check.
        TRANSLATE lv_pattern TO UPPER CASE.
        lv_upper = iv_output.
        TRANSLATE lv_upper TO UPPER CASE.
        rv_pass = xsdbool( lv_upper CS lv_pattern ).

      WHEN '%='.
        " Case-sensitive contains
        rv_pass = xsdbool( iv_output CS lv_check ).

      WHEN '%!'.
        " Case-insensitive NOT contains
        lv_pattern = lv_check.
        TRANSLATE lv_pattern TO UPPER CASE.
        lv_upper = iv_output.
        TRANSLATE lv_upper TO UPPER CASE.
        rv_pass = xsdbool( NOT ( lv_upper CS lv_pattern ) ).

      WHEN OTHERS.
        rv_pass = abap_false.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
