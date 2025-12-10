CLASS zcl_cpu_00_speedrun_basic DEFINITION
  PUBLIC
  CREATE PUBLIC.
************************************************************************
* MS-BASIC Speedrun Test Class
* Runs MS-BASIC with scripted input and verifies expected output
* Uses zcl_cpu_00_bus_basic with I/O at $FFF0-$FFF3
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
                it_commands TYPE zif_cpu_00_script_loader=>tt_commands.

    METHODS run
      RETURNING VALUE(rs_result) TYPE ts_result.

    METHODS get_log
      RETURNING VALUE(rt_log) TYPE tt_log.

    METHODS get_output
      RETURNING VALUE(rv_output) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_bus TYPE REF TO zcl_cpu_00_bus_basic.
    DATA mo_cpu TYPE REF TO zcl_cpu_00_cpu.
    DATA mt_commands TYPE zif_cpu_00_script_loader=>tt_commands.
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



CLASS zcl_cpu_00_speedrun_basic IMPLEMENTATION.

  METHOD constructor.
    " Create bus with MS-BASIC I/O ports
    mo_bus = NEW zcl_cpu_00_bus_basic( ).
    mo_bus->set_start_pc( c_start_pc ).

    " Load ROM at $0800
    mo_bus->zif_cpu_00_bus~load( iv_addr = c_load_addr iv_data = iv_rom ).

    " Create CPU (will read reset vector set by load)
    mo_cpu = NEW zcl_cpu_00_cpu( mo_bus ).

    mt_commands = it_commands.
    mv_seq = 0.
  ENDMETHOD.

  METHOD run.
    DATA: lv_line       TYPE string,
          lv_output     TYPE string,
          lt_assertions TYPE STANDARD TABLE OF string,
          lv_assertion  TYPE string,
          lv_pass       TYPE abap_bool.

    rs_result-commands_total = 0.
    rs_result-commands_run = 0.
    rs_result-assertions_total = 0.
    rs_result-assertions_pass = 0.
    rs_result-assertions_fail = 0.

    log( iv_role = 'S' iv_text = |MS-BASIC Speedrun starting at { c_start_pc }| ).

    LOOP AT mt_commands INTO lv_line.
      " Skip empty lines and comments
      IF lv_line IS INITIAL OR ( strlen( lv_line ) > 0 AND lv_line(1) = '#' ).
        CONTINUE.
      ENDIF.

      " Collect assertions (lines starting with %)
      IF lv_line(1) = '%'.
        APPEND lv_line TO lt_assertions.
        rs_result-assertions_total = rs_result-assertions_total + 1.
        CONTINUE.
      ENDIF.

      rs_result-commands_total = rs_result-commands_total + 1.

      " Check pending assertions before next command
      IF lines( lt_assertions ) > 0.
        lv_output = mo_bus->zif_cpu_00_bus~get_output( ).
        LOOP AT lt_assertions INTO lv_assertion.
          lv_pass = check_assertion( iv_assertion = lv_assertion iv_output = lv_output ).
          IF lv_pass = abap_true.
            rs_result-assertions_pass = rs_result-assertions_pass + 1.
            log( iv_role = '+' iv_text = lv_assertion ).
          ELSE.
            rs_result-assertions_fail = rs_result-assertions_fail + 1.
            log( iv_role = '-' iv_text = lv_assertion && | [got: { lv_output }]| ).
          ENDIF.
        ENDLOOP.
        CLEAR lt_assertions.
      ENDIF.

      " Send command
      log( iv_role = 'I' iv_text = lv_line ).
      mo_bus->zif_cpu_00_bus~clear_output( ).
      mo_bus->zif_cpu_00_bus~provide_input( lv_line ).

      " Run CPU until it needs more input or halts
      mo_cpu->run( iv_max_cycles = 50000000 ).
      rs_result-commands_run = rs_result-commands_run + 1.

      " Capture output
      lv_output = mo_bus->zif_cpu_00_bus~get_output( ).
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

    " Check final assertions
    IF lines( lt_assertions ) > 0.
      lv_output = mo_bus->zif_cpu_00_bus~get_output( ).
      LOOP AT lt_assertions INTO lv_assertion.
        lv_pass = check_assertion( iv_assertion = lv_assertion iv_output = lv_output ).
        IF lv_pass = abap_true.
          rs_result-assertions_pass = rs_result-assertions_pass + 1.
          log( iv_role = '+' iv_text = lv_assertion ).
        ELSE.
          rs_result-assertions_fail = rs_result-assertions_fail + 1.
          log( iv_role = '-' iv_text = lv_assertion && | [got: { lv_output }]| ).
        ENDIF.
      ENDLOOP.
    ENDIF.

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

    IF strlen( iv_assertion ) < 2.
      rv_pass = abap_false.
      RETURN.
    ENDIF.

    lv_check = iv_assertion+1.

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