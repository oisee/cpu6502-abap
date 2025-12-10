*&---------------------------------------------------------------------*
*& Report ZCPU6502_CONSOLE
*& 6502 CPU Emulator - Interactive Console for MS-BASIC
*& HTML-based display with input field
*&---------------------------------------------------------------------*
REPORT zcpu_6502_console.

CONSTANTS:
  gc_default_max_lines TYPE i VALUE 24,
  gc_font_size         TYPE i VALUE 14,
  gc_line_height       TYPE f VALUE '1.4',
  gc_default_rom       TYPE string VALUE 'ZMSBASIC.BIN'.

*----------------------------------------------------------------------*
* Local Class - HTML Display Helper
*----------------------------------------------------------------------*
CLASS lcl_html_display DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_container TYPE REF TO cl_gui_container
                  iv_max_lines TYPE i DEFAULT gc_default_max_lines,
      append_text
        IMPORTING iv_text TYPE string,
      append_to_last_line
        IMPORTING iv_text TYPE string,
      clear,
      refresh,
      free.

  PRIVATE SECTION.
    DATA: mo_html_viewer TYPE REF TO cl_gui_html_viewer,
          mt_lines       TYPE STANDARD TABLE OF string WITH EMPTY KEY,
          mv_max_lines   TYPE i.

    METHODS:
      build_html RETURNING VALUE(rv_html) TYPE string,
      escape_html
        IMPORTING iv_text        TYPE string
        RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
* Local Class - Console Controller
*----------------------------------------------------------------------*
CLASS lcl_console_controller DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_rom TYPE xstring,
      initialize_screen
        IMPORTING io_container TYPE REF TO cl_gui_custom_container,
      handle_command
        IMPORTING iv_command TYPE sy-ucomm,
      cleanup.

  PRIVATE SECTION.
    DATA: mo_bus       TYPE REF TO zcl_cpu_6502_bus_basic,
          mo_cpu       TYPE REF TO zcl_cpu_6502_cpu,
          mo_display   TYPE REF TO lcl_html_display,
          mo_container TYPE REF TO cl_gui_custom_container,
          mv_rom       TYPE xstring,
          mv_running   TYPE abap_bool,
          mv_started   TYPE abap_bool.

    CONSTANTS: c_load_addr TYPE i VALUE 2048,    " $0800
               c_start_pc  TYPE i VALUE 10032.   " $2730 MS-BASIC cold start

    METHODS:
      start_emulator,
      execute_step,
      process_input,
      calculate_max_lines RETURNING VALUE(rv_lines) TYPE i.
ENDCLASS.


*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-010.
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    PARAMETERS: r_smw0 RADIOBUTTON GROUP src DEFAULT 'X' USER-COMMAND src,
                r_file RADIOBUTTON GROUP src.
  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: p_rom TYPE wwwdatatab-objid DEFAULT 'ZMSBASIC.BIN'
                AS LISTBOX VISIBLE LENGTH 40 MODIF ID smw.
  SELECTION-SCREEN END OF BLOCK b2.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_path TYPE string LOWER CASE MODIF ID fil.
  SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b10.


*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gv_input TYPE string,
      gv_ok    TYPE sy-ucomm.

DATA: go_controller TYPE REF TO lcl_console_controller,
      go_container  TYPE REF TO cl_gui_custom_container,
      gv_rom        TYPE xstring.

" Debug event log (disabled - set gv_debug = abap_true to enable)
DATA: gt_log TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      gv_log_seq TYPE i VALUE 0,
      gv_debug TYPE abap_bool VALUE abap_false.

FORM add_log USING iv_msg TYPE string.
  CHECK gv_debug = abap_true.
  gv_log_seq = gv_log_seq + 1.
  DATA lv_entry TYPE string.
  lv_entry = |{ gv_log_seq WIDTH = 3 }: { iv_msg }|.
  APPEND lv_entry TO gt_log.
ENDFORM.


*----------------------------------------------------------------------*
* Selection Screen Events
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  " Build ROM list for dropdown from SMW0
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  DATA(lo_loader) = NEW zcl_cpu_6502_rom_loader_smw0( iv_pattern = '*.BIN' ).
  DATA(lt_roms) = lo_loader->zif_cpu_6502_rom_loader~list_roms( ).
  LOOP AT lt_roms INTO DATA(ls_rom).
    ls_value-key = ls_rom-id.
    ls_value-text = |{ ls_rom-name } ({ ls_rom-size } bytes)|.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING id     = 'P_ROM'
              values = lt_values.

  " Enable/disable fields based on radio button
  LOOP AT SCREEN.
    IF screen-group1 = 'SMW'.
      screen-active = COND #( WHEN r_smw0 = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 = 'FIL'.
      screen-active = COND #( WHEN r_file = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  DATA: lt_filetab TYPE filetable,
        lv_rc      TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title      = 'Select ROM File'
      file_filter       = 'Binary (*.bin)|*.bin|All Files (*.*)|*.*'
      default_extension = 'bin'
    CHANGING
      file_table        = lt_filetab
      rc                = lv_rc ).

  IF lv_rc >= 1.
    READ TABLE lt_filetab INDEX 1 INTO DATA(ls_file).
    p_path = ls_file-filename.
  ENDIF.


*----------------------------------------------------------------------*
* Class Implementations
*----------------------------------------------------------------------*
CLASS lcl_html_display IMPLEMENTATION.

  METHOD constructor.
    mv_max_lines = iv_max_lines.
    CREATE OBJECT mo_html_viewer
      EXPORTING parent = io_container.
  ENDMETHOD.

  METHOD append_text.
    DATA: lt_new_lines TYPE STANDARD TABLE OF string,
          lv_text      TYPE string,
          lv_ends_with_newline TYPE abap_bool.

    lv_text = iv_text.
    " Handle CR/LF variations
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_text
      WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1) IN lv_text
      WITH cl_abap_char_utilities=>newline.

    " Check if text ends with newline (SPLIT doesn't preserve trailing empty)
    DATA(lv_len) = strlen( lv_text ).
    IF lv_len > 0.
      DATA(lv_last) = lv_text+0(1).
      lv_last = substring( val = lv_text off = lv_len - 1 len = 1 ).
      lv_ends_with_newline = xsdbool( lv_last = cl_abap_char_utilities=>newline ).
    ENDIF.

    SPLIT lv_text AT cl_abap_char_utilities=>newline INTO TABLE lt_new_lines.

    LOOP AT lt_new_lines INTO DATA(lv_line).
      APPEND lv_line TO mt_lines.
    ENDLOOP.

    " If text ended with newline, add empty line for next input
    IF lv_ends_with_newline = abap_true.
      APPEND '' TO mt_lines.
    ENDIF.
  ENDMETHOD.

  METHOD append_to_last_line.
    DATA(lv_count) = lines( mt_lines ).
    IF lv_count > 0.
      FIELD-SYMBOLS <fs_line> TYPE string.
      READ TABLE mt_lines INDEX lv_count ASSIGNING <fs_line>.
      IF sy-subrc = 0.
        <fs_line> = <fs_line> && iv_text.
      ENDIF.
    ELSE.
      APPEND iv_text TO mt_lines.
    ENDIF.
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_lines.
  ENDMETHOD.

  METHOD refresh.
    DATA: lt_html TYPE TABLE OF char1024,
          lv_html TYPE string,
          lv_url  TYPE c LENGTH 250.

    lv_html = build_html( ).

    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) > 1024.
        APPEND lv_html+0(1024) TO lt_html.
        lv_html = lv_html+1024.
      ELSE.
        APPEND lv_html TO lt_html.
        CLEAR lv_html.
      ENDIF.
    ENDWHILE.

    mo_html_viewer->load_data(
      IMPORTING assigned_url = lv_url
      CHANGING  data_table   = lt_html ).
    mo_html_viewer->show_url( url = lv_url ).
  ENDMETHOD.

  METHOD build_html.
    DATA: lv_content TYPE string,
          lv_start   TYPE i,
          lv_total   TYPE i.

    lv_total = lines( mt_lines ).
    IF lv_total > mv_max_lines.
      lv_start = lv_total - mv_max_lines + 1.
    ELSE.
      lv_start = 1.
    ENDIF.

    LOOP AT mt_lines INTO DATA(lv_line) FROM lv_start.
      lv_content = lv_content && escape_html( lv_line ) && |<br>|.
    ENDLOOP.

    " Green on black terminal style with pre tag for preserving spaces
    rv_html =
      |<html><head><style>| &&
      |body \{ background-color: #000000; color: #00ff00; | &&
      |font-family: 'Courier New', monospace; font-size: { gc_font_size }px; | &&
      |padding: 10px; margin: 0; \}| &&
      |pre \{ margin: 0; white-space: pre-wrap; word-wrap: break-word; | &&
      |line-height: { gc_line_height }; \}| &&
      |</style></head><body>| &&
      |<pre>{ lv_content }</pre>| &&
      |</body></html>|.
  ENDMETHOD.

  METHOD escape_html.
    rv_text = iv_text.
    " Escape HTML special characters
    REPLACE ALL OCCURRENCES OF '&' IN rv_text WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN rv_text WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN rv_text WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_text WITH '&quot;'.
    " Convert spaces to &nbsp; to preserve multiple spaces
    REPLACE ALL OCCURRENCES OF ` ` IN rv_text WITH '&nbsp;'.
  ENDMETHOD.

  METHOD free.
    IF mo_html_viewer IS BOUND.
      mo_html_viewer->free( ).
      FREE mo_html_viewer.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_console_controller IMPLEMENTATION.

  METHOD constructor.
    mv_rom = iv_rom.
    mv_running = abap_false.
    mv_started = abap_false.
  ENDMETHOD.

  METHOD initialize_screen.
    mo_container = io_container.

    DATA(lv_max_lines) = calculate_max_lines( ).

    mo_display = NEW lcl_html_display(
      io_container = io_container
      iv_max_lines = lv_max_lines ).

    start_emulator( ).
  ENDMETHOD.

  METHOD calculate_max_lines.
    IF sy-srows > 10.
      rv_lines = sy-srows - 7.
    ELSE.
      rv_lines = gc_default_max_lines.
    ENDIF.
    IF rv_lines < 10.
      rv_lines = 10.
    ENDIF.
  ENDMETHOD.

  METHOD handle_command.
    DATA lv_log TYPE string.
    lv_log = |handle_command: iv_command='{ iv_command }'|.
    PERFORM add_log USING lv_log.

    CASE iv_command.
      WHEN 'EXIT' OR 'BACK' OR 'CANC' OR '&F03' OR '&F15' OR '&F12'.
        PERFORM add_log USING 'handle_command: EXIT - showing log'.
        IF gv_debug = abap_true.
          cl_demo_output=>write( '=== EVENT LOG ===' ).
          cl_demo_output=>write( gt_log ).
          cl_demo_output=>display( ).
        ENDIF.
        cleanup( ).
        LEAVE PROGRAM.

      WHEN 'ENTER' OR '' OR 'ONLI'.
        PERFORM add_log USING 'handle_command: calling process_input'.
        process_input( ).
        PERFORM add_log USING 'handle_command: process_input done'.
    ENDCASE.
  ENDMETHOD.

  METHOD start_emulator.
    DATA lv_log TYPE string.

    PERFORM add_log USING 'start_emulator: BEGIN'.

    IF mv_rom IS INITIAL.
      mo_display->append_text( |Error: No ROM file loaded| ).
      mo_display->refresh( ).
      RETURN.
    ENDIF.

    TRY.
        " Create bus and load ROM
        PERFORM add_log USING 'start_emulator: Creating bus'.
        mo_bus = NEW zcl_cpu_6502_bus_basic( ).
        mo_bus->set_start_pc( c_start_pc ).
        mo_bus->zif_cpu_6502_bus~load( iv_addr = c_load_addr iv_data = mv_rom ).

        " Create CPU
        PERFORM add_log USING 'start_emulator: Creating CPU'.
        mo_cpu = NEW zcl_cpu_6502_cpu( mo_bus ).

        mv_started = abap_true.
        mv_running = abap_true.
        mo_display->clear( ).

        mo_display->append_text( |6502 Emulator - MS-BASIC| ).
        mo_display->append_text( |ROM: { xstrlen( mv_rom ) } bytes| ).
        mo_display->append_text( |--------------------------------| ).

        PERFORM add_log USING 'start_emulator: Initial execute_step'.
        execute_step( ).
        PERFORM add_log USING 'start_emulator: Initial execute_step done'.

      CATCH cx_root INTO DATA(lx_error).
        lv_log = |start_emulator: ERROR { lx_error->get_text( ) }|.
        PERFORM add_log USING lv_log.
        mo_display->append_text( |Error: { lx_error->get_text( ) }| ).
        mo_display->refresh( ).
    ENDTRY.

    PERFORM add_log USING 'start_emulator: END'.
  ENDMETHOD.

  METHOD execute_step.
    DATA lv_log TYPE string.
    DATA ls_status TYPE zcl_cpu_6502_cpu=>ts_status.

    CHECK mo_cpu IS BOUND.

    ls_status = mo_cpu->get_status( ).
    lv_log = |execute_step: BEFORE run - running={ ls_status-running } waiting={ ls_status-waiting }|.
    PERFORM add_log USING lv_log.

    mo_cpu->run( iv_max_cycles = 10000000 ).

    ls_status = mo_cpu->get_status( ).
    lv_log = |execute_step: AFTER run - running={ ls_status-running } waiting={ ls_status-waiting }|.
    PERFORM add_log USING lv_log.

    DATA(lv_output) = mo_bus->zif_cpu_6502_bus~get_output( ).
    DATA(lv_out_len) = strlen( lv_output ).
    lv_log = |execute_step: output len={ lv_out_len }|.
    PERFORM add_log USING lv_log.

    IF lv_output IS NOT INITIAL.
      mo_display->append_text( lv_output ).
    ENDIF.

    " Clear output buffer AFTER we've retrieved it
    mo_bus->zif_cpu_6502_bus~clear_output( ).

    mv_running = ls_status-running.

    IF ls_status-waiting = abap_false AND ls_status-running = abap_false.
      mo_display->append_text( |[CPU Halted]| ).
    ENDIF.

    mo_display->refresh( ).
    PERFORM add_log USING 'execute_step: done'.
  ENDMETHOD.

  METHOD process_input.
    DATA lv_cmd TYPE string.
    DATA lv_log TYPE string.
    DATA ls_status TYPE zcl_cpu_6502_cpu=>ts_status.

    lv_cmd = gv_input.
    CLEAR gv_input.

    lv_log = |process_input: lv_cmd='{ lv_cmd }'|.
    PERFORM add_log USING lv_log.

    " Handle meta commands (only if non-empty)
    IF lv_cmd IS NOT INITIAL.
      DATA(lv_upper) = to_upper( lv_cmd ).
      CASE lv_upper.
        WHEN '/Q' OR '/QUIT' OR '/EXIT'.
          mo_display->append_text( |Goodbye!| ).
          mo_display->refresh( ).
          PERFORM add_log USING 'process_input: EXIT - showing log'.
          IF gv_debug = abap_true.
            cl_demo_output=>write( '=== EVENT LOG ===' ).
            cl_demo_output=>write( gt_log ).
            cl_demo_output=>display( ).
          ENDIF.
          cleanup( ).
          LEAVE PROGRAM.

        WHEN '/HELP' OR '/?'.
          mo_display->append_text( |Commands: /quit /help /reset| ).
          mo_display->append_text( |MS-BASIC: PRINT, LIST, RUN, NEW, etc.| ).
          mo_display->refresh( ).
          RETURN.

        WHEN '/RESET'.
          mo_display->clear( ).
          start_emulator( ).
          RETURN.
      ENDCASE.
    ENDIF.

    " Always provide input to CPU (even empty = just CR for prompts)
    IF mo_cpu IS BOUND AND mv_running = abap_true.
      ls_status = mo_cpu->get_status( ).
      lv_log = |process_input: CPU before - running={ ls_status-running } waiting={ ls_status-waiting }|.
      PERFORM add_log USING lv_log.

      " Echo user input to display (BASIC doesn't echo, we must)
      IF lv_cmd IS NOT INITIAL.
        mo_display->append_to_last_line( lv_cmd ).
      ENDIF.

      " DON'T clear output here - clear it AFTER execute_step retrieves it
      " This prevents losing output from previous cycle
      " PERFORM add_log USING 'process_input: clear_output'.
      " mo_bus->zif_cpu_6502_bus~clear_output( ).

      lv_log = |process_input: provide_input('{ lv_cmd }')|.
      PERFORM add_log USING lv_log.
      mo_cpu->provide_input( lv_cmd ).

      ls_status = mo_cpu->get_status( ).
      lv_log = |process_input: CPU after provide - running={ ls_status-running } waiting={ ls_status-waiting }|.
      PERFORM add_log USING lv_log.

      PERFORM add_log USING 'process_input: calling execute_step'.
      execute_step( ).
      PERFORM add_log USING 'process_input: execute_step done'.
    ELSE.
      PERFORM add_log USING 'process_input: CPU not bound or not running'.
    ENDIF.
  ENDMETHOD.

  METHOD cleanup.
    IF mo_display IS BOUND.
      mo_display->free( ).
      FREE mo_display.
    ENDIF.
    FREE mo_cpu.
    FREE mo_bus.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Load ROM based on selection
  IF r_smw0 = abap_true.
    IF p_rom IS INITIAL.
      MESSAGE 'Please select a ROM from SMW0' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    DATA(lo_smw0_loader) = NEW zcl_cpu_6502_rom_loader_smw0( iv_pattern = '*.BIN' ).
    gv_rom = lo_smw0_loader->zif_cpu_6502_rom_loader~load( CONV #( p_rom ) ).
  ELSE.
    IF p_path IS INITIAL.
      MESSAGE 'Please specify a ROM file path' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    DATA(lo_file_loader) = NEW zcl_cpu_6502_rom_loader_file( ).
    gv_rom = lo_file_loader->zif_cpu_6502_rom_loader~load( p_path ).
  ENDIF.

  IF gv_rom IS INITIAL.
    MESSAGE 'Failed to load ROM file' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  go_controller = NEW lcl_console_controller( gv_rom ).
  CALL SCREEN 100.


*----------------------------------------------------------------------*
* Screen 100 PBO
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA lv_log TYPE string.
  lv_log = |PBO: gv_input='{ gv_input }' gv_ok='{ gv_ok }'|.
  PERFORM add_log USING lv_log.

  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE'.

  IF go_container IS NOT BOUND.
    PERFORM add_log USING 'PBO: Creating container'.
    CREATE OBJECT go_container
      EXPORTING container_name = 'HTML_CONTAINER'
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc = 0.
      PERFORM add_log USING 'PBO: Initializing screen'.
      go_controller->initialize_screen( go_container ).
      PERFORM add_log USING 'PBO: Screen initialized'.
    ENDIF.
  ENDIF.

  PERFORM add_log USING 'PBO: Done'.
ENDMODULE.


*----------------------------------------------------------------------*
* Screen 100 PAI
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA lv_pai_log TYPE string.

  " Log state at PAI entry
  lv_pai_log = |PAI: ENTRY gv_input='{ gv_input }' gv_ok='{ gv_ok }'|.
  PERFORM add_log USING lv_pai_log.

  " Read the input field value directly from dynpro
  " This ensures we get the current value even without FIELD statement
  DATA lv_dynp_input TYPE string.
  DATA lt_dynpfields TYPE STANDARD TABLE OF dynpread WITH EMPTY KEY.
  DATA ls_dynpfield TYPE dynpread.

  ls_dynpfield-fieldname = 'GV_INPUT'.
  APPEND ls_dynpfield TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynpfields
    EXCEPTIONS
      OTHERS     = 0.

  READ TABLE lt_dynpfields INDEX 1 INTO ls_dynpfield.
  IF sy-subrc = 0.
    gv_input = ls_dynpfield-fieldvalue.
  ENDIF.

  lv_pai_log = |PAI: DYNP_READ='{ ls_dynpfield-fieldvalue }' gv_input='{ gv_input }'|.
  PERFORM add_log USING lv_pai_log.

  lv_pai_log = |PAI: handle_command('{ gv_ok }')|.
  PERFORM add_log USING lv_pai_log.

  go_controller->handle_command( gv_ok ).

  PERFORM add_log USING 'PAI: handle_command done'.

  CLEAR gv_ok.
  CLEAR gv_input.

  PERFORM add_log USING 'PAI: Done'.
ENDMODULE.
