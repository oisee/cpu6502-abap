*&---------------------------------------------------------------------*
*& Report ZCPU6502_SPEEDRUN
*& 6502 CPU Emulator - MS-BASIC Speedrun Test
*& Synced with ZCPU6502_CONSOLE for consistent ROM loading
*&---------------------------------------------------------------------*
REPORT zcpu_6502_speedrun.

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

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_script TYPE string LOWER CASE DEFAULT 'BASIC_TEST'.
SELECTION-SCREEN END OF BLOCK b4.


*----------------------------------------------------------------------*
* Selection Screen Events
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  " Build ROM list for dropdown from SMW0
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  DATA(lo_ldr) = NEW zcl_cpu_6502_rom_loader_smw0( iv_pattern = '*.BIN' ).
  DATA(lt_roms) = lo_ldr->zif_cpu_6502_rom_loader~list_roms( ).
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
* Main Processing
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM main.

FORM main.
  DATA: lv_rom      TYPE xstring,
        lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands,
        ls_result   TYPE zcl_cpu_6502_speedrun_basic=>ts_result,
        lt_log      TYPE zcl_cpu_6502_speedrun_basic=>tt_log,
        ls_log      TYPE zcl_cpu_6502_speedrun_basic=>ts_log_entry.

  " Load ROM based on selection (same as ZCPU6502_CONSOLE)
  IF r_smw0 = abap_true.
    IF p_rom IS INITIAL.
      WRITE: / 'ERROR: Please select a ROM from SMW0'.
      RETURN.
    ENDIF.
    DATA(lo_smw0_ldr) = NEW zcl_cpu_6502_rom_loader_smw0( iv_pattern = '*.BIN' ).
    lv_rom = lo_smw0_ldr->zif_cpu_6502_rom_loader~load( CONV #( p_rom ) ).
  ELSE.
    IF p_path IS INITIAL.
      WRITE: / 'ERROR: Please specify a ROM file path'.
      RETURN.
    ENDIF.
    DATA(lo_file_ldr) = NEW zcl_cpu_6502_rom_loader_file( ).
    lv_rom = lo_file_ldr->zif_cpu_6502_rom_loader~load( p_path ).
  ENDIF.

  IF lv_rom IS INITIAL.
    WRITE: / 'ERROR: Failed to load ROM file'.
    RETURN.
  ENDIF.

  " Build test script based on selection or use built-in
  IF p_script = 'BASIC_TEST' OR p_script IS INITIAL.
    " Built-in MS-BASIC test script
    " Assertions check output from PREVIOUS command (or initial run)
    " So assertions come BEFORE the next command
    "
    " Initial run produces: "MEMORY SIZE?"
    APPEND '%*MEMORY SIZE' TO lt_commands.          " Assert we saw prompt from initial run
    APPEND '10000' TO lt_commands.                  " Send 10000 for memory size
    APPEND '%*TERMINAL WIDTH' TO lt_commands.       " Assert we saw prompt
    APPEND '42' TO lt_commands.                     " Send 42 for terminal width
    APPEND '%*BYTES FREE' TO lt_commands.           " Assert memory info shown
    APPEND '%*OK' TO lt_commands.                   " Assert READY prompt
    APPEND '5 PRINT "HELLO WORLD"' TO lt_commands.   " Store string print in program (immediate PRINT has MS-BASIC bug)
    APPEND 'RUN' TO lt_commands.                    " Run to execute line 5
    APPEND '$CR' TO lt_commands.                    " Extra CR to flush RUN output
    APPEND '%*HELLO WORLD' TO lt_commands.          " Assert output
    APPEND 'NEW' TO lt_commands.                    " Clear program for next tests
    APPEND 'PRINT 2+2' TO lt_commands.              " Produces " 4 " + OK
    APPEND '%*4' TO lt_commands.
    APPEND 'PRINT 10*5' TO lt_commands.             " Produces " 50 " + OK
    APPEND '%*50' TO lt_commands.
    APPEND '10 PRINT "LINE 10"' TO lt_commands.     " Produces OK
    APPEND '20 PRINT "LINE 20"' TO lt_commands.     " Produces OK
    APPEND 'LIST' TO lt_commands.                   " Verify program stored
    APPEND '$CR' TO lt_commands.                    " Extra CR to flush LIST output
    APPEND '%*10 PRINT' TO lt_commands.             " Assert line 10 exists
    APPEND '%*20 PRINT' TO lt_commands.             " Assert line 20 exists
    APPEND 'RUN' TO lt_commands.                    " Produces "LINE 10" + "LINE 20" + OK
    APPEND '$CR' TO lt_commands.                    " Extra CR to flush RUN output
    APPEND '%*LINE 10' TO lt_commands.
    APPEND '%*LINE 20' TO lt_commands.
  ELSE.
    " Try to load from file
    DATA(lo_script_ldr) = NEW zcl_cpu_6502_script_ldr_file( p_script ).
    lt_commands = lo_script_ldr->zif_cpu_6502_script_ldr~load( ).
    IF lines( lt_commands ) = 0.
      WRITE: / 'ERROR: Failed to load script file:', p_script.
      RETURN.
    ENDIF.
  ENDIF.

  " Display header
  WRITE: / '6502 MS-BASIC Speedrun Test'.
  WRITE: / 'ROM:', xstrlen( lv_rom ), 'bytes'.
  WRITE: / 'Script:', lines( lt_commands ), 'commands'.
  ULINE.

  " Run speedrun using MS-BASIC specific class
  DATA(lo_speedrun) = NEW zcl_cpu_6502_speedrun_basic(
    iv_rom      = lv_rom
    it_commands = lt_commands ).

  ls_result = lo_speedrun->run( ).
  lt_log = lo_speedrun->get_log( ).

  " Display execution log
  WRITE: / 'Execution Log:'.
  WRITE: / '---'.
  LOOP AT lt_log INTO ls_log.
    CASE ls_log-role.
      WHEN 'I'.
        WRITE: / '>>>', ls_log-text.
      WHEN 'O'.
        WRITE: / ls_log-text.
      WHEN '+'.
        WRITE: / '[PASS]', ls_log-text COLOR COL_POSITIVE.
      WHEN '-'.
        WRITE: / '[FAIL]', ls_log-text COLOR COL_NEGATIVE.
      WHEN 'S'.
        WRITE: / '[INFO]', ls_log-text COLOR COL_TOTAL.
      WHEN 'E'.
        WRITE: / '[ERROR]', ls_log-text COLOR COL_NEGATIVE.
    ENDCASE.
  ENDLOOP.

  " Display summary
  ULINE.
  WRITE: / 'Results:'.
  WRITE: / '  Commands:', ls_result-commands_run, '/', ls_result-commands_total.
  WRITE: / '  Assertions:', ls_result-assertions_pass, '/', ls_result-assertions_total, 'passed'.

  IF ls_result-success = abap_true.
    WRITE: / 'TEST PASSED' COLOR COL_POSITIVE.
  ELSE.
    WRITE: / 'TEST FAILED' COLOR COL_NEGATIVE.
    WRITE: / '  Failures:', ls_result-assertions_fail.
  ENDIF.

  IF ls_result-cpu_halted = abap_true.
    WRITE: / '  (CPU halted during execution)'.
  ENDIF.
ENDFORM.
