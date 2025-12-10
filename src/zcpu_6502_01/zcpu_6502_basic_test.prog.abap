*&---------------------------------------------------------------------*
*& Report ZCPU6502_BASIC_TEST
*& Test MS-BASIC on 6502 Emulator
*&---------------------------------------------------------------------*
*& Upload MSBASIC.BIN to SMW0 (Binary data) before running
*&---------------------------------------------------------------------*
REPORT zcpu_6502_basic_test.

DATA: gs_result   TYPE zcl_cpu_6502_speedrun_basic=>ts_result,
      gt_log      TYPE zcl_cpu_6502_speedrun_basic=>tt_log.

START-OF-SELECTION.

  WRITE: / 'Running MS-BASIC test...'.
  SKIP.

  " Run test via helper class
  gs_result = zcl_cpu_6502_basic_test=>run_test( ).
  gt_log = zcl_cpu_6502_basic_test=>get_log( ).

  IF gs_result-error_message IS NOT INITIAL.
    WRITE: / |ERROR: { gs_result-error_message }|.
    RETURN.
  ENDIF.

  " Display results
  WRITE: / '=== MS-BASIC Test Results ==='.
  SKIP.

  LOOP AT gt_log INTO DATA(ls_log).
    CASE ls_log-role.
      WHEN 'S'.  " Status
        WRITE: / |[STATUS] { ls_log-text }|.
      WHEN 'I'.  " Input
        WRITE: / |[INPUT]  { ls_log-text }|.
      WHEN 'O'.  " Output
        WRITE: / |[OUTPUT] { ls_log-text }|.
      WHEN '+'.  " Pass
        FORMAT COLOR COL_POSITIVE.
        WRITE: / |[PASS]   { ls_log-text }|.
        FORMAT COLOR OFF.
      WHEN '-'.  " Fail
        FORMAT COLOR COL_NEGATIVE.
        WRITE: / |[FAIL]   { ls_log-text }|.
        FORMAT COLOR OFF.
      WHEN 'E'.  " Error
        FORMAT COLOR COL_NEGATIVE.
        WRITE: / |[ERROR]  { ls_log-text }|.
        FORMAT COLOR OFF.
    ENDCASE.
  ENDLOOP.

  SKIP.
  WRITE: / '=== Summary ==='.
  WRITE: / |Commands: { gs_result-commands_run }/{ gs_result-commands_total }|.
  WRITE: / |Assertions: { gs_result-assertions_pass }/{ gs_result-assertions_total }|.

  IF gs_result-success = abap_true.
    FORMAT COLOR COL_POSITIVE.
    WRITE: / 'SUCCESS! MS-BASIC is working!'.
    FORMAT COLOR OFF.
  ELSE.
    FORMAT COLOR COL_NEGATIVE.
    WRITE: / 'FAILURE - some tests failed'.
    FORMAT COLOR OFF.
  ENDIF.
