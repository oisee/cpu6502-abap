*&---------------------------------------------------------------------*
*& Report ZCPU6502_BASIC_DIAG
*& Quick diagnostic for MS-BASIC test
*&---------------------------------------------------------------------*
REPORT zcpu_6502_basic_diag.

START-OF-SELECTION.
  DATA(ls_result) = zcl_cpu_6502_basic_test=>run_test( ).
  DATA(lt_log) = zcl_cpu_6502_basic_test=>get_log( ).

  IF ls_result-error_message IS NOT INITIAL.
    WRITE: / |ERROR: { ls_result-error_message }|.
    RETURN.
  ENDIF.

  WRITE: / |Pass: { ls_result-assertions_pass }/{ ls_result-assertions_total }|.
  WRITE: / |Success: { ls_result-success }|.
  SKIP.

  LOOP AT lt_log INTO DATA(ls_log).
    WRITE: / |[{ ls_log-role }] { ls_log-text }|.
  ENDLOOP.
