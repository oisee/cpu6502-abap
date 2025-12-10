CLASS zcl_cpu_6502_speedrun_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.

  PRIVATE SECTION.
    METHODS test_assertion_contains FOR TESTING.
    METHODS test_assertion_exact FOR TESTING.
    METHODS test_assertion_not_contains FOR TESTING.
    METHODS test_assertion_short FOR TESTING.
    METHODS test_dollar_cr_handling FOR TESTING.
    METHODS test_comment_skip FOR TESTING.
    METHODS test_empty_line_skip FOR TESTING.

ENDCLASS.



CLASS ZCL_CPU_6502_SPEEDRUN_TEST IMPLEMENTATION.


  METHOD test_assertion_contains.
    " Test %* case-insensitive contains
    DATA: lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands,
          lv_rom      TYPE xstring.

    " Create minimal ROM (just NOP + BRK)
    lv_rom = 'EA00'.

    APPEND '%*HELLO' TO lt_commands.

    DATA(lo_speedrun) = NEW zcl_cpu_6502_speedrun_basic(
      iv_rom      = lv_rom
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " With empty output, %*HELLO should fail
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-assertions_fail
      exp = 1
      msg = 'Assertion should fail when pattern not found' ).
  ENDMETHOD.


  METHOD test_assertion_exact.
    " Test %= case-sensitive contains
    DATA: lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands,
          lv_rom      TYPE xstring.

    lv_rom = 'EA00'.
    APPEND '%=TEST' TO lt_commands.

    DATA(lo_speedrun) = NEW zcl_cpu_6502_speedrun_basic(
      iv_rom      = lv_rom
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-assertions_fail
      exp = 1
      msg = 'Case-sensitive assertion should fail' ).
  ENDMETHOD.


  METHOD test_assertion_not_contains.
    " Test %! case-insensitive NOT contains
    DATA: lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands,
          lv_rom      TYPE xstring.

    lv_rom = 'EA00'.
    APPEND '%!ERROR' TO lt_commands.

    DATA(lo_speedrun) = NEW zcl_cpu_6502_speedrun_basic(
      iv_rom      = lv_rom
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " With empty output, %!ERROR should pass (ERROR is not in output)
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-assertions_pass
      exp = 1
      msg = 'NOT contains should pass when pattern absent' ).
  ENDMETHOD.


  METHOD test_assertion_short.
    " Test assertion too short (< 3 chars)
    DATA: lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands,
          lv_rom      TYPE xstring.

    lv_rom = 'EA00'.
    APPEND '%*' TO lt_commands.  " Only 2 chars, should fail

    DATA(lo_speedrun) = NEW zcl_cpu_6502_speedrun_basic(
      iv_rom      = lv_rom
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-assertions_fail
      exp = 1
      msg = 'Short assertion should fail' ).
  ENDMETHOD.


  METHOD test_dollar_cr_handling.
    " Test that $CR sends empty input
    DATA: lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands,
          lv_rom      TYPE xstring.

    " Create minimal ROM that just returns (RTS opcode = $60)
    lv_rom = '6000'.

    APPEND '$CR' TO lt_commands.

    DATA(lo_speedrun) = NEW zcl_cpu_6502_speedrun_basic(
      iv_rom      = lv_rom
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " $CR should be counted as a command
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-commands_total
      exp = 1
      msg = '$CR should be counted as command' ).
  ENDMETHOD.


  METHOD test_comment_skip.
    " Test that # comments are skipped
    DATA: lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands,
          lv_rom      TYPE xstring.

    lv_rom = 'EA00'.

    APPEND '# This is a comment' TO lt_commands.
    APPEND '#Another comment' TO lt_commands.

    DATA(lo_speedrun) = NEW zcl_cpu_6502_speedrun_basic(
      iv_rom      = lv_rom
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " Comments should not be counted
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-commands_total
      exp = 0
      msg = 'Comments should be skipped' ).
  ENDMETHOD.


  METHOD test_empty_line_skip.
    " Test that empty lines are skipped
    DATA: lt_commands TYPE zif_cpu_6502_script_ldr=>tt_commands,
          lv_rom      TYPE xstring.

    lv_rom = 'EA00'.

    APPEND '' TO lt_commands.
    APPEND '' TO lt_commands.

    DATA(lo_speedrun) = NEW zcl_cpu_6502_speedrun_basic(
      iv_rom      = lv_rom
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " Empty lines should be skipped
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-commands_total
      exp = 0
      msg = 'Empty lines should be skipped' ).
  ENDMETHOD.
ENDCLASS.
