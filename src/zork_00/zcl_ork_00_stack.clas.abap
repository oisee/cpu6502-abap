*&---------------------------------------------------------------------*
*& Z-Machine Stack - Direct port from z3_minimal.py
*& Handles call frames and evaluation stack
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_stack DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_frame,
        return_pc  TYPE i,
        return_var TYPE i,
        locals     TYPE STANDARD TABLE OF i WITH EMPTY KEY,
        eval_stack TYPE STANDARD TABLE OF i WITH EMPTY KEY,
      END OF ty_frame.

    METHODS constructor.

    " Evaluation stack
    METHODS push IMPORTING iv_val TYPE i.
    METHODS pop RETURNING VALUE(rv_val) TYPE i.
    METHODS peek RETURNING VALUE(rv_val) TYPE i.

    " Local variables (1-15)
    METHODS get_local IMPORTING iv_var TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS set_local IMPORTING iv_var TYPE i iv_val TYPE i.

    " Call frame management
    METHODS call
      IMPORTING iv_return_pc  TYPE i
                iv_return_var TYPE i
                iv_num_locals TYPE i.

    METHODS ret
      EXPORTING ev_return_pc TYPE i
                ev_return_var TYPE i.

    METHODS set_frame_local IMPORTING iv_idx TYPE i iv_val TYPE i.

    METHODS get_depth RETURNING VALUE(rv_depth) TYPE i.

  PRIVATE SECTION.
    DATA mt_frames TYPE STANDARD TABLE OF ty_frame WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_ork_00_stack IMPLEMENTATION.

  METHOD constructor.
    DATA ls_frame TYPE ty_frame.
    ls_frame-return_pc = 0.
    ls_frame-return_var = -1.
    APPEND ls_frame TO mt_frames.
  ENDMETHOD.

  METHOD push.
    DATA(lv_val) = iv_val MOD 65536.
    IF lv_val < 0.
      lv_val = lv_val + 65536.
    ENDIF.
    DATA(lv_idx) = lines( mt_frames ).
    IF lv_idx > 0.
      FIELD-SYMBOLS <fs_frame> TYPE ty_frame.
      READ TABLE mt_frames INDEX lv_idx ASSIGNING <fs_frame>.
      IF sy-subrc = 0.
        APPEND lv_val TO <fs_frame>-eval_stack.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD pop.
    DATA(lv_idx) = lines( mt_frames ).
    IF lv_idx > 0.
      FIELD-SYMBOLS <fs_frame> TYPE ty_frame.
      READ TABLE mt_frames INDEX lv_idx ASSIGNING <fs_frame>.
      IF sy-subrc = 0.
        DATA(lv_stack_size) = lines( <fs_frame>-eval_stack ).
        IF lv_stack_size > 0.
          READ TABLE <fs_frame>-eval_stack INDEX lv_stack_size INTO rv_val.
          DELETE <fs_frame>-eval_stack INDEX lv_stack_size.
        ELSE.
          rv_val = 0.
        ENDIF.
      ELSE.
        rv_val = 0.
      ENDIF.
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.

  METHOD peek.
    DATA(lv_idx) = lines( mt_frames ).
    IF lv_idx > 0.
      FIELD-SYMBOLS <fs_frame> TYPE ty_frame.
      READ TABLE mt_frames INDEX lv_idx ASSIGNING <fs_frame>.
      IF sy-subrc = 0.
        DATA(lv_stack_size) = lines( <fs_frame>-eval_stack ).
        IF lv_stack_size > 0.
          READ TABLE <fs_frame>-eval_stack INDEX lv_stack_size INTO rv_val.
        ELSE.
          rv_val = 0.
        ENDIF.
      ELSE.
        rv_val = 0.
      ENDIF.
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.

  METHOD get_local.
    DATA(lv_idx) = lines( mt_frames ).
    IF lv_idx > 0 AND iv_var >= 1.
      FIELD-SYMBOLS <fs_frame> TYPE ty_frame.
      READ TABLE mt_frames INDEX lv_idx ASSIGNING <fs_frame>.
      IF sy-subrc = 0.
        IF iv_var <= lines( <fs_frame>-locals ).
          READ TABLE <fs_frame>-locals INDEX iv_var INTO rv_val.
        ELSE.
          rv_val = 0.
        ENDIF.
      ELSE.
        rv_val = 0.
      ENDIF.
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.

  METHOD set_local.
    DATA(lv_val) = iv_val MOD 65536.
    IF lv_val < 0.
      lv_val = lv_val + 65536.
    ENDIF.
    DATA(lv_idx) = lines( mt_frames ).
    IF lv_idx > 0 AND iv_var >= 1.
      FIELD-SYMBOLS <fs_frame> TYPE ty_frame.
      READ TABLE mt_frames INDEX lv_idx ASSIGNING <fs_frame>.
      IF sy-subrc = 0 AND iv_var <= lines( <fs_frame>-locals ).
        MODIFY <fs_frame>-locals INDEX iv_var FROM lv_val.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD call.
    DATA ls_frame TYPE ty_frame.
    ls_frame-return_pc = iv_return_pc.
    ls_frame-return_var = iv_return_var.

    DATA lv_i TYPE i.
    lv_i = 0.
    WHILE lv_i < iv_num_locals.
      APPEND 0 TO ls_frame-locals.
      lv_i = lv_i + 1.
    ENDWHILE.

    APPEND ls_frame TO mt_frames.
  ENDMETHOD.

  METHOD set_frame_local.
    " Set local in current frame (for call initialization)
    DATA(lv_frame_idx) = lines( mt_frames ).
    IF lv_frame_idx > 0 AND iv_idx >= 1.
      FIELD-SYMBOLS <fs_frame> TYPE ty_frame.
      READ TABLE mt_frames INDEX lv_frame_idx ASSIGNING <fs_frame>.
      IF sy-subrc = 0 AND iv_idx <= lines( <fs_frame>-locals ).
        DATA(lv_val) = iv_val MOD 65536.
        IF lv_val < 0.
          lv_val = lv_val + 65536.
        ENDIF.
        MODIFY <fs_frame>-locals INDEX iv_idx FROM lv_val.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD ret.
    DATA(lv_idx) = lines( mt_frames ).
    IF lv_idx > 1.
      FIELD-SYMBOLS <fs_frame> TYPE ty_frame.
      READ TABLE mt_frames INDEX lv_idx ASSIGNING <fs_frame>.
      IF sy-subrc = 0.
        ev_return_pc = <fs_frame>-return_pc.
        ev_return_var = <fs_frame>-return_var.
      ENDIF.
      DELETE mt_frames INDEX lv_idx.
    ELSE.
      ev_return_pc = 0.
      ev_return_var = -1.
    ENDIF.
  ENDMETHOD.

  METHOD get_depth.
    rv_depth = lines( mt_frames ).
  ENDMETHOD.

ENDCLASS.
