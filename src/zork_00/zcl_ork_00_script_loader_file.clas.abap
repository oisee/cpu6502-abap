*&---------------------------------------------------------------------*
*& Class ZCL_ORK_00_SCRIPT_LOADER_FILE
*& Load command scripts from frontend text file
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_script_loader_file DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ork_00_script_loader.

    " Constructor with file path (opens dialog if empty)
    METHODS constructor
      IMPORTING iv_filepath TYPE string OPTIONAL.

    " Get the loaded file path
    METHODS get_filepath
      RETURNING VALUE(rv_filepath) TYPE string.

  PRIVATE SECTION.
    DATA mv_filepath TYPE string.

ENDCLASS.


CLASS zcl_ork_00_script_loader_file IMPLEMENTATION.

  METHOD constructor.
    mv_filepath = iv_filepath.
  ENDMETHOD.


  METHOD get_filepath.
    rv_filepath = mv_filepath.
  ENDMETHOD.


  METHOD zif_ork_00_script_loader~list_scripts.
    " File loader doesn't support listing - returns empty
    CLEAR rt_list.
  ENDMETHOD.


  METHOD zif_ork_00_script_loader~load.
    DATA: lt_filetab TYPE filetable,
          lv_rc      TYPE i,
          lt_data    TYPE string_table,
          lv_path    TYPE string.

    lv_path = mv_filepath.

    " Use iv_id as path if provided
    IF iv_id IS NOT INITIAL.
      lv_path = iv_id.
    ENDIF.

    " If no path, open file dialog
    IF lv_path IS INITIAL.
      cl_gui_frontend_services=>file_open_dialog(
        EXPORTING
          window_title      = 'Select Script File (commands)'
          file_filter       = 'Text Files (*.txt)|*.txt|All (*.*)|*.*'
          default_extension = 'txt'
        CHANGING
          file_table        = lt_filetab
          rc                = lv_rc ).

      IF lv_rc < 1.
        RETURN.  " User cancelled
      ENDIF.

      READ TABLE lt_filetab INDEX 1 INTO DATA(ls_file).
      lv_path = ls_file-filename.
      mv_filepath = lv_path.
    ENDIF.

    " Upload text file from frontend
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename   = lv_path
        filetype   = 'ASC'
      CHANGING
        data_tab   = lt_data
      EXCEPTIONS
        OTHERS     = 1 ).

    IF sy-subrc <> 0.
      RETURN.  " Upload failed
    ENDIF.

    " Each line is a command (skip empty lines and comments)
    LOOP AT lt_data INTO DATA(lv_line).
      " Trim whitespace
      CONDENSE lv_line.

      " Skip empty lines
      IF lv_line IS INITIAL.
        CONTINUE.
      ENDIF.

      " Skip comments (starting with #)
      IF lv_line(1) = '#'.
        CONTINUE.
      ENDIF.

      APPEND lv_line TO rt_commands.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

