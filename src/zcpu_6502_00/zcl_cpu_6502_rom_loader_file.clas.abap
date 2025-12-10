CLASS zcl_cpu_6502_rom_loader_file DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_cpu_6502_rom_loader.

    METHODS constructor
      IMPORTING iv_directory TYPE string OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_directory TYPE string.
    DATA mv_filepath TYPE string.

ENDCLASS.



CLASS zcl_cpu_6502_rom_loader_file IMPLEMENTATION.

  METHOD constructor.
    mv_directory = iv_directory.
  ENDMETHOD.

  METHOD zif_cpu_6502_rom_loader~list_roms.
    DATA ls_info TYPE zif_cpu_6502_rom_loader=>ts_rom_info.
    IF mv_filepath IS NOT INITIAL.
      ls_info-id = mv_filepath.
      ls_info-name = mv_filepath.
      APPEND ls_info TO rt_list.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cpu_6502_rom_loader~load.
    DATA: lv_filepath TYPE string,
          lv_content  TYPE xstring.

    IF iv_id IS NOT INITIAL.
      lv_filepath = iv_id.
    ELSEIF mv_filepath IS NOT INITIAL.
      lv_filepath = mv_filepath.
    ELSE.
      RETURN.
    ENDIF.

    mv_filepath = lv_filepath.

    OPEN DATASET lv_filepath FOR INPUT IN BINARY MODE.
    IF sy-subrc = 0.
      READ DATASET lv_filepath INTO lv_content.
      CLOSE DATASET lv_filepath.
      rv_data = lv_content.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cpu_6502_rom_loader~get_load_address.
    rv_addr = 0.
  ENDMETHOD.

ENDCLASS.

