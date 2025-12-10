class ZCL_CPU_6502_ROM_LOADER_SMW0 definition
  public
  create public .

************************************************************************
* Load ROM files from SMW0 (SAP MIME Repository)
* Upload binary files via transaction SMW0 -> Binary data
************************************************************************
public section.

  interfaces ZIF_CPU_6502_ROM_LOADER .

  methods CONSTRUCTOR
    importing
      !IV_PATTERN type STRING default '*.BIN' .
  PRIVATE SECTION.
    DATA mv_pattern TYPE string.

ENDCLASS.



CLASS ZCL_CPU_6502_ROM_LOADER_SMW0 IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    mv_pattern = iv_pattern.
  ENDMETHOD.


  METHOD ZIF_CPU_6502_ROM_LOADER~GET_LOAD_ADDRESS.
    " MS-BASIC loads at $0800
    IF iv_id CS 'BASIC' OR iv_id CS 'basic'.
      rv_addr = 2048.  " $0800
    ELSE.
      rv_addr = 0.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_CPU_6502_ROM_LOADER~LIST_ROMS.
    DATA: lv_pattern TYPE string,
          ls_rom     TYPE zif_cpu_6502_rom_loader=>ts_rom_info.

    " Convert pattern for LIKE comparison
    lv_pattern = mv_pattern.
    REPLACE ALL OCCURRENCES OF '*' IN lv_pattern WITH '%'.

    " Get all MIME objects matching pattern
    SELECT DISTINCT objid FROM wwwparams
      INTO TABLE @DATA(lt_objids)
      WHERE relid = 'MI'
        AND objid LIKE @lv_pattern.

    LOOP AT lt_objids INTO DATA(ls_objid).
      CLEAR ls_rom.
      ls_rom-id = ls_objid-objid.
      ls_rom-name = ls_objid-objid.
      ls_rom-description = ls_objid-objid.

      " Get filesize
      SELECT SINGLE value FROM wwwparams INTO @DATA(lv_size)
        WHERE relid = 'MI' AND objid = @ls_objid-objid AND name = 'filesize'.

      IF sy-subrc = 0.
        ls_rom-size = lv_size.
      ENDIF.

      APPEND ls_rom TO rt_list.
    ENDLOOP.

    SORT rt_list BY id.
  ENDMETHOD.


  METHOD ZIF_CPU_6502_ROM_LOADER~LOAD.
    DATA: lt_mime   TYPE w3mimetabtype,
          ls_key    TYPE wwwdatatab,
          lv_size   TYPE i.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_id.

    " Get file size
    SELECT SINGLE value FROM wwwparams INTO @DATA(lv_filesize)
      WHERE relid = @ls_key-relid AND objid = @ls_key-objid AND name = 'filesize'.

    IF sy-subrc <> 0.
      RETURN.  " ROM not found
    ENDIF.

    lv_size = lv_filesize.

    " Import binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING key  = ls_key
      TABLES    mime = lt_mime
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc <> 0.
      RETURN.  " Import failed
    ENDIF.

    " Convert to xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING input_length = lv_size
      IMPORTING buffer       = rv_data
      TABLES    binary_tab   = lt_mime.
  ENDMETHOD.
ENDCLASS.
