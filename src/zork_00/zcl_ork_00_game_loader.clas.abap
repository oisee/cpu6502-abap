*&---------------------------------------------------------------------*
*& ZCL_ORK_00_GAME_LOADER
*& Static utility methods for Z-Machine games
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_game_loader DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      " Get game version from story data
      get_version
        IMPORTING iv_story          TYPE xstring
        RETURNING VALUE(rv_version) TYPE i.

ENDCLASS.


CLASS zcl_ork_00_game_loader IMPLEMENTATION.

  METHOD get_version.
    " Z-Machine version is stored in byte 0 of story file
    IF xstrlen( iv_story ) > 0.
      rv_version = iv_story+0(1).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
