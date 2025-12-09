*&---------------------------------------------------------------------*
*& Interface ZIF_ORK_00_SCRIPT_LOADER
*& Load command scripts for automated game testing
*&---------------------------------------------------------------------*
INTERFACE zif_ork_00_script_loader PUBLIC.

  TYPES: tt_commands TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  TYPES: BEGIN OF ts_script_info,
           id          TYPE string,
           description TYPE string,
         END OF ts_script_info,
         tt_script_list TYPE STANDARD TABLE OF ts_script_info WITH EMPTY KEY.

  METHODS:
    " List available scripts (optional - may return empty)
    list_scripts
      RETURNING VALUE(rt_list) TYPE tt_script_list,

    " Load commands from source by ID
    load
      IMPORTING iv_id              TYPE string OPTIONAL
      RETURNING VALUE(rt_commands) TYPE tt_commands.

ENDINTERFACE.
