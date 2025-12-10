INTERFACE zif_cpu_6502_rom_loader PUBLIC.
************************************************************************
* ROM/Binary Loader Interface for 6502 Emulator
* Implementations can load from SMW0, filesystem, etc.
************************************************************************

  TYPES: BEGIN OF ts_rom_info,
           id          TYPE string,
           name        TYPE string,
           description TYPE string,
           size        TYPE i,
           load_addr   TYPE i,
         END OF ts_rom_info.

  TYPES tt_rom_list TYPE STANDARD TABLE OF ts_rom_info WITH DEFAULT KEY.

  METHODS list_roms
    RETURNING VALUE(rt_list) TYPE tt_rom_list.

  METHODS load
    IMPORTING iv_id           TYPE string OPTIONAL
    RETURNING VALUE(rv_data)  TYPE xstring.

  METHODS get_load_address
    IMPORTING iv_id         TYPE string OPTIONAL
    RETURNING VALUE(rv_addr) TYPE i.

ENDINTERFACE.
