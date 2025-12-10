INTERFACE zif_cpu_6502_script_ldr PUBLIC.
************************************************************************
* Script Loader Interface for 6502 Speedrun Tests
* Scripts contain input commands and assertions for automated testing
************************************************************************

  TYPES tt_commands TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

  METHODS load
    RETURNING VALUE(rt_commands) TYPE tt_commands.

ENDINTERFACE.
