INTERFACE zif_cpu_6502_bus PUBLIC.
************************************************************************
* 6502 Bus Interface
* Memory-mapped I/O for 64KB address space
* Implementations can provide RAM, ROM, and I/O devices
************************************************************************

  " Read byte from address (0-65535)
  METHODS read
    IMPORTING iv_addr       TYPE i
    RETURNING VALUE(rv_val) TYPE i.

  " Write byte to address (0-65535)
  METHODS write
    IMPORTING iv_addr TYPE i
              iv_val  TYPE i.

  " Load binary data into memory at specified address
  METHODS load
    IMPORTING iv_addr TYPE i
              iv_data TYPE xstring.

  " Check if input is available (for blocking I/O)
  METHODS is_input_ready
    RETURNING VALUE(rv_ready) TYPE abap_bool.

  " Get accumulated output buffer
  METHODS get_output
    RETURNING VALUE(rv_output) TYPE string.

  " Clear output buffer
  METHODS clear_output.

  " Provide input text (queued for reading)
  METHODS provide_input
    IMPORTING iv_text TYPE string.

ENDINTERFACE.
