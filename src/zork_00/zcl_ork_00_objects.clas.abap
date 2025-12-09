*&---------------------------------------------------------------------*
*& Z-Machine Objects - Direct port from z3_minimal.py
*& Object table handling for V3
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_objects DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      c_attr_bytes    TYPE i VALUE 4,   " 4 bytes = 32 attributes
      c_entry_size    TYPE i VALUE 9,   " V3: 4+1+1+1+2 = 9 bytes
      c_max_objects   TYPE i VALUE 255, " V3 uses 1-byte object numbers
      c_defaults_size TYPE i VALUE 31.  " 31 words of property defaults

    METHODS constructor IMPORTING io_mem TYPE REF TO zcl_ork_00_memory.

    " Attributes
    METHODS get_attr IMPORTING iv_obj TYPE i iv_attr TYPE i RETURNING VALUE(rv_set) TYPE abap_bool.
    METHODS set_attr IMPORTING iv_obj TYPE i iv_attr TYPE i.
    METHODS clear_attr IMPORTING iv_obj TYPE i iv_attr TYPE i.

    " Object tree
    METHODS get_parent IMPORTING iv_obj TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS get_sibling IMPORTING iv_obj TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS get_child IMPORTING iv_obj TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS set_parent IMPORTING iv_obj TYPE i iv_val TYPE i.
    METHODS set_sibling IMPORTING iv_obj TYPE i iv_val TYPE i.
    METHODS set_child IMPORTING iv_obj TYPE i iv_val TYPE i.

    " Property table
    METHODS get_prop_addr IMPORTING iv_obj TYPE i RETURNING VALUE(rv_addr) TYPE i.
    METHODS get_name_addr IMPORTING iv_obj TYPE i RETURNING VALUE(rv_addr) TYPE i.
    METHODS get_prop IMPORTING iv_obj TYPE i iv_prop TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS get_prop_len IMPORTING iv_prop_data_addr TYPE i RETURNING VALUE(rv_len) TYPE i.
    METHODS get_next_prop IMPORTING iv_obj TYPE i iv_prop TYPE i RETURNING VALUE(rv_next) TYPE i.
    METHODS put_prop IMPORTING iv_obj TYPE i iv_prop TYPE i iv_val TYPE i.
    METHODS get_prop_data_addr IMPORTING iv_obj TYPE i iv_prop TYPE i RETURNING VALUE(rv_addr) TYPE i.

    " Tree manipulation
    METHODS remove_obj IMPORTING iv_obj TYPE i.
    METHODS insert_obj IMPORTING iv_obj TYPE i iv_dest TYPE i.

  PRIVATE SECTION.
    DATA mo_mem TYPE REF TO zcl_ork_00_memory.

    METHODS obj_addr IMPORTING iv_obj TYPE i RETURNING VALUE(rv_addr) TYPE i.
ENDCLASS.

CLASS zcl_ork_00_objects IMPLEMENTATION.

  METHOD constructor.
    mo_mem = io_mem.
  ENDMETHOD.

  METHOD obj_addr.
    " Get address of object entry (1-based object number)
    IF iv_obj < 1 OR iv_obj > c_max_objects.
      rv_addr = 0.
      RETURN.
    ENDIF.
    " Property defaults: 31 words = 62 bytes
    " First object at base + 62
    rv_addr = mo_mem->obj_addr + c_defaults_size * 2 + ( iv_obj - 1 ) * c_entry_size.
  ENDMETHOD.

  METHOD get_attr.
    " Test attribute (0-31), big-endian bit order
    rv_set = abap_false.
    IF iv_attr < 0 OR iv_attr > 31.
      RETURN.
    ENDIF.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      RETURN.
    ENDIF.
    DATA(lv_byte_idx) = iv_attr DIV 8.
    DATA(lv_bit_idx) = 7 - ( iv_attr MOD 8 ).  " Big-endian bit order!

    DATA(lv_byte) = mo_mem->u8( lv_addr + lv_byte_idx ).

    " Calculate mask: 1 << lv_bit_idx
    DATA lv_mask TYPE i.
    lv_mask = 1.
    DATA lv_i TYPE i.
    lv_i = 0.
    WHILE lv_i < lv_bit_idx.
      lv_mask = lv_mask * 2.
      lv_i = lv_i + 1.
    ENDWHILE.

    IF ( lv_byte DIV lv_mask ) MOD 2 = 1.
      rv_set = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD set_attr.
    IF iv_attr < 0 OR iv_attr > 31.
      RETURN.
    ENDIF.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      RETURN.
    ENDIF.
    DATA(lv_byte_idx) = iv_attr DIV 8.
    DATA(lv_bit_idx) = 7 - ( iv_attr MOD 8 ).

    DATA(lv_byte) = mo_mem->u8( lv_addr + lv_byte_idx ).

    DATA lv_mask TYPE i.
    lv_mask = 1.
    DATA lv_i TYPE i.
    lv_i = 0.
    WHILE lv_i < lv_bit_idx.
      lv_mask = lv_mask * 2.
      lv_i = lv_i + 1.
    ENDWHILE.

    " Set bit if not already set
    IF ( lv_byte DIV lv_mask ) MOD 2 = 0.
      lv_byte = lv_byte + lv_mask.
    ENDIF.

    mo_mem->w8( iv_addr = lv_addr + lv_byte_idx iv_val = lv_byte ).
  ENDMETHOD.

  METHOD clear_attr.
    IF iv_attr < 0 OR iv_attr > 31.
      RETURN.
    ENDIF.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      RETURN.
    ENDIF.
    DATA(lv_byte_idx) = iv_attr DIV 8.
    DATA(lv_bit_idx) = 7 - ( iv_attr MOD 8 ).

    DATA(lv_byte) = mo_mem->u8( lv_addr + lv_byte_idx ).

    DATA lv_mask TYPE i.
    lv_mask = 1.
    DATA lv_i TYPE i.
    lv_i = 0.
    WHILE lv_i < lv_bit_idx.
      lv_mask = lv_mask * 2.
      lv_i = lv_i + 1.
    ENDWHILE.

    " Clear bit if set
    IF ( lv_byte DIV lv_mask ) MOD 2 = 1.
      lv_byte = lv_byte - lv_mask.
    ENDIF.

    mo_mem->w8( iv_addr = lv_addr + lv_byte_idx iv_val = lv_byte ).
  ENDMETHOD.

  METHOD get_parent.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      rv_val = 0.
      RETURN.
    ENDIF.
    rv_val = mo_mem->u8( lv_addr + c_attr_bytes ).
  ENDMETHOD.

  METHOD get_sibling.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      rv_val = 0.
      RETURN.
    ENDIF.
    rv_val = mo_mem->u8( lv_addr + c_attr_bytes + 1 ).
  ENDMETHOD.

  METHOD get_child.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      rv_val = 0.
      RETURN.
    ENDIF.
    rv_val = mo_mem->u8( lv_addr + c_attr_bytes + 2 ).
  ENDMETHOD.

  METHOD set_parent.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      RETURN.
    ENDIF.
    mo_mem->w8( iv_addr = lv_addr + c_attr_bytes iv_val = iv_val ).
  ENDMETHOD.

  METHOD set_sibling.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      RETURN.
    ENDIF.
    mo_mem->w8( iv_addr = lv_addr + c_attr_bytes + 1 iv_val = iv_val ).
  ENDMETHOD.

  METHOD set_child.
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      RETURN.
    ENDIF.
    mo_mem->w8( iv_addr = lv_addr + c_attr_bytes + 2 iv_val = iv_val ).
  ENDMETHOD.

  METHOD get_prop_addr.
    " Get address of property table for object
    DATA(lv_addr) = obj_addr( iv_obj ).
    IF lv_addr = 0.
      rv_addr = 0.
      RETURN.
    ENDIF.
    rv_addr = mo_mem->u16( lv_addr + c_attr_bytes + 3 ).
  ENDMETHOD.

  METHOD get_name_addr.
    " Get address of object short name (Z-string after length byte)
    DATA(lv_prop_addr) = get_prop_addr( iv_obj ).
    IF lv_prop_addr = 0.
      rv_addr = 0.
      RETURN.
    ENDIF.
    DATA(lv_name_len) = mo_mem->u8( lv_prop_addr ).
    IF lv_name_len = 0.
      rv_addr = 0.
      RETURN.
    ENDIF.
    rv_addr = lv_prop_addr + 1.  " String starts after length byte
  ENDMETHOD.

  METHOD get_prop.
    " Get property value (returns default if not found)
    DATA(lv_prop_addr) = get_prop_addr( iv_obj ).
    IF lv_prop_addr = 0.
      " Return default
      rv_val = mo_mem->u16( mo_mem->obj_addr + ( iv_prop - 1 ) * 2 ).
      RETURN.
    ENDIF.

    DATA(lv_name_len) = mo_mem->u8( lv_prop_addr ).
    DATA(lv_addr) = lv_prop_addr + 1 + lv_name_len * 2.  " Skip name

    DATA lv_size_byte TYPE i.
    lv_size_byte = mo_mem->u8( lv_addr ).
    WHILE lv_size_byte <> 0.
      DATA(lv_prop_num) = lv_size_byte MOD 32.
      DATA(lv_prop_len) = ( lv_size_byte DIV 32 ) + 1.

      IF lv_prop_num = iv_prop.
        IF lv_prop_len = 1.
          rv_val = mo_mem->u8( lv_addr + 1 ).
        ELSE.
          rv_val = mo_mem->u16( lv_addr + 1 ).
        ENDIF.
        RETURN.
      ENDIF.

      IF lv_prop_num < iv_prop.
        EXIT.  " Properties are in descending order
      ENDIF.

      lv_addr = lv_addr + 1 + lv_prop_len.
      lv_size_byte = mo_mem->u8( lv_addr ).
    ENDWHILE.

    " Return default
    rv_val = mo_mem->u16( mo_mem->obj_addr + ( iv_prop - 1 ) * 2 ).
  ENDMETHOD.

  METHOD get_prop_len.
    " Get length of property from its data address
    IF iv_prop_data_addr = 0.
      rv_len = 0.
      RETURN.
    ENDIF.
    DATA(lv_size_byte) = mo_mem->u8( iv_prop_data_addr - 1 ).
    rv_len = ( lv_size_byte DIV 32 ) + 1.
  ENDMETHOD.

  METHOD get_next_prop.
    " Get next property number after given property (0 = first)
    DATA(lv_prop_addr) = get_prop_addr( iv_obj ).
    IF lv_prop_addr = 0.
      rv_next = 0.
      RETURN.
    ENDIF.

    DATA(lv_name_len) = mo_mem->u8( lv_prop_addr ).
    DATA(lv_addr) = lv_prop_addr + 1 + lv_name_len * 2.

    IF iv_prop = 0.
      " Return first property
      DATA(lv_size_byte) = mo_mem->u8( lv_addr ).
      IF lv_size_byte = 0.
        rv_next = 0.
      ELSE.
        rv_next = lv_size_byte MOD 32.
      ENDIF.
      RETURN.
    ENDIF.

    " Find the property first
    lv_size_byte = mo_mem->u8( lv_addr ).
    WHILE lv_size_byte <> 0.
      DATA(lv_prop_num) = lv_size_byte MOD 32.
      DATA(lv_prop_len) = ( lv_size_byte DIV 32 ) + 1.

      IF lv_prop_num = iv_prop.
        " Return next property
        lv_addr = lv_addr + 1 + lv_prop_len.
        lv_size_byte = mo_mem->u8( lv_addr ).
        IF lv_size_byte = 0.
          rv_next = 0.
        ELSE.
          rv_next = lv_size_byte MOD 32.
        ENDIF.
        RETURN.
      ENDIF.

      lv_addr = lv_addr + 1 + lv_prop_len.
      lv_size_byte = mo_mem->u8( lv_addr ).
    ENDWHILE.

    rv_next = 0.
  ENDMETHOD.

  METHOD put_prop.
    " Set property value
    DATA(lv_prop_addr) = get_prop_addr( iv_obj ).
    IF lv_prop_addr = 0.
      RETURN.
    ENDIF.

    DATA(lv_name_len) = mo_mem->u8( lv_prop_addr ).
    DATA(lv_addr) = lv_prop_addr + 1 + lv_name_len * 2.

    DATA lv_size_byte TYPE i.
    lv_size_byte = mo_mem->u8( lv_addr ).
    WHILE lv_size_byte <> 0.
      DATA(lv_prop_num) = lv_size_byte MOD 32.
      DATA(lv_prop_len) = ( lv_size_byte DIV 32 ) + 1.

      IF lv_prop_num = iv_prop.
        IF lv_prop_len = 1.
          mo_mem->w8( iv_addr = lv_addr + 1 iv_val = iv_val MOD 256 ).
        ELSE.
          mo_mem->w16( iv_addr = lv_addr + 1 iv_val = iv_val MOD 65536 ).
        ENDIF.
        RETURN.
      ENDIF.

      lv_addr = lv_addr + 1 + lv_prop_len.
      lv_size_byte = mo_mem->u8( lv_addr ).
    ENDWHILE.
  ENDMETHOD.

  METHOD get_prop_data_addr.
    " Get address of property data (0 if not found)
    DATA(lv_prop_addr) = get_prop_addr( iv_obj ).
    IF lv_prop_addr = 0.
      rv_addr = 0.
      RETURN.
    ENDIF.

    DATA(lv_name_len) = mo_mem->u8( lv_prop_addr ).
    DATA(lv_addr) = lv_prop_addr + 1 + lv_name_len * 2.

    DATA lv_size_byte TYPE i.
    lv_size_byte = mo_mem->u8( lv_addr ).
    WHILE lv_size_byte <> 0.
      DATA(lv_prop_num) = lv_size_byte MOD 32.
      DATA(lv_prop_len) = ( lv_size_byte DIV 32 ) + 1.

      IF lv_prop_num = iv_prop.
        rv_addr = lv_addr + 1.  " Data starts after size byte
        RETURN.
      ENDIF.

      IF lv_prop_num < iv_prop.
        rv_addr = 0.
        RETURN.
      ENDIF.

      lv_addr = lv_addr + 1 + lv_prop_len.
      lv_size_byte = mo_mem->u8( lv_addr ).
    ENDWHILE.

    rv_addr = 0.
  ENDMETHOD.

  METHOD remove_obj.
    " Remove object from its parent
    DATA(lv_parent) = get_parent( iv_obj ).
    IF lv_parent = 0.
      RETURN.
    ENDIF.

    " Find and unlink from parent's child list
    DATA(lv_child) = get_child( lv_parent ).
    IF lv_child = iv_obj.
      " First child - update parent's child pointer
      set_child( iv_obj = lv_parent iv_val = get_sibling( iv_obj ) ).
    ELSE.
      " Find previous sibling
      WHILE lv_child <> 0.
        DATA(lv_next_sib) = get_sibling( lv_child ).
        IF lv_next_sib = iv_obj.
          set_sibling( iv_obj = lv_child iv_val = get_sibling( iv_obj ) ).
          EXIT.
        ENDIF.
        lv_child = lv_next_sib.
      ENDWHILE.
    ENDIF.

    set_parent( iv_obj = iv_obj iv_val = 0 ).
    set_sibling( iv_obj = iv_obj iv_val = 0 ).
  ENDMETHOD.

  METHOD insert_obj.
    " Insert object as first child of destination
    remove_obj( iv_obj ).
    set_sibling( iv_obj = iv_obj iv_val = get_child( iv_dest ) ).
    set_child( iv_obj = iv_dest iv_val = iv_obj ).
    set_parent( iv_obj = iv_obj iv_val = iv_dest ).
  ENDMETHOD.

ENDCLASS.
