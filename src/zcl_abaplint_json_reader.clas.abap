CLASS zcl_abaplint_json_reader DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_json TYPE string .
    METHODS exists
      IMPORTING
        !iv_path         TYPE string
      RETURNING
        VALUE(rv_exists) TYPE abap_bool .
    METHODS members
      IMPORTING
        !iv_path          TYPE string
      RETURNING
        VALUE(rt_members) TYPE string_table .
    METHODS value
      IMPORTING
        !iv_path        TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
    METHODS value_boolean
      IMPORTING
        !iv_path        TYPE string
      RETURNING
        VALUE(rv_value) TYPE abap_bool .
    METHODS value_integer
      IMPORTING
        !iv_path        TYPE string
      RETURNING
        VALUE(rv_value) TYPE i .
    METHODS value_number
      IMPORTING
        !iv_path        TYPE string
      RETURNING
        VALUE(rv_value) TYPE i .
    METHODS value_string
      IMPORTING
        !iv_path        TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
  PROTECTED SECTION.

    DATA mo_parser TYPE REF TO /ui5/cl_json_parser .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_JSON_READER IMPLEMENTATION.


  METHOD constructor.

    mo_parser = NEW /ui5/cl_json_parser( ).
    mo_parser->parse( iv_json ).

  ENDMETHOD.


  METHOD exists.

    rv_exists = mo_parser->exists( iv_path ).

  ENDMETHOD.


  METHOD members.

    rt_members = mo_parser->members( iv_path ).

  ENDMETHOD.


  METHOD value.

    rv_value = mo_parser->value( iv_path ).

  ENDMETHOD.


  METHOD value_boolean.

    DATA(lv_value) = mo_parser->value( iv_path ).

    rv_value = boolc( lv_value = 'true' ) ##NO_TEXT.

  ENDMETHOD.


  METHOD value_integer.

    DATA(lv_value) = mo_parser->value( iv_path ).
    IF lv_value <> 'null'.
      rv_value = lv_value.
    ENDIF.

  ENDMETHOD.


  METHOD value_number.

    rv_value = mo_parser->value( iv_path ).

  ENDMETHOD.


  METHOD value_string.

    rv_value = mo_parser->value( iv_path ).

  ENDMETHOD.
ENDCLASS.
