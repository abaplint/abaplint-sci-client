class ZCL_ABAPLINT_JSON_READER definition
  public
  create public .

public section.

  INTERFACES zif_abaplint_json_reader.

  CLASS-METHODS parse
    IMPORTING
      !IV_JSON type STRING
    RETURNING
      VALUE(ro_instance) TYPE REF TO zcl_abaplint_json_reader
    RAISING
      zcx_abaplint_error.

  METHODS CONSTRUCTOR
    IMPORTING
      !IV_JSON type STRING
    RAISING
      zcx_abaplint_error.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mt_json_tree TYPE zif_abaplint_json_reader=>tt_nodes.

ENDCLASS.



CLASS ZCL_ABAPLINT_JSON_READER IMPLEMENTATION.


  METHOD constructor.

    DATA lo_parser TYPE REF TO lcl_json_parser.
    CREATE OBJECT lo_parser.
    mt_json_tree = lo_parser->parse( iv_json ).

  ENDMETHOD.


  METHOD parse.

    CREATE OBJECT ro_instance EXPORTING iv_json = iv_json.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~exists.

*    rv_exists = mo_parser->exists( iv_path ).

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~members.

*    rt_members = mo_parser->members( iv_path ).

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value.

*    rv_value = mo_parser->value( iv_path ).

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value_boolean.

*    DATA(lv_value) = mo_parser->value( iv_path ).
*
*    rv_value = boolc( lv_value = 'true' ) ##NO_TEXT.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value_integer.

*    DATA(lv_value) = mo_parser->value( iv_path ).
*    IF lv_value <> 'null'.
*      rv_value = lv_value.
*    ENDIF.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value_number.

*    rv_value = mo_parser->value( iv_path ).

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value_string.

*    rv_value = mo_parser->value( iv_path ).

  ENDMETHOD.
ENDCLASS.
