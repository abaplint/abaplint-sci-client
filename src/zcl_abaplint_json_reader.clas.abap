CLASS zcl_abaplint_json_reader DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abaplint_json_reader.

    CLASS-METHODS parse
      IMPORTING
        iv_json TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abaplint_json_reader
      RAISING
        zcx_abaplint_error.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_path_name,
        path TYPE string,
        name TYPE string,
      END OF ty_path_name.

    DATA mt_json_tree TYPE zif_abaplint_json_reader=>ty_nodes_ts.

    CLASS-METHODS normalize_path
      IMPORTING
        iv_path TYPE string
      RETURNING
        VALUE(rv_path) TYPE string.
    CLASS-METHODS split_path
      IMPORTING
        iv_path TYPE string
      RETURNING
        VALUE(rv_path_name) TYPE ty_path_name.
    METHODS get_item
      IMPORTING
        iv_path TYPE string
      RETURNING
        VALUE(rv_item) TYPE REF TO zif_abaplint_json_reader=>ty_node.

ENDCLASS.



CLASS ZCL_ABAPLINT_JSON_READER IMPLEMENTATION.


  METHOD get_item.

    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.
    DATA ls_path_name TYPE ty_path_name.
    ls_path_name = split_path( iv_path ).

    READ TABLE mt_json_tree
      ASSIGNING <item>
      WITH KEY
        path = ls_path_name-path
        name = ls_path_name-name.
    IF sy-subrc = 0.
      GET REFERENCE OF <item> INTO rv_item.
    ENDIF.

  ENDMETHOD.


  METHOD normalize_path.

    rv_path = iv_path.
    IF strlen( rv_path ) = 0.
      rv_path = '/'.
    ENDIF.
    IF rv_path+0(1) <> '/'.
      rv_path = '/' && rv_path.
    ENDIF.
    IF substring( val = rv_path off = strlen( rv_path ) - 1 ) <> '/'.
      rv_path = rv_path && '/'.
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    DATA lo_parser TYPE REF TO lcl_json_parser.

    CREATE OBJECT ro_instance.
    CREATE OBJECT lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse( iv_json ).

  ENDMETHOD.


  METHOD split_path.

    DATA lv_offs TYPE i.
    DATA lv_len TYPE i.
    DATA lv_trim_slash TYPE i.

    lv_len = strlen( iv_path ).
    IF lv_len = 0 OR iv_path = '/'.
      RETURN. " empty path is the alias for root item = '' + ''
    ENDIF.

    IF substring( val = iv_path off = lv_len - 1 ) = '/'.
      lv_trim_slash = 1. " ignore last '/'
    ENDIF.

    lv_offs = find( val = reverse( iv_path ) sub = '/' off = lv_trim_slash ).
    IF lv_offs = -1.
      lv_offs  = lv_len. " treat whole string as the 'name' part
    ENDIF.
    lv_offs = lv_len - lv_offs.

    rv_path_name-path = normalize_path( substring( val = iv_path len = lv_offs ) ).
    rv_path_name-name = substring( val = iv_path off = lv_offs len = lv_len - lv_offs - lv_trim_slash ).

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~exists.

    DATA lv_item TYPE REF TO zif_abaplint_json_reader=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL.
      rv_exists = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~members.

    DATA lv_normalized_path TYPE string.
    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = normalize_path( iv_path ).

    LOOP AT mt_json_tree ASSIGNING <item> WHERE path = lv_normalized_path.
      APPEND <item>-name TO rt_members.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~sub_section.

    DATA lo_section         TYPE REF TO zcl_abaplint_json_reader.
    DATA ls_item            LIKE LINE OF mt_json_tree.
    DATA lv_normalized_path TYPE string.
    DATA ls_path_parts      TYPE ty_path_name.
    DATA lv_path_len        TYPE i.

    CREATE OBJECT lo_section.
    lv_normalized_path = normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = split_path( lv_normalized_path ).

    LOOP AT mt_json_tree INTO ls_item.
      IF strlen( ls_item-path ) >= lv_path_len
        AND substring( val = ls_item-path len = lv_path_len ) = lv_normalized_path.
        ls_item-path = substring( val = ls_item-path off = lv_path_len - 1 ). " less closing '/'
        INSERT ls_item INTO TABLE lo_section->mt_json_tree.
      ELSEIF ls_item-path = ls_path_parts-path AND ls_item-name = ls_path_parts-name.
        CLEAR: ls_item-path, ls_item-name. " this becomes a new root
        INSERT ls_item INTO TABLE lo_section->mt_json_tree.
      ENDIF.
    ENDLOOP.

    ri_json = lo_section.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value.

    DATA lv_item TYPE REF TO zif_abaplint_json_reader=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value_boolean.

    DATA lv_item TYPE REF TO zif_abaplint_json_reader=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS INITIAL OR lv_item->type = 'null'.
      RETURN.
    ELSEIF lv_item->type = 'bool'.
      rv_value = boolc( lv_item->value = 'true' ).
    ELSEIF lv_item->value IS NOT INITIAL.
      rv_value = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value_integer.

    DATA lv_item TYPE REF TO zif_abaplint_json_reader=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type = 'num'.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value_number.

    DATA lv_item TYPE REF TO zif_abaplint_json_reader=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type = 'num'.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abaplint_json_reader~value_string.

    DATA lv_item TYPE REF TO zif_abaplint_json_reader=>ty_node.
    lv_item = get_item( iv_path ).
    IF lv_item IS NOT INITIAL AND lv_item->type <> 'null'.
      rv_value = lv_item->value.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
