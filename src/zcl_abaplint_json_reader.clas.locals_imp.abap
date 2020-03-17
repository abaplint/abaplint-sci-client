CLASS lcl_json_parser DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        !IV_JSON type STRING
      RETURNING
        VALUE(rt_json_tree) TYPE zif_abaplint_json_reader=>tt_nodes
      RAISING
        zcx_abaplint_error.

  PRIVATE SECTION.

    TYPES:
      tt_stack TYPE STANDARD TABLE OF REF TO zif_abaplint_json_reader=>ty_node.

    DATA mt_stack TYPE tt_stack.

    CLASS-METHODS join_path
      IMPORTING
        it_stack TYPE tt_stack
      RETURNING
        VALUE(rv_path) TYPE string.

    METHODS raise
      IMPORTING
        iv_error TYPE string
      RAISING
        zcx_abaplint_error.

ENDCLASS.

CLASS lcl_json_parser IMPLEMENTATION.

  METHOD parse.

    DATA lo_reader TYPE REF TO if_sxml_reader.
    DATA lr_stack_top LIKE LINE OF mt_stack.
    DATA lo_node TYPE REF TO if_sxml_node.
    FIELD-SYMBOLS <item> LIKE LINE OF rt_json_tree.

    CLEAR mt_stack.
    lo_reader = cl_sxml_string_reader=>create( cl_abap_codepage=>convert_to( iv_json ) ).

    " TODO: self protection, check non-empty, check starting from object ...

    DO.
      lo_node = lo_reader->read_next_node( ).
      IF lo_node IS NOT BOUND.
        EXIT.
      ENDIF.


      CASE lo_node->type.
        WHEN if_sxml_node=>co_nt_element_open.
          DATA lt_attributes TYPE if_sxml_attribute=>attributes.
          DATA lo_attr LIKE LINE OF lt_attributes.
          DATA lo_open TYPE REF TO if_sxml_open_element.
          lo_open ?= lo_node.

          APPEND INITIAL LINE TO rt_json_tree ASSIGNING <item>.

          <item>-type = to_lower( lo_open->qname-name ).

          READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
          IF sy-subrc = 0.
            <item>-path = join_path( mt_stack ).
            lr_stack_top->children = lr_stack_top->children + 1.

            IF lr_stack_top->type = 'array'.
              <item>-name = |{ lr_stack_top->children }|.
            ELSE.
              lt_attributes = lo_open->get_attributes( ).
              LOOP AT lt_attributes INTO lo_attr.
                IF lo_attr->qname-name = 'name' AND lo_attr->value_type = if_sxml_value=>co_vt_text.
                  <item>-name = lo_attr->get_value( ).
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

          GET REFERENCE OF <item> INTO lr_stack_top.
          INSERT lr_stack_top INTO mt_stack INDEX 1.

        WHEN if_sxml_node=>co_nt_element_close.
          DATA lo_close TYPE REF TO if_sxml_close_element.
          lo_close ?= lo_node .

          READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
          DELETE mt_stack INDEX 1.
          IF lo_close->qname-name <> lr_stack_top->type.
            raise( 'Unexpected closing node type' ).
          ENDIF.

        WHEN if_sxml_node=>co_nt_value.
          DATA lo_value TYPE REF TO if_sxml_value_node.
          lo_value ?= lo_node.

          <item>-value = lo_value->get_value( ).

        WHEN OTHERS.
          raise( 'Unexpected node type' ).
      ENDCASE.
    ENDDO.

    IF lines( mt_stack ) > 0.
      raise( 'Unexpected end of data' ).
    ENDIF.

  ENDMETHOD.

  METHOD join_path.

    FIELD-SYMBOLS <ref> LIKE LINE OF it_stack.

    LOOP AT it_stack ASSIGNING <ref>.
      rv_path =  <ref>->name && '/' && rv_path.
    ENDLOOP.

  ENDMETHOD.

  METHOD raise.

    RAISE EXCEPTION TYPE zcx_abaplint_error
      EXPORTING
        message = |JSON: { iv_error } @ { join_path( mt_stack ) }|.

  ENDMETHOD.

ENDCLASS.
