CLASS ltcl_parser_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    CLASS-DATA gv_sample TYPE string.

    CLASS-METHODS class_setup.

    METHODS parse FOR TESTING RAISING zcx_abaplint_error.
    METHODS normalize_path FOR TESTING.
    METHODS split_path FOR TESTING.
    METHODS get_value FOR TESTING RAISING zcx_abaplint_error.
    METHODS exists FOR TESTING RAISING zcx_abaplint_error.
    METHODS value_integer FOR TESTING RAISING zcx_abaplint_error.
    METHODS value_boolean FOR TESTING RAISING zcx_abaplint_error.
    METHODS members FOR TESTING RAISING zcx_abaplint_error.

ENDCLASS.

CLASS zcl_abaplint_json_reader DEFINITION LOCAL FRIENDS ltcl_parser_test.

CLASS ltcl_parser_test IMPLEMENTATION.

  METHOD normalize_path.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>normalize_path( '' )
      exp = '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>normalize_path( '/' )
      exp = '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>normalize_path( 'abc' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>normalize_path( '/abc' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>normalize_path( 'abc/' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>normalize_path( '/abc/' )
      exp = '/abc/' ).

  ENDMETHOD.

  METHOD split_path.

    DATA ls_exp TYPE zcl_abaplint_json_reader=>ty_path_name.

    ls_exp-path = '/'.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>split_path( '/' )
      exp = ls_exp ).

    ls_exp-path = '/abc/'.
    ls_exp-name = 'xyz'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>split_path( '/abc/xyz' )
      exp = ls_exp ).

    ls_exp-path = '/abc/'.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>split_path( '/abc/' )
      exp = ls_exp ).

    ls_exp-path = '/abc/xyz/'.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abaplint_json_reader=>split_path( '/abc/xyz/' )
      exp = ls_exp ).

  ENDMETHOD.

  METHOD class_setup.

    gv_sample =
      '{' &&
      '  "string": "abc",' &&
      '  "number": 123,' &&
      '  "float": 123.45,' &&
      '  "boolean": true,' &&
      '  "false": false,' &&
      '  "null": null,' &&
      '  "date": "2020-03-15",' &&
      '  "issues": [' &&
      '    {' &&
      '      "message": "Indentation problem ...",' &&
      '      "key": "indentation",' &&
      '      "start": {' &&
      '        "row": 4,' &&
      '        "col": 3' &&
      '      },' &&
      '      "end": {' &&
      '        "row": 4,' &&
      '        "col": 26' &&
      '      },' &&
      '      "filename": "./zxxx.prog.abap"' &&
      '    },' &&
      '    {' &&
      '      "message": "Remove space before XXX",' &&
      '      "key": "space_before_dot",' &&
      '      "start": {' &&
      '        "row": 3,' &&
      '        "col": 21' &&
      '      },' &&
      '      "end": {' &&
      '        "row": 3,' &&
      '        "col": 22' &&
      '      },' &&
      '      "filename": "./zxxx.prog.abap"' &&
      '    }' &&
      '  ]' &&
      '}'.

  ENDMETHOD.

  METHOD parse.

    DATA lo_cut TYPE REF TO lcl_json_parser.
    DATA lt_act TYPE zif_abaplint_json_reader=>ty_nodes_tt.
    DATA lt_exp LIKE lt_act.
    FIELD-SYMBOLS <exp> LIKE LINE OF lt_exp.

    DEFINE _exp.
      APPEND INITIAL LINE TO lt_exp ASSIGNING <exp>.
      <exp>-path = &1.
      <exp>-name = &2.
      <exp>-type = &3.
      <exp>-value = &4.
      <exp>-children = &5.
    END-OF-DEFINITION.

    _exp ''                 ''         'object' ''       8.
    _exp '/'                'string'   'str'    'abc'    0.
    _exp '/'                'number'   'num'    '123'    0.
    _exp '/'                'float'    'num'    '123.45' 0.
    _exp '/'                'boolean'  'bool'   'true'   0.
    _exp '/'                'false'    'bool'   'false'  0.
    _exp '/'                'null'     'null'   ''       0.
    _exp '/'                'date'     'str'    '2020-03-15' 0.
    _exp '/'                'issues'   'array'  ''       2.
    _exp '/issues/'         '1'        'object' ''       5.
    _exp '/issues/1/'       'message'  'str'    'Indentation problem ...' 0.
    _exp '/issues/1/'       'key'      'str'    'indentation' 0.
    _exp '/issues/1/'       'start'    'object' ''       2.
    _exp '/issues/1/start/' 'row'      'num'    '4'      0.
    _exp '/issues/1/start/' 'col'      'num'    '3'      0.
    _exp '/issues/1/'       'end'      'object' ''       2.
    _exp '/issues/1/end/'   'row'      'num'    '4'      0.
    _exp '/issues/1/end/'   'col'      'num'    '26'     0.
    _exp '/issues/1/'       'filename' 'str'    './zxxx.prog.abap' 0.
    _exp '/issues/'         '2'        'object' ''       5.
    _exp '/issues/2/'       'message'  'str'    'Remove space before XXX' 0.
    _exp '/issues/2/'       'key'      'str'    'space_before_dot' 0.
    _exp '/issues/2/'       'start'    'object' ''       2.
    _exp '/issues/2/start/' 'row'      'num'    '3'      0.
    _exp '/issues/2/start/' 'col'      'num'    '21'     0.
    _exp '/issues/2/'       'end'      'object' ''       2.
    _exp '/issues/2/end/'   'row'      'num'    '3'      0.
    _exp '/issues/2/end/'   'col'      'num'    '22'     0.
    _exp '/issues/2/'       'filename' 'str'    './zxxx.prog.abap' 0.

    CREATE OBJECT lo_cut.
    lt_act = lo_cut->parse( gv_sample ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

  METHOD get_value.

    DATA lo_cut TYPE REF TO zif_abaplint_json_reader.
    lo_cut ?= zcl_abaplint_json_reader=>parse( gv_sample ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value( '/string' )
      exp = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value( '/string/' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value( '/boolean' )
      exp = 'true' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value( '/issues/2/start/row' )
      exp = '3' ).

  ENDMETHOD.

  METHOD exists.

    DATA lo_cut TYPE REF TO zif_abaplint_json_reader.
    lo_cut ?= zcl_abaplint_json_reader=>parse( gv_sample ).


    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/string' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/string/' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/xxx' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/issues/2/start/row' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD value_integer.

    DATA lo_cut TYPE REF TO zif_abaplint_json_reader.
    lo_cut ?= zcl_abaplint_json_reader=>parse( gv_sample ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_integer( '/string' )
      exp = 0 ). " Hmmmm ????

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_integer( '/number' )
      exp = 123 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_integer( '/float' )
      exp = 123 ).

  ENDMETHOD.

  METHOD value_boolean.

    DATA lo_cut TYPE REF TO zif_abaplint_json_reader.
    lo_cut ?= zcl_abaplint_json_reader=>parse( gv_sample ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_boolean( '/string' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_boolean( '/number' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_boolean( '/xxx' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->value_boolean( '/boolean' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD members.

    DATA lt_exp TYPE string_table.
    DATA lo_cut TYPE REF TO zif_abaplint_json_reader.
    lo_cut ?= zcl_abaplint_json_reader=>parse( gv_sample ).

    CLEAR lt_exp.
    APPEND '1' TO lt_exp.
    APPEND '2' TO lt_exp.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->members( '/issues' )
      exp = lt_exp ).

    CLEAR lt_exp.
    APPEND 'col' TO lt_exp.
    APPEND 'row' TO lt_exp.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->members( '/issues/1/start/' )
      exp = lt_exp ).

  ENDMETHOD.

ENDCLASS.
