CLASS lcl_parser_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    METHODS parse FOR TESTING RAISING zcx_abaplint_error.

ENDCLASS.

CLASS lcl_parser_test IMPLEMENTATION.

  METHOD parse.

    DATA lv_sample TYPE string.
    lv_sample =
      '{' &&
      '  "string": "abc",' &&
      '  "number": 123,' &&
      '  "float": 123.45,' &&
      '  "boolean": true,' &&
      '  "null": null,' &&
      '  "date": "2020-03-15",' &&
      '  "issues": [' &&
      '    {' &&
      '      "message": "Indentation problem, expected 0 spaces",' &&
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
      '      "message": "Remove space before \",\" or \".\"",' &&
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

    DATA lo_cut TYPE REF TO lcl_json_parser.
    DATA lt_act TYPE zif_abaplint_json_reader=>tt_nodes.
    DATA lt_exp LIKE lt_act.
    FIELD-SYMBOLS <exp> LIKE LINE OF lt_exp.


    CREATE OBJECT lo_cut.
    lt_act = lo_cut->parse( lv_sample ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

ENDCLASS.
