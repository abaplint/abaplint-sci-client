CLASS lcl_parser_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    METHODS integrated FOR TESTING.

ENDCLASS.

CLASS lcl_parser_test IMPLEMENTATION.

  METHOD integrated.

    DATA lv_sample TYPE string.
    lv_sample =
      '{' &&
      '  "str": "abc",' &&
      '  "num": 123,' &&
      '  "float": 123.45,' &&
      '  "bool": true,' &&
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

    DATA lo_cut TYPE REF TO zcl_abaplint_json_reader.
    lo_cut = zcl_abaplint_json_reader=>parse( lv_sample ).

    " TODO

  ENDMETHOD.

ENDCLASS.
