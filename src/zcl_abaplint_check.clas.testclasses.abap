CLASS lcl_check_double DEFINITION INHERITING FROM zcl_abaplint_check FINAL.
  PUBLIC SECTION.
    METHODS call_hash
      IMPORTING
        iv_value        TYPE clike
      RETURNING
        VALUE(rv_hash)  TYPE sci_errc.
ENDCLASS.

CLASS lcl_check_double IMPLEMENTATION.
  METHOD call_hash.
    rv_hash = hash( iv_value ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS hash FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD hash.

    DATA lo_cut TYPE REF TO lcl_check_double.

    CREATE OBJECT lo_cut.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->call_hash( 'prefer_is_not' )
      exp = '0C5BD' ).

  ENDMETHOD.
ENDCLASS.
