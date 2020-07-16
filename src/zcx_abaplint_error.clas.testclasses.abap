CLASS ltcl_error DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS raise FOR TESTING.

ENDCLASS.

CLASS ltcl_error IMPLEMENTATION.

  METHOD raise.

    DATA lx TYPE REF TO zcx_abaplint_error.
    DATA lv_msg TYPE string.

    lv_msg = repeat( val = 'a' occ = 50 ) && repeat( val = 'b' occ = 50 ) && '123'.

    TRY.
        zcx_abaplint_error=>raise( lv_msg ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abaplint_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
          exp = lv_msg
          act = lx->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
