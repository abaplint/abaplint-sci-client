CLASS ltcl_find_by_item DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abaplint_deps_find.

    METHODS:
      setup,
      txmilograw FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_find_by_item IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD txmilograw.

    DATA: lt_results TYPE zif_abapgit_definitions=>ty_tadir_tt.

    lt_results = mo_cut->find_by_item(
      iv_object_type = 'TABL'
      iv_object_name = 'TXMILOGRAW' ).

    READ TABLE lt_results WITH KEY object = 'DTEL' obj_name = 'XMILOGID' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_results WITH KEY object = 'DOMA' obj_name = 'XMILOGID' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

    READ TABLE lt_results WITH KEY object = 'TABL' obj_name = 'SXMILOGADM' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_results WITH KEY object = 'TABL' obj_name = 'SXMIMSGRAW' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_number_between(
      lower  = 20
      upper  = 50
      number = lines( lt_results ) ).

  ENDMETHOD.

ENDCLASS.
