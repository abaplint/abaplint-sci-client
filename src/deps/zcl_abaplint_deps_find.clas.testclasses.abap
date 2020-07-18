CLASS ltcl_find_by_item DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abaplint_deps_find.

    METHODS:
      setup,
      usr02 FOR TESTING RAISING cx_static_check,
      zcl_abapgit_object_dtel FOR TESTING RAISING cx_static_check,
      txmilograw FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_find_by_item IMPLEMENTATION.

  METHOD setup.
    DATA ls_options TYPE zcl_abaplint_deps_find=>ty_options.

    ls_options-continue_into_sap = abap_true.

    CREATE OBJECT mo_cut
      EXPORTING
        is_options = ls_options.

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

* not sure the result is the same across systems/versions
    cl_abap_unit_assert=>assert_number_between(
      lower  = 20
      upper  = 50
      number = lines( lt_results ) ).

  ENDMETHOD.

  METHOD usr02.

    DATA: lt_results TYPE zif_abapgit_definitions=>ty_tadir_tt.

    lt_results = mo_cut->find_by_item(
      iv_object_type = 'TABL'
      iv_object_name = 'USR02' ).

* the check tables should not be found by the dependency analysis, they are not relevant to abaplint
    READ TABLE lt_results WITH KEY object = 'TABL' obj_name = 'SEC_POLICY_CUST' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).

  ENDMETHOD.

  METHOD zcl_abapgit_object_dtel.

    DATA: lt_results TYPE zif_abapgit_definitions=>ty_tadir_tt.

    lt_results = mo_cut->find_by_item(
      iv_object_type = 'CLAS'
      iv_object_name = 'ZCL_ABAPGIT_OBJECT_DTEL' ).

* the class calls function module DDIF_DTEL_PUT, so this function group is a dependency
    READ TABLE lt_results WITH KEY object = 'FUGR' obj_name = 'SDIF' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

ENDCLASS.
