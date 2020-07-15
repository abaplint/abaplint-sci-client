
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_abaplint_deps_serializer.

    METHODS:
      setup,
      clas_cl_ci_tests FOR TESTING RAISING cx_static_check,
      fugr_zabaplint_ui FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD fugr_zabaplint_ui.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'ZABAPLINT_UI'.

    mo_cut->serialize_item( ls_item ).

  ENDMETHOD.

  METHOD clas_cl_ci_tests.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_CI_TESTS'.

    mo_cut->serialize_item( ls_item ).

  ENDMETHOD.

ENDCLASS.
