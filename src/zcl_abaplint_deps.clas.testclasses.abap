
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abaplint_deps.

    METHODS:
      setup,
      find FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD find.

    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_tt.

    lt_files = mo_cut->find(
      iv_depth       = 10
      iv_object_type = 'CLAS'
      iv_object_name = 'ZCL_ABAPGIT_BACKGROUND_PULL' ).

    cl_abap_unit_assert=>assert_number_between(
      lower  = 10
      upper  = 100
      number = lines( lt_files ) ).

  ENDMETHOD.

ENDCLASS.
