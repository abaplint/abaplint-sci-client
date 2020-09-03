
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_abaplint_deps_serializer.

    METHODS:
      setup,
      cl_ci_tests FOR TESTING RAISING cx_static_check,
      svrs FOR TESTING RAISING cx_static_check,
      zabaplint_dependencies FOR TESTING RAISING cx_static_check,
      cx_iac_helper_check_7bit_acsii FOR TESTING RAISING cx_static_check,
      zabaplint_ui FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD zabaplint_ui.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'ZABAPLINT_UI'.

    mo_cut->serialize_item( ls_item ).

  ENDMETHOD.

  METHOD cl_ci_tests.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_CI_TESTS'.

    mo_cut->serialize_item( ls_item ).

  ENDMETHOD.

  METHOD svrs.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'FUGR'.
    ls_item-obj_name = 'SVRS'.

    mo_cut->serialize_item( ls_item ).

  ENDMETHOD.

  METHOD zabaplint_dependencies.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = 'ZABAPLINT_DEPENDENCIES'.

    mo_cut->serialize_item( ls_item ).

  ENDMETHOD.

  METHOD cx_iac_helper_check_7bit_acsii.

    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_tt.
    DATA ls_file LIKE LINE OF lt_files.


    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CX_IAC_HELPER_CHECK_7BIT_ACSII'.

    lt_files = mo_cut->serialize_item( ls_item ).

    LOOP AT lt_files INTO ls_file.
      cl_abap_unit_assert=>assert_not_initial( ls_file-data ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
