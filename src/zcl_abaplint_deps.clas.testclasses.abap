
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abaplint_deps.

    METHODS:
      setup,
      find FOR TESTING.
ENDCLASS.       "ltcl_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD find.

    DATA(lt_files) = mo_cut->find(
      iv_depth       = 1
      iv_object_type = 'CLAS'
      iv_object_name = 'ZCL_ABAPGIT_BACKGROUND_PULL' ).

* todo, assertions?

  ENDMETHOD.

ENDCLASS.
