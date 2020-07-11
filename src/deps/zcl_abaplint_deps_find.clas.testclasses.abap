CLASS ltcl_find_by_item DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abaplint_deps_find.

    METHODS:
      setup,
      find_by_item FOR TESTING.
ENDCLASS.


CLASS ltcl_find_by_item IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD find_by_item.

* todo

  ENDMETHOD.

ENDCLASS.
