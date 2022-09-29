*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_longtexts DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_longtexts.
ENDCLASS.

CLASS lcl_longtexts IMPLEMENTATION.

  METHOD zif_abapgit_longtexts~changed_by.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_longtexts~serialize.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_longtexts~deserialize.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_longtexts~delete.
    RETURN.
  ENDMETHOD.

ENDCLASS.
