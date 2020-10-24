CLASS zcl_abaplint_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_injector .

  PUBLIC SECTION.

    CLASS-METHODS get_code_inspector
      IMPORTING
        !iv_items                TYPE zif_abapgit_definitions=>ty_items_tt
      RETURNING
        VALUE(ri_code_inspector) TYPE REF TO zif_abaplint_code_inspector
      RAISING
        zcx_abaplint_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_code_inspector,
        items    TYPE zif_abapgit_definitions=>ty_items_tt,
        instance TYPE REF TO zif_abaplint_code_inspector,
      END OF ty_code_inspector .
    TYPES:
      ty_code_inspector_t TYPE TABLE OF ty_code_inspector.

    CLASS-DATA gt_code_inspector TYPE ty_code_inspector_t .
ENDCLASS.



CLASS ZCL_ABAPLINT_FACTORY IMPLEMENTATION.


  METHOD get_code_inspector.

    CREATE OBJECT ri_code_inspector TYPE zcl_abaplint_code_inspector
      EXPORTING
        iv_items = iv_items.

  ENDMETHOD.
ENDCLASS.
