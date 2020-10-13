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

    DATA: ls_code_inspector LIKE LINE OF gt_code_inspector.
    FIELD-SYMBOLS: <ls_code_inspector> TYPE ty_code_inspector.

    READ TABLE gt_code_inspector ASSIGNING <ls_code_inspector> INDEX 1.
    IF sy-subrc <> 0.
      ls_code_inspector-items = iv_items.

      CREATE OBJECT ls_code_inspector-instance TYPE zcl_abaplint_code_inspector
        EXPORTING
          iv_items = iv_items.

      INSERT ls_code_inspector
             INTO TABLE gt_code_inspector
             ASSIGNING <ls_code_inspector>.

    ENDIF.

    ri_code_inspector = <ls_code_inspector>-instance.

  ENDMETHOD.
ENDCLASS.
