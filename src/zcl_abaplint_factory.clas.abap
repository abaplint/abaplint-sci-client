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
      BEGIN OF ty_sap_package,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_sap_package,
      END OF ty_sap_package .
    TYPES:
      ty_sap_package_t TYPE HASHED TABLE OF ty_sap_package
                                    WITH UNIQUE KEY package .
    TYPES:
      BEGIN OF ty_code_inspector,
        items    TYPE zif_abapgit_definitions=>ty_items_tt,
        instance TYPE REF TO zif_abaplint_code_inspector,
      END OF ty_code_inspector .
    TYPES:
      ty_code_inspector_t TYPE TABLE OF ty_code_inspector.

    CLASS-DATA gi_tadir TYPE REF TO zif_abapgit_tadir .
    CLASS-DATA gt_sap_package TYPE ty_sap_package_t .
    CLASS-DATA gt_code_inspector TYPE ty_code_inspector_t .
    CLASS-DATA gi_stage_logic TYPE REF TO zif_abapgit_stage_logic .
    CLASS-DATA gi_cts_api TYPE REF TO zif_abapgit_cts_api .
    CLASS-DATA gi_environment TYPE REF TO zif_abapgit_environment .
    CLASS-DATA gi_longtext TYPE REF TO zif_abapgit_longtexts .
    CLASS-DATA gi_http_agent TYPE REF TO zif_abapgit_http_agent .
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
