INTERFACE zif_abaplint_code_inspector
  PUBLIC .


  METHODS run
    IMPORTING
      !iv_variant    TYPE sci_chkv
      !iv_save       TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rt_list) TYPE scit_alvlist
    RAISING
      zcx_abaplint_error .
  METHODS is_successful
    RETURNING
      VALUE(rv_success) TYPE abap_bool .
  METHODS get_inspection
    RETURNING
      VALUE(rv_inspection) TYPE REF TO cl_ci_inspection .
ENDINTERFACE.
