CLASS zcl_abaplint_dao DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES: rfcdes_tt TYPE STANDARD TABLE OF rfcdes WITH DEFAULT KEY.

    METHODS: get_rfcs IMPORTING i_rfc_type TYPE rfctype_d RETURNING VALUE(r_rfcs) TYPE rfcdes_tt.

    CLASS-METHODS get_instance RETURNING VALUE(r_instance) TYPE REF TO zcl_abaplint_dao.

    DATA rfc_type TYPE rfctype_d VALUE 'G'.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA instance TYPE REF TO zcl_abaplint_dao.

ENDCLASS.



CLASS ZCL_ABAPLINT_DAO IMPLEMENTATION.


  METHOD get_instance.
    IF instance IS NOT BOUND.
      instance = NEW #(  ).
    ENDIF.
    r_instance = instance.
  ENDMETHOD.


  METHOD get_rfcs.

    SELECT rfcdest
               FROM rfcdes
               INTO TABLE @DATA(it_cols)
               WHERE rfctype EQ @i_rfc_type.
    r_rfcs = it_cols.
  ENDMETHOD.
ENDCLASS.
