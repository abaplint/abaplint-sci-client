CLASS zcl_abaplint_exit DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abaplint_exit .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ri_exit) TYPE REF TO zif_abaplint_exit .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gi_exit TYPE REF TO zif_abaplint_exit .
ENDCLASS.



CLASS ZCL_ABAPLINT_EXIT IMPLEMENTATION.


  METHOD get_instance.

    IF gi_exit IS INITIAL.
      TRY.
          CREATE OBJECT gi_exit TYPE ('ZCL_ABAPLINT_USER_EXIT').
        CATCH cx_sy_create_object_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    CREATE OBJECT ri_exit TYPE zcl_abaplint_exit.

  ENDMETHOD.


  METHOD zif_abaplint_exit~handle_special_abaps.

    TRY.
        gi_exit->handle_special_abaps( EXPORTING iv_program_name = iv_program_name
          CHANGING cs_files_item = cs_files_item ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
