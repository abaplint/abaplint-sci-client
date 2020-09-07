class ZCL_ABAPLINT_EXIT definition
  public
  create public .

public section.

  interfaces ZIF_ABAPLINT_EXIT .

  class-methods GET_INSTANCE
    returning
      value(RI_EXIT) type ref to ZIF_ABAPLINT_EXIT .
  PROTECTED SECTION.
private section.

  class-data GI_EXIT type ref to ZIF_ABAPLINT_EXIT .
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


  method ZIF_ABAPLINT_EXIT~HANDLE_SPECIAL_ABAPS.

    TRY.
        gi_exit->handle_special_abaps( EXPORTING iv_program_name = iv_program_name
          CHANGING cs_files_item = cs_files_item ).
      CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
