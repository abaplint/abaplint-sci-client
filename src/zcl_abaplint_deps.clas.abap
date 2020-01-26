CLASS zcl_abaplint_deps DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS find
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS IMPLEMENTATION.


  METHOD find.

* todo, currently it only runs single level, this wont work in the general case

    DATA: lt_environment TYPE senvi_tab,
          lv_obj_type    TYPE euobj-id.


    lv_obj_type = iv_object_type.

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
      EXPORTING
        obj_type       = lv_obj_type
        object_name    = iv_object_name
        online_force   = abap_true
      TABLES
        environment    = lt_environment
      EXCEPTIONS
        batch          = 1
        batchjob_error = 2
        not_executed   = 3
        OTHERS         = 4.
    IF sy-subrc = 3.
      RETURN.
    ENDIF.

* todo, not sure if this is alright to do
    DELETE lt_environment WHERE encl_obj IS NOT INITIAL.
    DELETE lt_environment WHERE type = 'STRU'.
    DELETE lt_environment WHERE type = 'TYPE'.
    DELETE lt_environment WHERE type = 'INCL'.

    LOOP AT lt_environment INTO DATA(ls_environment).
      DATA(ls_files_item) = VALUE zcl_abapgit_objects=>ty_serialization(
        item-obj_type = ls_environment-type
        item-obj_name = ls_environment-object ).

      TRY.
          ls_files_item = zcl_abapgit_objects=>serialize(
            is_item     = ls_files_item-item
            iv_language = sy-langu ).
        CATCH zcx_abapgit_exception.
          ASSERT 0 = 1.
      ENDTRY.

      APPEND LINES OF ls_files_item-files TO rt_files.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
