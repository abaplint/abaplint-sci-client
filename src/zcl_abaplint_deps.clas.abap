CLASS zcl_abaplint_deps DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS find
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
        !iv_depth       TYPE zabaplint_glob-depth
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt .
  PROTECTED SECTION.

    METHODS list
      IMPORTING
        !iv_object_type   TYPE trobjtype
        !iv_object_name   TYPE sobj_name
        !iv_depth         TYPE i
      RETURNING
        VALUE(rt_objects) TYPE senvi_tab .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS IMPLEMENTATION.


  METHOD find.

    IF iv_depth > 0.
      DATA(lt_environment) = list(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name
        iv_depth       = iv_depth ).
    ENDIF.

* make sure itself is not a dependency of itself
    DELETE lt_environment WHERE type = iv_object_type AND object = iv_object_name.

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


  METHOD list.

    DATA: lt_next     TYPE senvi_tab,
          lv_obj_type TYPE euobj-id.


    lv_obj_type = iv_object_type.

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
      EXPORTING
        obj_type       = lv_obj_type
        object_name    = iv_object_name
        online_force   = abap_true
      TABLES
        environment    = rt_objects
      EXCEPTIONS
        batch          = 1
        batchjob_error = 2
        not_executed   = 3
        OTHERS         = 4.
    IF sy-subrc = 3.
      RETURN.
    ENDIF.

* todo, not sure if this is alright to do
    DELETE rt_objects WHERE encl_obj IS NOT INITIAL.
    DELETE rt_objects WHERE type = 'STRU'.
    DELETE rt_objects WHERE type = 'TYPE'.
    DELETE rt_objects WHERE type = 'INCL'.
    DELETE rt_objects WHERE type = 'FUNC'.
    DELETE rt_objects WHERE type = 'ACCO'.

    IF iv_depth > 1.
      LOOP AT rt_objects INTO DATA(ls_environment).
        APPEND LINES OF list(
          iv_object_type = CONV #( ls_environment-type )
          iv_object_name = CONV #( ls_environment-object )
          iv_depth       = iv_depth - 1 ) TO lt_next.
      ENDLOOP.

      APPEND LINES OF lt_next TO rt_objects.
      SORT rt_objects BY type object.
      DELETE ADJACENT DUPLICATES FROM rt_objects COMPARING type object.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
