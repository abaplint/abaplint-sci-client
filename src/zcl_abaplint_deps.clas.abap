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
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS IMPLEMENTATION.


  METHOD find.

    DATA lo_serializer TYPE REF TO zcl_abaplint_deps_serializer.
    DATA lo_find TYPE REF TO zcl_abaplint_deps_find.

    CREATE OBJECT lo_serializer.
    CREATE OBJECT lo_find.

    DATA lt_environment TYPE senvi_tab.
    IF iv_depth > 0.
      lt_environment = lo_find->list(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name
        iv_depth       = iv_depth ).
    ENDIF.

* make sure itself is not a dependency of itself
    DELETE lt_environment WHERE type = iv_object_type AND object = iv_object_name.

    DATA ls_environment LIKE LINE OF lt_environment.
    DATA ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.
    LOOP AT lt_environment INTO ls_environment.
      ls_files_item-item-obj_type = ls_environment-type.
      ls_files_item-item-obj_name = ls_environment-object.

      TRY.
          ls_files_item-files = lo_serializer->serialize_item( ls_files_item-item ).
        CATCH zcx_abapgit_exception.
          ASSERT 0 = 1.
      ENDTRY.

      APPEND LINES OF ls_files_item-files TO rt_files.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
