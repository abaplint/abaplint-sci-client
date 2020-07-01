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
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS IMPLEMENTATION.


  METHOD find.

    DATA lo_serializer TYPE REF TO zcl_abaplint_deps_serializer.
    DATA lo_find TYPE REF TO zcl_abaplint_deps_find.

    CREATE OBJECT lo_serializer.
    CREATE OBJECT lo_find EXPORTING iv_max_level = iv_depth.

    DATA lt_deps TYPE zif_abapgit_definitions=>ty_tadir_tt.
    IF iv_depth > 0.
      lt_deps = lo_find->find_by_item(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ENDIF.

    DATA ls_dep LIKE LINE OF lt_deps.
    DATA ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.
    LOOP AT lt_deps INTO ls_dep.
      ls_files_item-item-obj_type = ls_dep-object.
      ls_files_item-item-obj_name = ls_dep-obj_name.

      TRY.
          ls_files_item-files = lo_serializer->serialize_item( ls_files_item-item ).
        CATCH zcx_abapgit_exception.
          ASSERT 0 = 1.
      ENDTRY.

      APPEND LINES OF ls_files_item-files TO rt_files.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
