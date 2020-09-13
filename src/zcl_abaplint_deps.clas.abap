CLASS zcl_abaplint_deps DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS find
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
        !iv_depth       TYPE zabaplint_glob-depth
        !iv_continue    TYPE zabaplint_glob-conti
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception
        zcx_abaplint_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS IMPLEMENTATION.


  METHOD find.

    DATA lo_serializer TYPE REF TO zcl_abaplint_deps_serializer.
    DATA lo_find TYPE REF TO zcl_abaplint_deps_find.
    DATA ls_options TYPE zcl_abaplint_deps_find=>ty_options.
    DATA li_log TYPE REF TO zif_abapgit_log.

    CREATE OBJECT lo_serializer.

    ls_options-max_level = iv_depth.
    ls_options-continue_into_sap = iv_continue.
    CREATE OBJECT lo_find EXPORTING is_options = ls_options.

    DATA lt_deps TYPE zif_abapgit_definitions=>ty_tadir_tt.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.

    lt_deps = lo_find->find_by_item(
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name
      ii_log         = li_log ).

    DATA ls_dep LIKE LINE OF lt_deps.
    DATA ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.
    LOOP AT lt_deps INTO ls_dep.
      ls_files_item-item-obj_type = ls_dep-object.
      ls_files_item-item-obj_name = ls_dep-obj_name.

      ls_files_item-files = lo_serializer->serialize_item( ls_files_item-item ).

      APPEND LINES OF ls_files_item-files TO rt_files.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
