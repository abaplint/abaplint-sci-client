REPORT zabaplint_list_deps.

PARAMETERS: p_type  TYPE tadir-object OBLIGATORY,
            p_name  TYPE tadir-obj_name OBLIGATORY,
            p_depth TYPE i DEFAULT 1.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING cx_static_check.

  DATA lo_find TYPE REF TO zcl_abaplint_deps_find.
  DATA lt_deps TYPE zif_abapgit_definitions=>ty_tadir_tt.
  DATA ls_deps LIKE LINE OF lt_deps.

  CREATE OBJECT lo_find
    EXPORTING
      iv_max_level = p_depth.

  IF p_depth > 0.
    lt_deps = lo_find->find_by_item(
      iv_object_type = p_type
      iv_object_name = p_name ).
  ENDIF.

  LOOP AT lt_deps INTO ls_deps.
    WRITE: / ls_deps-object, ls_deps-obj_name.
  ENDLOOP.

ENDFORM.
