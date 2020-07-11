REPORT zabaplint_list_deps.

PARAMETERS: p_type  TYPE tadir-object OBLIGATORY,
            p_name  TYPE tadir-obj_name OBLIGATORY,
            p_depth TYPE i DEFAULT 1.

*
* This report depends on the cross reference generated automatically in SAP
*
* In case it you think there is an issue, run these reports in background:
* - SAPRSEUB for SAP objects
* - SAPRSEUC for Customer objects (Y*/Z*)
*
* Also to ensure that the repository is kept clean, run the abap SAPRSEUJ
* it schedules the jobs required
*
* OSS # 2752795 - Environment analysis progams and classes - use of BAdI definitions
* OSS # 2243139 - REPOSITORY_ENVIRONMENT_ALL - too few hits for enhancement implementations
START-OF-SELECTION.

  PERFORM run.

FORM run RAISING cx_static_check.

  DATA lo_find TYPE REF TO zcl_abaplint_deps_find.
  DATA lt_deps TYPE zif_abapgit_definitions=>ty_tadir_tt.
  DATA ls_deps LIKE LINE OF lt_deps.
  DATA lv_lines TYPE n LENGTH 6.

  CREATE OBJECT lo_find
    EXPORTING
      iv_max_level = p_depth
      is_output    = abap_true.

  lt_deps = lo_find->find_by_item(
    iv_object_type = p_type
    iv_object_name = p_name ).

  FORMAT INTENSIFIED OFF.
  LOOP AT lt_deps INTO ls_deps.
    WRITE: / ls_deps-object, ls_deps-obj_name, ls_deps-devclass.
  ENDLOOP.

  ULINE.
  lv_lines = lines( lt_deps ).
  FORMAT INTENSIFIED ON.
  WRITE: / 'Found', lv_lines, 'dependencies for', p_type, p_name.
  FORMAT INTENSIFIED OFF.
  ULINE.
ENDFORM.
