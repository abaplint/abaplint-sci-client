REPORT zabaplint_list_deps.
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

TABLES: tdevc.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_obje RADIOBUTTON GROUP g2.
PARAMETERS: p_type TYPE tadir-object,
            p_name TYPE tadir-obj_name.

PARAMETERS: p_devc RADIOBUTTON GROUP g2.
SELECT-OPTIONS: s_devc FOR tdevc-devclass.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS p_depth TYPE i DEFAULT 1.
SELECTION-SCREEN SKIP.
PARAMETERS: p_skip RADIOBUTTON GROUP g1,
            p_seri RADIOBUTTON GROUP g1,
            p_down RADIOBUTTON GROUP g1.
SELECTION-SCREEN: END OF BLOCK b2.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING cx_static_check.

  DATA lo_find TYPE REF TO zcl_abaplint_deps_find.
  DATA lt_deps TYPE zif_abapgit_definitions=>ty_tadir_tt.
  DATA lt_packages TYPE tr_devclasses.
  DATA ls_deps LIKE LINE OF lt_deps.
  DATA lv_lines TYPE n LENGTH 6.

  CREATE OBJECT lo_find
    EXPORTING
      iv_max_level = p_depth
      is_output    = abap_true.

  CASE abap_true.
    WHEN p_obje.
      lt_deps = lo_find->find_by_item(
        iv_object_type = p_type
        iv_object_name = p_name ).
    WHEN p_devc.
      SELECT devclass FROM tdevc INTO TABLE lt_packages WHERE devclass IN s_devc.
      IF sy-subrc = 0.
        lt_deps = lo_find->find_by_packages( lt_packages ).
      ENDIF.
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

  FORMAT INTENSIFIED OFF.
  LOOP AT lt_deps INTO ls_deps.
    WRITE: / ls_deps-object, ls_deps-obj_name, ls_deps-devclass.
  ENDLOOP.

  PERFORM serialize USING lt_deps.

  ULINE.
  lv_lines = lines( lt_deps ).
  FORMAT INTENSIFIED ON.
  WRITE: / 'Found', lv_lines, 'dependencies for', p_type, p_name.
  FORMAT INTENSIFIED OFF.
  ULINE.

ENDFORM.

FORM serialize USING pt_deps TYPE zif_abapgit_definitions=>ty_tadir_tt RAISING zcx_abapgit_exception.

  DATA lt_local TYPE zif_abapgit_definitions=>ty_files_tt.
  DATA lo_dep_ser TYPE REF TO zcl_abaplint_deps_serializer.

  IF p_skip = abap_true.
    RETURN.
  ENDIF.

  CREATE OBJECT lo_dep_ser.
  lt_local = lo_dep_ser->serialize( pt_deps ).

  IF p_down = abap_true.
    PERFORM download USING lt_local.
  ENDIF.

ENDFORM.

FORM download USING pt_local TYPE zif_abapgit_definitions=>ty_files_tt RAISING zcx_abapgit_exception.

  DATA: lo_zip      TYPE REF TO cl_abap_zip,
        lv_xstr     TYPE xstring,
        lv_path     TYPE string,
        lv_filename TYPE string.

  FIELD-SYMBOLS: <ls_file> LIKE LINE OF pt_local.


  lv_path = zcl_abapgit_ui_factory=>get_frontend_services( )->show_file_save_dialog(
    iv_title            = 'Save'
    iv_extension        = 'zip'
    iv_default_filename = 'dependencies.zip' ).

  CREATE OBJECT lo_zip.
  LOOP AT pt_local ASSIGNING <ls_file>.
    CONCATENATE <ls_file>-path+1 <ls_file>-filename INTO lv_filename.
    lo_zip->add( name    = lv_filename
                 content = <ls_file>-data ).
  ENDLOOP.

  lv_xstr = lo_zip->save( ).

  zcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
    iv_path = lv_path
    iv_xstr = lv_xstr ).

ENDFORM.
