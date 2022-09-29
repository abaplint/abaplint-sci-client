REPORT zabaplint_list_deps.

*/usi/cl_auth=>check_tcode( ).
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
PARAMETERS: p_depth TYPE i DEFAULT 10,
            p_sap   TYPE c AS CHECKBOX,
            p_cache TYPE c AS CHECKBOX.
SELECTION-SCREEN SKIP.
PARAMETERS: p_skip RADIOBUTTON GROUP g1,
            p_seri RADIOBUTTON GROUP g1,
            p_down RADIOBUTTON GROUP g1.
SELECTION-SCREEN SKIP.
PARAMETERS: p_log TYPE c AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b2.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING cx_static_check.

  DATA lo_find TYPE REF TO zcl_abaplint_deps_find.
  DATA lt_deps TYPE zif_abapgit_definitions=>ty_tadir_tt.
  DATA lv_package TYPE devclass.
  DATA lt_packages TYPE tr_devclasses.
  DATA ls_deps LIKE LINE OF lt_deps.
  DATA lv_lines TYPE i.
  DATA lx_error TYPE REF TO zcx_abaplint_error.
  DATA lx_error2 TYPE REF TO zcx_abapgit_exception.
  DATA ls_options TYPE zcl_abaplint_deps_find=>ty_options.
  DATA li_log TYPE REF TO zif_abapgit_log.
  DATA lo_cache TYPE REF TO zcl_abaplint_deps_cache.

  ls_options-depth = p_depth.
  ls_options-conti = p_sap.
  ls_options-cache = p_cache.

  CREATE OBJECT lo_find
    EXPORTING
      is_options = ls_options.

  TRY.
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
    CATCH zcx_abaplint_error INTO lx_error.
      MESSAGE lx_error->message TYPE 'E'.
    CATCH zcx_abapgit_exception INTO lx_error2.
      MESSAGE lx_error2 TYPE 'E'.
  ENDTRY.

  li_log = lo_find->get_log( ).
  li_log->set_title( sy-title ).
  IF li_log->count( ) > 0 AND p_log = abap_true.
    zcl_abapgit_log_viewer=>show_log( li_log ).
  ENDIF.

  FORMAT INTENSIFIED OFF.
  LOOP AT lt_deps INTO ls_deps.
    WRITE: / ls_deps-object, ls_deps-obj_name, ls_deps-devclass.
  ENDLOOP.

  PERFORM serialize USING lt_deps ls_options.

  lo_cache = zcl_abaplint_deps_cache=>get_instance( ls_options-cache ).
  lo_cache->save( ).

  ULINE.
  lv_lines = lines( lt_deps ).
  FORMAT INTENSIFIED ON.
  IF p_obje = abap_true.
    WRITE: / 'Found', lv_lines, 'dependencies for', p_type, p_name.
  ELSE.
    WRITE: / 'Found', lv_lines, 'dependencies for the following packages:'.
    LOOP AT lt_packages INTO lv_package.
      WRITE: AT /5 lv_package.
    ENDLOOP.
  ENDIF.
  FORMAT INTENSIFIED OFF.
  ULINE.

ENDFORM.

FORM serialize USING
    pt_deps    TYPE zif_abapgit_definitions=>ty_tadir_tt
    ps_options TYPE zcl_abaplint_deps_find=>ty_options
    RAISING zcx_abapgit_exception.

  DATA lt_local TYPE zif_abapgit_definitions=>ty_files_tt.
  DATA lo_dep_ser TYPE REF TO zcl_abaplint_deps_serializer.
  DATA lx_error2 TYPE REF TO zcx_abapgit_exception.

  IF p_skip = abap_true.
    RETURN.
  ENDIF.

  IF p_down = abap_true OR p_seri = abap_true.
    CREATE OBJECT lo_dep_ser
      EXPORTING
        is_options = ps_options.
    TRY.
        lt_local = lo_dep_ser->serialize( pt_deps ).
      CATCH zcx_abapgit_exception INTO lx_error2.
        MESSAGE lx_error2 TYPE 'E'.
    ENDTRY.
  ENDIF.

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
