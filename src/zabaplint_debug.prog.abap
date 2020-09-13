REPORT zabaplint_debug.

TYPES: BEGIN OF ty_data,
         config TYPE string,
         deps   TYPE zif_abapgit_definitions=>ty_files_tt,
         object TYPE zcl_abapgit_objects=>ty_serialization,
       END OF ty_data.

PARAMETERS: p_type TYPE tadir-object OBLIGATORY,
            p_name TYPE tadir-obj_name OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_abapgit_exception zcx_abaplint_error.

  DATA ls_data TYPE ty_data.
  DATA lv_xstr TYPE xstring.

  PERFORM find CHANGING ls_data.
  PERFORM zip USING ls_data CHANGING lv_xstr.
  PERFORM save USING lv_xstr.

ENDFORM.

FORM save USING iv_xstr TYPE xstring RAISING zcx_abapgit_exception.

  DATA: lo_zip      TYPE REF TO cl_abap_zip,
        lv_xstr     TYPE xstring,
        lv_path     TYPE string,
        lv_filename TYPE string.

  lv_path = zcl_abapgit_ui_factory=>get_frontend_services( )->show_file_save_dialog(
    iv_title            = 'Save'
    iv_extension        = 'zip'
    iv_default_filename = 'abaplint-sci-client-data.zip' ).

  zcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
    iv_path = lv_path
    iv_xstr = iv_xstr ).

ENDFORM.

FORM zip USING is_data TYPE ty_data CHANGING cv_xstr TYPE xstring.

  DATA lo_zip TYPE REF TO cl_abap_zip.
  DATA ls_file LIKE LINE OF is_data-deps.

  CREATE OBJECT lo_zip.
  lo_zip->add( name    = 'abaplint.json'
               content = zcl_abapgit_convert=>string_to_xstring_utf8( is_data-config ) ).

  LOOP AT is_data-deps INTO ls_file.
    REPLACE FIRST OCCURRENCE OF '/src/' IN ls_file-path WITH 'deps/'.
    lo_zip->add( name    = |{ ls_file-path }{ ls_file-filename }|
                 content = ls_file-data ).
  ENDLOOP.

  LOOP AT is_data-object-files INTO ls_file.
    lo_zip->add( name    = |src{ ls_file-path }{ ls_file-filename }|
                 content = ls_file-data ).
  ENDLOOP.

  cv_xstr = lo_zip->save( ).

ENDFORM.

FORM find CHANGING cs_data TYPE ty_data RAISING zcx_abapgit_exception zcx_abaplint_error.

* this FORM duplicates some of the logic from class ZCL_ABAPLINT_BACKEND

  DATA lo_config TYPE REF TO zcl_abaplint_configuration.
  DATA ls_config TYPE zabaplint_glob_data.
  DATA lo_deps TYPE REF TO zcl_abaplint_deps.

  cs_data-config = zcl_abaplint_configuration=>find_from_object(
    iv_object_type = p_type
    iv_object_name = p_name ).

  CREATE OBJECT lo_config.
  ls_config = lo_config->get_global( ).

* dep files

  CREATE OBJECT lo_deps.
  cs_data-deps = lo_deps->find(
    iv_depth       = ls_config-depth
    iv_continue    = ls_config-conti
    iv_object_type = p_type
    iv_object_name = p_name ).

* files for the object
  cs_data-object-item-obj_type = p_type.
  cs_data-object-item-obj_name = p_name.

  cs_data-object = zcl_abapgit_objects=>serialize(
    is_item     = cs_data-object-item
    iv_language = sy-langu ).

ENDFORM.
