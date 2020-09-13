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

  PERFORM find CHANGING ls_data.
  BREAK-POINT.
  PERFORM zip USING ls_data.

ENDFORM.

FORM zip USING ls_data TYPE ty_data.

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
