REPORT zabaplint_runner.

PARAMETERS: p_devc TYPE devclass OBLIGATORY,
            p_max  TYPE i DEFAULT 10 OBLIGATORY,
            p_inc  TYPE c AS CHECKBOX DEFAULT abap_true.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_abapgit_exception zcx_abaplint_error cx_salv_msg.

  DATA lt_total TYPE zcl_abaplint_backend=>ty_issues.

* todo, call zcl_abapgit_objects=>supported_list( ) to get the full list of supported
* this will be required in the future to perform full checks
  DATA lt_supported TYPE zcl_abapgit_objects=>ty_types_tt.
  APPEND 'CLAS' TO lt_supported.
  APPEND 'PROG' TO lt_supported.
  APPEND 'FUGR' TO lt_supported.

  DATA lv_config TYPE string.
  lv_config = zcl_abaplint_configuration=>find_from_package( p_devc ).
  IF lv_config IS INITIAL.
    MESSAGE e003(zabaplint) WITH p_devc.
    RETURN.
  ENDIF.

  DATA lt_objects TYPE zif_abapgit_definitions=>ty_tadir_tt.
  lt_objects = zcl_abapgit_factory=>get_tadir( )->read( p_devc ).

  DELETE lt_objects WHERE obj_name CA '.'.

  DATA ls_object LIKE LINE OF lt_objects.
  DATA lv_index LIKE sy-tabix.
  DATA lv_subc TYPE reposrc-subc.

  LOOP AT lt_objects INTO ls_object.
    lv_index = sy-tabix.
    READ TABLE lt_supported WITH KEY table_line = ls_object-object TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DELETE lt_objects INDEX lv_index.
    ELSEIF p_inc = abap_true AND ls_object-object = 'PROG'.
      SELECT SINGLE subc FROM reposrc INTO lv_subc WHERE progname = ls_object-obj_name AND r3state = 'A'.
      IF sy-subrc = 0 AND lv_subc = 'I'.
        DELETE lt_objects INDEX lv_index.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE lt_objects FROM p_max + 1.

  DATA lv_text TYPE string.
  DATA lo_backend TYPE REF TO zcl_abaplint_backend.
  DATA lt_issues TYPE zcl_abaplint_backend=>ty_issues.
  DATA lx_error TYPE REF TO zcx_abaplint_error.
  FIELD-SYMBOLS <issue> LIKE LINE OF lt_total.
  CREATE OBJECT lo_backend.

  LOOP AT lt_objects INTO ls_object.
    lv_text = |{ ls_object-object } { ls_object-obj_name } { sy-tabix }/{ lines( lt_objects ) }, {
      lines( lt_total ) } issues|.
    cl_progress_indicator=>progress_indicate(
      i_text               = lv_text
      i_processed          = sy-tabix
      i_total              = lines( lt_objects )
      i_output_immediately = abap_true ).

    TRY.
        lt_issues = lo_backend->check_object(
          iv_configuration = lv_config
          iv_object_type   = ls_object-object
          iv_object_name   = ls_object-obj_name ).

        APPEND LINES OF lt_issues TO lt_total.
      CATCH zcx_abaplint_error INTO lx_error.
        APPEND INITIAL LINE TO lt_total ASSIGNING <issue>.
        <issue>-message  = lx_error->get_text( ).
        <issue>-key      = 'EXCEPTION'.
        <issue>-filename = |{ ls_object-object } { ls_object-obj_name }|.
    ENDTRY.
  ENDLOOP.

  PERFORM show USING lt_total.

ENDFORM.

FORM show USING pt_total TYPE zcl_abaplint_backend=>ty_issues RAISING cx_salv_msg.

  DATA: lo_alv TYPE REF TO cl_salv_table.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = lo_alv
    CHANGING
      t_table      = pt_total ).

  lo_alv->get_columns( )->set_optimize( ).
  lo_alv->get_functions( )->set_all( ).
  lo_alv->display( ).

ENDFORM.
