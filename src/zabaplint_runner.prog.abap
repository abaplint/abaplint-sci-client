REPORT zabaplint_runner.

PARAMETERS: p_devc TYPE devclass OBLIGATORY,
            p_max  TYPE i DEFAULT 10 OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_abapgit_exception zcx_abaplint_error cx_salv_msg.

  DATA: lt_total TYPE zcl_abaplint_backend=>ty_issues.


  DATA(lt_supported) = zcl_abapgit_objects=>supported_list( ).
  DATA(lv_config) = NEW zcl_abaplint_configuration( )->read_package( p_devc ).
  DATA(lt_objects) = zcl_abapgit_factory=>get_tadir( )->read( p_devc ).

  DELETE lt_objects WHERE object = 'AVAS'.

  LOOP AT lt_objects INTO DATA(ls_object).
    DATA(lv_index) = sy-tabix.
    READ TABLE lt_supported WITH KEY table_line = ls_object-object TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DELETE lt_objects INDEX lv_index.
    ENDIF.
  ENDLOOP.

  DELETE lt_objects FROM p_max + 1.

  LOOP AT lt_objects INTO ls_object.
    cl_progress_indicator=>progress_indicate(
      i_text               = |{ ls_object-obj_name } { sy-tabix }/{ lines( lt_objects ) }|
      i_processed          = sy-tabix
      i_total              = lines( lt_objects )
      i_output_immediately = abap_true ).

    DATA(lt_issues) = NEW zcl_abaplint_backend( )->check_object(
      iv_configuration = lv_config
      iv_object_type   = ls_object-object
      iv_object_name   = ls_object-obj_name ).

    APPEND LINES OF lt_issues TO lt_total.
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
  lo_alv->display( ).

ENDFORM.
