CLASS zcl_abaplint_check DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS consolidate_for_display
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
    METHODS get_result_node
        REDEFINITION .
    METHODS if_ci_test~display_documentation
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.

    CONSTANTS c_no_config TYPE sci_errc VALUE 'NO_CONFIG' ##NO_TEXT.
    CONSTANTS c_stats TYPE trobjtype VALUE '1STA' ##NO_TEXT.

    METHODS output_issues
      IMPORTING
        !it_issues TYPE zcl_abaplint_backend=>ty_issues .
    METHODS find_configuration
      RETURNING
        VALUE(rv_config) TYPE string
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_CHECK IMPLEMENTATION.


  METHOD consolidate_for_display.

    READ TABLE p_results WITH KEY test = myname sobjtype = c_stats code = c_no_config TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      DELETE p_results FROM sy-tabix + 1 WHERE test = myname AND sobjtype = c_stats AND code = c_no_config.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description = 'abaplint'.                               "#EC NOTEXT
    category    = 'CL_CI_CATEGORY_TOP'.
    position    = '999'.

    add_obj_type( 'PROG' ).
    add_obj_type( 'FUGR' ).
    add_obj_type( 'CLAS' ).
    add_obj_type( 'INTF' ).
* todo, add all types that are supported by abapGit

    has_display_consolidation = abap_true.
    has_attributes = abap_true.
    has_documentation = abap_true.
    attributes_ok = abap_true.

  ENDMETHOD.


  METHOD find_configuration.

    DATA lv_devclass TYPE tadir-devclass.

    SELECT SINGLE devclass FROM tadir
      INTO lv_devclass
      WHERE pgmid = 'R3TR'
      AND object = object_type
      AND obj_name = object_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_config = zcl_abaplint_configuration=>find_from_package( lv_devclass ).

  ENDMETHOD.


  METHOD get_attributes.
    RETURN.
  ENDMETHOD.


  METHOD get_message_text.

    IF p_code = c_no_config.
      p_text = 'No configuration found when looking at package hierarchy, &1'.
    ELSE.
      p_text = '&1'.
    ENDIF.

  ENDMETHOD.


  METHOD get_result_node.

    CREATE OBJECT p_result TYPE cl_ci_result_program
      EXPORTING
        p_kind = p_kind.

  ENDMETHOD.


  METHOD if_ci_test~display_documentation.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = 'https://abaplint.org'
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    CALL FUNCTION 'Z_ABAPLINT_CONFIGURATION'
      EXPORTING
        iv_read_only = p_display.

    attributes_ok = abap_true.

  ENDMETHOD.


  METHOD output_issues.

    DATA: lv_sub_obj_type TYPE trobjtype,
          lv_sub_obj_name TYPE sobj_name.

    DATA ls_issue LIKE LINE OF it_issues.
    DATA lo_pih TYPE REF TO cl_oo_source_pos_index_helper.
    DATA li_index_helper TYPE REF TO if_oo_source_pos_index_helper.

    LOOP AT it_issues INTO ls_issue.

      CASE object_type.
        WHEN 'CLAS'.
* todo, make sure the index exists?
* todo, what if the issue is in the XML file?
* todo, handle the 5 different global class includes
          CREATE OBJECT lo_pih.
          li_index_helper ?= lo_pih.

          DATA ls_position TYPE if_oo_source_pos_index_helper=>ty_source_pos_index.
          DATA lv_col TYPE i.
          lv_col = ls_issue-start-col. " ??? how to avoid ?
          ls_position = li_index_helper->get_class_include_by_position(
            class_name = object_name
            version    = 'A'
            line       = ls_issue-start-row
            column     = lv_col ).

          lv_sub_obj_type = 'PROG'.
          lv_sub_obj_name = ls_position-include_name.
          ls_issue-start-row = ls_position-start_line.
        WHEN OTHERS.
          lv_sub_obj_type = object_type.
          lv_sub_obj_name = object_name.
      ENDCASE.

      TRANSLATE ls_issue-filename TO UPPER CASE.
      IF NOT ls_issue-filename CP |*{ object_name }*|.
        ls_issue-message = |{ ls_issue-message }, { ls_issue-filename }|.
      ENDIF.

      inform(
        p_sub_obj_type = lv_sub_obj_type
        p_sub_obj_name = lv_sub_obj_name
        p_line         = ls_issue-start-row
        p_column       = ls_issue-start-col
        p_test         = myname
        p_kind         = c_error
        p_param_1      = ls_issue-message
        p_code         = |{ ls_issue-key }| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA lx_error TYPE REF TO zcx_abaplint_error.
    DATA lv_config TYPE string.

    TRY.
        lv_config = find_configuration( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

    IF lv_config IS INITIAL.
      inform(
        p_sub_obj_type = c_stats
        p_test         = myname
        p_kind         = c_note
        p_param_1      = |{ object_type } { object_name }|
        p_code         = c_no_config ).
    ELSE.
      TRY.
          DATA lo_backend TYPE REF TO zcl_abaplint_backend.
          DATA lt_issues TYPE zcl_abaplint_backend=>ty_issues.
          CREATE OBJECT lo_backend.
          lt_issues = lo_backend->check_object(
            iv_configuration = lv_config
            iv_object_type   = object_type
            iv_object_name   = object_name ).

          output_issues( lt_issues ).
        CATCH zcx_abaplint_error INTO lx_error.
          inform(
            p_test    = myname
            p_kind    = c_error
            p_param_1 = lx_error->message
            p_code    = 'ERROR' ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
