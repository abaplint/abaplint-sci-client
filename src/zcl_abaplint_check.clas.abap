CLASS zcl_abaplint_check DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

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
    METHODS consolidate_for_display
        REDEFINITION .
  PROTECTED SECTION.

    CONSTANTS c_no_config TYPE sci_errc VALUE 'NO_CONFIG' ##NO_TEXT.
    CONSTANTS c_stats TYPE trobjtype VALUE '1STA' ##NO_TEXT.

    METHODS find_configuration
      RETURNING
        VALUE(rv_config) TYPE string .
    METHODS output_issues
      IMPORTING
        !it_issues TYPE zcl_abaplint_backend=>ty_issues .
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

    SELECT SINGLE devclass FROM tadir
      INTO @DATA(lv_devclass)
      WHERE pgmid = 'R3TR'
      AND object = @object_type
      AND obj_name = @object_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_packages) = zcl_abapgit_factory=>get_sap_package( lv_devclass )->list_superpackages( ).
* todo, cache this in static variable
    DATA(lt_config) = NEW zcl_abaplint_configuration( )->list_packages( ).

    LOOP AT lt_packages INTO DATA(lv_package).
      READ TABLE lt_config WITH KEY devclass = lv_package INTO DATA(ls_config).
      IF sy-subrc = 0.
        rv_config = ls_config-json.
        RETURN.
      ENDIF.
    ENDLOOP.

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

    p_result = NEW cl_ci_result_program( p_kind ).

  ENDMETHOD.


  METHOD if_ci_test~display_documentation.

    cl_gui_frontend_services=>execute( document = 'https://abaplint.org' ).

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


    LOOP AT it_issues INTO DATA(ls_issue).

      CASE object_type.
        WHEN 'CLAS'.
* todo, make sure the index exists?
* todo, what if the issue is in the XML file?
* todo, handle the 5 different global class includes
          DATA(li_index_helper) = CAST if_oo_source_pos_index_helper( NEW cl_oo_source_pos_index_helper( ) ).
          DATA(ls_position) = li_index_helper->get_class_include_by_position(
            class_name = object_name
            version    = 'A'
            line       = ls_issue-start-row
            column     = CONV #( ls_issue-start-col ) ).

          lv_sub_obj_type = 'PROG'.
          lv_sub_obj_name = ls_position-include_name.
          ls_issue-start-row = ls_position-start_line.
        WHEN OTHERS.
          lv_sub_obj_type = object_type.
          lv_sub_obj_name = object_name.
      ENDCASE.

      inform(
        p_sub_obj_type = lv_sub_obj_type
        p_sub_obj_name = lv_sub_obj_name
        p_line         = ls_issue-start-row
        p_column       = ls_issue-start-col
        p_test         = myname
        p_kind         = c_error
        p_param_1      = ls_issue-message
        p_code         = CONV #( ls_issue-key ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA(lv_config) = find_configuration( ).

    IF lv_config IS INITIAL.
      inform(
        p_sub_obj_type = c_stats
        p_test         = myname
        p_kind         = c_note
        p_param_1      = |{ object_type } { object_name }|
        p_code         = c_no_config ).
    ELSE.
      DATA(lt_issues) = NEW zcl_abaplint_backend( )->check_object(
        iv_configuration = lv_config
        iv_object_type   = object_type
        iv_object_name   = object_name ).

      output_issues( lt_issues ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
