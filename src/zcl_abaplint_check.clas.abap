CLASS zcl_abaplint_check DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

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
    METHODS get_attributes
        REDEFINITION .
  PROTECTED SECTION.

    DATA mo_config TYPE REF TO zcl_abaplint_configuration .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_CHECK IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    description = 'abaplint'.                               "#EC NOTEXT
    category    = 'CL_CI_CATEGORY_TOP'.
    position    = '999'.

    add_obj_type( 'PROG' ).
    add_obj_type( 'FUGR' ).
    add_obj_type( 'CLAS' ).
    add_obj_type( 'INTF' ).

    has_attributes = abap_true.
    has_documentation = abap_true.
    attributes_ok = abap_true.

    mo_config = NEW #( ).

  ENDMETHOD.


  METHOD get_attributes.
    RETURN.
  ENDMETHOD.


  METHOD get_message_text.

    p_text = '&1'.

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


  METHOD run.

    DATA: lv_sub_obj_type TYPE trobjtype,
          lv_sub_obj_name TYPE sobj_name.


    DATA(lo_backend) = NEW zcl_abaplint_backend( ).

    DATA(lt_issues) = lo_backend->check_object(
      iv_configuration = 'hello'
      iv_object_type   = object_type
      iv_object_name   = object_name ).

    LOOP AT lt_issues INTO DATA(ls_issue).

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
          lv_sub_obj_name = object_name .
      ENDCASE.

      inform(
        p_sub_obj_type = lv_sub_obj_type
        p_sub_obj_name = lv_sub_obj_name
        p_line         = ls_issue-start-row
        p_column       = ls_issue-start-col
        p_test         = myname
        p_kind         = 'E'
        p_param_1      = ls_issue-message
        p_code         = CONV #( ls_issue-key ) ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
