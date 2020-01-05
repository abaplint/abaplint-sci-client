CLASS zcl_abaplint_check DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS if_ci_test~display_documentation
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
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

  ENDMETHOD.


  METHOD get_message_text.

    p_text = '&1'.                                          "#EC NOTEXT

  ENDMETHOD.


  METHOD if_ci_test~display_documentation.

    BREAK-POINT.

  ENDMETHOD.


  METHOD run.

* object_type, object_name

    DATA(lo_backend) = NEW zcl_abaplint_backend( ).

    LOOP AT lo_backend->check_object( ) INTO DATA(ls_issue).
      inform(
        p_sub_obj_type = object_type
        p_sub_obj_name = object_name
        p_test    = myname
        p_kind    = 'E'
        p_param_1 = ls_issue-message
        p_code    = '0123456789' ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
