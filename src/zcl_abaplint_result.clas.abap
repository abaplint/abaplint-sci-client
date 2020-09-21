CLASS zcl_abaplint_result DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_result_program
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_kind TYPE sychar01 .

    METHODS if_ci_test~display_documentation
        REDEFINITION .
  PROTECTED SECTION.

    METHODS get_text
        REDEFINITION .
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPLINT_RESULT IMPLEMENTATION.


  METHOD constructor.

    super->constructor( p_kind              = iv_kind
                        p_has_documentation = abap_true ).

  ENDMETHOD.


  METHOD get_text.

    DATA lv_code TYPE sci_errc.

    " Temporarily remove code so we get the message template from ZCL_ABAPLINT_CHECK
    lv_code = result-code.
    IF lv_code CP 'LINT_*'.
      result-code = ''.
    ENDIF.

    p_result = super->get_text( ).

    result-code = lv_code.

  ENDMETHOD.


  METHOD if_ci_test~display_documentation.

    DATA lv_rule TYPE string.

    IF result-param2 IS INITIAL.
      lv_rule = zcl_abaplint_check=>get_rule( result-code ).
    ELSE.
      lv_rule = result-param2.
    ENDIF.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = 'https://rules.abaplint.org/' && lv_rule
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
ENDCLASS.
