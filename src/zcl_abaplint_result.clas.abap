CLASS zcl_abaplint_result DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_result_program
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_map,
        rule TYPE string,
        code TYPE sci_errc,
      END OF ty_map .

    CLASS-DATA:
      mt_map TYPE TABLE OF ty_map READ-ONLY .

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !p_kind TYPE sychar01 .
    CLASS-METHODS map_code_to_rule
      IMPORTING
        !iv_code       TYPE sci_errc
      RETURNING
        VALUE(rv_rule) TYPE string .
    CLASS-METHODS map_rule_to_code
      IMPORTING
        !iv_rule       TYPE string
      RETURNING
        VALUE(rv_code) TYPE sci_errc .

    METHODS if_ci_test~display_documentation
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_RESULT IMPLEMENTATION.


  METHOD class_constructor.

    DATA ls_map TYPE ty_map.

    ls_map-rule = '7bit_ascii'.
    ls_map-code = 'LINT_0001'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'abapdoc'.
    ls_map-code = 'LINT_0002'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'allowed_object_naming'.
    ls_map-code = 'LINT_0003'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'allowed_object_types'.
    ls_map-code = 'LINT_0004'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'ambiguous_statement'.
    ls_map-code = 'LINT_0005'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'avoid_use'.
    ls_map-code = 'LINT_0006'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'begin_end_names'.
    ls_map-code = 'LINT_0007'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'begin_single_include'.
    ls_map-code = 'LINT_0008'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'chain_mainly_declarations'.
    ls_map-code = 'LINT_0009'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'check_abstract'.
    ls_map-code = 'LINT_0010'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'check_comments'.
    ls_map-code = 'LINT_0011'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'check_ddic'.
    ls_map-code = 'LINT_0012'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'check_include'.
    ls_map-code = 'LINT_0013'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'check_no_handler_pragma'.
    ls_map-code = 'LINT_0014'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'check_syntax'.
    ls_map-code = 'LINT_0015'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'check_text_elements'.
    ls_map-code = 'LINT_0016'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'check_transformation_exists'.
    ls_map-code = 'LINT_0017'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'class_attribute_names'.
    ls_map-code = 'LINT_0018'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'cloud_types'.
    ls_map-code = 'LINT_0019'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'colon_missing_space'.
    ls_map-code = 'LINT_0020'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'commented_code'.
    ls_map-code = 'LINT_0021'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'constructor_visibility_public'.
    ls_map-code = 'LINT_0022'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'contains_tab'.
    ls_map-code = 'LINT_0023'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'definitions_top'.
    ls_map-code = 'LINT_0024'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'description_empty'.
    ls_map-code = 'LINT_0025'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'double_space'.
    ls_map-code = 'LINT_0026'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'downport'.
    ls_map-code = 'LINT_0027'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'empty_line_in_statement'.
    ls_map-code = 'LINT_0028'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'empty_statement'.
    ls_map-code = 'LINT_0029'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'empty_structure'.
    ls_map-code = 'LINT_0030'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'exit_or_check'.
    ls_map-code = 'LINT_0031'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'exporting'.
    ls_map-code = 'LINT_0032'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'forbidden_identifier'.
    ls_map-code = 'LINT_0033'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'forbidden_pseudo_and_pragma'.
    ls_map-code = 'LINT_0034'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'forbidden_void_type'.
    ls_map-code = 'LINT_0035'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'form_tables_obsolete'.
    ls_map-code = 'LINT_0036'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'fully_type_constants'.
    ls_map-code = 'LINT_0037'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'functional_writing'.
    ls_map-code = 'LINT_0038'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'global_class'.
    ls_map-code = 'LINT_0039'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'identical_form_names'.
    ls_map-code = 'LINT_0040'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'if_in_if'.
    ls_map-code = 'LINT_0041'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'implement_methods'.
    ls_map-code = 'LINT_0042'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'in_statement_indentation'.
    ls_map-code = 'LINT_0043'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'indentation'.
    ls_map-code = 'LINT_0044'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'inline_data_old_versions'.
    ls_map-code = 'LINT_0045'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'keep_single_parameter_on_one_line'.
    ls_map-code = 'LINT_0046'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'keyword_case'.
    ls_map-code = 'LINT_0047'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'line_break_multiple_parameters'.
    ls_map-code = 'LINT_0048'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'line_break_style'.
    ls_map-code = 'LINT_0049'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'line_length'.
    ls_map-code = 'LINT_0050'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'line_only_punc'.
    ls_map-code = 'LINT_0051'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'local_class_naming'.
    ls_map-code = 'LINT_0052'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'local_testclass_location'.
    ls_map-code = 'LINT_0053'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'local_variable_names'.
    ls_map-code = 'LINT_0054'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'main_file_contents'.
    ls_map-code = 'LINT_0055'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'max_one_statement'.
    ls_map-code = 'LINT_0056'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'message_exists'.
    ls_map-code = 'LINT_0057'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'method_length'.
    ls_map-code = 'LINT_0058'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'method_parameter_names'.
    ls_map-code = 'LINT_0059'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'mix_returning'.
    ls_map-code = 'LINT_0060'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'msag_consistency'.
    ls_map-code = 'LINT_0061'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'names_no_dash'.
    ls_map-code = 'LINT_0062'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'nesting'.
    ls_map-code = 'LINT_0063'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'newline_between_methods'.
    ls_map-code = 'LINT_0064'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'no_public_attributes'.
    ls_map-code = 'LINT_0065'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'object_naming'.
    ls_map-code = 'LINT_0066'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'obsolete_statement'.
    ls_map-code = 'LINT_0067'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'parser_error'.
    ls_map-code = 'LINT_0068'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'parser_missing_space'.
    ls_map-code = 'LINT_0069'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'prefer_inline'.
    ls_map-code = 'LINT_0070'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'prefer_returning_to_exporting'.
    ls_map-code = 'LINT_0071'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'preferred_compare_operator'.
    ls_map-code = 'LINT_0072'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'prefix_is_current_class'.
    ls_map-code = 'LINT_0073'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'reduce_string_templates'.
    ls_map-code = 'LINT_0074'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'release_idoc'.
    ls_map-code = 'LINT_0075'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'remove_descriptions'.
    ls_map-code = 'LINT_0076'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'rfc_error_handling'.
    ls_map-code = 'LINT_0077'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'selection_screen_naming'.
    ls_map-code = 'LINT_0078'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'sequential_blank'.
    ls_map-code = 'LINT_0079'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'short_case'.
    ls_map-code = 'LINT_0080'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'sicf_consistency'.
    ls_map-code = 'LINT_0081'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'space_before_colon'.
    ls_map-code = 'LINT_0082'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'space_before_dot'.
    ls_map-code = 'LINT_0083'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'sql_escape_host_variables'.
    ls_map-code = 'LINT_0084'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'start_at_tab'.
    ls_map-code = 'LINT_0085'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'superclass_final'.
    ls_map-code = 'LINT_0086'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'tabl_enhancement_category'.
    ls_map-code = 'LINT_0087'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'try_without_catch'.
    ls_map-code = 'LINT_0088'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'type_form_parameters'.
    ls_map-code = 'LINT_0089'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'types_naming'.
    ls_map-code = 'LINT_0090'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'unknown_types'.
    ls_map-code = 'LINT_0091'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'unreachable_code'.
    ls_map-code = 'LINT_0092'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'unused_types'.
    ls_map-code = 'LINT_0093'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'unused_variables'.
    ls_map-code = 'LINT_0094'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'use_new'.
    ls_map-code = 'LINT_0095'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'when_others_last'.
    ls_map-code = 'LINT_0096'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'whitespace_end'.
    ls_map-code = 'LINT_0097'.
    INSERT ls_map INTO TABLE mt_map.
    ls_map-rule = 'xml_consistency'.
    ls_map-code = 'LINT_0098'.
    INSERT ls_map INTO TABLE mt_map.

    SORT mt_map.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( p_kind              = p_kind
                        p_has_documentation = abap_true ).

  ENDMETHOD.


  METHOD if_ci_test~display_documentation.

    cl_abap_browser=>show_url(
      url     = 'https://rules.abaplint.org/' && map_code_to_rule( result-code )
      title   = 'abaplint Rule'
      buttons = cl_abap_browser=>navigate_html_after_sap_event ).

  ENDMETHOD.


  METHOD map_code_to_rule.

    DATA ls_map TYPE ty_map.

    READ TABLE mt_map INTO ls_map WITH KEY code = iv_code.
    IF sy-subrc = 0.
      rv_rule = ls_map-rule.
    ENDIF.

  ENDMETHOD.


  METHOD map_rule_to_code.

    DATA ls_map TYPE ty_map.

    READ TABLE mt_map INTO ls_map WITH KEY rule = iv_rule.
    IF sy-subrc = 0.
      rv_code = ls_map-code.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
