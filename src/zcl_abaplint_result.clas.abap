CLASS zcl_abaplint_result DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_result_program
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !iv_kind TYPE sychar01 .
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
    TYPES:
      BEGIN OF ty_map,
        rule TYPE string,
        code TYPE sci_errc,
      END OF ty_map .

    CLASS-DATA:
      mt_map TYPE TABLE OF ty_map.
ENDCLASS.



CLASS ZCL_ABAPLINT_RESULT IMPLEMENTATION.


  METHOD class_constructor.

    DATA:
      lv_map TYPE string,
      lt_map TYPE TABLE OF string,
      ls_map TYPE ty_map.

    lv_map = '7bit_ascii,LINT_0001|abapdoc,LINT_0002|allowed_object_naming,LINT_0003|allowed_object_types,LINT_0004|'
      && 'ambiguous_statement,LINT_0005|avoid_use,LINT_0006|begin_end_names,LINT_0007|begin_single_include,LINT_0008|'
      && 'chain_mainly_declarations,LINT_0009|check_abstract,LINT_0010|check_comments,LINT_0011|check_ddic,LINT_0012|'
      && 'check_include,LINT_0013|check_no_handler_pragma,LINT_0014|check_syntax,LINT_0015|check_text_elements,'
      && 'LINT_0016|check_transformation_exists,LINT_0017|class_attribute_names,LINT_0018|cloud_types,LINT_0019|'
      && 'colon_missing_space,LINT_0020|commented_code,LINT_0021|constructor_visibility_public,LINT_0022|'
      && 'contains_tab,LINT_0023|definitions_top,LINT_0024|description_empty,LINT_0025|double_space,LINT_0026|'
      && 'downport,LINT_0027|empty_line_in_statement,LINT_0028|empty_statement,LINT_0029|empty_structure,LINT_0030|'
      && 'exit_or_check,LINT_0031|exporting,LINT_0032|forbidden_identifier,LINT_0033|forbidden_pseudo_and_pragma,'
      && 'LINT_0034|forbidden_void_type,LINT_0035|form_tables_obsolete,LINT_0036|fully_type_constants,LINT_0037|'
      && 'functional_writing,LINT_0038|global_class,LINT_0039|identical_form_names,LINT_0040|if_in_if,LINT_0041|'
      && 'implement_methods,LINT_0042|in_statement_indentation,LINT_0043|indentation,LINT_0044|'
      && 'inline_data_old_versions,LINT_0045|keep_single_parameter_on_one_line,LINT_0046|keyword_case,LINT_0047|'
      && 'line_break_multiple_parameters,LINT_0048|line_break_style,LINT_0049|line_length,LINT_0050|line_only_punc,'
      && 'LINT_0051|local_class_naming,LINT_0052|local_testclass_location,LINT_0053|local_variable_names,LINT_0054|'
      && 'main_file_contents,LINT_0055|max_one_statement,LINT_0056|message_exists,LINT_0057|method_length,LINT_0058|'
      && 'method_parameter_names,LINT_0059|mix_returning,LINT_0060|msag_consistency,LINT_0061|names_no_dash,'
      && 'LINT_0062|nesting,LINT_0063|newline_between_methods,LINT_0064|no_public_attributes,LINT_0065|object_naming,'
      && 'LINT_0066|obsolete_statement,LINT_0067|parser_error,LINT_0068|parser_missing_space,LINT_0069|prefer_inline,'
      && 'LINT_0070|prefer_returning_to_exporting,LINT_0071|preferred_compare_operator,LINT_0072|'
      && 'prefix_is_current_class,LINT_0073|reduce_string_templates,LINT_0074|release_idoc,LINT_0075|'
      && 'remove_descriptions,LINT_0076|rfc_error_handling,LINT_0077|selection_screen_naming,LINT_0078|'
      && 'sequential_blank,LINT_0079|short_case,LINT_0080|sicf_consistency,LINT_0081|space_before_colon,LINT_0082|'
      && 'space_before_dot,LINT_0083|sql_escape_host_variables,LINT_0084|start_at_tab,LINT_0085|superclass_final,'
      && 'LINT_0086|tabl_enhancement_category,LINT_0087|try_without_catch,LINT_0088|type_form_parameters,LINT_0089|'
      && 'types_naming,LINT_0090|unknown_types,LINT_0091|unreachable_code,LINT_0092|unused_types,LINT_0093|'
      && 'unused_variables,LINT_0094|use_new,LINT_0095|when_others_last,LINT_0096|whitespace_end,LINT_0097|'
      && 'xml_consistency,LINT_0098'.

    SPLIT lv_map AT '|' INTO TABLE lt_map.

    LOOP AT lt_map INTO lv_map.
      SPLIT lv_map AT ',' INTO ls_map-rule ls_map-code.
      INSERT ls_map INTO TABLE mt_map.
    ENDLOOP.

    SORT mt_map.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( p_kind              = iv_kind
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
