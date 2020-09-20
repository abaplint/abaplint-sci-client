CLASS zcl_abaplint_check DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    METHODS constructor .
    CLASS-METHODS get_rule
      IMPORTING
        !iv_code         TYPE sci_errc
      RETURNING
        VALUE(rv_result) TYPE string .

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

    TYPES:
      BEGIN OF ty_internal,
        sub_obj_type TYPE trobjtype,
        sub_obj_name TYPE sobj_name,
        line         TYPE token_row,
        column       TYPE token_col,
      END OF ty_internal .

    CONSTANTS c_no_config TYPE sci_errc VALUE 'NO_CONFIG' ##NO_TEXT.
    CONSTANTS c_stats TYPE trobjtype VALUE '1STA' ##NO_TEXT.

    METHODS hash
      IMPORTING
        !iv_value      TYPE clike
      RETURNING
        VALUE(rv_hash) TYPE sci_errc .
    METHODS map_to_internal
      IMPORTING
        !is_issue        TYPE zcl_abaplint_backend=>ty_issue
      RETURNING
        VALUE(rs_result) TYPE ty_internal .
    METHODS output_issues
      IMPORTING
        !it_issues TYPE zcl_abaplint_backend=>ty_issues .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_map,
        rule     TYPE string,
        code     TYPE sci_errc,
        title    TYPE string,
        severity TYPE sci_errty,
      END OF ty_map .

    CLASS-DATA:
      gt_map TYPE STANDARD TABLE OF ty_map WITH DEFAULT KEY .

    METHODS add_messages .
    CLASS-METHODS init_mapping .
    METHODS get_mapping
      IMPORTING
        !iv_rule         TYPE string
      RETURNING
        VALUE(rv_result) TYPE sci_errc .
ENDCLASS.



CLASS ZCL_ABAPLINT_CHECK IMPLEMENTATION.


  METHOD add_messages.

    DATA:
      ls_map TYPE ty_map,
      ls_msg TYPE scimessage.

    LOOP AT gt_map INTO ls_map.
      CLEAR ls_msg.
      ls_msg-test = myname.
      ls_msg-code = ls_map-code.
      ls_msg-kind = ls_map-severity.
      ls_msg-text = ls_map-title.
      ls_msg-pcom = ''. "Pseudo Comment
      ls_msg-pcom_alt = ''. "Pragma
      INSERT ls_msg INTO TABLE scimessages.
    ENDLOOP.

  ENDMETHOD.


  METHOD class_constructor.

    init_mapping( ).

  ENDMETHOD.


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

    add_messages( ).

  ENDMETHOD.


  METHOD get_attributes.
    RETURN.
  ENDMETHOD.


  METHOD get_mapping.

    DATA ls_map TYPE ty_map.

    READ TABLE gt_map INTO ls_map WITH KEY rule = iv_rule.
    IF sy-subrc <> 0.
      " In case of no mapping fallback to hash of rule name
      ls_map-rule = iv_rule.
      ls_map-code = hash( iv_rule ).
      INSERT ls_map INTO TABLE gt_map.
    ENDIF.

    rv_result = ls_map-code.

  ENDMETHOD.


  METHOD get_message_text.

    DATA ls_smsg TYPE LINE OF scimessages.

    IF p_code = c_no_config.
      p_text = 'No configuration found when looking at package hierarchy, &1'.
    ELSEIF p_code CP 'LINT_*'.
      READ TABLE scimessages INTO ls_smsg TRANSPORTING text
        WITH TABLE KEY test = myname code = p_code.
      IF sy-subrc = 0.
        p_text = ls_smsg-text.
      ELSE.
        p_text = '&1 (&2)'.
      ENDIF.
    ELSE.
      p_text = '&1 (&2)'.
    ENDIF.

  ENDMETHOD.


  METHOD get_result_node.

    CREATE OBJECT p_result TYPE zcl_abaplint_result
      EXPORTING
        iv_kind = p_kind.

  ENDMETHOD.


  METHOD get_rule.

    DATA ls_map TYPE ty_map.

    READ TABLE gt_map INTO ls_map WITH KEY code = iv_code.
    IF sy-subrc = 0.
      rv_result = ls_map-rule.
    ENDIF.

  ENDMETHOD.


  METHOD hash.

    DATA: lv_hash TYPE hash160.

    CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
      EXPORTING
        data           = iv_value
      IMPORTING
        hash           = lv_hash
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Take the first 5 characters of the hash
    rv_hash = lv_hash(5).

  ENDMETHOD.


  METHOD if_ci_test~display_documentation.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = 'https://rules.abaplint.org/'
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


  METHOD init_mapping.

    DATA:
      lo_backend TYPE REF TO zcl_abaplint_backend,
      lt_rules   TYPE zcl_abaplint_backend=>ty_rules,
      lv_code    TYPE n LENGTH 4,
      ls_map     TYPE ty_map.

    FIELD-SYMBOLS <ls_rule> LIKE LINE OF lt_rules.

    CREATE OBJECT lo_backend.

    TRY.
        lt_rules = lo_backend->list_rules( ).
      CATCH zcx_abaplint_error.
        " This will fallback to hashed rule names
        RETURN.
    ENDTRY.

    " Number rules sequentially
    LOOP AT lt_rules ASSIGNING <ls_rule>.
      lv_code = lv_code + 1.
      CLEAR ls_map.
      ls_map-code = 'LINT_' && lv_code.
      ls_map-rule = <ls_rule>-key.
      ls_map-title = <ls_rule>-title.
      " todo, <ls_rule>-severity once available in API
      ls_map-severity = 'E'.
      " SCI_ERRTY is using N for info (sic)
      TRANSLATE ls_map-severity USING 'IN'.
      INSERT ls_map INTO TABLE gt_map.
    ENDLOOP.

  ENDMETHOD.


  METHOD map_to_internal.

    DATA:
      lt_tabl         TYPE TABLE OF string,
      lv_subc         TYPE subc,
      lv_name         TYPE string,
      lv_target       TYPE string,
      lv_pname        TYPE pname,
      lv_include      TYPE includenr,
      lv_namespace    TYPE namespace,
      lv_area         TYPE rs38l_area,
      lo_pih          TYPE REF TO cl_oo_source_pos_index_helper,
      li_index_helper TYPE REF TO if_oo_source_pos_index_helper,
      ls_position     TYPE if_oo_source_pos_index_helper=>ty_source_pos_index,
      lv_col          TYPE i.

    rs_result-line   = is_issue-start-row.
    rs_result-column = is_issue-start-col.

    lv_name = to_upper( cl_http_utility=>unescape_url( is_issue-filename ) ).
    REPLACE ALL OCCURRENCES OF '#' IN lv_name WITH '/'.
    SPLIT lv_name AT '.' INTO TABLE lt_tabl.

    CASE object_type.
      WHEN 'FUGR'.
        "Different cases need to be distinguished
        "1. Function Group Level
        "2. Function Level (TFDIR exists)
        "3. Include Level (TRDIR-SUBC = I)

        "Determine Object name
        READ TABLE lt_tabl INDEX 3 INTO lv_name. "Object Name

        "3. Include?
        SELECT SINGLE subc FROM trdir INTO lv_subc WHERE name = lv_name.
        IF sy-subrc = 0 AND lv_subc = 'I'.
          rs_result-sub_obj_type = 'PROG'.
          rs_result-sub_obj_name = lv_name.
        ELSE.
          "2. Function Module?
          SELECT SINGLE pname include FROM tfdir INTO (lv_pname, lv_include) WHERE funcname = lv_name.
          IF sy-subrc = 0.
            CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
              EXPORTING
                program   = lv_pname
              IMPORTING
                namespace = lv_namespace
                group     = lv_area
              EXCEPTIONS
                OTHERS    = 6.
            CONCATENATE lv_namespace 'L' lv_area 'U' lv_include INTO lv_target.
            rs_result-sub_obj_type = 'PROG'.
            rs_result-sub_obj_name = lv_target.
          ELSE.
            "1. Must be main program
            rs_result-sub_obj_type = 'PROG'.
            rs_result-sub_obj_name = lv_name.
          ENDIF.
        ENDIF.
      WHEN 'CLAS'.
        " todo, what if the issue is in the XML file?

        " Determine class part (see ZCL_ABAPGIT_OBJECT_CLAS->SERIALIZE)
        SPLIT is_issue-filename AT '.' INTO TABLE lt_tabl.
        IF lines( lt_tabl ) = 4.
          rs_result-sub_obj_type = 'PROG'.
          rs_result-sub_obj_name = object_name.

          READ TABLE lt_tabl INDEX 3 INTO lv_name.
          CASE lv_name.
            WHEN 'locals_def'.
              rs_result-sub_obj_name+30 = seop_incextapp_definition.
            WHEN 'locals_imp'.
              rs_result-sub_obj_name+30 = seop_incextapp_implementation.
            WHEN 'testclasses'.
              rs_result-sub_obj_name+30 = seop_incextapp_testclasses.
            WHEN 'macros'.
              rs_result-sub_obj_name+30 = seop_incextapp_macros.
          ENDCASE.

          REPLACE ALL OCCURRENCES OF ` ` IN rs_result-sub_obj_name(30) WITH '='.
        ELSE. "methods
          CREATE OBJECT lo_pih.
          li_index_helper ?= lo_pih.

          lv_col = is_issue-start-col. " cast int2 > int4

          " Check class/method index (will automatically create index if it's missing)
          ls_position = li_index_helper->get_class_include_by_position(
            class_name = object_name
            version    = 'A'
            line       = is_issue-start-row
            column     = lv_col ).

          rs_result-sub_obj_type = 'PROG'.
          rs_result-sub_obj_name = ls_position-include_name.
          rs_result-line = ls_position-start_line.
        ENDIF.

      WHEN OTHERS.
        rs_result-sub_obj_type = object_type.
        rs_result-sub_obj_name = object_name.
    ENDCASE.

  ENDMETHOD.


  METHOD output_issues.

    DATA ls_issue LIKE LINE OF it_issues.
    DATA ls_result TYPE ty_internal.

    LOOP AT it_issues INTO ls_issue.

      ls_result = map_to_internal( ls_issue ).

      TRANSLATE ls_issue-filename TO UPPER CASE.
      TRANSLATE ls_issue-filename USING '#/'.
      IF NOT ls_issue-filename CP |*{ object_name }*|.
        ls_issue-message = |{ ls_issue-message }, { ls_issue-filename }|.
      ENDIF.

      inform(
        p_sub_obj_type = ls_result-sub_obj_type
        p_sub_obj_name = ls_result-sub_obj_name
        p_line         = ls_result-line
        p_column       = ls_result-column
        p_test         = myname
        p_kind         = c_error
        p_param_1      = ls_issue-message
        p_param_2      = ls_issue-key
        p_code         = get_mapping( ls_issue-key ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA lx_error TYPE REF TO zcx_abaplint_error.
    DATA lv_config TYPE string.

    TRY.
        lv_config = zcl_abaplint_configuration=>find_from_object(
          iv_object_type = object_type
          iv_object_name = object_name ).
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
        CATCH zcx_abapgit_exception.
          ASSERT 0 = 1. " todo
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
