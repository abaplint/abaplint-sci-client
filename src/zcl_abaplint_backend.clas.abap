CLASS zcl_abaplint_backend DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_position,
        row TYPE token_row,
        col TYPE token_col,
      END OF ty_position .
    TYPES:
      BEGIN OF ty_issue,
        message  TYPE string,
        key      TYPE string,
        filename TYPE string,
        severity TYPE string,
        start    TYPE ty_position,
      END OF ty_issue .
    TYPES:
      ty_issues TYPE STANDARD TABLE OF ty_issue WITH KEY table_line .
    TYPES:
      BEGIN OF ty_message,
        error   TYPE abap_bool,
        message TYPE string,
      END OF ty_message .
    TYPES:
      " Must match the content of the payload for /api/v1/list_rules
      " https://github.com/abaplint/abaplint-sci-client/pull/158
      BEGIN OF ty_rule,
        key   TYPE string,
        title TYPE string,
      END OF ty_rule .
    TYPES:
      ty_rules TYPE STANDARD TABLE OF ty_rule WITH KEY key .

    METHODS check_object
      IMPORTING
        !iv_configuration TYPE string
        !iv_object_type   TYPE trobjtype
        !iv_object_name   TYPE sobj_name
      RETURNING
        VALUE(rt_issues)  TYPE ty_issues
      RAISING
        zcx_abaplint_error
        zcx_abapgit_exception .
    METHODS ping
      RETURNING
        VALUE(rs_message) TYPE ty_message
      RAISING
        zcx_abaplint_error .
    METHODS get_default_config
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abaplint_error .
    METHODS list_rules
      RETURNING
        VALUE(rt_rules) TYPE ty_rules
      RAISING
        zcx_abaplint_error .
    METHODS constructor
      IMPORTING
        !is_config TYPE zabaplint_glob_data OPTIONAL .
  PROTECTED SECTION.

    DATA ms_config TYPE zabaplint_glob_data .

    METHODS escape
      IMPORTING
        !iv_input        TYPE clike
      RETURNING
        VALUE(rv_output) TYPE string .
    METHODS base64_encode
      IMPORTING
        !iv_bin          TYPE xstring
      RETURNING
        VALUE(rv_base64) TYPE string .
    METHODS build_deps
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rv_files) TYPE string
      RAISING
        zcx_abaplint_error
        zcx_abapgit_exception .
    METHODS build_files
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rv_files) TYPE string .
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_uri,
        ping               TYPE string VALUE '/api/v1/ping',
        check_file         TYPE string VALUE '/api/v1/check_file',
        get_default_config TYPE string VALUE '/api/v1/default_config',
        list_rules         TYPE string VALUE '/api/v1/list_rules',
      END OF c_uri.
ENDCLASS.



CLASS ZCL_ABAPLINT_BACKEND IMPLEMENTATION.


  METHOD base64_encode.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata = iv_bin
      IMPORTING
        b64data = rv_base64.

  ENDMETHOD.


  METHOD build_deps.

    DATA lt_files TYPE string_table.
    DATA lo_deps TYPE REF TO zcl_abaplint_deps.
    DATA lt_found TYPE zif_abapgit_definitions=>ty_files_tt.

    CREATE OBJECT lo_deps.
    lt_found = lo_deps->find(
      iv_depth       = ms_config-depth
      iv_continue    = ms_config-conti
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name ).

    DATA ls_file LIKE LINE OF lt_found.
    DATA lv_contents TYPE string.
    LOOP AT lt_found INTO ls_file.
      lv_contents = base64_encode( ls_file-data ).
      lv_contents = |\{"name": "{ escape( ls_file-filename ) }", "contents": "{ lv_contents }"\}|.
      APPEND lv_contents TO lt_files.
    ENDLOOP.

    CONCATENATE LINES OF lt_files INTO rv_files SEPARATED BY ','.
    rv_files = |[{ rv_files }]|.

  ENDMETHOD.


  METHOD build_files.

* todo, this should be done relative to what is the root abapGit folder and
* also take settings into account, but not super important?

    DATA lt_files TYPE string_table.
    DATA ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.

    ls_files_item-item-obj_type = iv_object_type.
    ls_files_item-item-obj_name = iv_object_name.

    TRY.
        ls_files_item = zcl_abapgit_objects=>serialize(
          is_item     = ls_files_item-item
          iv_language = sy-langu ).
      CATCH zcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

    DATA ls_file LIKE LINE OF ls_files_item-files.
    DATA lv_contents TYPE string.
    LOOP AT ls_files_item-files INTO ls_file.
      lv_contents = base64_encode( ls_file-data ).
      lv_contents = |\{"name": "{ escape( ls_file-filename ) }", "contents": "{ lv_contents }"\}|.
      APPEND lv_contents TO lt_files.
    ENDLOOP.

    CONCATENATE LINES OF lt_files INTO rv_files SEPARATED BY ','.
    rv_files = |[{ rv_files }]|.

  ENDMETHOD.


  METHOD check_object.

    IF iv_object_type = 'DEVC'.
      RETURN.
    ENDIF.

    DATA lv_files TYPE string.
    lv_files = build_files(
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name ).

    DATA lv_deps TYPE string.
    lv_deps = build_deps(
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name ).

    DATA lv_config TYPE string.
    lv_config = base64_encode( zcl_abapgit_convert=>string_to_xstring_utf8( iv_configuration ) ).

    DATA lv_cdata TYPE string.
    lv_cdata = |\{\n| &&
      |  "configuration": "{ lv_config }",\n| &&
      |  "object": \{\n| &&
      |    "objectName": "{ escape( iv_object_name ) }",\n| &&
      |    "objectType": "{ iv_object_type }"\n| &&
      |  \},\n| &&
      |  "deps": { lv_deps },\n| &&
      |  "files": { lv_files }\n| &&
      |\}|.

    DATA lo_agent TYPE REF TO zcl_abaplint_backend_api_agent.
    DATA li_json TYPE REF TO zif_ajson_reader.

    lo_agent = zcl_abaplint_backend_api_agent=>create( ms_config-url ).
    li_json = lo_agent->request(
      iv_method = if_http_request=>co_request_method_post
      iv_uri    = c_uri-check_file
      iv_payload = lv_cdata ).

    DATA lt_issues TYPE string_table.
    DATA lv_issue LIKE LINE OF lt_issues.
    DATA lv_prefix TYPE string.
    FIELD-SYMBOLS <issue> LIKE LINE OF rt_issues.

    lt_issues = li_json->members( '/issues' ).

    LOOP AT lt_issues INTO lv_issue.
      lv_prefix = '/issues/' && lv_issue && '/data'.
      APPEND INITIAL LINE TO rt_issues ASSIGNING <issue>.
      <issue>-message   = li_json->get_string( lv_prefix && '/message' ).
      <issue>-key       = li_json->get_string( lv_prefix && '/key' ).
      <issue>-filename  = li_json->get_string( lv_prefix && '/filename' ).
      <issue>-severity  = li_json->get_string( lv_prefix && '/severity' ).
      <issue>-start-row = li_json->get_string( lv_prefix && '/start/row' ).
      <issue>-start-col = li_json->get_string( lv_prefix && '/start/col' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    IF is_config IS SUPPLIED.
      ms_config = is_config.
    ELSE.
      DATA lo_config TYPE REF TO zcl_abaplint_configuration.
      CREATE OBJECT lo_config.
      ms_config = lo_config->get_global( ).
    ENDIF.

  ENDMETHOD.


  METHOD escape.

    rv_output = iv_input.

    REPLACE ALL OCCURRENCES OF '"' IN rv_output WITH '\"'.

  ENDMETHOD.


  METHOD get_default_config.

    DATA lo_agent TYPE REF TO zcl_abaplint_backend_api_agent.
    DATA li_json TYPE REF TO zif_ajson_reader.
    DATA lo_json TYPE REF TO zcl_ajson.
    DATA lx_error TYPE REF TO zcx_ajson_error.

    lo_agent = zcl_abaplint_backend_api_agent=>create( ms_config-url ).

    li_json = lo_agent->request( c_uri-get_default_config ).
    li_json = li_json->slice( '/config' ).
    lo_json ?= li_json.
    TRY.
        rv_json = lo_json->stringify( iv_indent = 2 ).
      CATCH zcx_ajson_error INTO lx_error.
        RAISE EXCEPTION TYPE zcx_abaplint_error
          EXPORTING
            message = lx_error->message.
    ENDTRY.

    IF rv_json IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = 'Fetched Config is empty!'.
    ENDIF.


  ENDMETHOD.


  METHOD list_rules.

    DATA:
      lo_agent TYPE REF TO zcl_abaplint_backend_api_agent,
      li_json  TYPE REF TO zif_ajson_reader,
      lx_error TYPE REF TO zcx_ajson_error.

    " Get a list of all abaplint rules via /api/v1/list_rules
    " https://github.com/abaplint/abaplint-sci-server/pull/223
    lo_agent = zcl_abaplint_backend_api_agent=>create( ms_config-url ).

    li_json = lo_agent->request( c_uri-list_rules ).

    IF NOT li_json IS BOUND.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = 'Fetched rules are empty!'.
    ENDIF.

    TRY.
        li_json->to_abap( IMPORTING ev_container = rt_rules ).
      CATCH zcx_ajson_error INTO lx_error.
        RAISE EXCEPTION TYPE zcx_abaplint_error
          EXPORTING
            message = lx_error->message.
    ENDTRY.

  ENDMETHOD.


  METHOD ping.

    DATA lx_error TYPE REF TO zcx_abaplint_error.
    DATA lo_agent TYPE REF TO zcl_abaplint_backend_api_agent.
    DATA li_json TYPE REF TO zif_ajson_reader.

    lo_agent = zcl_abaplint_backend_api_agent=>create( ms_config-url ).

    TRY.
        li_json = lo_agent->request( c_uri-ping ).
        rs_message-message = li_json->get_string( '' ).
        rs_message-error   = abap_false.
      CATCH zcx_abaplint_error INTO lx_error.
        rs_message-message = lx_error->message.
        rs_message-error   = abap_true.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
