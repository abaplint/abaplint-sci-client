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
        start    TYPE ty_position,
      END OF ty_issue .
    TYPES:
      ty_issues TYPE STANDARD TABLE OF ty_issue WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_message,
        error   TYPE abap_bool,
        message TYPE string,
      END OF ty_message .

    METHODS check_object
      IMPORTING
        !iv_configuration TYPE string
        !iv_object_type   TYPE trobjtype
        !iv_object_name   TYPE sobj_name
      RETURNING
        VALUE(rt_issues)  TYPE ty_issues
      RAISING
        zcx_abaplint_error .
    METHODS ping
      RETURNING
        VALUE(rs_message) TYPE ty_message
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
        VALUE(rv_files) TYPE string .
    METHODS build_files
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rv_files) TYPE string .
    METHODS send
      IMPORTING
        !ii_client TYPE REF TO if_http_client
      RAISING
        zcx_abaplint_error .
    METHODS create_client
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client
      RAISING
        zcx_abaplint_error .
  PRIVATE SECTION.
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

    DATA lt_files TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lo_deps TYPE REF TO zcl_abaplint_deps.
    DATA lt_found TYPE zif_abapgit_definitions=>ty_files_tt.

    CREATE OBJECT lo_deps.
    lt_found = lo_deps->find(
      iv_depth       = ms_config-depth
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

    DATA li_client TYPE REF TO if_http_client.
    li_client = create_client( ).

    cl_http_utility=>set_request_uri(
      request = li_client->request
      uri     = |/api/v1/check_file| ).

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

    li_client->request->set_cdata( lv_cdata ).

    send( li_client ).

    DATA lv_response TYPE string.
    lv_response = li_client->response->get_cdata( ).

    DATA lo_reader TYPE REF TO zcl_abaplint_json_reader.
    CREATE OBJECT lo_reader EXPORTING iv_json = lv_response.

    DATA lt_issues TYPE string_table.
    DATA lv_issue LIKE LINE OF lt_issues.
    DATA lv_prefix TYPE string.
    FIELD-SYMBOLS <issue> LIKE LINE OF rt_issues.

    lt_issues = lo_reader->members( '/issues' ).

    LOOP AT lt_issues INTO lv_issue.
      lv_prefix = '/issues/' && lv_issue.
      APPEND INITIAL LINE TO rt_issues ASSIGNING <issue>.
      <issue>-message   = lo_reader->value_string( lv_prefix && '/message' ).
      <issue>-key       = lo_reader->value_string( lv_prefix && '/key' ).
      <issue>-filename  = lo_reader->value_string( lv_prefix && '/filename' ).
      <issue>-start-row = lo_reader->value_string( lv_prefix && '/start/row' ).
      <issue>-start-col = lo_reader->value_string( lv_prefix && '/start/col' ).
    ENDLOOP.

    li_client->close( ).

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


  METHOD create_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = |{ ms_config-url }|
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = ri_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |Create_client error: sy-subrc={ sy-subrc }, url={ ms_config-url }|.
    ENDIF.

  ENDMETHOD.


  METHOD escape.

    rv_output = iv_input.

    REPLACE ALL OCCURRENCES OF '"' IN rv_output WITH '\"'.

  ENDMETHOD.


  METHOD ping.

    DATA li_client TYPE REF TO if_http_client.
    li_client = create_client( ).

    cl_http_utility=>set_request_uri(
      request = li_client->request
      uri     = |/api/v1/ping| ).

    DATA lx_error TYPE REF TO zcx_abaplint_error.
    TRY.
        send( li_client ).
        rs_message-message = li_client->response->get_cdata( ).
        rs_message-error   = abap_false.
      CATCH zcx_abaplint_error INTO lx_error.
        rs_message-message = lx_error->message.
        rs_message-error   = abap_true.
    ENDTRY.

    li_client->close( ).

  ENDMETHOD.


  METHOD send.

    ii_client->request->set_header_field(
      name  = 'content-type'
      value = 'application/json' ).
    ii_client->request->set_header_field(
      name  = '~request_method'
      value = 'POST' ).

    ii_client->send( timeout = 6000 ).

    ii_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      DATA lv_ecode TYPE i.
      DATA lv_emessage TYPE string.
      ii_client->get_last_error(
        IMPORTING
          code    = lv_ecode
          message = lv_emessage ).

      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |{ lv_ecode } { lv_emessage }|.
    ENDIF.

    DATA lv_scode TYPE i.
    DATA lv_sreason TYPE string.
    ii_client->response->get_status(
      IMPORTING
        code   = lv_scode
        reason = lv_sreason ).
    IF lv_scode <> 200.
      DATA lv_error_response TYPE string.
      lv_error_response = ii_client->response->get_cdata( ). " good for debugging

      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |{ lv_scode } { lv_sreason }|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
