CLASS zcl_abaplint_backend DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_position,
             row TYPE token_row,
             col TYPE token_col,
           END OF ty_position.

    TYPES:
      BEGIN OF ty_issue,
        message  TYPE string,
        key      TYPE string,
        filename TYPE string,
        start    TYPE ty_position,
      END OF ty_issue .
    TYPES:
      ty_issues TYPE STANDARD TABLE OF ty_issue WITH EMPTY KEY .

    METHODS check_object
      IMPORTING
        !iv_configuration TYPE string
        !iv_object_type   TYPE trobjtype
        !iv_object_name   TYPE sobj_name
      RETURNING
        VALUE(rt_issues)  TYPE ty_issues .
  PROTECTED SECTION.

    METHODS build_files
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rv_files) TYPE string .
    METHODS send
      IMPORTING
        !ii_client TYPE REF TO if_http_client .
    METHODS create_client
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_BACKEND IMPLEMENTATION.


  METHOD build_files.

* todo, this should be done relative to what is the root abapGit folder and
* also take settings into account

    DATA: ls_files_item TYPE zcl_abapgit_objects=>ty_serialization,
          lv_contents   TYPE string,
          lt_files      TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    ls_files_item-item-obj_type = iv_object_type.
    ls_files_item-item-obj_name = iv_object_name.

    TRY.
        ls_files_item = zcl_abapgit_objects=>serialize(
          is_item     = ls_files_item-item
          iv_language = sy-langu ).
      CATCH zcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

    LOOP AT ls_files_item-files INTO DATA(ls_file).
      CALL FUNCTION 'SSFC_BASE64_ENCODE'
        EXPORTING
          bindata = ls_file-data
        IMPORTING
          b64data = lv_contents.
      APPEND |\{"name": "{ ls_file-filename }", "contents": "{ lv_contents }"\}| TO lt_files.
    ENDLOOP.

    CONCATENATE LINES OF lt_files INTO rv_files SEPARATED BY ','.
    rv_files = |[{ rv_files }]|.

  ENDMETHOD.


  METHOD check_object.

    DATA(li_client) = create_client( ).

    DATA(lv_files) = build_files(
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name ).

    DATA(lv_cdata) = |\{\n| &&
      |  "configuration": "{ iv_configuration }",\n| &&
      |  "object": \{\n| &&
      |    "objectName": "{ iv_object_name }",\n| &&
      |    "objectType": "{ iv_object_type }"\n| &&
      |  \},\n| &&
      |  "files": { lv_files }\n| &&
      |\}|.

    li_client->request->set_cdata( lv_cdata ).

    send( li_client ).

    DATA(lv_response) = li_client->response->get_cdata( ).
    DATA(lo_reader) = NEW zcl_abaplint_json_reader( lv_response ).
    LOOP AT lo_reader->members( '/issues' ) INTO DATA(lv_issue).
      DATA(lv_prefix) = '/issues/' && lv_issue.
      APPEND VALUE #(
        message  = lo_reader->value_string( lv_prefix && '/message' )
        key      = lo_reader->value_string( lv_prefix && '/key' )
        filename = lo_reader->value_string( lv_prefix && '/filename' )
        start    = VALUE #(
          row = lo_reader->value_string( lv_prefix && '/start/row' )
          col = lo_reader->value_string( lv_prefix && '/start/col' ) )
        ) TO rt_issues.
    ENDLOOP.

    li_client->close( ).

  ENDMETHOD.


  METHOD create_client.

*    IF NOT is_client-rfc IS INITIAL.
*
*      cl_http_client=>create_by_destination(
*        EXPORTING
*          destination              = is_client-rfc
*        IMPORTING
*          client                   = ri_client
*        EXCEPTIONS
*          argument_not_found       = 1
*          destination_not_found    = 2
*          destination_no_authority = 3
*          plugin_not_active        = 4
*          internal_error           = 5
*          OTHERS                   = 6 ).
*      ASSERT sy-subrc = 0.
*
*    ELSE.

    cl_http_client=>create_by_url(
      EXPORTING
*          url                = is_client-url
*          ssl_id             = is_client-ssl_id
        url                = 'https://abaplint-relaxed-nyala-dg.cfapps.eu10.hana.ondemand.com/'
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = ri_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    ASSERT sy-subrc = 0.

*      ri_client->propertytype_logon_popup = if_http_client=>co_disabled.
*      ri_client->authenticate(
*        username = is_client-user
*        password = is_client-pass ).
*
*    ENDIF.

    cl_http_utility=>set_request_uri(
      request = ri_client->request
      uri     = |/api/v1/check_file| ).

  ENDMETHOD.


  METHOD send.

    ii_client->request->set_header_field(
      name  = 'content-type'
      value = 'application/json' ).
    ii_client->request->set_header_field(
      name  = '~request_method'
      value = 'POST' ).

    ii_client->send( ).
    ii_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      ii_client->get_last_error(
        IMPORTING
          code    = DATA(lv_ecode)
          message = DATA(lv_emessage) ).

* todo,     RAISE EXCEPTION
    ENDIF.

    ii_client->response->get_status(
      IMPORTING
        code   = DATA(lv_scode)
        reason = DATA(lv_sreason) ).
    IF lv_scode <> 200.
* todo,      RAISE EXCEPTION
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
