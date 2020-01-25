CLASS zcl_abaplint_backend DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_issue,
             message TYPE string,
           END OF ty_issue.

    TYPES: ty_issues TYPE STANDARD TABLE OF ty_issue WITH EMPTY KEY.

    METHODS check_object
      RETURNING VALUE(rt_issues) TYPE ty_issues.
  PROTECTED SECTION.

    METHODS send
      IMPORTING
        !ii_client TYPE REF TO if_http_client .
    METHODS create_client
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_BACKEND IMPLEMENTATION.


  METHOD check_object.

    DATA(li_client) = create_client( ).

    DATA(lv_cdata) = |\{\n| &&
      |  "configuration": "hello",\n| &&
      |  "object": \{\n| &&
      |    "objectName": "MOO",\n| &&
      |    "objectType": "PROG"\n| &&
      |  \},\n| &&
      |  "files": [\{"name": "zfoobar.prog.abap", "contents": "WRITE hello."\}]\n| &&
      |\}|.

    li_client->request->set_cdata( lv_cdata ).

    send( li_client ).

    DATA(lv_response) = li_client->response->get_cdata( ).
    DATA(lo_reader) = NEW zcl_abaplint_json_reader( lv_response ).
    LOOP AT lo_reader->members( '/issues' ) INTO DATA(lv_issue).
      DATA(lv_message) = lo_reader->value_string( '/issues/' && lv_issue && '/message').
      APPEND VALUE #( message = lv_message ) TO rt_issues.
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
        url                = 'https://abaplint-grumpy-crocodile-tl.cfapps.eu10.hana.ondemand.com'
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
    ENDIF.

  ENDMETHOD.
ENDCLASS.
