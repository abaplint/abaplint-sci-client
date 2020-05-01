CLASS ZCL_ABAPLINT_BACKEND_API_AGENT DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        iv_host TYPE zabaplint_glob_data-url
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abaplint_backend_api_agent.
    METHODS request
      IMPORTING
        iv_uri TYPE string
        iv_method TYPE string DEFAULT if_http_request=>co_request_method_get
        " TODO query ?
        " TODO headers ?
        iv_json TYPE string OPTIONAL
      RETURNING
        VALUE(ro_json) TYPE REF TO zif_abaplint_json_reader
      RAISING
        zcx_abaplint_error .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_host TYPE zabaplint_glob_data-url.

    METHODS create_client
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client
      RAISING
        zcx_abaplint_error .
    METHODS parse_response
      IMPORTING
        ii_response TYPE REF TO if_http_response
      RETURNING
        VALUE(ro_json) TYPE REF TO zif_abaplint_json_reader
      RAISING
        zcx_abaplint_error .
    METHODS send_receive
      IMPORTING
        ii_client TYPE REF TO if_http_client
      RAISING
        zcx_abaplint_error .
ENDCLASS.



CLASS ZCL_ABAPLINT_BACKEND_API_AGENT IMPLEMENTATION.


  METHOD create.

    CREATE OBJECT ro_instance.
    ro_instance->mv_host = iv_host.

  ENDMETHOD.


  METHOD create_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = |{ mv_host }|
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
          message = |Create_client error: sy-subrc={ sy-subrc }, url={ mv_host }|.
    ENDIF.

  ENDMETHOD.


  METHOD parse_response.

    DATA lv_scode TYPE i.
    DATA lv_sreason TYPE string.
    DATA lv_response TYPE string.

    ii_response->get_status(
      IMPORTING
        code   = lv_scode
        reason = lv_sreason ).
    lv_response = ii_response->get_cdata( ).

    IF lv_response IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |API request failed [{ lv_scode }]|.
    ENDIF.

    DATA li_reader TYPE REF TO zif_abaplint_json_reader.
    li_reader = zcl_abaplint_json_reader=>parse( lv_response ).

    IF li_reader->exists( '/success' ) = abap_false.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |API request failed [{ lv_scode }]: Unexpected API response shape|.
    ENDIF.

    IF li_reader->value_integer( '/success' ) <> 1.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |API request failed [{ lv_scode }]: { li_reader->value_string( '/error/message' ) }|.
    ENDIF.

    IF lv_scode < 200 OR lv_scode >= 300.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |API request failed [{ lv_scode }], but API response is OK (?)|.
    ENDIF.

    ro_json = li_reader->sub_section( '/payload' ).

  ENDMETHOD.


  METHOD request.

    DATA li_client TYPE REF TO if_http_client.
    li_client = create_client( ).

    cl_http_utility=>set_request_uri(
      request = li_client->request
      uri     = iv_uri ).

    li_client->request->set_method( iv_method ).
    li_client->request->set_header_field(
      name  = 'content-type'
      value = 'application/json' ).

    send_receive( li_client ).
    ro_json = parse_response( li_client->response ).
    li_client->close( ).

  ENDMETHOD.


  METHOD send_receive.

    ii_client->send(
      EXPORTING
        timeout = 6000
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc  = 0.
      ii_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

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

  ENDMETHOD.
ENDCLASS.
