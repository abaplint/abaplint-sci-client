CLASS zcl_abaplint_backend_api_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !is_config         TYPE zabaplint_glob_data
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abaplint_backend_api_agent .
    " TODO query ?
    " TODO headers ?
    METHODS request
      IMPORTING
        !iv_uri        TYPE string
        !iv_method     TYPE string DEFAULT if_http_request=>co_request_method_get
        !iv_payload    TYPE string OPTIONAL
      RETURNING
        VALUE(ro_json) TYPE REF TO zif_abapgit_ajson_reader
      RAISING
        zcx_abaplint_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_config TYPE zabaplint_glob_data.

    METHODS create_client
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client
      RAISING
        zcx_abaplint_error .
    METHODS parse_response
      IMPORTING
        ii_response    TYPE REF TO if_http_response
      RETURNING
        VALUE(ro_json) TYPE REF TO zif_abapgit_ajson_reader
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
    ro_instance->ms_config = is_config.
  ENDMETHOD.


  METHOD create_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = |{ ms_config-url }|
        ssl_id             = ms_config-ssl_id
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

    DATA li_reader TYPE REF TO zif_abapgit_ajson_reader.
    DATA lo_ajson_err TYPE REF TO zcx_abapgit_ajson_error.
    TRY.
        li_reader = zcl_abapgit_ajson=>parse( lv_response ).
      CATCH zcx_abapgit_ajson_error INTO lo_ajson_err.
        RAISE EXCEPTION TYPE zcx_abaplint_error EXPORTING message = lo_ajson_err->get_text( ).
    ENDTRY.

    IF li_reader->exists( '/success' ) = abap_false.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |API request failed [{ lv_scode }]: Unexpected API response shape|.
    ENDIF.

    IF li_reader->get_integer( '/success' ) <> 1.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |API request failed [{ lv_scode }]: { li_reader->get_string( '/error/message' ) }|.
    ENDIF.

    IF lv_scode < 200 OR lv_scode >= 300.
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = |API request failed [{ lv_scode }], but API response is OK (?)|.
    ENDIF.

    ro_json = li_reader->slice( '/payload' ).

  ENDMETHOD.


  METHOD request.

    DATA li_client TYPE REF TO if_http_client.
    li_client = create_client( ).

    cl_http_utility=>set_request_uri(
      request = li_client->request
      uri     = iv_uri ).

    li_client->request->set_method( iv_method ).
    li_client->request->set_compression( ).

    IF iv_method = 'POST' AND iv_payload IS NOT INITIAL. " OR PUT ... maybe in future
      li_client->request->set_data( zcl_abapgit_convert=>string_to_xstring_utf8( iv_payload ) ).
    ENDIF.

    li_client->request->set_header_field(
      name  = 'content-type'
      value = 'application/json; charset=utf-8' ).

    send_receive( li_client ).
    ro_json = parse_response( li_client->response ).
    li_client->close( ).

  ENDMETHOD.


  METHOD send_receive.

    ii_client->send(
      EXPORTING
        timeout = ms_config-http_timeout
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
