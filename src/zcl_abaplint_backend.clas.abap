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
      ty_issues TYPE STANDARD TABLE OF ty_issue WITH EMPTY KEY .

    METHODS check_object
      IMPORTING
        !iv_configuration TYPE string
        !iv_object_type   TYPE trobjtype
        !iv_object_name   TYPE sobj_name
      RETURNING
        VALUE(rt_issues)  TYPE ty_issues .
    METHODS ping
      RETURNING
        VALUE(rv_error) TYPE string .
    METHODS constructor
      IMPORTING
        !is_config TYPE zabaplint_glob_data OPTIONAL .
  PROTECTED SECTION.

    DATA ms_config TYPE zabaplint_glob_data .

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
        !ii_client TYPE REF TO if_http_client .
    METHODS create_client
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client .
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

    DATA: lt_files TYPE STANDARD TABLE OF string WITH EMPTY KEY.


    DATA(lt_found) = NEW zcl_abaplint_deps( )->find(
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name ).

    LOOP AT lt_found INTO DATA(ls_file).
      DATA(lv_contents) = base64_encode( ls_file-data ).
      APPEND |\{"name": "{ ls_file-filename }", "contents": "{ lv_contents }"\}| TO lt_files.
    ENDLOOP.

    CONCATENATE LINES OF lt_files INTO rv_files SEPARATED BY ','.
    rv_files = |[{ rv_files }]|.

  ENDMETHOD.


  METHOD build_files.

* todo, this should be done relative to what is the root abapGit folder and
* also take settings into account, but not super important?

    DATA: lt_files TYPE STANDARD TABLE OF string WITH EMPTY KEY.


    DATA(ls_files_item) = VALUE zcl_abapgit_objects=>ty_serialization(
      item-obj_type = iv_object_type
      item-obj_name = iv_object_name ).

    TRY.
        ls_files_item = zcl_abapgit_objects=>serialize(
          is_item     = ls_files_item-item
          iv_language = sy-langu ).
      CATCH zcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

    LOOP AT ls_files_item-files INTO DATA(ls_file).
      DATA(lv_contents) = base64_encode( ls_file-data ).
      APPEND |\{"name": "{ ls_file-filename }", "contents": "{ lv_contents }"\}| TO lt_files.
    ENDLOOP.

    CONCATENATE LINES OF lt_files INTO rv_files SEPARATED BY ','.
    rv_files = |[{ rv_files }]|.

  ENDMETHOD.


  METHOD check_object.

    DATA(li_client) = create_client( ).

    cl_http_utility=>set_request_uri(
      request = li_client->request
      uri     = |/api/v1/check_file| ).

    DATA(lv_files) = build_files(
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name ).

    DATA(lv_deps) = build_deps(
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name ).

    DATA(lv_config) = base64_encode( zcl_abapgit_convert=>string_to_xstring_utf8( iv_configuration ) ).

    DATA(lv_cdata) = |\{\n| &&
      |  "configuration": "{ lv_config }",\n| &&
      |  "object": \{\n| &&
      |    "objectName": "{ iv_object_name }",\n| &&
      |    "objectType": "{ iv_object_type }"\n| &&
      |  \},\n| &&
      |  "deps": { lv_deps },\n| &&
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


  METHOD constructor.

    IF is_config IS SUPPLIED.
      ms_config = is_config.
    ELSE.
      ms_config = NEW zcl_abaplint_configuration( )->get_global( ).
    ENDIF.

  ENDMETHOD.


  METHOD create_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = CONV #( ms_config-url )
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = ri_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD ping.

    DATA(li_client) = create_client( ).

    cl_http_utility=>set_request_uri(
      request = li_client->request
      uri     = |/api/v1/ping| ).

    send( li_client ).

    DATA(lv_response) = li_client->response->get_cdata( ).
    MESSAGE lv_response TYPE 'S'.

    li_client->close( ).

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
