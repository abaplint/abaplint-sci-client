CLASS zcl_abaplint_deps_serializer DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS serialize_item
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    METHODS serialize
      IMPORTING
        !it_tadir       TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    METHODS strip_form_body
      IMPORTING
        !iv_string       TYPE string
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS strip_xml
      IMPORTING
        !iv_string       TYPE string
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS build_clas
      CHANGING
        !cs_files TYPE zcl_abapgit_objects=>ty_serialization .
    METHODS build_prog
      CHANGING
        !cs_files TYPE zcl_abapgit_objects=>ty_serialization .
    METHODS build_fugr
      CHANGING
        !cs_files TYPE zcl_abapgit_objects=>ty_serialization .
    METHODS build_clas_code
      IMPORTING
        !iv_class      TYPE clike
      RETURNING
        VALUE(rt_code) TYPE abaptxt255_tab .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS_SERIALIZER IMPLEMENTATION.


  METHOD build_clas.

    DATA: lv_string TYPE string.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF cs_files-files.


    DELETE cs_files-files WHERE filename CP '*.clas.locals_def.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.locals_imp.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.macros.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.xml'. " todo?
    DELETE cs_files-files WHERE filename CP '*.clas.testclasses.abap'.

    DATA lt_text TYPE abaptxt255_tab.
    lt_text = build_clas_code( cs_files-item-obj_name ).
    IF lines( lt_text ) > 0.
      CONCATENATE LINES OF lt_text INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.

      LOOP AT cs_files-files ASSIGNING <ls_file> WHERE filename CP '*.clas.abap'.
        <ls_file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD build_clas_code.

    DATA lt_text TYPE abaptxt255_tab.
    DATA lv_tmp LIKE LINE OF rt_code.
    DATA lo_class TYPE REF TO cl_oo_class.
    DATA lv_final TYPE abap_bool.
    DATA lt_includes TYPE seop_methods_w_include.
    DATA lt_methods TYPE seo_methods.
    DATA lv_text LIKE LINE OF lt_text.
    DATA lv_include TYPE program.

    TRY.
        lo_class ?= cl_oo_class=>get_instance( |{ iv_class }| ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    lt_includes = cl_oo_classname_service=>get_all_method_includes( |{ iv_class }| ).
    lt_methods = lo_class->get_methods( ).
    lv_final = lo_class->is_final( ).

    lv_include = cl_oo_classname_service=>get_pubsec_name( |{ iv_class }| ).
    READ REPORT lv_include INTO lt_text.
    LOOP AT lt_text INTO lv_text.
      IF lv_text(1) = '*'.
        CONTINUE.
      ENDIF.
      APPEND lv_text TO rt_code.
    ENDLOOP.

    IF lv_final = abap_false.
      lv_include = cl_oo_classname_service=>get_prosec_name( |{ iv_class }| ).
      READ REPORT lv_include INTO lt_text.
      LOOP AT lt_text INTO lv_text.
        IF lv_text(1) = '*'.
          CONTINUE.
        ENDIF.
        APPEND lv_text TO rt_code.
      ENDLOOP.
    ENDIF.

    APPEND 'ENDCLASS.' TO rt_code.
    lv_tmp-line = |CLASS { to_lower( iv_class ) } IMPLEMENTATION.|.
    APPEND lv_tmp TO rt_code.

    DATA ls_include LIKE LINE OF lt_includes.

    LOOP AT lt_includes INTO ls_include.
      READ TABLE lt_methods TRANSPORTING NO FIELDS
        WITH KEY cmpname = ls_include-cpdkey-cpdname exposure = 0.
      IF sy-subrc = 0.
        CONTINUE. " private method
      ENDIF.

      READ TABLE lt_methods TRANSPORTING NO FIELDS
        WITH KEY cmpname = ls_include-cpdkey-cpdname exposure = 1.
      IF lv_final = abap_true AND sy-subrc = 0.
        CONTINUE. " protected method
      ENDIF.

      READ TABLE lt_methods TRANSPORTING NO FIELDS
        WITH KEY cmpname = ls_include-cpdkey-cpdname.
      IF NOT sy-subrc = 0.
        CONTINUE.
      ENDIF.

      READ REPORT ls_include-incname INTO lt_text.
      IF lines( lt_text ) = 1.
        READ TABLE lt_text INTO lv_tmp INDEX 1.
        ASSERT sy-subrc = 0.
        IF lv_tmp-line = '*** inactive new ***'.
          CONTINUE.
        ENDIF.
      ENDIF.
      lv_tmp-line = |  METHOD { to_lower( ls_include-cpdkey-cpdname ) }.|.
      APPEND lv_tmp TO rt_code.
      APPEND '  ENDMETHOD.' TO rt_code.
    ENDLOOP.

    APPEND 'ENDCLASS.' TO rt_code.

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo = abap_true
      TABLES
        ntext  = rt_code
        otext  = rt_code.

  ENDMETHOD.


  METHOD build_fugr.
* In function groups, the actual function module code is not required
* so it is replaced with the empty FUNCTION.

    DATA lv_string TYPE string.
    DATA lt_functab TYPE STANDARD TABLE OF rs38l_incl.
    DATA lv_area TYPE rs38l-area.
    DATA lt_files LIKE cs_files-files.
    DATA lv_filename TYPE string.
    DATA ls_functab LIKE LINE OF lt_functab.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF cs_files-files.

    lt_files = cs_files-files.
    CLEAR cs_files-files.

    lv_area = cs_files-item-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_area
      TABLES
        functab                 = lt_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_functab INTO ls_functab.
      lv_filename = |{ to_lower( cs_files-item-obj_name ) }.fugr.{ to_lower( ls_functab-funcname ) }.abap|.
      READ TABLE lt_files ASSIGNING <ls_file> WITH KEY filename = lv_filename.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lv_string = |FUNCTION { to_lower( ls_functab-funcname ) }.\n  RETURN.\nENDFUNCTION.|.
      <ls_file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

      APPEND <ls_file> TO cs_files-files.
    ENDLOOP.

    lv_filename = |{ to_lower( cs_files-item-obj_name ) }.fugr.xml|.
    READ TABLE lt_files ASSIGNING <ls_file> WITH KEY filename = lv_filename.
    IF sy-subrc = 0.
      lv_string = strip_xml( zcl_abapgit_convert=>xstring_to_string_utf8( <ls_file>-data ) ).
      <ls_file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

      APPEND <ls_file> TO cs_files-files.
    ENDIF.

  ENDMETHOD.


  METHOD build_prog.

    DATA lv_filename TYPE string.
    DATA lv_string TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF cs_files-files.


    lv_filename = |{ to_lower( translate( val = cs_files-item-obj_name from = '/' to = '#' ) ) }.prog.xml|.
    READ TABLE cs_files-files ASSIGNING <ls_file> WITH KEY filename = lv_filename.
    IF sy-subrc = 0.
      lv_string = strip_xml( zcl_abapgit_convert=>xstring_to_string_utf8( <ls_file>-data ) ).
      <ls_file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).
    ENDIF.

    lv_filename = |{ to_lower( translate( val = cs_files-item-obj_name from = '/' to = '#' ) ) }.prog.abap|.
    READ TABLE cs_files-files ASSIGNING <ls_file> WITH KEY filename = lv_filename.
    IF sy-subrc = 0.
      lv_string = strip_form_body( zcl_abapgit_convert=>xstring_to_string_utf8( <ls_file>-data ) ).
      <ls_file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize.

    DATA ls_tadir LIKE LINE OF it_tadir.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_longtexts TYPE REF TO lcl_longtexts.
    DATA ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.
    DATA ls_path TYPE string.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF rt_files.


    CREATE OBJECT lo_longtexts.
    zcl_abapgit_injector=>set_longtexts( lo_longtexts ).

    LOOP AT it_tadir INTO ls_tadir.
      IF ls_tadir-object = 'DOMA' AND ls_tadir-obj_name = 'DATA'.
* special case, does not exist, built-in?
        CONTINUE.
      ELSEIF ls_tadir-devclass IS INITIAL.
* assumption: does not exist in TADIR
        CONTINUE.
      ENDIF.

      IF sy-tabix MOD 20 = 0.
        cl_progress_indicator=>progress_indicate(
          i_text               = |Serializing, { ls_tadir-object } { ls_tadir-obj_name }|
          i_processed          = sy-tabix
          i_total              = lines( it_tadir )
          i_output_immediately = abap_true ).
      ENDIF.

      ls_item-obj_type = ls_tadir-object.
      ls_item-obj_name = ls_tadir-obj_name.

      ls_files_item = zcl_abapgit_objects=>serialize(
        is_item                       = ls_item
        iv_serialize_master_lang_only = abap_true
        iv_language                   = sy-langu ).

      CASE ls_tadir-object.
        WHEN 'CLAS'.
          build_clas( CHANGING cs_files = ls_files_item ).
        WHEN 'FUGR'.
          build_fugr( CHANGING cs_files = ls_files_item ).
        WHEN 'PROG'.
          "Exit call
          zcl_abaplint_exit=>get_instance( )->handle_special_abaps(
                  EXPORTING iv_program_name = ls_tadir-obj_name
                  CHANGING cs_files_item = ls_files_item ).

          build_prog( CHANGING cs_files = ls_files_item ).
        WHEN OTHERS.
      ENDCASE.

      LOOP AT ls_files_item-files ASSIGNING <ls_file>.
        ls_path = ls_tadir-devclass.
        REPLACE ALL OCCURRENCES OF '/' IN ls_path WITH '#' IN CHARACTER MODE.
        <ls_file>-path = |/src/{ to_lower( ls_path ) }/|.
        APPEND <ls_file> TO rt_files.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_item.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_tadir LIKE LINE OF lt_tadir.

    ls_tadir-object = is_item-obj_type.
    ls_tadir-obj_name = is_item-obj_name.
    SELECT SINGLE devclass FROM tadir INTO ls_tadir-devclass
      WHERE object = ls_tadir-object AND obj_name = ls_tadir-obj_name.
    APPEND ls_tadir TO lt_tadir.

    rt_files = serialize( lt_tadir ).

  ENDMETHOD.


  METHOD strip_form_body.

    CONSTANTS lc_form TYPE c LENGTH 1 VALUE 'O'.

    DATA lt_structures TYPE STANDARD TABLE OF sstruc WITH DEFAULT KEY.
    DATA lt_statements TYPE sstmnt_tab.
    DATA lt_tokens     TYPE stokesx_tab.
    DATA ls_last       LIKE LINE OF lt_structures.
    DATA ls_from       LIKE LINE OF lt_statements.
    DATA ls_to         LIKE LINE OF lt_statements.
    DATA lv_modified   TYPE abap_bool VALUE abap_false.
    DATA lt_code       TYPE STANDARD TABLE OF string WITH DEFAULT KEY.


    SPLIT iv_string AT |\n| INTO TABLE lt_code.

    SCAN ABAP-SOURCE lt_code
      TOKENS INTO lt_tokens
      STRUCTURES INTO lt_structures
      STATEMENTS INTO lt_statements
      WITH ANALYSIS.

    DELETE lt_structures WHERE stmnt_type <> lc_form.
    WHILE lines( lt_structures ) > 0.
      READ TABLE lt_structures INDEX lines( lt_structures ) INTO ls_last.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      READ TABLE lt_statements INTO ls_from INDEX ls_last-stmnt_from.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      READ TABLE lt_statements INTO ls_to INDEX ls_last-stmnt_to.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      DELETE lt_code FROM ls_from-trow + 1 TO ls_to-trow - 1.
      IF sy-subrc = 0.
        lv_modified = abap_true.
      ENDIF.

      DELETE lt_structures INDEX lines( lt_structures ).
    ENDWHILE.

    IF lv_modified = abap_true.
      rv_string = concat_lines_of( table = lt_code sep = |\n| ).
    ELSE.
      rv_string = iv_string.
    ENDIF.

  ENDMETHOD.


  METHOD strip_xml.

    DATA lt_strings TYPE STANDARD TABLE OF string.
    DATA lt_result TYPE STANDARD TABLE OF string.
    DATA lv_skip TYPE abap_bool.
    DATA lv_string TYPE string.


    SPLIT iv_string AT |\n| INTO TABLE lt_strings.

    LOOP AT lt_strings INTO lv_string.
      IF lv_string = |   <DYNPROS>|
          OR lv_string = |   <CUA>|
          OR lv_string = |   <INCLUDES>|.
        lv_skip = abap_true.
      ENDIF.

      IF lv_skip = abap_false.
        APPEND lv_string TO lt_result.
      ENDIF.

      IF lv_string = |   </DYNPROS>|
          OR lv_string = |   </CUA>|
          OR lv_string = |   </INCLUDES>|.
        lv_skip = abap_false.
      ENDIF.
    ENDLOOP.

    rv_string = concat_lines_of( table = lt_result sep = |\n| ).

  ENDMETHOD.
ENDCLASS.
