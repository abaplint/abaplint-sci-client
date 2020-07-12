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

    METHODS build_clas
      CHANGING
        !cs_files TYPE zcl_abapgit_objects=>ty_serialization .
    METHODS build_code
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

    IF cs_files-item-obj_type <> 'CLAS'.
      RETURN.
    ENDIF.

    DELETE cs_files-files WHERE filename CP '*.clas.locals_def.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.locals_imp.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.macros.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.xml'. " todo?
    DELETE cs_files-files WHERE filename CP '*.clas.testclasses.abap'.

    DATA lt_text TYPE abaptxt255_tab.
    lt_text = build_code( cs_files-item-obj_name ).

    CONCATENATE LINES OF lt_text INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.

    LOOP AT cs_files-files ASSIGNING <ls_file> WHERE filename CP '*.clas.abap'.
      <ls_file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).
    ENDLOOP.

  ENDMETHOD.


  METHOD build_code.

    DATA lt_text TYPE abaptxt255_tab.
    DATA lv_tmp LIKE LINE OF rt_code.
    DATA lo_class TYPE REF TO cl_oo_class.
    DATA lv_final TYPE abap_bool.
    DATA lt_includes TYPE seop_methods_w_include.
    DATA lt_methods TYPE seo_methods.
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
    APPEND LINES OF lt_text TO rt_code.

    IF lv_final = abap_false.
      lv_include = cl_oo_classname_service=>get_prosec_name( |{ iv_class }| ).
      READ REPORT lv_include INTO lt_text.
      APPEND LINES OF lt_text TO rt_code.
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


  METHOD serialize.

    DATA ls_tadir LIKE LINE OF it_tadir.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_longtexts TYPE REF TO lcl_longtexts.
    DATA ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF rt_files.


    CREATE OBJECT lo_longtexts.
    zcl_abapgit_injector=>set_longtexts( lo_longtexts ).

    LOOP AT it_tadir INTO ls_tadir.
      IF ls_tadir-object = 'DOMA' AND ls_tadir-obj_name = 'DATA'.
* special case, does not exist, built-in?
        CONTINUE.
      ENDIF.
      ASSERT NOT ls_tadir-devclass IS INITIAL.

      IF sy-tabix MOD 10 = 0.
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

      build_clas( CHANGING cs_files = ls_files_item ).

      LOOP AT ls_files_item-files ASSIGNING <ls_file>.
        <ls_file>-path = |/src/{ to_lower( ls_tadir-devclass ) }/|.
        APPEND <ls_file> TO rt_files.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_item.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_tadir LIKE LINE OF lt_tadir.

    ls_tadir-object = is_item-obj_type.
    ls_tadir-obj_name = is_item-obj_name.
    APPEND ls_tadir TO lt_tadir.

    rt_files = serialize( lt_tadir ).

  ENDMETHOD.
ENDCLASS.
