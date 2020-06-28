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


    IF cs_files-item-obj_type <> 'CLAS'.
      RETURN.
    ENDIF.

* todo, this is everything?
    DELETE cs_files-files WHERE filename CP '*.clas.locals_def.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.locals_imp.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.macros.abap'.
    DELETE cs_files-files WHERE filename CP '*.clas.xml'. " todo?
    DELETE cs_files-files WHERE filename CP '*.clas.testclasses.abap'.


    DATA(lt_text) = build_code( cs_files-item-obj_name ).

    CONCATENATE LINES OF lt_text INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.

    LOOP AT cs_files-files ASSIGNING FIELD-SYMBOL(<ls_file>) WHERE filename CP '*.clas.abap'.
      <ls_file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).
    ENDLOOP.

  ENDMETHOD.


  METHOD build_code.

    DATA lt_text TYPE abaptxt255_tab.

    TRY.
        DATA(lo_class) = CAST cl_oo_class( cl_oo_class=>get_instance( CONV #( iv_class ) ) ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    DATA(lt_includes) = cl_oo_classname_service=>get_all_method_includes( CONV #( iv_class ) ).
    DATA(lt_methods) = lo_class->get_methods( ).
    DATA(lv_final) = lo_class->is_final( ).

    DATA(lv_include) = cl_oo_classname_service=>get_pubsec_name( CONV #( iv_class ) ).
    READ REPORT lv_include INTO lt_text.
    APPEND LINES OF lt_text TO rt_code.

    IF lv_final = abap_false.
      lv_include = cl_oo_classname_service=>get_prosec_name( CONV #( iv_class ) ).
      READ REPORT lv_include INTO lt_text.
      APPEND LINES OF lt_text TO rt_code.
    ENDIF.

    APPEND |ENDCLASS.| TO rt_code.
    APPEND |CLASS { to_lower( iv_class ) } IMPLEMENTATION.| TO rt_code.

    LOOP AT lt_includes INTO DATA(ls_include).
      IF line_exists( lt_methods[ cmpname = ls_include-cpdkey-cpdname exposure = 0 ] ).
        CONTINUE. " private method
      ELSEIF lv_final = abap_true AND line_exists( lt_methods[ cmpname = ls_include-cpdkey-cpdname exposure = 1 ] ).
        CONTINUE. " protected method
      ELSEIF NOT line_exists( lt_methods[ cmpname = ls_include-cpdkey-cpdname ] ).
        CONTINUE.
      ENDIF.
      READ REPORT ls_include-incname INTO lt_text.
      IF lines( lt_text ) = 1 AND lt_text[ 1 ] = '*** inactive new ***'.
        CONTINUE.
      ENDIF.
      APPEND |  METHOD { to_lower( ls_include-cpdkey-cpdname ) }.| TO rt_code.
      APPEND |  ENDMETHOD.| TO rt_code.
    ENDLOOP.

    APPEND |ENDCLASS.| TO rt_code.

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo = abap_true
      TABLES
        ntext  = rt_code
        otext  = rt_code.

  ENDMETHOD.


  METHOD serialize.

    LOOP AT it_tadir INTO DATA(ls_tadir).
      cl_progress_indicator=>progress_indicate(
        i_text               = |Serializing, { ls_tadir-object } { ls_tadir-obj_name }|
        i_processed          = sy-tabix
        i_total              = lines( it_tadir )
        i_output_immediately = abap_true ).

      DATA(ls_files_item) = zcl_abapgit_objects=>serialize(
        is_item     = VALUE #( obj_type = ls_tadir-object obj_name = ls_tadir-obj_name )
        iv_language = sy-langu ).

      build_clas( CHANGING cs_files = ls_files_item ).

      APPEND LINES OF ls_files_item-files TO rt_files.
    ENDLOOP.

    LOOP AT rt_files ASSIGNING FIELD-SYMBOL(<ls_file>).
      <ls_file>-path = '/src/'.
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
