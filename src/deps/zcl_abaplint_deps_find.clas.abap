CLASS zcl_abaplint_deps_find DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_options,
        max_level         TYPE i,
        continue_into_sap TYPE abap_bool,
        cache_memory      TYPE abap_bool,
        cache_cluster     TYPE abap_bool,
      END OF ty_options .
    TYPES:
      BEGIN OF ty_tadir,
        ref_obj_type TYPE trobjtype,
        ref_obj_name TYPE sobj_name,
        devclass     TYPE devclass,
      END OF ty_tadir .
    TYPES:
      ty_tadir_tt TYPE SORTED TABLE OF ty_tadir WITH UNIQUE KEY ref_obj_type ref_obj_name .

    METHODS constructor
      IMPORTING
        !is_options TYPE ty_options OPTIONAL .
    METHODS get_log
      RETURNING
        VALUE(ri_log) TYPE REF TO zif_abapgit_log .
    METHODS find_by_item
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception
        zcx_abaplint_error .
    METHODS find_by_packages
      IMPORTING
        !it_packages    TYPE tr_devclasses
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception
        zcx_abaplint_error .
    CLASS-METHODS determine_package
      IMPORTING
        !iv_object_type   TYPE trobjtype
        !iv_object_name   TYPE sobj_name
      RETURNING
        VALUE(rv_package) TYPE devclass .
  PROTECTED SECTION.

    DATA ms_options TYPE ty_options .

    METHODS convert_senvi_to_tadir
      IMPORTING
        !it_senvi       TYPE senvi_tab
      RETURNING
        VALUE(rt_tadir) TYPE ty_tadir_tt .
    METHODS find_clas_dependencies
      IMPORTING
        !iv_name        TYPE tadir-obj_name
        !iv_level       TYPE i
      RETURNING
        VALUE(rt_tadir) TYPE ty_tadir_tt
      RAISING
        zcx_abaplint_error .
    METHODS find_extra_fugr_dependencies
      IMPORTING
        !iv_name        TYPE tadir-obj_name
      CHANGING
        VALUE(ct_tadir) TYPE ty_tadir_tt
      RAISING
        zcx_abaplint_error .
    METHODS find_extra_prog_dependencies
      IMPORTING
        !iv_name        TYPE tadir-obj_name
      CHANGING
        VALUE(ct_tadir) TYPE ty_tadir_tt
      RAISING
        zcx_abaplint_error .
    METHODS find_tabl_dependencies
      IMPORTING
        !iv_name        TYPE tadir-obj_name
      RETURNING
        VALUE(rt_tadir) TYPE ty_tadir_tt
      RAISING
        zcx_abaplint_error .
    METHODS find_dtel_dependencies
      IMPORTING
        !iv_name        TYPE tadir-obj_name
      RETURNING
        VALUE(rt_tadir) TYPE ty_tadir_tt .
    METHODS get_dependencies
      IMPORTING
        !is_object  TYPE zif_abapgit_definitions=>ty_tadir
        !iv_minimal TYPE abap_bool DEFAULT abap_true
        !iv_level   TYPE i
      RAISING
        zcx_abaplint_error .
    METHODS resolve
      IMPORTING
        !it_wbcrossgt   TYPE wbcrossgtt
      RETURNING
        VALUE(rt_tadir) TYPE ty_tadir_tt .
    METHODS update_index
      IMPORTING
        !iv_name TYPE seoclsname .
  PRIVATE SECTION.

    DATA mt_packages TYPE tr_devclasses .
    DATA mt_results TYPE ty_tadir_tt .
    DATA ms_types TYPE envi_types .
    DATA mi_log TYPE REF TO zif_abapgit_log .
    DATA ms_item TYPE zif_abapgit_definitions=>ty_item .

    METHODS get_dependencies_deep
      IMPORTING
        !it_tadir TYPE ty_tadir_tt
        !iv_level TYPE i
      RAISING
        zcx_abaplint_error .
    METHODS get_environment
      IMPORTING
        !is_object      TYPE zif_abapgit_definitions=>ty_tadir
        !iv_minimal     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_tadir) TYPE ty_tadir_tt
      RAISING
        zcx_abaplint_error .
    METHODS remove_own_package
      CHANGING
        !ct_tadir TYPE ty_tadir_tt .
    METHODS remove_duplicates
      CHANGING
        !ct_tadir TYPE ty_tadir_tt .
    METHODS remove_deleted_objects
      CHANGING
        !ct_tadir TYPE ty_tadir_tt .
    METHODS add_subpackages
      IMPORTING
        !iv_package TYPE devclass .
    METHODS clean_own_packages .
    METHODS clear_results .
    METHODS prepare_supported_types
      RETURNING
        VALUE(rs_types) TYPE envi_types .
    METHODS set_package_tree
      IMPORTING
        !it_packages TYPE tr_devclasses .
    METHODS convert_type_to_r3tr
      IMPORTING
        !iv_object_type  TYPE trobjtype
        !iv_object_name  TYPE sobj_name
        !iv_encl_object  TYPE sobj_name
      RETURNING
        VALUE(rs_object) TYPE zif_abapgit_definitions=>ty_tadir .
    METHODS convert_wbtype_to_r3tr
      IMPORTING
        !iv_object_type  TYPE trobjtype
        !iv_object_name  TYPE sobj_name
        !iv_encl_object  TYPE sobj_name
      RETURNING
        VALUE(rs_object) TYPE zif_abapgit_definitions=>ty_tadir .
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS_FIND IMPLEMENTATION.


  METHOD add_subpackages.

    DATA: lt_packages LIKE mt_packages.
    DATA: lv_package LIKE LINE OF mt_packages.

    APPEND iv_package TO mt_packages.
    SELECT devclass FROM tdevc INTO TABLE lt_packages WHERE parentcl = iv_package.
    LOOP AT lt_packages INTO lv_package.
      add_subpackages( lv_package ).
    ENDLOOP.

  ENDMETHOD.


  METHOD clean_own_packages.

    DATA: lv_package LIKE LINE OF mt_packages.

    LOOP AT mt_packages INTO lv_package.
      READ TABLE mt_results
        WITH KEY ref_obj_type = 'DEVC' ref_obj_name = lv_package
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE mt_results INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD clear_results.
    CLEAR mt_results[].
    mi_log->clear( ).
  ENDMETHOD.


  METHOD constructor.

    ms_options = is_options.
    IF ms_options-max_level IS INITIAL.
      ms_options-max_level = 20.
    ENDIF.

    ms_types = prepare_supported_types( ).

    CREATE OBJECT mi_log TYPE zcl_abapgit_log.

  ENDMETHOD.


  METHOD convert_senvi_to_tadir.

* do not use CL_WB_RIS_ENVIRONMENT, it does not exist in 740sp08

    DATA: ls_senvi       LIKE LINE OF it_senvi,
          ls_tadir       LIKE LINE OF rt_tadir,
          lv_object      TYPE trobjtype,
          lv_object_name TYPE sobj_name,
          ls_object      TYPE zif_abapgit_definitions=>ty_tadir,
          lv_obj         TYPE zif_abapgit_definitions=>ty_item.

    LOOP AT it_senvi INTO ls_senvi.
      lv_object = ls_senvi-type.
      lv_object_name = ls_senvi-object.

      ls_object = convert_type_to_r3tr(
        iv_object_type = lv_object
        iv_object_name = lv_object_name
        iv_encl_object = ls_senvi-encl_obj ).

      "successfull translation and suppoeted type
      IF ls_object-object IS NOT INITIAL AND ls_object-obj_name IS NOT INITIAL.
        lv_obj = ls_object-object.
        IF zcl_abapgit_objects=>is_supported( lv_obj ) = abap_true.
          CLEAR ls_tadir.
          ls_tadir-ref_obj_type = ls_object-object.
          ls_tadir-ref_obj_name = ls_object-obj_name.
          INSERT ls_tadir INTO TABLE rt_tadir.
        ELSE.
          MESSAGE s004(zabaplint) WITH ls_object-object ls_object-obj_name.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_type_to_r3tr.

    DATA: lv_program TYPE progname,
          lv_area    TYPE rs38l_area.

    CLEAR rs_object.

    CASE iv_object_type.
      WHEN 'INCL' OR 'PROG'.
        rs_object-object = 'PROG'.
        rs_object-obj_name = iv_object_name.
        " Check if it's a function group
        lv_program = iv_object_name.
        CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
          CHANGING
            program                  = lv_program
            complete_area            = lv_area
          EXCEPTIONS
            not_enough_input         = 1
            no_function_pool         = 2
            delimiter_wrong_position = 3
            OTHERS                   = 4.
        IF sy-subrc = 0.
          rs_object-object = 'FUGR'.
          rs_object-obj_name = lv_area.
        ENDIF.
      WHEN 'PARA' OR 'SUSO' OR 'TABL' OR 'CLAS' OR 'INTF' OR 'ENHS' OR 'ENHO' OR 'DTEL'.
        rs_object-object = iv_object_type.
        rs_object-obj_name = iv_object_name.
      WHEN 'TTYP' OR 'XSLT' OR 'VIEW' OR 'TRAN' OR 'MSAG'.
        rs_object-object = iv_object_type.
        rs_object-obj_name = iv_object_name.
      WHEN 'STRU'.
        rs_object-object = 'TABL'.
        rs_object-obj_name = iv_object_name.
      WHEN 'SQLT'.
        "Not Supported
      WHEN 'MESS'.
        rs_object-object = 'MSAG'.
        rs_object-obj_name = iv_encl_object.
      WHEN 'FUNC'.
        rs_object-object = 'FUGR'.
        rs_object-obj_name = iv_encl_object.
      WHEN 'CUS0' OR 'CUS1' OR 'CUS2'.
        rs_object-object = iv_object_type.
        rs_object-obj_name = iv_encl_object.
      WHEN OTHERS.
        rs_object = convert_wbtype_to_r3tr( iv_object_type = iv_object_type
                                            iv_object_name = iv_object_name
                                            iv_encl_object = iv_encl_object ).
    ENDCASE.

  ENDMETHOD.


  METHOD convert_wbtype_to_r3tr.

    DATA:
      lv_object      TYPE trobjtype,
      lv_object_name TYPE sobj_name,
      ls_e071        TYPE e071,
      ls_tadir       TYPE tadir,
      lv_clstype     TYPE seoclstype.

    CLEAR rs_object.

    " Map WB type to TADIR
    CALL FUNCTION 'GET_TADIR_TYPE_FROM_WB_TYPE'
      EXPORTING
        wb_objtype        = iv_object_type(3)
      IMPORTING
        transport_objtype = lv_object
      EXCEPTIONS
        no_mapping_found  = 1
        no_unique_mapping = 2
        OTHERS            = 3.
    " Unknown type, try finding it via enclosing object in tadir
    IF iv_encl_object IS NOT INITIAL AND sy-subrc = 1.
      SELECT SINGLE object FROM tadir INTO lv_object
        WHERE pgmid = 'R3TR'
        AND object IN ('CLAS','ENHS','CUS0','CUS1','CUS2')
        AND obj_name = iv_encl_object.
    ENDIF.
    IF sy-subrc <> 0.
      mi_log->add_error( iv_msg  = |No mapping found for { iv_object_type } { iv_object_name }|
                         is_item = ms_item ).
      RETURN.
    ENDIF.
    IF lv_object <> 'CLAS' AND lv_object <> 'ENHS'
        AND lv_object <> 'CUS0' AND lv_object <> 'CUS1' AND lv_object <> 'CUS2'.
      mi_log->add_error( iv_msg  = |Unexpected mapping to { lv_object } for { iv_object_type } { iv_object_name }|
                         is_item = ms_item ).
      RETURN.
    ENDIF.

    lv_object_name = iv_encl_object.
    " Class vs. interface is not differenciated
    IF lv_object = 'CLAS'.
      " Decide if interface / class
      SELECT SINGLE clstype FROM seoclass INTO lv_clstype WHERE clsname = lv_object_name.
      IF sy-subrc = 0.
        IF lv_clstype = seoc_clstype_interface.
          lv_object = 'INTF'.
        ENDIF.
      ELSE.
        " Decide if Enhancement Spot
        SELECT SINGLE badi_name FROM badi_spot INTO lv_object_name WHERE badi_name = lv_object_name.
        IF sy-subrc = 0.
          lv_object = 'ENHS'.
        ENDIF.
      ENDIF.
    ENDIF.

    " Translate TADIR entry
    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = lv_object.
    ls_e071-obj_name = lv_object_name.

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071              = ls_e071
        iv_translate_objname = 'X'
      IMPORTING
        we_tadir             = ls_tadir.

    " Object name empty -> does not exist
    IF ls_tadir-obj_name IS NOT INITIAL.
      rs_object-object = ls_tadir-object.
      rs_object-obj_name = ls_tadir-obj_name.
    ENDIF.

  ENDMETHOD.


  METHOD determine_package.
*
* Translate certain Object Types
*
    DATA: lv_type      TYPE trobjtype,
          lv_name      TYPE sobj_name,
          lv_is_fugr   TYPE flag,
          lv_namespace TYPE namespace,
          lv_fugr      TYPE rs38l_area.

    CLEAR rv_package.
    lv_type = iv_object_type.
    lv_name = iv_object_name.

    "Translation of Types / Names
    CASE iv_object_type.
      WHEN 'FUNC'.
        SELECT SINGLE pname FROM tfdir INTO lv_name.
        lv_type = 'FUGR'.
      WHEN 'PROG'.
        CALL FUNCTION 'RS_PROGNAME_SPLIT'
          EXPORTING
            progname_with_namespace = lv_name
          IMPORTING
            namespace               = lv_namespace
            fugr_is_name            = lv_is_fugr
            fugr_group              = lv_fugr
          EXCEPTIONS
            delimiter_error         = 1
            OTHERS                  = 2.
        IF sy-subrc = 0 AND lv_is_fugr IS NOT INITIAL.
          lv_type = 'FUGR'.
          IF lv_namespace IS NOT INITIAL.
            CONCATENATE lv_namespace lv_fugr INTO lv_name.
          ELSE.
            lv_name = lv_fugr.
          ENDIF.
        ENDIF.
    ENDCASE.

    SELECT SINGLE devclass FROM tadir INTO rv_package
       WHERE pgmid = 'R3TR'
       AND object = lv_type
       AND obj_name = lv_name.

    IF sy-subrc <> 0 OR rv_package IS INITIAL.
      rv_package = '$NONE'.
    ENDIF.

  ENDMETHOD.


  METHOD find_by_item.

* assumption: the input item is a checked top level item, ie do not find only minimal dependencies
* finds dependencies by item, package tree is ignored

    DATA: ls_object   TYPE zif_abapgit_definitions=>ty_tadir,
          lt_packages TYPE tr_devclasses,
          lv_package  TYPE devclass,
          ls_result   LIKE LINE OF mt_results.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF rt_tadir.

    ls_object-object = iv_object_type.
    ls_object-obj_name = iv_object_name.
    ls_object-devclass = lv_package.

    IF ls_object-object = 'DEVC'.
      APPEND ls_object-object TO lt_packages.
    ENDIF.

    set_package_tree( lt_packages ).
    clear_results( ).

    get_dependencies(
      is_object  = ls_object
      iv_minimal = abap_false
      iv_level   = 1 ).

    LOOP AT mt_results INTO ls_result.
      APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
      <ls_tadir>-object = ls_result-ref_obj_type.
      <ls_tadir>-obj_name = ls_result-ref_obj_name.
      <ls_tadir>-devclass = ls_result-devclass.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_by_packages.

* given a set of packages, all dependencies are found for these packages
* only returns dependencies outside of the input package structure

* the input packages are the objects to be checked

    DATA: lv_package LIKE LINE OF it_packages,
          ls_result  LIKE LINE OF mt_results,
          lt_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_tadir   LIKE LINE OF lt_tadir,
          ls_object  TYPE zif_abapgit_definitions=>ty_tadir.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF rt_tadir.

    "Determine Package Tree
    set_package_tree( it_packages ).
    clear_results( ).

    LOOP AT it_packages INTO lv_package.
      APPEND LINES OF zcl_abapgit_factory=>get_tadir( )->read( lv_package ) TO lt_tadir.
    ENDLOOP.
    SORT lt_tadir BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_tadir COMPARING object obj_name.

    LOOP AT lt_tadir INTO ls_tadir WHERE object <> 'DEVC'.
      ls_object-object   = ls_tadir-object.
      ls_object-obj_name = ls_tadir-obj_name.

      IF sy-tabix MOD 10 = 0.
        cl_progress_indicator=>progress_indicate(
          i_text               = |Processing, { ls_object-object } { ls_object-obj_name }|
          i_processed          = sy-tabix
          i_total              = lines( lt_tadir )
          i_output_immediately = abap_true ).
      ENDIF.

      get_dependencies(
        is_object  = ls_object
        iv_minimal = abap_false
        iv_level   = 1 ).
    ENDLOOP.

    clean_own_packages( ).

    LOOP AT mt_results INTO ls_result.
      APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
      <ls_tadir>-object = ls_result-ref_obj_type.
      <ls_tadir>-obj_name = ls_result-ref_obj_name.
      <ls_tadir>-devclass = ls_result-devclass.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_clas_dependencies.

    DATA lt_includes TYPE STANDARD TABLE OF programm WITH DEFAULT KEY.
    DATA lt_wbcrossgt TYPE wbcrossgtt.
    DATA lv_clsname TYPE seoclsname.
    DATA lv_final TYPE abap_bool.
    DATA lv_msg TYPE string.

    lv_clsname = |{ iv_name }|.

    SELECT SINGLE clsfinal FROM seoclassdf INTO lv_final WHERE clsname = lv_clsname AND version = '1'.
    IF sy-subrc <> 0.
      " class does not exist
      RETURN.
    ENDIF.

    APPEND cl_oo_classname_service=>get_pubsec_name( |{ iv_name }| ) TO lt_includes.
    IF lv_final = abap_false.
      APPEND cl_oo_classname_service=>get_prosec_name( |{ iv_name }| ) TO lt_includes.
    ENDIF.

    SELECT * FROM wbcrossgt INTO CORRESPONDING FIELDS OF TABLE lt_wbcrossgt
      FOR ALL ENTRIES IN lt_includes
      WHERE include = lt_includes-table_line
      AND name <> iv_name.
    IF lines( lt_wbcrossgt ) = 0.
      " update so it is correct in the next run
      update_index( lv_clsname ).

      SELECT * FROM wbcrossgt INTO CORRESPONDING FIELDS OF TABLE lt_wbcrossgt
        FOR ALL ENTRIES IN lt_includes
        WHERE include = lt_includes-table_line
        AND name <> iv_name.
    ENDIF.

    IF iv_level < ms_options-max_level.
      rt_tadir = resolve( lt_wbcrossgt ).
    ELSE.
      lv_msg = |Max depth { ms_options-max_level } reached, class { lv_clsname }, exiting|.
      mi_log->add_error( lv_msg ).
      RAISE EXCEPTION TYPE zcx_abaplint_error
        EXPORTING
          message = lv_msg.
    ENDIF.

  ENDMETHOD.


  METHOD find_dtel_dependencies.

* find just the domain for the data element if exists, ignores value tables and more

    DATA ls_x030l TYPE x030l.
    DATA lv_tabname TYPE dd02l-tabname.
    DATA ls_tadir LIKE LINE OF rt_tadir.
    DATA lv_clstype TYPE seoclass-clstype.

    lv_tabname = iv_name.

* black magic, read the nametab to get the domain for the data element
* this is faster as it runs only on application server
    CALL FUNCTION 'DD_GET_NAMETAB_HEADER'
      EXPORTING
        tabname   = lv_tabname
      IMPORTING
        x030l_wa  = ls_x030l
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      mi_log->add_error( |Error reading nametab for { lv_tabname }| ).
      RETURN.
    ENDIF.

    IF NOT ls_x030l-refname IS INITIAL.
      SELECT SINGLE clstype FROM seoclass INTO lv_clstype
        WHERE clsname = ls_x030l-refname.
      IF sy-subrc = 0.
        IF lv_clstype = '1'.
          ls_tadir-ref_obj_type = 'INTF'.
        ELSE.
          ls_tadir-ref_obj_type = 'CLAS'.
        ENDIF.
      ELSE.
        ls_tadir-ref_obj_type = 'DOMA'.
      ENDIF.
      ls_tadir-ref_obj_name = ls_x030l-refname.
      INSERT ls_tadir INTO TABLE rt_tadir.
    ENDIF.

  ENDMETHOD.


  METHOD find_extra_fugr_dependencies.

    DATA lv_progname LIKE sy-repid.
    DATA lt_includes TYPE STANDARD TABLE OF d010inc WITH DEFAULT KEY.
    DATA lv_devclass TYPE tadir-devclass.
    DATA ls_tadir LIKE LINE OF ct_tadir.
    DATA ls_include LIKE LINE OF lt_includes.
    DATA lv_group TYPE rs38l-area.
    DATA lv_pname TYPE tfdir-pname.

    lv_group = iv_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      IMPORTING
        pname               = lv_pname
      CHANGING
        group               = lv_group
      EXCEPTIONS
        function_not_exists = 1
        include_not_exists  = 2
        group_not_exists    = 3
        no_selections       = 4
        no_function_include = 5
        OTHERS              = 6.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_progname = lv_pname.

    find_extra_prog_dependencies(
      EXPORTING
        iv_name  = lv_progname
      CHANGING
        ct_tadir = ct_tadir ).

  ENDMETHOD.


  METHOD find_extra_prog_dependencies.

    DATA lv_progname LIKE sy-repid.
    DATA lt_includes TYPE STANDARD TABLE OF d010inc WITH DEFAULT KEY.
    DATA lv_devclass TYPE tadir-devclass.
    DATA ls_tadir LIKE LINE OF ct_tadir.
    DATA ls_include LIKE LINE OF lt_includes.

    lv_progname = iv_name.

* search for INCLUDEs in the main program
    SELECT * FROM d010inc
      INTO TABLE lt_includes
      WHERE master = lv_progname.

    LOOP AT lt_includes INTO ls_include.
      IF ls_include-include = '<REPINI>'
          OR ls_include-include = '<SYSINI>'
          OR ls_include-include = '<SYSSEL>'.
        CONTINUE.
      ENDIF.

* make sure its an independent object and lookup the package
      SELECT SINGLE devclass FROM tadir
        INTO lv_devclass
        WHERE pgmid = 'R3TR'
        AND object = 'PROG'
        AND obj_name = ls_include-include.
      IF sy-subrc = 0.
        CLEAR ls_tadir.
        ls_tadir-ref_obj_type = 'PROG'.
        ls_tadir-ref_obj_name = ls_include-include.
        ls_tadir-devclass = lv_devclass.
        INSERT ls_tadir INTO TABLE ct_tadir.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD find_tabl_dependencies.

    DATA lv_tabname TYPE dd02l-tabname.
    DATA ls_tadir LIKE LINE OF rt_tadir.
    DATA lt_x031l TYPE STANDARD TABLE OF x031l.
    DATA lv_clstype TYPE seoclass-clstype.
    DATA lv_typekind TYPE ddtypes-typekind.

    FIELD-SYMBOLS: <ls_x031l> LIKE LINE OF lt_x031l.

    lv_tabname = iv_name.

    CALL FUNCTION 'DD_GET_NAMETAB'
      EXPORTING
        tabname   = lv_tabname
        get_all   = abap_true
      TABLES
        x031l_tab = lt_x031l
      EXCEPTIONS
        not_found = 1
        no_fields = 2
        OTHERS    = 3.
    IF sy-subrc <> 0.
      mi_log->add_error( |Error reading nametab for { lv_tabname }| ).
      RETURN.
    ENDIF.

    LOOP AT lt_x031l ASSIGNING <ls_x031l> WHERE NOT rollname IS INITIAL.
      IF <ls_x031l>-fieldname(8) = '.INCLUDE'.
        ls_tadir-ref_obj_type = 'TABL'.
      ELSE.
        CASE <ls_x031l>-dtyp.
          WHEN 'OREF'. "type ref to
            SELECT SINGLE clstype FROM seoclass INTO lv_clstype
              WHERE clsname = <ls_x031l>-rollname.
            IF sy-subrc = 0 AND lv_clstype = '1'.
              ls_tadir-ref_obj_type = 'INTF'.
            ELSE.
              ls_tadir-ref_obj_type = 'CLAS'.
            ENDIF.
          WHEN 'STR1' OR 'STR2'. "structure
            ls_tadir-ref_obj_type = 'TABL'.
          WHEN 'TTAB'. "table type
            ls_tadir-ref_obj_type = 'TTYP'.
          WHEN 'BREF' OR ''. "boxed or initial
            ls_tadir-ref_obj_type = 'TABL'.
          WHEN OTHERS.
            ls_tadir-ref_obj_type = 'DTEL'.
        ENDCASE.
      ENDIF.

      " Get correct type of include structures
      IF ls_tadir-ref_obj_type = 'TABL'.
        SELECT SINGLE typekind FROM ddtypes INTO lv_typekind WHERE typename = <ls_x031l>-rollname.
        IF sy-subrc = 0.
          ls_tadir-ref_obj_type = lv_typekind.
        ELSE.
          mi_log->add_error( iv_msg  = |Error reading ddtypes for { <ls_x031l>-rollname }|
                             is_item = ms_item ).
          CONTINUE.
        ENDIF.
      ENDIF.

      ls_tadir-ref_obj_name = <ls_x031l>-rollname.
      INSERT ls_tadir INTO TABLE rt_tadir.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_dependencies.

    DATA: lt_tadir TYPE ty_tadir_tt,
          BEGIN OF ls_tadir_obj,
            object    TYPE trobjtype,
            obj_name  TYPE sobj_name,
            srcsystem TYPE srcsystem,
            author    TYPE responsibl,
            devclass  TYPE devclass,
            genflag   TYPE genflag,
          END OF ls_tadir_obj.

    " Remember item for logging
    ms_item-obj_type = is_object-object.
    ms_item-obj_name = is_object-obj_name.

    mi_log->add_info( iv_msg  = |Level { iv_level }: Getting dependencies|
                      is_item = ms_item ).

    IF is_object-object <> 'DEVC' OR is_object-obj_name(1) <> '$'.
      SELECT SINGLE object obj_name srcsystem author devclass genflag
        FROM tadir INTO CORRESPONDING FIELDS OF ls_tadir_obj
        WHERE pgmid = 'R3TR' AND object = is_object-object AND obj_name = is_object-obj_name.
      IF sy-subrc = 0.
        IF ls_tadir_obj-genflag = abap_true.
          mi_log->add_info( iv_msg  = |Object is marked as "generated" in TADIR and will be skipped|
                            is_item = ms_item ).
          RETURN.
        ENDIF.
      ELSE.
        mi_log->add_warning( iv_msg  = |Object not found in TADIR|
                             is_item = ms_item ).
        RETURN.
      ENDIF.
    ENDIF.

    DATA lo_cache TYPE REF TO zcl_abaplint_deps_cache.
    DATA lv_found TYPE abap_bool.

    lo_cache = zcl_abaplint_deps_cache=>get_instance( iv_memory = ms_options-cache_memory
                                                      iv_disk   = ms_options-cache_cluster ).

    lo_cache->read_deps(
      EXPORTING
        is_item    = ms_item
        iv_minimal = iv_minimal
      IMPORTING
        et_tadir   = lt_tadir
        ev_found   = lv_found ).

    IF lv_found = abap_false.
      " Determine direct dependency
      IF is_object-object = 'CLAS' AND iv_minimal = abap_true.
        lt_tadir = find_clas_dependencies( iv_name  = is_object-obj_name
                                           iv_level = iv_level ).
      ELSEIF is_object-object = 'TABL'.
        lt_tadir = find_tabl_dependencies( is_object-obj_name ).
      ELSEIF is_object-object = 'DTEL'.
        lt_tadir = find_dtel_dependencies( is_object-obj_name ).
      ELSE.
        lt_tadir = get_environment( is_object  = is_object
                                    iv_minimal = iv_minimal ).
      ENDIF.

      IF is_object-object = 'PROG'.
        find_extra_prog_dependencies(
          EXPORTING
            iv_name  = is_object-obj_name
          CHANGING
            ct_tadir = lt_tadir ).
      ENDIF.

      " Remove entries from own package (or sub packages)
      remove_own_package( CHANGING ct_tadir = lt_tadir ).

      " Remove entries without TADIR or with delete flag set
      remove_deleted_objects( CHANGING ct_tadir = lt_tadir ).

      lo_cache->write_deps(
        is_item    = ms_item
        iv_minimal = iv_minimal
        it_tadir   = lt_tadir
        iv_package = ls_tadir_obj-devclass ).
    ENDIF.

    " Remove entries already in collection
    remove_duplicates( CHANGING ct_tadir = lt_tadir ).

    IF lines( lt_tadir ) = 0.
      RETURN.
    ENDIF.

    " Add to dependency collection
    INSERT LINES OF lt_tadir INTO TABLE mt_results.

    " If SAP object, do not go deeper
    IF ( ls_tadir_obj-author = 'SAP' OR ls_tadir_obj-author = 'SAP*' ) AND ls_tadir_obj-srcsystem = 'SAP'
        AND ms_options-continue_into_sap = abap_false.
      RETURN.
    ENDIF.

    " Find dependend objects
    get_dependencies_deep(
      it_tadir   = lt_tadir
      iv_level   = iv_level ).

  ENDMETHOD.


  METHOD get_dependencies_deep.

    DATA: ls_object TYPE zif_abapgit_definitions=>ty_tadir,
          lv_level  TYPE i.

    FIELD-SYMBOLS <ls_tadir> TYPE ty_tadir.

    lv_level = iv_level + 1.

    LOOP AT it_tadir ASSIGNING <ls_tadir> WHERE ref_obj_type <> 'MSAG' AND ref_obj_type <> 'DOMA'.
      ls_object-object   = <ls_tadir>-ref_obj_type.
      ls_object-obj_name = <ls_tadir>-ref_obj_name.
      ls_object-devclass = <ls_tadir>-devclass.

      get_dependencies(
        is_object  = ls_object
        iv_level   = lv_level
        iv_minimal = abap_true ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_environment.

    DATA:
      lv_obj_type    TYPE euobj-id,
      lt_environment TYPE senvi_tab.

    lv_obj_type = is_object-object.

    " Never call with package as input, its out of our control
    ASSERT lv_obj_type <> 'DEVC'.

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
      EXPORTING
        obj_type          = lv_obj_type
        object_name       = is_object-obj_name
        environment_types = ms_types
        online_force      = 'X'
      TABLES
        environment       = lt_environment
      EXCEPTIONS
        batch             = 1
        batchjob_error    = 2
        not_executed      = 3
        OTHERS            = 4.
    IF sy-subrc = 3.
      mi_log->add_warning( |Error getting environment for { is_object-obj_name }| ).
      RETURN.
    ENDIF.
    ASSERT sy-subrc = 0.

    rt_tadir = convert_senvi_to_tadir( lt_environment ).

    IF lv_obj_type = 'FUGR' AND iv_minimal = abap_true.
      " Function module parameter types cannot reference types in OO?
      DELETE rt_tadir WHERE ref_obj_type = 'PROG'
        OR ref_obj_type = 'TRAN'
        OR ref_obj_type = 'MSAG'
        OR ref_obj_type = 'FUGR'.
    ENDIF.

  ENDMETHOD.


  METHOD get_log.
    ri_log = mi_log.
  ENDMETHOD.


  METHOD prepare_supported_types.

    DATA ls_type TYPE zif_abapgit_definitions=>ty_item.

    ls_type-obj_type = 'PROG'.
    rs_types-prog = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'FUGR'.
    rs_types-fugr = zcl_abapgit_objects=>is_supported( ls_type ).

* not needed by abaplint yet
*    ls_type-obj_type = 'LDBA'
*    rs_types-ldba = zcl_abapgit_objects=>is_supported( ls_type )

    ls_type-obj_type = 'MSAG'.
    rs_types-msag = zcl_abapgit_objects=>is_supported( ls_type ).

    ls_type-obj_type = 'TRAN'.
    rs_types-tran = zcl_abapgit_objects=>is_supported( ls_type ).

    rs_types-func = rs_types-fugr.

* not needed by abaplint yet
*    ls_type-obj_type = 'DIAL'
*    rs_types-dial = zcl_abapgit_objects=>is_supported( ls_type )

    ls_type-obj_type = 'TABL'.
    rs_types-tabl = zcl_abapgit_objects=>is_supported( ls_type ).

* not needed by abaplint yet
*    ls_type-obj_type = 'SHLP'
*    rs_types-shlp = zcl_abapgit_objects=>is_supported( ls_type )

* handled manually in the code
    rs_types-doma = abap_false.

    ls_type-obj_type = 'DTEL'.
    rs_types-dtel = zcl_abapgit_objects=>is_supported( ls_type ).

    ls_type-obj_type = 'VIEW'.
    rs_types-view = zcl_abapgit_objects=>is_supported( ls_type ).

* not needed by abaplint yet
*    ls_type-obj_type = 'MCOB'
*    rs_types-mcob = zcl_abapgit_objects=>is_supported( ls_type )

* not needed by abaplint yet
*    ls_type-obj_type = 'MCID'
*    rs_types-mcid = zcl_abapgit_objects=>is_supported( ls_type )

    ls_type-obj_type = 'PARA'.
    rs_types-para = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'CONV'.
    rs_types-conv = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'SUSO'.
    rs_types-suso = zcl_abapgit_objects=>is_supported( ls_type ).

* TYPE are not handled properly in abaplint right now, so skip
*    ls_type-obj_type = 'TYPE'
*    rs_types-type = zcl_abapgit_objects=>is_supported( ls_type )

    ls_type-obj_type = 'TTYP'.
    rs_types-ttyp = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'PROG'.
    rs_types-prog = zcl_abapgit_objects=>is_supported( ls_type ).
    rs_types-stru = rs_types-tabl.

* not needed by abaplint yet
*    ls_type-obj_type = 'ENQU'
*    rs_types-enqu = zcl_abapgit_objects=>is_supported( ls_type )

* not needed by abaplint yet
*    ls_type-obj_type = 'SQLT'
*    rs_types-sqlt = zcl_abapgit_objects=>is_supported( ls_type )

    ls_type-obj_type = 'CLAS'.
    rs_types-clas = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'INTF'.
    rs_types-intf = zcl_abapgit_objects=>is_supported( ls_type ).

* not needed by abaplint yet
*    ls_type-obj_type = 'UDMO'
*    rs_types-udmo = zcl_abapgit_objects=>is_supported( ls_type )
* not needed by abaplint yet
*    ls_type-obj_type = 'UENO'
*    rs_types-ueno = zcl_abapgit_objects=>is_supported( ls_type )
* not needed by abaplint yet
*    ls_type-obj_type = 'SHI3'
*    rs_types-shi3 = zcl_abapgit_objects=>is_supported( ls_type )
* not needed by abaplint yet
*    ls_type-obj_type = 'CNTX'
*    rs_types-cntx = zcl_abapgit_objects=>is_supported( ls_type )

    ls_type-obj_type = 'TTAB'.
    rs_types-ttab = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'IASP'.
    rs_types-iasp = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'IATU'.
    rs_types-iatu = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'CLIF'.
    rs_types-clif = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'SOBJ'.
    rs_types-sobj = zcl_abapgit_objects=>is_supported( ls_type ).

* not needed by abaplint yet
*    ls_type-obj_type = 'WDYN'
*    rs_types-wdyn = zcl_abapgit_objects=>is_supported( ls_type )

* not needed by abaplint yet
*    ls_type-obj_type = 'WDYA'
*    rs_types-wdya = zcl_abapgit_objects=>is_supported( ls_type )

    ls_type-obj_type = 'XSLT'.
    rs_types-xslt = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'ENHS'.
    rs_types-enhs = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'ENSC'.
    rs_types-ensc = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'ENHC'.
    rs_types-enhc = zcl_abapgit_objects=>is_supported( ls_type ).
    ls_type-obj_type = 'ENHO'.
    rs_types-enho = zcl_abapgit_objects=>is_supported( ls_type ).

* not needed by abaplint yet
*    ls_type-obj_type = 'SFBF'
*    rs_types-sfbf = zcl_abapgit_objects=>is_supported( ls_type )
* not needed by abaplint yet
*    ls_type-obj_type = 'SFSW'
*    rs_types-sfsw = zcl_abapgit_objects=>is_supported( ls_type )

* no
*    ls_type-obj_type = 'DEVC'
*    rs_types-devc = zcl_abapgit_objects=>is_supported( ls_type )

* not needed by abaplint yet
*    ls_type-obj_type = 'SQSC'
*    rs_types-sqsc = zcl_abapgit_objects=>is_supported( ls_type )
* not needed by abaplint yet
*    ls_type-obj_type = 'STOB'
*    rs_types-stob = zcl_abapgit_objects=>is_supported( ls_type )
* not needed by abaplint yet (attn: not in older releases)
*    ls_type-obj_type = 'DDLS'
*    rs_types-ddls = zcl_abapgit_objects=>is_supported( ls_type )

  ENDMETHOD.


  METHOD remove_deleted_objects.

    DATA: lv_object  TYPE tadir-object,
          lv_delflag TYPE tadir-delflag,
          lv_msg     TYPE string.

    FIELD-SYMBOLS <ls_tadir> TYPE ty_tadir.

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      " Check if this object has been marked as deleted
      SELECT SINGLE delflag FROM tadir INTO lv_delflag
        WHERE pgmid = 'R3TR' AND object = <ls_tadir>-ref_obj_type AND obj_name = <ls_tadir>-ref_obj_name.
      IF sy-subrc = 0.
        IF lv_delflag = abap_true.
          MESSAGE s007(zabaplint) WITH <ls_tadir>-ref_obj_type <ls_tadir>-ref_obj_name INTO lv_msg.
          mi_log->add_warning( iv_msg  = lv_msg
                               is_item = ms_item ).
          DELETE ct_tadir.
        ENDIF.
      ELSE.
        " Check if this object exists with another type
        " This would indicate an error in getting the right dependencies
        SELECT SINGLE object FROM tadir INTO lv_object
          WHERE pgmid = 'R3TR' AND obj_name = <ls_tadir>-ref_obj_name.
        IF sy-subrc = 0.
          MESSAGE s006(zabaplint) WITH <ls_tadir>-ref_obj_type <ls_tadir>-ref_obj_name lv_object INTO lv_msg.
        ELSE.
          MESSAGE s005(zabaplint) WITH <ls_tadir>-ref_obj_type <ls_tadir>-ref_obj_name INTO lv_msg.
        ENDIF.
        mi_log->add_warning( iv_msg  = lv_msg
                             is_item = ms_item ).
        DELETE ct_tadir.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD remove_duplicates.

    FIELD-SYMBOLS <ls_tadir> TYPE ty_tadir.

    IF lines( mt_results ) > 0.
      LOOP AT ct_tadir ASSIGNING <ls_tadir>.
        READ TABLE mt_results WITH KEY ref_obj_type = <ls_tadir>-ref_obj_type
                                       ref_obj_name = <ls_tadir>-ref_obj_name
                              TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          DELETE ct_tadir.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD remove_own_package.

    DATA: lv_devclass TYPE devclass,
          lv_msg      TYPE string.

    FIELD-SYMBOLS <ls_tadir> TYPE ty_tadir.

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      " Sub packages are not handled otherwise
      IF <ls_tadir>-ref_obj_type = 'DEVC'.
        CONTINUE.
      ENDIF.

      lv_devclass = determine_package( iv_object_type = <ls_tadir>-ref_obj_type
                                       iv_object_name = <ls_tadir>-ref_obj_name ).

      READ TABLE mt_packages FROM lv_devclass TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE ct_tadir.
      ELSE.
        <ls_tadir>-devclass = lv_devclass.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve.

    DATA ls_wbcrossgt LIKE LINE OF it_wbcrossgt.
    DATA ls_tadir LIKE LINE OF rt_tadir.
    DATA lv_clstype TYPE seoclass-clstype.
    DATA lv_name TYPE badi_spot.

    LOOP AT it_wbcrossgt INTO ls_wbcrossgt.
      CASE ls_wbcrossgt-otype.
        WHEN 'TY'.
          " Skip class-specific types since DDIC types they refer to are already in wbcrossgt
          IF ls_wbcrossgt-name CS '\TY:'.
            CONTINUE.
          ENDIF.

          SELECT SINGLE clstype FROM seoclass INTO lv_clstype WHERE clsname = ls_wbcrossgt-name(30).
          IF sy-subrc <> 0.
            "Decide if Enhancement Spot
            SELECT SINGLE badi_name FROM badi_spot INTO lv_name WHERE badi_name = ls_wbcrossgt-name.
            IF sy-subrc = 0.
              CLEAR ls_tadir.
              ls_tadir-ref_obj_type = 'ENHS'.
              ls_tadir-ref_obj_name = ls_wbcrossgt-name.
              INSERT ls_tadir INTO TABLE rt_tadir.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.

          CASE lv_clstype.
            WHEN seoc_clstype_class.
              CLEAR ls_tadir.
              ls_tadir-ref_obj_type = 'CLAS'.
              ls_tadir-ref_obj_name = ls_wbcrossgt-name.
              INSERT ls_tadir INTO TABLE rt_tadir.

            WHEN seoc_clstype_interface.
              CLEAR ls_tadir.
              ls_tadir-ref_obj_type = 'INTF'.
              ls_tadir-ref_obj_name = ls_wbcrossgt-name.
              INSERT ls_tadir INTO TABLE rt_tadir.

            WHEN OTHERS.
              ASSERT 0 = 1.
          ENDCASE.

        WHEN OTHERS.
          CONTINUE. " todo ?
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_package_tree.

    DATA: lv_package LIKE LINE OF it_packages.

    CLEAR mt_packages[].
    "Determine sub packages
    LOOP AT it_packages INTO lv_package.
      add_subpackages( lv_package ).
    ENDLOOP.

  ENDMETHOD.


  METHOD update_index.

    DATA lv_include TYPE seoclsname.
    DATA lo_cross TYPE REF TO cl_wb_crossreference.

    lv_include = cl_oo_classname_service=>get_classpool_name( iv_name ).

    CREATE OBJECT lo_cross
      EXPORTING
        p_name    = |{ lv_include }|
        p_include = |{ lv_include }|.

    lo_cross->index_actualize( ).

  ENDMETHOD.
ENDCLASS.
