CLASS zcl_abaplint_deps_find DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_max_level TYPE i DEFAULT 20 .
    METHODS find_by_item
      IMPORTING
        !iv_object_type TYPE trobjtype
        !iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS find_by_packages
      IMPORTING
        !it_packages    TYPE tr_devclasses
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
PROTECTED SECTION.

  TYPES:
    BEGIN OF ty_tadir,
      ref_obj_type TYPE trobjtype,
      ref_obj_name TYPE sobj_name,
    END OF ty_tadir .
  TYPES:
    ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY .

  DATA mv_max_level TYPE i .

  METHODS convert_senvi_to_tadir
    IMPORTING
      !it_senvi       TYPE senvi_tab
    RETURNING
      VALUE(rt_tadir) TYPE ty_tadir_tt .
  METHODS find_clas_dependencies
    IMPORTING
      !iv_name  TYPE tadir-obj_name
      !iv_level TYPE i
    CHANGING
      !ct_tadir TYPE ty_tadir_tt .
  METHODS get_dependencies
    IMPORTING
      !is_object TYPE zif_abapgit_definitions=>ty_tadir
      !iv_level  TYPE i .
  METHODS resolve
    IMPORTING
      !it_wbcrossgt TYPE wbcrossgtt
    CHANGING
      !ct_tadir     TYPE ty_tadir_tt .
  METHODS update_index
    IMPORTING
      !iv_name TYPE seoclsname .
private section.

  data MV_PACKAGES type TR_DEVCLASSES .
  data MV_RESULTS type TY_TADIR_TT .

  methods CLEAN_OWN_PACKAGES .
  methods CLEAR_RESULTS .
  methods DETERMINE_PACKAGE
    importing
      !IV_OBJECT_TYPE type TROBJTYPE
      !IV_OBJECT_NAME type SOBJ_NAME
    returning
      value(RV_PACKAGE) type DEVCLASS .
  methods PREPARE_SUPPORTED_TYPES
    returning
      value(RS_TYPES) type ENVI_TYPES .
  methods SET_PACKAGE_TREE
    importing
      !IT_PACKAGES type TR_DEVCLASSES .
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS_FIND IMPLEMENTATION.


  METHOD clean_own_packages.

    DATA: lv_package LIKE LINE OF mv_packages.

    LOOP AT mv_packages INTO lv_package.
      READ TABLE mv_results
        WITH KEY ref_obj_type = 'DEVC' ref_obj_name = lv_package
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE mv_results INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method CLEAR_RESULTS.
    clear mv_results[].
  endmethod.


  METHOD constructor.
    mv_max_level = iv_max_level.
  ENDMETHOD.


  METHOD convert_senvi_to_tadir.

* do not use CL_WB_RIS_ENVIRONMENT, it does not exist in 740sp08

    DATA: ls_senvi LIKE LINE OF it_senvi,
          lv_clstype type SEOCLSTYPE.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF rt_tadir.

    LOOP AT it_senvi INTO ls_senvi.
      "Translate when required
      IF ls_senvi-type = 'BADI'. "Ignore
        CONTINUE.

      ELSEIF ls_senvi-type = 'INCL'. "Include is PROG
        APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-ref_obj_type = 'PROG'.
        <ls_tadir>-ref_obj_name = ls_senvi-object.

      ELSEIF ls_senvi-type = 'STRU'.  "Structure is TABLE
        APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-ref_obj_type = 'TABL'.
        <ls_tadir>-ref_obj_name = ls_senvi-object.

      ELSEIF ls_senvi-type = 'FUNC'. "Convert to function group
        IF ls_senvi-encl_obj IS NOT INITIAL.
          APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
          <ls_tadir>-ref_obj_type = 'FUGR'.
          <ls_tadir>-ref_obj_name = ls_senvi-encl_obj.
        ENDIF.

      ELSEIF ls_senvi-type = 'MESS'. "Always keep complete message area
        IF ls_senvi-encl_obj IS NOT INITIAL.
          APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
          <ls_tadir>-ref_obj_type = 'MSAG'.
          <ls_tadir>-ref_obj_name = ls_senvi-encl_obj.
        ENDIF.

      ELSEIF ls_senvi-type = 'DGT'. "Type Pool is always loaded for type to be used
        APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-ref_obj_type = 'TYPE'.
        <ls_tadir>-ref_obj_name = ls_senvi-encl_obj.

      ELSEIF ls_senvi-type = 'OA' OR   "Object Attributes
             ls_senvi-type = 'OE' OR   "Object Events
             ls_senvi-type = 'OM' OR   "Object Method
             ls_senvi-type = 'OT'.     "Object Type
        IF ls_senvi-encl_obj IS NOT INITIAL.
          "Determine class or interface
          SELECT SINGLE clstype FROM seoclass INTO @lv_clstype WHERE clsname = @ls_senvi-encl_obj.

          IF lv_clstype = seoc_clstype_class.
            APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
            <ls_tadir>-ref_obj_type = 'CLAS'.
            <ls_tadir>-ref_obj_name = ls_senvi-encl_obj.
          ELSE.
            APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
            <ls_tadir>-ref_obj_type = 'INTF'.
            <ls_tadir>-ref_obj_name = ls_senvi-encl_obj.
          ENDIF.
        ENDIF.

      ELSE.
        APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-ref_obj_type = ls_senvi-type.
        <ls_tadir>-ref_obj_name = ls_senvi-object.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD determine_package.
*
* Translate certain Object Types
*
    DATA: lv_type TYPE trobjtype,
          lv_name TYPE sobj_name.

    CLEAR rv_package.
    lv_type = iv_object_type.
    lv_name = iv_object_name.
    IF iv_object_type = 'FUNC'.
      SELECT SINGLE pname FROM tfdir INTO lv_name.
      lv_type = 'FUGR'.
    ENDIF.

    SELECT SINGLE devclass FROM tadir INTO @rv_package
       WHERE pgmid = 'R3TR'
       AND object = @lv_type
       AND obj_name = @lv_name.

  ENDMETHOD.


  METHOD find_by_item.

    DATA: ls_object   TYPE zif_abapgit_definitions=>ty_tadir,
          lt_packages TYPE tr_devclasses.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF rt_tadir.

    "Determine Package Tree
    DATA(lv_package) = determine_package( iv_object_type = iv_object_type
                                          iv_object_name = iv_object_name ).

    IF lv_package IS INITIAL.
      RETURN.
    ENDIF.
    APPEND lv_package TO lt_packages.
    set_package_tree( it_packages = lt_packages ).
    clear_results( ).

    ls_object-object = iv_object_type.
    ls_object-obj_name = iv_object_name.

    get_dependencies(
      is_object  = ls_object
      iv_level   = 1 ).

    clean_own_packages( ).

    LOOP AT mv_results INTO DATA(ls_result).
      APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
      <ls_tadir>-object = ls_result-ref_obj_type.
      <ls_tadir>-obj_name = ls_result-ref_obj_name.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_by_packages.

    data: lv_package like line of it_packages.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF rt_tadir.

    "Determine Package Tree
    set_package_tree( it_packages = it_packages ).
    clear_results( ).

    LOOP AT it_packages INTO lv_package.

      DATA(ls_object) = VALUE zif_abapgit_definitions=>ty_tadir(
              object   = 'DEVC'
              obj_name = lv_package ).

      cl_progress_indicator=>progress_indicate(
        i_text               = |Process package, { ls_object-object } { ls_object-obj_name }|
        i_processed          = sy-tabix
        i_total              = lines( it_packages )
        i_output_immediately = abap_true ).

      get_dependencies(
        is_object  = ls_object
        iv_level   = 1 ).
    ENDLOOP.

    clean_own_packages( ).

    LOOP AT mv_results INTO DATA(ls_result).
      APPEND INITIAL LINE TO rt_tadir ASSIGNING <ls_tadir>.
      <ls_tadir>-object = ls_result-ref_obj_type.
      <ls_tadir>-obj_name = ls_result-ref_obj_name.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_clas_dependencies.

    DATA lt_includes TYPE STANDARD TABLE OF programm WITH DEFAULT KEY.
    DATA lt_wbcrossgt TYPE wbcrossgtt.
    DATA lv_clsname TYPE seoclsname.
    DATA lv_final TYPE abap_bool.
    DATA lo_oo_class TYPE REF TO cl_oo_class.

    lv_clsname = |{ iv_name }|.

    TRY.
        lo_oo_class ?= cl_oo_class=>get_instance( lv_clsname ).
        lv_final = lo_oo_class->is_final( ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    APPEND cl_oo_classname_service=>get_pubsec_name( |{ iv_name }| ) TO lt_includes.
    IF lv_final = abap_false.
      APPEND cl_oo_classname_service=>get_prosec_name( |{ iv_name }| ) TO lt_includes.
    ENDIF.

    SELECT * FROM wbcrossgt INTO CORRESPONDING FIELDS OF TABLE @lt_wbcrossgt
      FOR ALL ENTRIES IN @lt_includes
      WHERE include = @lt_includes-table_line
      AND name <> @iv_name.
    IF lines( lt_wbcrossgt ) = 0.
* update so it is correct in the next run
      update_index( lv_clsname ).

      SELECT * FROM wbcrossgt INTO CORRESPONDING FIELDS OF TABLE @lt_wbcrossgt
        FOR ALL ENTRIES IN @lt_includes
        WHERE include = @lt_includes-table_line
        AND name <> @iv_name.
    ENDIF.

    IF iv_level < mv_max_level.
      resolve(
        EXPORTING
          it_wbcrossgt = lt_wbcrossgt
        CHANGING
          ct_tadir     = ct_tadir ).
    ELSE.
      FORMAT INTENSIFIED ON.
      WRITE: / 'Level limit ', mv_max_level, 'reached for', lv_clsname, '. Not all dependencies collected.'.
      FORMAT INTENSIFIED OFF.
    ENDIF.

  ENDMETHOD.


  METHOD get_dependencies.

    DATA: lv_obj_type    TYPE euobj-id,
          lt_tadir       TYPE ty_tadir_tt,
          lt_environment TYPE senvi_tab,
          lv_types       TYPE envi_types,
          lv_index       LIKE sy-tabix,
          ls_tadir       TYPE LINE OF ty_tadir_tt,
          lv_flag        TYPE flag,
          lv_devclass    TYPE devclass.
    DATA: BEGIN OF ls_tadir_obj,
            object    TYPE trobjtype,
            obj_name  TYPE sobj_name,
            srcsystem TYPE srcsystem,
            author    TYPE responsibl,
            devclass  TYPE devclass,
            genflag   TYPE genflag,
          END OF ls_tadir_obj.

    SELECT SINGLE object, obj_name, srcsystem, author, devclass, genflag
      FROM tadir INTO @ls_tadir_obj
      WHERE pgmid = 'R3TR' AND object = @is_object-object AND obj_name = @is_object-obj_name.

    IF sy-subrc <> 0 "no tadir"
        OR ls_tadir_obj-genflag IS NOT INITIAL. "Ignore generated objects
      RETURN.
    ENDIF.
*
* Determine direct dependency
*
    IF is_object-object = 'CLAS'.
      find_clas_dependencies(
        EXPORTING
          iv_name  = is_object-obj_name
          iv_level = iv_level
        CHANGING
          ct_tadir = lt_tadir ).
    ELSE.
      lv_obj_type = is_object-object.
      lv_types = prepare_supported_types( ).

      CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
        EXPORTING
          obj_type          = lv_obj_type
          object_name       = is_object-obj_name
          environment_types = lv_types
          online_force      = 'X'
        TABLES
          environment       = lt_environment
        EXCEPTIONS
          batch             = 1
          batchjob_error    = 2
          not_executed      = 3
          OTHERS            = 4.
      IF sy-subrc = 3.
        RETURN.
      ENDIF.
      ASSERT sy-subrc = 0.

      lt_tadir = convert_senvi_to_tadir( lt_environment ).
    ENDIF.

    SORT lt_tadir BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_tadir COMPARING ref_obj_type ref_obj_name.

    IF lines( lt_tadir ) = 0.
      RETURN.
    ENDIF.

*
* Remove entries already in collection
*
    IF lines( mv_results ) > 0.
      LOOP AT lt_tadir INTO ls_tadir.
        lv_index = sy-tabix.
        READ TABLE mv_results WITH KEY ref_obj_type = ls_tadir-ref_obj_type
                                       ref_obj_name = ls_tadir-ref_obj_name
                              TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          DELETE lt_tadir INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lines( lt_tadir ) = 0.
      RETURN.
    ENDIF.
*
* Remove entries from own package (or sub packages)
*
    LOOP AT lt_tadir INTO ls_tadir.
      IF ls_tadir-ref_obj_type = 'DEVC'. "sub packages are not handled otherwise
        CONTINUE.
      ENDIF.
      lv_index = sy-tabix.
      lv_devclass = determine_package( iv_object_type   = ls_tadir-ref_obj_type
                                       iv_object_name = ls_tadir-ref_obj_name ).
      lv_flag = 'X'.
      IF sy-subrc = 0.
        READ TABLE mv_packages FROM lv_devclass TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CLEAR lv_flag.
        ENDIF.
      ENDIF.
      IF lv_flag = 'X'.
        DELETE lt_tadir INDEX lv_index.
      ENDIF.
    ENDLOOP.

    "Add to dependency collection
    APPEND LINES OF lt_tadir TO mv_results.

    SORT mv_results BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM mv_results COMPARING ref_obj_type ref_obj_name.

*
* if sap object, do not go deeper
*
    IF ( ls_tadir_obj-author = 'SAP'
         OR ls_tadir_obj-author = 'SAP*' )
       AND ls_tadir_obj-srcsystem = 'SAP'.
      RETURN.
    ENDIF.
*
*   Certain types do not have dependent object or are resolved by this call
*
    DELETE lt_tadir WHERE ref_obj_type = 'MSAG'. "Message AG
    DELETE lt_tadir WHERE ref_obj_type = 'DTEL'. "Data Element
*
* Try to find dependend objects
*
    LOOP AT lt_tadir INTO ls_tadir.

      DATA(ls_object) = VALUE zif_abapgit_definitions=>ty_tadir(
        object   = ls_tadir-ref_obj_type
        obj_name = ls_tadir-ref_obj_name ).

      DATA(lv_level) = iv_level + 1.

      get_dependencies(
        is_object  = ls_object
        iv_level   = lv_level ).
    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_supported_types.
    rs_types-prog = 'X'.
    rs_types-fugr = 'X'.
    rs_types-msag = 'X'.
    rs_types-tran = 'X'.
    rs_types-func = 'X'.
    rs_types-tabl = 'X'.
    rs_types-doma = ' '.
    rs_types-dtel = 'X'.
    rs_types-view = 'X'.
    rs_types-para = 'X'.
    rs_types-type = 'X'.
    rs_types-ttyp = 'X'.
    rs_types-stru = 'X'.
    rs_types-enqu = ' '.
    rs_types-clas = 'X'.
    rs_types-intf = 'X'.
    rs_types-ttab = 'X'.
    rs_types-devc = 'X'.
  ENDMETHOD.


  METHOD resolve.

    DATA ls_wbcrossgt LIKE LINE OF it_wbcrossgt.
    DATA lv_clstype TYPE seoclass-clstype.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF ct_tadir.

    LOOP AT it_wbcrossgt INTO ls_wbcrossgt.
      CASE ls_wbcrossgt-otype.
        WHEN 'TY'.
          SELECT SINGLE clstype FROM seoclass INTO @lv_clstype WHERE clsname = @ls_wbcrossgt-name(30).
          IF sy-subrc = 0.
            CASE lv_clstype.
              WHEN '0'.
                APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_tadir>.
                <ls_tadir>-ref_obj_type = 'CLASS'.
                <ls_tadir>-ref_obj_name = ls_wbcrossgt-name.

              WHEN '1'.
                APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_tadir>.
                <ls_tadir>-ref_obj_type = 'INTF'.
                <ls_tadir>-ref_obj_name = ls_wbcrossgt-name.

              WHEN OTHERS.
                ASSERT 0 = 1.
            ENDCASE.
          ENDIF.
        WHEN OTHERS.
          CONTINUE. " todo ?
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_package_tree.

    DATA: lv_package LIKE LINE OF it_packages.

    CLEAR mv_packages[].
    "Determine sub packages
    LOOP AT it_packages INTO lv_package.
      SELECT devclass FROM tdevc APPENDING TABLE mv_packages WHERE parentcl = lv_package.
      APPEND lv_package TO mv_packages.
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
