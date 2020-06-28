CLASS zcl_abaplint_deps_find DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_max_level TYPE i DEFAULT 20.
    METHODS find_by_item
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS find_by_package
      IMPORTING
        !iv_package     TYPE devclass
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

    DATA mv_max_level TYPE i.

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
        !iv_package     TYPE devclass
        !is_object      TYPE zif_abapgit_definitions=>ty_tadir
        !iv_level       TYPE i
      RETURNING
        VALUE(rt_total) TYPE ty_tadir_tt .
    METHODS resolve
      IMPORTING
        !it_wbcrossgt TYPE wbcrossgtt
      CHANGING
        !ct_tadir     TYPE ty_tadir_tt .
    METHODS update_index
      IMPORTING
        !iv_name TYPE seoclsname .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS_FIND IMPLEMENTATION.


  METHOD constructor.
    mv_max_level = iv_max_level.
  ENDMETHOD.


  METHOD convert_senvi_to_tadir.

* do not use CL_WB_RIS_ENVIRONMENT, it does not exist in 740sp08

    LOOP AT it_senvi INTO DATA(ls_senvi).
      IF ls_senvi-type = 'CLAS'
          OR ls_senvi-type = 'DTEL'
          OR ls_senvi-type = 'TABL'
          OR ls_senvi-type = 'TYPE'
          OR ls_senvi-type = 'INTF'.
        APPEND VALUE #(
          ref_obj_type = ls_senvi-type
          ref_obj_name = ls_senvi-object ) TO rt_tadir.
      ELSEIF ls_senvi-type = 'INCL'.
        APPEND VALUE #(
          ref_obj_type = 'PROG'
          ref_obj_name = ls_senvi-object ) TO rt_tadir.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_by_item.

* todo
    CLEAR rt_tadir.

  ENDMETHOD.


  METHOD find_by_package.

    DATA lt_total TYPE ty_tadir_tt.

    DATA(lt_objects) = zcl_abapgit_factory=>get_tadir( )->read( iv_package ).
    DELETE lt_objects WHERE object = 'DEVC'.
    DELETE lt_objects WHERE object = 'TRAN'. " todo, hmm?

* todo, skip generated maintenance view function groups?

    LOOP AT lt_objects INTO DATA(ls_object).
      cl_progress_indicator=>progress_indicate(
        i_text               = |Finding dependencies, { ls_object-object } { ls_object-obj_name }|
        i_processed          = sy-tabix
        i_total              = lines( lt_objects )
        i_output_immediately = abap_true ).

      APPEND LINES OF get_dependencies(
        iv_package = iv_package
        is_object  = ls_object
        iv_level   = 1 ) TO lt_total.
    ENDLOOP.

    SORT lt_total BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_total COMPARING ref_obj_type ref_obj_name.

    LOOP AT lt_total INTO DATA(ls_total).
      WRITE: / ls_total-ref_obj_type, ls_total-ref_obj_name.

      APPEND VALUE #(
        object   = ls_total-ref_obj_type
        obj_name = ls_total-ref_obj_name ) TO rt_tadir.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_clas_dependencies.

    DATA lt_includes TYPE STANDARD TABLE OF programm WITH EMPTY KEY.
    DATA lt_wbcrossgt TYPE wbcrossgtt.


    DATA(lv_clsname) = CONV seoclsname( iv_name ).

    TRY.
        DATA(lv_final) = CAST cl_oo_class( cl_oo_class=>get_instance( lv_clsname ) )->is_final( ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    APPEND cl_oo_classname_service=>get_pubsec_name( CONV #( iv_name ) ) TO lt_includes.
    IF lv_final = abap_false.
      APPEND cl_oo_classname_service=>get_prisec_name( CONV #( iv_name ) ) TO lt_includes.
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
      ASSERT 0 = 1.
    ENDIF.

  ENDMETHOD.


  METHOD get_dependencies.

    DATA: lv_obj_type    TYPE euobj-id,
          lt_tadir       TYPE ty_tadir_tt,
          lt_environment TYPE senvi_tab.


    IF iv_level > 1 AND is_object-object = 'CLAS'.
      find_clas_dependencies(
        EXPORTING
          iv_name  = is_object-obj_name
          iv_level = iv_level
        CHANGING
          ct_tadir = lt_tadir ).
    ELSEIF is_object-object = 'TABL'.
* do not traverse further
    ELSE.
      lv_obj_type = is_object-object.
      CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
        EXPORTING
          obj_type       = lv_obj_type
          object_name    = is_object-obj_name
        TABLES
          environment    = lt_environment
        EXCEPTIONS
          batch          = 1
          batchjob_error = 2
          not_executed   = 3
          OTHERS         = 4.
      IF sy-subrc = 3.
        RETURN.
      ENDIF.
      ASSERT sy-subrc = 0.

      lt_tadir = convert_senvi_to_tadir( lt_environment ).
    ENDIF.

    SORT lt_tadir BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_tadir COMPARING ref_obj_type ref_obj_name.

    DELETE lt_tadir WHERE ref_obj_type = 'FUGR'.
    DELETE lt_tadir WHERE ref_obj_type = 'DTEL'.
    DELETE lt_tadir WHERE ref_obj_type = 'SFSW'.
    DELETE lt_tadir WHERE ref_obj_type = 'DEVC'.
    DELETE lt_tadir WHERE ref_obj_type = 'SUSO'.
    DELETE lt_tadir WHERE ref_obj_type = 'TYPE'.
    DELETE lt_tadir WHERE ref_obj_type = 'TTYP'.
    DELETE lt_tadir WHERE ref_obj_type = 'DOMA'.
    DELETE lt_tadir WHERE ref_obj_type = 'XSLT'.
    DELETE lt_tadir WHERE ref_obj_type = 'SHLP'.
    DELETE lt_tadir WHERE ref_obj_type = 'SQLT'.

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      DATA(lv_index) = sy-tabix.
      SELECT SINGLE devclass FROM tadir INTO @DATA(lv_devclass)
        WHERE pgmid = 'R3TR'
        AND object = @ls_tadir-ref_obj_type
        AND obj_name = @ls_tadir-ref_obj_name.
* todo, should check if its a sub-package
      IF sy-subrc <> 0 OR lv_devclass CP |{ iv_package }*|.
        DELETE lt_tadir INDEX lv_index.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    APPEND LINES OF lt_tadir TO rt_total.

    LOOP AT lt_tadir INTO ls_tadir.
      DATA(ls_object) = VALUE zif_abapgit_definitions=>ty_tadir(
        object   = ls_tadir-ref_obj_type
        obj_name = ls_tadir-ref_obj_name ).

      DATA(lv_level) = iv_level + 1.

      get_dependencies(
        iv_package = iv_package
        is_object  = ls_object
        iv_level   = lv_level ).
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve.

    LOOP AT it_wbcrossgt INTO DATA(ls_wbcrossgt).
      CASE ls_wbcrossgt-otype.
        WHEN 'TY'.
          SELECT SINGLE clstype FROM seoclass INTO @DATA(lv_clstype) WHERE clsname = @ls_wbcrossgt-name(30).
          IF sy-subrc = 0.
            CASE lv_clstype.
              WHEN '0'.
                APPEND VALUE #( ref_obj_type = 'CLAS' ref_obj_name = ls_wbcrossgt-name ) TO ct_tadir.
              WHEN '1'.
                APPEND VALUE #( ref_obj_type = 'INTF' ref_obj_name = ls_wbcrossgt-name ) TO ct_tadir.
              WHEN OTHERS.
                ASSERT 0 = 1.
            ENDCASE.
          ENDIF.
        WHEN OTHERS.
          CONTINUE. " todo ?
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_index.

    DATA(lv_include) = cl_oo_classname_service=>get_classpool_name( iv_name ).

    DATA(lo_cross) = NEW cl_wb_crossreference(
      p_name    = lv_include
      p_include = lv_include ).

    lo_cross->index_actualize( ).

  ENDMETHOD.
ENDCLASS.
