CLASS zcl_abaplint_deps_cache DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_memory TYPE abap_bool DEFAULT abap_false
        !iv_disk   TYPE abap_bool DEFAULT abap_false .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_memory      TYPE abap_bool DEFAULT abap_false
        !iv_disk        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_cache) TYPE REF TO zcl_abaplint_deps_cache .
    METHODS clear .
    METHODS save .
    METHODS read_deps
      IMPORTING
        !is_item    TYPE zif_abapgit_definitions=>ty_item
        !iv_minimal TYPE abap_bool
      EXPORTING
        !et_tadir   TYPE zcl_abaplint_deps_find=>ty_tadir_tt
        !ev_found   TYPE abap_bool .
    METHODS write_deps
      IMPORTING
        !is_item    TYPE zif_abapgit_definitions=>ty_item
        !iv_minimal TYPE abap_bool
        !it_tadir   TYPE zcl_abaplint_deps_find=>ty_tadir_tt
        !iv_package TYPE devclass .
    METHODS reset_deps .
    METHODS read_files
      IMPORTING
        !is_item  TYPE zif_abapgit_definitions=>ty_item
      EXPORTING
        !es_files TYPE zcl_abapgit_objects=>ty_serialization
        !ev_found TYPE abap_bool .
    METHODS write_files
      IMPORTING
        !is_item    TYPE zif_abapgit_definitions=>ty_item
        !is_files   TYPE zcl_abapgit_objects=>ty_serialization
        !iv_package TYPE devclass .
    METHODS reset_files .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_deps,
        item    TYPE zif_abapgit_definitions=>ty_item,
        minimal TYPE abap_bool,
        tadir   TYPE zcl_abaplint_deps_find=>ty_tadir_tt,
        package TYPE devclass,
      END OF ty_deps .
    TYPES:
      BEGIN OF ty_files,
        item    TYPE zif_abapgit_definitions=>ty_item,
        files   TYPE zcl_abapgit_objects=>ty_serialization,
        package TYPE devclass,
      END OF ty_files .

    CLASS-DATA go_cache TYPE REF TO zcl_abaplint_deps_cache .
    DATA:
      mt_deps TYPE HASHED TABLE OF ty_deps WITH UNIQUE KEY item minimal .
    DATA:
      mt_files TYPE HASHED TABLE OF ty_files WITH UNIQUE KEY item .
    DATA mv_memory TYPE abap_bool .
    DATA mv_disk TYPE abap_bool .

    METHODS load_deps
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_minimal     TYPE abap_bool
      EXPORTING
        !es_deps        TYPE ty_deps
        VALUE(ev_found) TYPE abap_bool .
    METHODS load_files
      IMPORTING
        !is_item  TYPE zif_abapgit_definitions=>ty_item
      EXPORTING
        !es_files TYPE ty_files
        !ev_found TYPE abap_bool .
    METHODS check_timestamp
      IMPORTING
        !iv_timestamp    TYPE timestamp
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS_CACHE IMPLEMENTATION.


  METHOD check_timestamp.

    CONSTANTS:
      lc_minute_in_sec TYPE i VALUE 60,
      lc_day_in_sec    TYPE i VALUE 86400.

    DATA:
      lv_timestamp  TYPE timestamp,
      lv_seconds    TYPE i,
      lv_name_class TYPE c LENGTH 1.

    TRY.
        GET TIME STAMP FIELD lv_timestamp.
        lv_seconds = cl_abap_tstmp=>subtract( tstmp1 = lv_timestamp
                                              tstmp2 = iv_timestamp ).
      CATCH cx_parameter_invalid_range.
        RETURN.
    ENDTRY.

    CALL FUNCTION 'TR_CHECK_NAME_CLASS'
      EXPORTING
        wi_pgmid               = 'R3TR'
        wi_object              = is_item-obj_type
        wi_objname             = is_item-obj_name
      IMPORTING
        we_name_class          = lv_name_class
      EXCEPTIONS
        namespace_not_existing = 1
        invalid_object         = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE lv_name_class.
      WHEN 'C'. " customer
        IF lv_seconds < 10 * lc_minute_in_sec.
          rv_result = abap_true.
        ENDIF.
      WHEN 'S'. " sap
        IF lv_seconds < 10 * lc_day_in_sec.
          rv_result = abap_true.
        ENDIF.
      WHEN 'N'. " namespace
        IF lv_seconds < 10 * lc_minute_in_sec.
          rv_result = abap_true.
        ENDIF.
      WHEN OTHERS.
        IF lv_seconds < 10 * lc_minute_in_sec.
          rv_result = abap_true.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD clear.
    DELETE FROM zabaplint_cache1 WHERE obj_type <> ''.
    DELETE FROM zabaplint_cache2 WHERE obj_type <> ''.
  ENDMETHOD.


  METHOD constructor.
    mv_memory = iv_memory.
    mv_disk = iv_disk.
  ENDMETHOD.


  METHOD get_instance.
    IF NOT go_cache IS BOUND.
      CREATE OBJECT go_cache
        EXPORTING
          iv_memory = iv_memory
          iv_disk   = iv_disk.
    ENDIF.

    ro_cache = go_cache.
  ENDMETHOD.


  METHOD load_deps.

    DATA:
      lt_cache1 TYPE TABLE OF zabaplint_cache1,
      ls_item   TYPE zif_abapgit_definitions=>ty_item,
      ls_tadir  TYPE zcl_abaplint_deps_find=>ty_tadir.

    FIELD-SYMBOLS:
      <ls_cache1> TYPE zabaplint_cache1.

    IF mv_disk = abap_false.
      RETURN.
    ENDIF.

    CLEAR: es_deps, ev_found.

    SELECT * FROM zabaplint_cache1 INTO TABLE lt_cache1
      WHERE obj_type = is_item-obj_type AND obj_name = is_item-obj_name AND minimal = iv_minimal
      ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      " Check if cache entry is still valid
      READ TABLE lt_cache1 ASSIGNING <ls_cache1> INDEX 1.
      IF check_timestamp( iv_timestamp = <ls_cache1>-timestamp
                          is_item      = is_item ) = abap_false.
        RETURN.
      ENDIF.

      " Build memory cache from persisted data
      LOOP AT lt_cache1 ASSIGNING <ls_cache1>.
        AT NEW obj_name.
          CLEAR es_deps.
          es_deps-item-obj_type = <ls_cache1>-obj_type.
          es_deps-item-obj_name = <ls_cache1>-obj_name.
          es_deps-minimal       = <ls_cache1>-minimal.
          es_deps-package       = <ls_cache1>-obj_pack.
        ENDAT.
        ls_tadir-ref_obj_type = <ls_cache1>-ref_obj_type.
        ls_tadir-ref_obj_name = <ls_cache1>-ref_obj_name.
        ls_tadir-devclass     = <ls_cache1>-devclass.
        INSERT ls_tadir INTO TABLE es_deps-tadir.
        AT END OF obj_name.
          INSERT es_deps INTO TABLE mt_deps.
        ENDAT.
      ENDLOOP.

      ev_found = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD load_files.

    DATA:
      lt_cache2 TYPE TABLE OF zabaplint_cache2,
      ls_item   TYPE zif_abapgit_definitions=>ty_item,
      ls_file   TYPE zif_abapgit_definitions=>ty_file.

    FIELD-SYMBOLS:
      <ls_cache2> TYPE zabaplint_cache2.

    IF mv_disk = abap_false.
      RETURN.
    ENDIF.

    CLEAR: es_files, ev_found.

    SELECT * FROM zabaplint_cache2 INTO TABLE lt_cache2
      WHERE obj_type = is_item-obj_type AND obj_name = is_item-obj_name.
    IF sy-subrc = 0.
      " Check if cache entry is still valid
      READ TABLE lt_cache2 ASSIGNING <ls_cache2> INDEX 1.
      IF check_timestamp( iv_timestamp = <ls_cache2>-timestamp
                          is_item      = is_item ) = abap_false.
        RETURN.
      ENDIF.

      " Build memory cache from persisted data
      LOOP AT lt_cache2 ASSIGNING <ls_cache2>.
        AT NEW obj_name.
          CLEAR es_files.
          es_files-item-obj_type = <ls_cache2>-obj_type.
          es_files-item-obj_name = <ls_cache2>-obj_name.
          es_files-package       = <ls_cache2>-obj_pack.
        ENDAT.
        es_files-files-item-obj_type = <ls_cache2>-item_type.
        es_files-files-item-obj_name = <ls_cache2>-item_name.
        CLEAR ls_file.
        ls_file-path     = <ls_cache2>-path.
        ls_file-filename = <ls_cache2>-filename.
        ls_file-sha1     = <ls_cache2>-sha1.
        ls_file-data     = <ls_cache2>-content.
        INSERT ls_file INTO TABLE es_files-files-files.
        AT END OF obj_name.
          INSERT es_files INTO TABLE mt_files.
        ENDAT.
      ENDLOOP.

      ev_found = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD read_deps.

    DATA ls_deps LIKE LINE OF mt_deps.

    IF mv_memory = abap_false AND mv_disk = abap_false.
      RETURN.
    ENDIF.

    CLEAR: et_tadir, ev_found.

    READ TABLE mt_deps INTO ls_deps WITH TABLE KEY item = is_item minimal = iv_minimal.
    IF sy-subrc <> 0 AND mv_disk = abap_true.
      load_deps(
        EXPORTING
          is_item    = is_item
          iv_minimal = iv_minimal
        IMPORTING
          es_deps    = ls_deps
          ev_found   = ev_found ).
      IF ev_found = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    et_tadir = ls_deps-tadir.
    ev_found = abap_true.

  ENDMETHOD.


  METHOD read_files.

    DATA ls_files LIKE LINE OF mt_files.

    IF mv_memory = abap_false AND mv_disk = abap_false.
      RETURN.
    ENDIF.

    CLEAR: es_files, ev_found.

    READ TABLE mt_files INTO ls_files WITH TABLE KEY item = is_item.
    IF sy-subrc <> 0 AND mv_disk = abap_true.
      load_files(
        EXPORTING
          is_item    = is_item
        IMPORTING
          es_files   = ls_files
          ev_found   = ev_found ).
      IF ev_found = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    es_files = ls_files-files.
    ev_found = abap_true.

  ENDMETHOD.


  METHOD reset_deps.
    CLEAR mt_deps.
  ENDMETHOD.


  METHOD reset_files.
    CLEAR mt_files.
  ENDMETHOD.


  METHOD save.

    DATA:
      lv_tabix     TYPE i,
      lv_timestamp TYPE timestamp,
      lt_cache1    TYPE TABLE OF zabaplint_cache1,
      lt_cache2    TYPE TABLE OF zabaplint_cache2.

    FIELD-SYMBOLS:
      <ls_deps>   TYPE ty_deps,
      <ls_tadir>  TYPE zcl_abaplint_deps_find=>ty_tadir,
      <ls_files>  TYPE ty_files,
      <ls_file>   TYPE zif_abapgit_definitions=>ty_file,
      <ls_cache1> TYPE zabaplint_cache1,
      <ls_cache2> TYPE zabaplint_cache2.

    IF mv_disk = abap_false.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD lv_timestamp.

    " Save dependencies to persistency
    LOOP AT mt_deps ASSIGNING <ls_deps>.
      CLEAR lv_tabix.
      LOOP AT <ls_deps>-tadir ASSIGNING <ls_tadir>.
        APPEND INITIAL LINE TO lt_cache1 ASSIGNING <ls_cache1>.
        <ls_cache1>-idx          = lv_tabix.
        <ls_cache1>-timestamp    = lv_timestamp.
        <ls_cache1>-obj_type     = <ls_deps>-item-obj_type.
        <ls_cache1>-obj_name     = <ls_deps>-item-obj_name.
        <ls_cache1>-obj_pack     = <ls_deps>-package.
        <ls_cache1>-minimal      = <ls_deps>-minimal.
        <ls_cache1>-ref_obj_type = <ls_tadir>-ref_obj_type.
        <ls_cache1>-ref_obj_name = <ls_tadir>-ref_obj_name.
        <ls_cache1>-devclass     = <ls_tadir>-devclass.
        lv_tabix = lv_tabix + 1.
      ENDLOOP.
    ENDLOOP.

    " Save serialized files to persistency
    LOOP AT mt_files ASSIGNING <ls_files>.
      CLEAR lv_tabix.
      LOOP AT <ls_files>-files-files ASSIGNING <ls_file>.
        APPEND INITIAL LINE TO lt_cache2 ASSIGNING <ls_cache2>.
        <ls_cache2>-idx       = lv_tabix.
        <ls_cache2>-timestamp = lv_timestamp.
        <ls_cache2>-obj_type  = <ls_files>-item-obj_type.
        <ls_cache2>-obj_name  = <ls_files>-item-obj_name.
        <ls_cache1>-obj_pack  = <ls_files>-package.
        <ls_cache2>-path      = <ls_file>-path.
        <ls_cache2>-filename  = <ls_file>-filename.
        <ls_cache2>-sha1      = <ls_file>-sha1.
        <ls_cache2>-len       = xstrlen( <ls_file>-data ).
        <ls_cache2>-content   = <ls_file>-data.
        <ls_cache2>-item_type = <ls_files>-files-item-obj_type.
        <ls_cache2>-item_name = <ls_files>-files-item-obj_name.
        lv_tabix = lv_tabix + 1.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_cache1 ASSIGNING <ls_cache1>.
      INSERT zabaplint_cache1 FROM <ls_cache1>.
      IF sy-subrc <> 0.
        UPDATE zabaplint_cache1 FROM <ls_cache1>.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_cache2 ASSIGNING <ls_cache2>.
      INSERT zabaplint_cache2 FROM <ls_cache2>.
      IF sy-subrc <> 0.
        UPDATE zabaplint_cache2 FROM <ls_cache2>.
      ENDIF.
    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD write_deps.

    DATA ls_deps LIKE LINE OF mt_deps.

    IF mv_memory = abap_false AND mv_disk = abap_false.
      RETURN.
    ENDIF.

    READ TABLE mt_deps TRANSPORTING NO FIELDS
      WITH TABLE KEY item = is_item minimal = iv_minimal.
    IF sy-subrc <> 0.
      ls_deps-item    = is_item.
      ls_deps-minimal = iv_minimal.
      ls_deps-tadir   = it_tadir.
      ls_deps-package = iv_package.
      INSERT ls_deps INTO TABLE mt_deps.
    ENDIF.

  ENDMETHOD.


  METHOD write_files.

    DATA ls_files LIKE LINE OF mt_files.

    IF mv_memory = abap_false AND mv_disk = abap_false.
      RETURN.
    ENDIF.

    READ TABLE mt_files TRANSPORTING NO FIELDS
      WITH TABLE KEY item = is_item.
    IF sy-subrc <> 0.
      ls_files-item    = is_item.
      ls_files-files   = is_files.
      ls_files-package = iv_package.
      INSERT ls_files INTO TABLE mt_files.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
