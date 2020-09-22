CLASS zcl_abaplint_deps_cache DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_memory  TYPE abap_bool DEFAULT abap_false
        !iv_cluster TYPE abap_bool DEFAULT abap_false .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_memory      TYPE abap_bool DEFAULT abap_false
        !iv_cluster     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_cache) TYPE REF TO zcl_abaplint_deps_cache .
    METHODS clear .
    METHODS load .
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
        !it_tadir   TYPE zcl_abaplint_deps_find=>ty_tadir_tt .
    METHODS reset_deps .
    METHODS read_files
      IMPORTING
        !is_item  TYPE zif_abapgit_definitions=>ty_item
      EXPORTING
        !es_files TYPE zcl_abapgit_objects=>ty_serialization
        !ev_found TYPE abap_bool .
    METHODS write_files
      IMPORTING
        !is_item  TYPE zif_abapgit_definitions=>ty_item
        !is_files TYPE zcl_abapgit_objects=>ty_serialization .
    METHODS reset_files .
  PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    BEGIN OF ty_deps,
      item    TYPE zif_abapgit_definitions=>ty_item,
      minimal TYPE abap_bool,
      tadir   TYPE zcl_abaplint_deps_find=>ty_tadir_tt,
    END OF ty_deps .
  TYPES:
    BEGIN OF ty_files,
      item  TYPE zif_abapgit_definitions=>ty_item,
      files TYPE zcl_abapgit_objects=>ty_serialization,
    END OF ty_files .

  CLASS-DATA go_cache TYPE REF TO zcl_abaplint_deps_cache .
  DATA:
    mt_deps TYPE HASHED TABLE OF ty_deps WITH UNIQUE KEY item minimal .
  DATA:
    mt_files TYPE HASHED TABLE OF ty_files WITH UNIQUE KEY item .
  DATA mv_memory TYPE abap_bool .
  DATA mv_cluster TYPE abap_bool .
ENDCLASS.



CLASS ZCL_ABAPLINT_DEPS_CACHE IMPLEMENTATION.


  METHOD clear.

    DELETE FROM DATABASE zabaplint_cache(zr) ID 'CACHE'.

  ENDMETHOD.


  METHOD constructor.
    mv_memory = iv_memory.
    mv_cluster = iv_cluster.
  ENDMETHOD.


  METHOD get_instance.
    IF NOT go_cache IS BOUND.
      CREATE OBJECT go_cache
        EXPORTING
          iv_memory  = iv_memory
          iv_cluster = iv_cluster.
    ENDIF.

    ro_cache = go_cache.

    IF iv_cluster = abap_true.
      ro_cache->load( ).
    ENDIF.
  ENDMETHOD.


  METHOD load.

    IF mv_cluster = abap_false.
      RETURN.
    ENDIF.

    IMPORT deps = mt_deps files = mt_files
      FROM DATABASE zabaplint_cache(zr) ID 'CACHE'.

  ENDMETHOD.


  METHOD read_deps.
    FIELD-SYMBOLS <ls_deps> LIKE LINE OF mt_deps.

    IF mv_memory = abap_false AND mv_cluster = abap_false.
      RETURN.
    ENDIF.

    CLEAR: et_tadir, ev_found.

    READ TABLE mt_deps ASSIGNING <ls_deps>
      WITH TABLE KEY item = is_item minimal = iv_minimal.
    IF sy-subrc = 0.
      et_tadir = <ls_deps>-tadir.
      ev_found = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD read_files.
    FIELD-SYMBOLS <ls_files> LIKE LINE OF mt_files.

    IF mv_memory = abap_false AND mv_cluster = abap_false.
      RETURN.
    ENDIF.

    CLEAR: es_files, ev_found.

    READ TABLE mt_files ASSIGNING <ls_files>
      WITH TABLE KEY item = is_item.
    IF sy-subrc = 0.
      es_files = <ls_files>-files.
      ev_found = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD reset_deps.
    CLEAR mt_deps.
  ENDMETHOD.


  METHOD reset_files.
    CLEAR mt_files.
  ENDMETHOD.


  METHOD save.

    DATA:
      lt_deps  LIKE mt_deps,
      lt_files LIKE mt_files.

    IF mv_cluster = abap_false.
      RETURN.
    ENDIF.

    lt_deps = mt_deps.
    DELETE lt_deps WHERE item-obj_name CP 'Y*' OR item-obj_name CP 'Z*' OR item-obj_name CP '/*'.

    lt_files = mt_files.
    DELETE lt_files WHERE item-obj_name CP 'Y*' OR item-obj_name CP 'Z*' OR item-obj_name CP '/*'.

    EXPORT deps = lt_deps files = lt_files
      TO DATABASE zabaplint_cache(zr) ID 'CACHE'.

  ENDMETHOD.


  METHOD write_deps.
    DATA ls_deps LIKE LINE OF mt_deps.

    IF mv_memory = abap_false AND mv_cluster = abap_false.
      RETURN.
    ENDIF.

    READ TABLE mt_deps TRANSPORTING NO FIELDS
      WITH TABLE KEY item = is_item minimal = iv_minimal.
    IF sy-subrc <> 0.
      ls_deps-item = is_item.
      ls_deps-minimal = iv_minimal.
      ls_deps-tadir = it_tadir.
      INSERT ls_deps INTO TABLE mt_deps.
    ENDIF.
  ENDMETHOD.


  METHOD write_files.
    DATA ls_files LIKE LINE OF mt_files.

    IF mv_memory = abap_false AND mv_cluster = abap_false.
      RETURN.
    ENDIF.

    READ TABLE mt_files TRANSPORTING NO FIELDS
      WITH TABLE KEY item = is_item.
    IF sy-subrc <> 0.
      ls_files-item = is_item.
      ls_files-files = is_files.
      INSERT ls_files INTO TABLE mt_files.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
