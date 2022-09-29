CLASS zcl_abaplint_deps_git DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_git_url     TYPE string
        !it_packages    TYPE tab_packages
        !iv_git_name    TYPE string
        !iv_git_email   TYPE string
        !iv_git_comment TYPE string .
    METHODS run
      IMPORTING
        !iv_test       TYPE abap_bool DEFAULT abap_false
        !iv_depth      TYPE i
        !it_additional TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception
        zcx_abaplint_error .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_stage,
        comment TYPE zif_abapgit_definitions=>ty_comment,
        stage   TYPE REF TO zcl_abapgit_stage,
      END OF ty_stage .

    DATA mv_git_url TYPE string .
    DATA mt_packages TYPE tab_packages .
    DATA mv_git_name TYPE string .
    DATA mv_git_email TYPE string .
    DATA mv_git_comment TYPE string .

    METHODS build_stage
      IMPORTING
        !it_local       TYPE zif_abapgit_definitions=>ty_files_tt
        !it_remote      TYPE zif_abapgit_definitions=>ty_files_tt
      RETURNING
        VALUE(rs_stage) TYPE ty_stage
      RAISING
        zcx_abapgit_exception .
    METHODS get_local
      IMPORTING
        !it_additional  TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_local) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception
        zcx_abaplint_error .
  PRIVATE SECTION.

    DATA mv_depth TYPE i .
ENDCLASS.



CLASS zcl_abaplint_deps_git IMPLEMENTATION.


  METHOD build_stage.

    DATA ls_remote LIKE LINE OF it_remote.
    DATA ls_local LIKE LINE OF it_local.
    FIELD-SYMBOLS: <r> LIKE ls_remote.

    rs_stage-comment-committer-email = mv_git_email.
    rs_stage-comment-committer-name = mv_git_name.
    rs_stage-comment-comment = mv_git_comment.

    CREATE OBJECT rs_stage-stage.

    LOOP AT it_local INTO ls_local.
      READ TABLE it_remote WITH KEY file_path COMPONENTS
        path = ls_local-path
        filename = ls_local-filename
        ASSIGNING <r>.
      IF sy-subrc <> 0.
        WRITE: / 'Add', ls_local-path, ls_local-filename.
        rs_stage-stage->add(
          iv_path     = ls_local-path
          iv_filename = ls_local-filename
          iv_data     = ls_local-data ).
      ELSEIF <r>-sha1 <> ls_local-sha1.
        WRITE: / 'Changed', ls_local-path, ls_local-filename.
        rs_stage-stage->add(
          iv_path     = ls_local-path
          iv_filename = ls_local-filename
          iv_data     = ls_local-data ).
      ENDIF.
    ENDLOOP.

    LOOP AT it_remote INTO ls_remote.
      READ TABLE it_local WITH KEY file_path COMPONENTS
        path = ls_remote-path
        filename = ls_remote-filename TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        WRITE: / 'Remove', ls_remote-path, ls_remote-filename.
        rs_stage-stage->rm(
          iv_path     = ls_remote-path
          iv_filename = ls_remote-filename ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mv_git_url = iv_git_url.
    mt_packages = it_packages.
    mv_git_name = iv_git_name.
    mv_git_email = iv_git_email.
    mv_git_comment = iv_git_comment.

  ENDMETHOD.


  METHOD get_local.

    DATA lo_dep_find TYPE REF TO zcl_abaplint_deps_find.
    DATA lo_dep_ser TYPE REF TO zcl_abaplint_deps_serializer.
    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_tt.
    DATA ls_options TYPE zcl_abaplint_deps_find=>ty_options.

    FIELD-SYMBOLS <ls_local> LIKE LINE OF rt_local.


    ls_options-depth = mv_depth.

    CREATE OBJECT lo_dep_find EXPORTING is_options = ls_options.
    CREATE OBJECT lo_dep_ser EXPORTING is_options = ls_options.

    lt_tadir = lo_dep_find->find_by_packages( mt_packages ).
    APPEND LINES OF it_additional TO lt_tadir.
    lt_local = lo_dep_ser->serialize( lt_tadir ).
    APPEND LINES OF lt_local TO rt_local.

* recalculate SHA1, as file contents is changed after serialization
    LOOP AT rt_local ASSIGNING <ls_local>.
      <ls_local>-sha1 = zcl_abapgit_hash=>sha1(
        iv_type = zif_abapgit_definitions=>c_type-blob
        iv_data = <ls_local>-data ).
    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_tt.
    DATA ls_remote TYPE zcl_abapgit_git_porcelain=>ty_pull_result.
    DATA lv_branch_name TYPE string.
    DATA ls_stage TYPE ty_stage.

    mv_depth = iv_depth.
    lt_local = get_local( it_additional ).

    cl_progress_indicator=>progress_indicate(
      i_text               = |GIT, Pulling files from Repository|
      i_processed          = 1
      i_total              = 3
      i_output_immediately = abap_true ).

    lv_branch_name = zcl_abapgit_git_transport=>branches( mv_git_url )->get_head_symref( ).

    ls_remote = zcl_abapgit_git_porcelain=>pull_by_branch(
      iv_url         = mv_git_url
      iv_branch_name = lv_branch_name ).

* dont push to repositories containing abapgit code
* everything will be overwritten in the remote repo
    READ TABLE ls_remote-files WITH KEY file_path COMPONENTS path = '/' filename = '.abapgit.xml'
      TRANSPORTING NO FIELDS.
    ASSERT sy-subrc <> 0.

    DELETE ls_remote-files WHERE path NP '/src/*'.

    cl_progress_indicator=>progress_indicate(
      i_text               = |GIT, Staging files|
      i_processed          = 2
      i_total              = 3
      i_output_immediately = abap_true ).

    ls_stage = build_stage(
      it_local  = lt_local
      it_remote = ls_remote-files ).

    IF ls_stage-stage->count( ) > 0 AND iv_test = abap_false.
      cl_progress_indicator=>progress_indicate(
        i_text               = |GIT, Pushing files to Repository|
        i_processed          = 3
        i_total              = 3
        i_output_immediately = abap_true ).

      zcl_abapgit_git_porcelain=>push(
        is_comment     = ls_stage-comment
        io_stage       = ls_stage-stage
        it_old_objects = ls_remote-objects
        iv_parent      = ls_remote-commit
        iv_url         = mv_git_url
        iv_branch_name = lv_branch_name ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
