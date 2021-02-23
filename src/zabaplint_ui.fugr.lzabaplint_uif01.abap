*----------------------------------------------------------------------*
***INCLUDE LZABAPLINT_UIF01.
*
*----------------------------------------------------------------------*

CLASS lcl_editor DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      save,
      get RETURNING VALUE(rv_json) TYPE string,
      is_dirty RETURNING VALUE(rv_dirty) TYPE abap_bool,
      get_devclass RETURNING VALUE(rv_devclass) TYPE devclass,
      update IMPORTING iv_json TYPE string,
      switch IMPORTING iv_devclass TYPE devclass.

  PRIVATE SECTION.
    CLASS-DATA:
      gv_devclass TYPE devclass.

ENDCLASS.

CLASS lcl_editor IMPLEMENTATION.

  METHOD get_devclass.
    rv_devclass = gv_devclass.
  ENDMETHOD.

  METHOD is_dirty.

    DATA: lv_status TYPE i.


    IF gv_devclass IS INITIAL.
      rv_dirty = abap_false.
      RETURN.
    ENDIF.

    go_editor->get_textmodified_status( IMPORTING status = lv_status ).
    cl_gui_cfw=>flush( ).

    rv_dirty = boolc( lv_status <> 0 ).

  ENDMETHOD.

  METHOD get.

    go_editor->get_textstream( IMPORTING text = rv_json ).
    cl_gui_cfw=>flush( ).

  ENDMETHOD.

  METHOD update.

    go_editor->set_textstream( iv_json ).
    go_editor->set_textmodified_status( 1 ).
    cl_gui_cfw=>flush( ).

  ENDMETHOD.

  METHOD save.

    DATA: lv_string TYPE string.

    IF gv_read_only = abap_true.
      RETURN.
    ENDIF.

    IF is_dirty( ) = abap_false.
      MESSAGE 'Nothing changed' TYPE 'S'.
      RETURN.
    ENDIF.

    go_editor->get_textstream( IMPORTING text = lv_string ).
    cl_gui_cfw=>flush( ).

    DATA lo_config TYPE REF TO zcl_abaplint_configuration.
    CREATE OBJECT lo_config.
    lo_config->change_package(
      iv_devclass = gv_devclass
      iv_json     = lv_string ).
    MESSAGE s000(zabaplint).

    go_editor->set_textmodified_status( 0 ).

  ENDMETHOD.

  METHOD switch.

    DATA: lv_content TYPE string.


    IF is_dirty( ) = abap_true.
      MESSAGE 'Not saved' TYPE 'W'.
      RETURN.
    ENDIF.

    gv_devclass = iv_devclass.

    go_editor->set_enable( abap_true ).

    IF gv_read_only = abap_true.
      go_editor->set_readonly_mode( 1 ).
    ELSE.
      go_editor->set_readonly_mode( 0 ).
    ENDIF.

    DATA lo_config TYPE REF TO zcl_abaplint_configuration.
    CREATE OBJECT lo_config.
    lv_content = lo_config->read_package( gv_devclass ).

    go_editor->set_textstream( lv_content ).
    go_editor->set_focus( go_editor ).

  ENDMETHOD.

ENDCLASS.


CLASS lcl_tree_content DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      init,
      refresh,
      get_by_key
        IMPORTING iv_key         TYPE tv_nodekey
        RETURNING VALUE(rv_devc) TYPE devclass.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_package,
             key  TYPE tv_nodekey,
             text TYPE string,
           END OF ty_package.

    CLASS-DATA: gt_packages TYPE STANDARD TABLE OF ty_package WITH DEFAULT KEY.

    CLASS-METHODS:
      build
        RETURNING VALUE(rt_nodes) TYPE ty_nodes,
      find_config.

ENDCLASS.

CLASS lcl_tree_content IMPLEMENTATION.

  METHOD get_by_key.

    DATA: ls_package LIKE LINE OF gt_packages.

    READ TABLE gt_packages INTO ls_package WITH KEY key = iv_key.
    ASSERT sy-subrc = 0.

    rv_devc = ls_package-text.

  ENDMETHOD.

  METHOD init.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.


    ls_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    go_tree->set_registered_events(
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3 ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD refresh.

    DATA: lt_nodes TYPE ty_nodes.

    lt_nodes = build( ).

    go_tree->delete_all_nodes( ).

    go_tree->add_nodes(
      EXPORTING
        table_structure_name           = 'MTREESNODE'
        node_table                     = lt_nodes
      EXCEPTIONS
        failed                         = 1
        error_in_node_table            = 2
        dp_error                       = 3
        table_structure_name_not_found = 4
        OTHERS                         = 5 ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD build.

    DATA: ls_node LIKE LINE OF rt_nodes.

    find_config( ).

    DATA ls_smim LIKE LINE OF gt_packages.
    LOOP AT gt_packages INTO ls_smim.
      ls_node-node_key = ls_smim-key.
      ls_node-text     = ls_smim-text.
      APPEND ls_node TO rt_nodes.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_config.

    DATA: lv_index TYPE i.

    CLEAR gt_packages.

    DATA lo_config TYPE REF TO zcl_abaplint_configuration.
    DATA lt_packages TYPE zcl_abaplint_configuration=>ty_packages.
    DATA ls_package LIKE LINE OF lt_packages.
    FIELD-SYMBOLS <pkg> LIKE LINE OF gt_packages.

    CREATE OBJECT lo_config.
    lt_packages = lo_config->list_packages( ).

    LOOP AT lt_packages INTO ls_package.
      lv_index = sy-tabix.
      APPEND INITIAL LINE TO gt_packages ASSIGNING <pkg>.
      <pkg>-key  = |KEY{ lv_index }|.
      <pkg>-text = ls_package-devclass.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_handler DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.

  METHOD double_click.

    lcl_editor=>switch( lcl_tree_content=>get_by_key( node_key ) ).

  ENDMETHOD.

ENDCLASS.

FORM init_2000.

  IF NOT go_container IS BOUND.
    CREATE OBJECT go_container EXPORTING container_name = 'CUSTOM_2000'.

    CREATE OBJECT go_splitter
      EXPORTING
        parent      = go_container
        orientation = 1.
    go_splitter->set_sash_position( 20 ).

    CREATE OBJECT go_tree
      EXPORTING
        parent              = go_splitter->top_left_container
        node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single.

    SET HANDLER lcl_handler=>double_click FOR go_tree.
    lcl_tree_content=>init( ).
    lcl_tree_content=>refresh( ).

    CREATE OBJECT go_editor EXPORTING parent = go_splitter->bottom_right_container.
    go_editor->set_font_fixed( ).
    go_editor->set_enable( abap_false ).
    go_editor->set_readonly_mode( 1 ).
  ENDIF.

ENDFORM.

FORM init_3000.

  IF zabaplint_glob_data IS INITIAL.
    DATA lo_config TYPE REF TO zcl_abaplint_configuration.

    CREATE OBJECT lo_config.
    zabaplint_glob_data = lo_config->get_global( ).
  ENDIF.

  IF gv_read_only = abap_true.
    LOOP AT SCREEN.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.

FORM add_raw.

  DATA: lv_answer TYPE c LENGTH 1,
        lt_fields TYPE TABLE OF sval.

  FIELD-SYMBOLS <field> LIKE LINE OF lt_fields.
  APPEND INITIAL LINE TO lt_fields ASSIGNING <field>.
  <field>-tabname   = 'TADIR'.
  <field>-fieldname = 'DEVCLASS'.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title = 'Package'
    IMPORTING
      returncode  = lv_answer
    TABLES
      fields      = lt_fields
    EXCEPTIONS
      OTHERS      = 1 ##NO_TEXT.
  IF sy-subrc <> 0 OR lv_answer = 'A'.
    RETURN.
  ENDIF.

  DATA lo_config TYPE REF TO zcl_abaplint_configuration.
  DATA ls_field LIKE LINE OF lt_fields.

  CREATE OBJECT lo_config.
  READ TABLE lt_fields INTO ls_field INDEX 1.

  IF zcl_abapgit_factory=>get_sap_package( |{ ls_field-value }| )->exists( ) = abap_false.
    MESSAGE e001(zabaplint) WITH ls_field-value.
    RETURN.
  ENDIF.

  lo_config->add_package( |{ ls_field-value }| ).
  lcl_tree_content=>refresh( ).

  MESSAGE s000(zabaplint).

ENDFORM.

FORM test.

  DATA lx_cx TYPE REF TO zcx_abaplint_error.
  DATA lo_backend TYPE REF TO zcl_abaplint_backend.
  DATA ls_message TYPE zcl_abaplint_backend=>ty_message.
  CREATE OBJECT lo_backend EXPORTING is_config = zabaplint_glob_data.

  TRY.
      ls_message = lo_backend->ping( ).
      IF ls_message-error = abap_true.
        MESSAGE ls_message-message TYPE 'E'.
      ELSE.
        MESSAGE ls_message-message TYPE 'S'.
      ENDIF.
    CATCH zcx_abaplint_error INTO lx_cx.
      MESSAGE lx_cx->message TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.


ENDFORM.

FORM save_config.

  DATA lo_config TYPE REF TO zcl_abaplint_configuration.

  CREATE OBJECT lo_config.
  lo_config->set_global( zabaplint_glob_data ).

  MESSAGE s000(zabaplint).

ENDFORM.

FORM call_3000.

  DATA lo_config TYPE REF TO zcl_abaplint_configuration.

  CREATE OBJECT lo_config.
  lo_config->get_global( ).

  CALL SCREEN 3000.

ENDFORM.

FORM add_git.

  DATA lo_abaplint TYPE REF TO zcl_abaplint_abapgit.
  CREATE OBJECT lo_abaplint.

  DATA lt_list TYPE zcl_abaplint_abapgit=>ty_devclass_tt.
  lt_list = lo_abaplint->list_online( ).

  MESSAGE 'todo' TYPE 'S'.

* todo, remove already existing
* todo, add url in list
* todo, do like in ZIF_ABAPGIT_POPUPS~POPUP_TO_SELECT_FROM_LIST

  lcl_tree_content=>refresh( ).

ENDFORM.

FORM save.
  lcl_editor=>save( ).
ENDFORM.

FORM update_git.

  DATA lv_devclass TYPE devclass.
  lv_devclass = lcl_editor=>get_devclass( ).
  IF lv_devclass IS INITIAL.
    RETURN.
  ENDIF.

  DATA lx_error TYPE REF TO zcx_abapgit_exception.
  TRY.
      DATA lo_abaplint TYPE REF TO zcl_abaplint_abapgit.
      DATA lv_json TYPE string.
      CREATE OBJECT lo_abaplint.

      lv_json = lo_abaplint->fetch_config( lv_devclass ).
      IF NOT lv_json IS INITIAL.
        lcl_editor=>update( lv_json ).
      ELSE.
        MESSAGE e002(zabaplint).
      ENDIF.
    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

ENDFORM.

FORM update_with_default_conf.

  DATA lv_devclass TYPE devclass.
  lv_devclass = lcl_editor=>get_devclass( ).
  IF lv_devclass IS INITIAL.
    RETURN.
  ENDIF.

  DATA lo_backend TYPE REF TO zcl_abaplint_backend.
  DATA lv_json TYPE string.
  DATA lx_error TYPE REF TO zcx_abaplint_error.
  CREATE OBJECT lo_backend.

  TRY.
      lv_json = lo_backend->get_default_config( ).
      lcl_editor=>update( lv_json ).
    CATCH zcx_abaplint_error INTO lx_error.
      MESSAGE lx_error->message TYPE 'E'.
  ENDTRY.

ENDFORM.

FORM show_diff USING iv_type TYPE c.

  TYPES:
    BEGIN OF ty_alv,
      new_num TYPE c LENGTH 6,
      new     TYPE string,
      result  TYPE c LENGTH 1,
      old_num TYPE c LENGTH 6,
      old     TYPE string,
      scol    TYPE lvc_t_scol,
    END OF ty_alv.

  DATA:
    lv_devclass            TYPE devclass,
    lo_abaplint            TYPE REF TO zcl_abaplint_abapgit,
    lo_backend             TYPE REF TO zcl_abaplint_backend,
    lo_ajson_util          TYPE REF TO zcl_ajson_utilities,
    lv_json_comp           TYPE string,
    lv_json_curr           TYPE string,
    lo_diff                TYPE REF TO zcl_abapgit_diff,
    lt_diff                TYPE zif_abapgit_definitions=>ty_diffs_tt,
    lt_alv                 TYPE STANDARD TABLE OF ty_alv WITH DEFAULT KEY,
    lo_alv                 TYPE REF TO cl_salv_table,
    lo_display_settings    TYPE REF TO cl_salv_display_settings,
    lo_functions           TYPE REF TO cl_salv_functions,
    lo_functional_settings TYPE REF TO cl_salv_functional_settings,
    lo_selections          TYPE REF TO cl_salv_selections,
    lo_columns             TYPE REF TO cl_salv_columns_table,
    lo_column              TYPE REF TO cl_salv_column,
    lx_error               TYPE REF TO cx_static_check.

  FIELD-SYMBOLS:
    <ls_diff>  LIKE LINE OF lt_diff,
    <ls_alv>   LIKE LINE OF lt_alv,
    <ls_color> TYPE lvc_s_scol.

  lv_devclass = lcl_editor=>get_devclass( ).
  IF lv_devclass IS INITIAL.
    MESSAGE 'No package selected' TYPE 'W' DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  TRY.
      " Get current and comparison JSON
      lv_json_curr = lcl_editor=>get( ).

      CASE iv_type.
        WHEN c_compare_type-git.
          CREATE OBJECT lo_abaplint.
          lv_json_comp = lo_abaplint->fetch_config( lv_devclass ).
        WHEN c_compare_type-default.
          CREATE OBJECT lo_backend.
          lv_json_comp = lo_backend->get_default_config( ).
      ENDCASE.

      " Prepare for and get JSON diff
      CREATE OBJECT lo_ajson_util.

      lv_json_comp = lo_ajson_util->sort( iv_json = lv_json_comp ).
      lv_json_curr = lo_ajson_util->sort( iv_json = lv_json_curr ).

      CREATE OBJECT lo_diff
        EXPORTING
          iv_new = zcl_abapgit_convert=>string_to_xstring( lv_json_curr )
          iv_old = zcl_abapgit_convert=>string_to_xstring( lv_json_comp ).

      lt_diff = lo_diff->get( ).

      " Colorize diffs
      LOOP AT lt_diff ASSIGNING <ls_diff>.
        APPEND INITIAL LINE TO lt_alv ASSIGNING <ls_alv>.
        MOVE-CORRESPONDING <ls_diff> TO <ls_alv>.
        TRANSLATE <ls_alv>-result USING 'I+D-U~'.
        APPEND INITIAL LINE TO <ls_alv>-scol ASSIGNING <ls_color>.

        CASE <ls_alv>-result.
          WHEN '+'.
            <ls_color>-color-col = 5.
          WHEN '-'.
            <ls_color>-color-col = 6.
          WHEN '~'.
            <ls_color>-color-col = 3.
        ENDCASE.
      ENDLOOP.

      " Output as ALV
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = lt_alv ).

      lo_functions = lo_alv->get_functions( ).
      lo_functions->set_all( ).

      lo_functional_settings = lo_alv->get_functional_settings( ).

      lo_selections = lo_alv->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

      lo_columns = lo_alv->get_columns( ).
      lo_columns->set_optimize( ).
      lo_columns->set_color_column( 'SCOL' ).

      lo_column = lo_columns->get_column( 'RESULT' ).
      lo_column->set_medium_text( 'Diff' ).
      lo_column->set_output_length( 5 ).

      lo_column = lo_columns->get_column( 'NEW_NUM' ).
      lo_column->set_medium_text( 'Line' ).
      lo_column->set_output_length( 5 ).

      lo_column = lo_columns->get_column( 'NEW' ).
      lo_column->set_long_text( 'Local: abaplint.json' ).
      lo_column->set_output_length( 60 ).
      lo_column->set_leading_spaces( abap_true ).

      lo_column = lo_columns->get_column( 'OLD_NUM' ).
      lo_column->set_medium_text( 'Line' ).
      lo_column->set_output_length( 5 ).

      lo_column = lo_columns->get_column( 'OLD' ).
      lo_column->set_output_length( 60 ).
      lo_column->set_leading_spaces( abap_true ).

      CASE iv_type.
        WHEN c_compare_type-git.
          lo_column->set_long_text( 'abapGit: abaplint.json' ).
        WHEN c_compare_type-default.
          lo_column->set_long_text( 'Default: abaplint.json' ).
      ENDCASE.

      lo_display_settings = lo_alv->get_display_settings( ).
      lo_display_settings->set_list_header( sy-title ).
      lo_display_settings->set_fit_column_to_table_size( ).

      lo_alv->display( ).

    CATCH zcx_abapgit_exception zcx_abaplint_error zcx_ajson_error cx_salv_error INTO lx_error.
      MESSAGE lx_error TYPE 'E' DISPLAY LIKE 'S'.
  ENDTRY.

ENDFORM.

FORM status_2000.

  DATA: lt_exclude TYPE TABLE OF sy-ucomm.

  IF gv_read_only = abap_true.
    APPEND 'SAVE' TO lt_exclude.
    APPEND 'ADD_RAW' TO lt_exclude.
    APPEND 'ADD_GIT' TO lt_exclude.
    APPEND 'UPDATE_GIT' TO lt_exclude.
    APPEND 'UPDATE_DEF' TO lt_exclude.
    APPEND 'DELETE' TO lt_exclude.
  ENDIF.

  SET PF-STATUS 'STATUS_2000' EXCLUDING lt_exclude.
  SET TITLEBAR 'TITLE_2000'.

ENDFORM.

FORM status_3000.

  DATA: lt_exclude TYPE TABLE OF sy-ucomm.

  IF gv_read_only = abap_true.
    APPEND 'SAVE' TO lt_exclude.
  ENDIF.

  SET PF-STATUS 'STATUS_3000' EXCLUDING lt_exclude.
  SET TITLEBAR 'TITLE_3000'.

ENDFORM.

FORM delete.

  DATA: lv_answer TYPE c LENGTH 1,
        lt_fields TYPE TABLE OF sval.

  FIELD-SYMBOLS <field> LIKE LINE OF lt_fields.
  APPEND INITIAL LINE TO lt_fields ASSIGNING <field>.
  <field>-tabname   = 'TADIR'.
  <field>-fieldname = 'DEVCLASS'.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title = 'Package'
    IMPORTING
      returncode  = lv_answer
    TABLES
      fields      = lt_fields
    EXCEPTIONS
      OTHERS      = 1 ##NO_TEXT.
  IF sy-subrc <> 0 OR lv_answer = 'A'.
    RETURN.
  ENDIF.

  DATA lo_config TYPE REF TO zcl_abaplint_configuration.
  DATA ls_field LIKE LINE OF lt_fields.

  READ TABLE lt_fields INTO ls_field INDEX 1.
  CREATE OBJECT lo_config.
  lo_config->remove_package( |{ ls_field-value }| ).
  lcl_tree_content=>refresh( ).

  MESSAGE s000(zabaplint).

ENDFORM.

FORM pick_3000.

  DATA: lv_field TYPE string,
        lv_url   TYPE string.

  GET CURSOR FIELD lv_field.

  CASE lv_field.
    WHEN 'TEXT05'.
      lv_url = 'https://github.com/abaplint/abaplint-sci-client/blob/master/README.md'.
    WHEN 'TEXT02'.
      lv_url = 'http://sci.abaplint.org'.
  ENDCASE.

  IF NOT lv_url IS INITIAL.
    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = lv_url
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
  ENDIF.

ENDFORM.
