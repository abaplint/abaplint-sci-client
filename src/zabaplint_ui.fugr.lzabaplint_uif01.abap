*----------------------------------------------------------------------*
***INCLUDE LZABAPLINT_UIF01.
*----------------------------------------------------------------------*

CLASS lcl_editor DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      save,
      is_dirty RETURNING VALUE(rv_dirty) TYPE abap_bool,
      get_devclass RETURNING VALUE(rv_devclass) TYPE devclass,
      update IMPORTING iv_json TYPE string,
      switch IMPORTING iv_devclass TYPE devclass.

  PRIVATE SECTION.
    CLASS-DATA:
      mv_devclass TYPE devclass.

ENDCLASS.

CLASS lcl_editor IMPLEMENTATION.

  METHOD get_devclass.
    rv_devclass = mv_devclass.
  ENDMETHOD.

  METHOD is_dirty.

    DATA: lv_status TYPE i.


    IF mv_devclass IS INITIAL.
      rv_dirty = abap_false.
      RETURN.
    ENDIF.

    go_editor->get_textmodified_status( IMPORTING status = lv_status ).
    cl_gui_cfw=>flush( ).

    IF lv_status = 0.
      rv_dirty = abap_false.
    ELSE.
      rv_dirty = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD update.

    go_editor->set_textstream( iv_json ).
    go_editor->set_textmodified_status( 1 ).
    cl_gui_cfw=>flush( ).

  ENDMETHOD.

  METHOD save.

    DATA: lv_string TYPE string.


    IF is_dirty( ) = abap_false.
      MESSAGE 'Nothing changed' TYPE 'S'.
      RETURN.
    ENDIF.

    go_editor->get_textstream( IMPORTING text = lv_string ).
    cl_gui_cfw=>flush( ).

    NEW zcl_abaplint_configuration( )->change_package(
      iv_devclass = mv_devclass
      iv_json     = lv_string ).
    MESSAGE 'Saved' TYPE 'S'.

    go_editor->set_textmodified_status( 0 ).

  ENDMETHOD.

  METHOD switch.

    DATA: lv_content TYPE string.


    IF is_dirty( ) = abap_true.
      MESSAGE 'Not saved' TYPE 'W'.
      RETURN.
    ENDIF.

    mv_devclass = iv_devclass.

    go_editor->set_enable( abap_true ).
    go_editor->set_readonly_mode( 0 ).

    lv_content = NEW zcl_abaplint_configuration( )->read_package( mv_devclass ).

    go_editor->set_textstream( lv_content ).
    go_editor->set_focus( go_editor ).

  ENDMETHOD.

ENDCLASS.


CLASS lcl_tree_content DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      init,
      get_by_key
        IMPORTING iv_key         TYPE tv_nodekey
        RETURNING VALUE(rv_devc) TYPE devclass.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_package,
             key  TYPE tv_nodekey,
             text TYPE string,
           END OF ty_package.

    CLASS-DATA: mt_packages TYPE STANDARD TABLE OF ty_package WITH DEFAULT KEY.

    CLASS-METHODS:
      build
        RETURNING VALUE(rt_nodes) TYPE ty_nodes,
      find_config.

ENDCLASS.

CLASS lcl_tree_content IMPLEMENTATION.

  METHOD get_by_key.

    DATA: ls_package LIKE LINE OF mt_packages.

    READ TABLE mt_packages INTO ls_package WITH KEY key = iv_key.
    ASSERT sy-subrc = 0.

    rv_devc = ls_package-text.

  ENDMETHOD.

  METHOD init.

    DATA: lt_events TYPE cntl_simple_events,
          lt_nodes  TYPE ty_nodes,
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

    lt_nodes = build( ).

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

    LOOP AT mt_packages INTO DATA(ls_smim).
      ls_node-node_key = ls_smim-key.
      ls_node-text     = ls_smim-text.
      APPEND ls_node TO rt_nodes.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_config.

    DATA: lv_index TYPE i.

    LOOP AT NEW zcl_abaplint_configuration( )->list_packages( ) INTO DATA(ls_package).
      lv_index = sy-tabix.
      APPEND VALUE #(
        key      = |KEY{ lv_index }|
        text     = ls_package-devclass ) TO mt_packages.
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

*&---------------------------------------------------------------------*
*& Form INIT_2000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_2000.

  IF NOT go_container IS BOUND.
    go_container = NEW #( 'CUSTOM_2000' ).

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

    CREATE OBJECT go_editor
      EXPORTING
        parent = go_splitter->bottom_right_container.
    go_editor->set_font_fixed( ).
    go_editor->set_enable( abap_false ).
    go_editor->set_readonly_mode( 1 ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ADD_RAW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM add_raw.

  DATA: lv_answer TYPE c LENGTH 1,
        lt_fields TYPE TABLE OF sval.


  lt_fields = VALUE #( (
    tabname   = 'TADIR'
    fieldname = 'DEVCLASS' ) ).

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

  NEW zcl_abaplint_configuration( )->add_package( CONV #( lt_fields[ 1 ]-value ) ).

* todo, refresh tree

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TEST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM test.

  DATA(lv_error) = NEW zcl_abaplint_backend( zabaplint_glob_data )->ping( ).

* todo, handle errors

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_CONFIG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_config.

  NEW zcl_abaplint_configuration( )->set_global( zabaplint_glob_data ).

  MESSAGE s000(zabaplint).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALL_3000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM call_3000.

  zabaplint_glob_data = NEW zcl_abaplint_configuration( )->get_global( ).

  CALL SCREEN 3000.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ADD_GIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM add_git.

  DATA(lt_list) = NEW zcl_abaplint_abapgit( )->list_online( ).

  BREAK-POINT.

* todo, remove already existing
* todo, add url in list
* todo, do like in ZIF_ABAPGIT_POPUPS~POPUP_TO_SELECT_FROM_LIST

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save.
  lcl_editor=>save( ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_GIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_git.

  DATA(lv_devclass) = lcl_editor=>get_devclass( ).
  IF lv_devclass IS INITIAL.
    RETURN.
  ENDIF.

  DATA(lv_json) = NEW zcl_abaplint_abapgit( )->fetch_config( lv_devclass ).
  IF NOT lv_json IS INITIAL.
    lcl_editor=>update( lv_json ).
  ELSE.
    MESSAGE e002(zabaplint).
  ENDIF.

ENDFORM.
