REPORT zabaplint_dependencies.

/usi/cl_auth=>check_tcode( ).

TABLES: sscrfields, tdevc, tadir.

TYPES ty_names TYPE RANGE OF tadir-obj_name.
DATA ltb_devc TYPE tab_packages.

PARAMETERS p_git TYPE text200 OBLIGATORY.
SELECT-OPTIONS: s_devc FOR tdevc-devclass OBLIGATORY.

PARAMETERS: p_depth TYPE i DEFAULT 10,
            p_test  TYPE c AS CHECKBOX.

* Objects to be added to dependency list
SELECTION-SCREEN: SKIP,
                  BEGIN OF BLOCK add WITH FRAME TITLE l_tx_add,
                  SKIP,
                  COMMENT 1(30) l_tx_com,
                  BEGIN OF LINE.
PARAMETERS: p_obj1 TYPE trobjtype.
SELECT-OPTIONS: s_name1 FOR tadir-obj_name NO INTERVALS.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE.
PARAMETERS: p_obj2 TYPE trobjtype.
SELECT-OPTIONS: s_name2 FOR tadir-obj_name NO INTERVALS.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE.
PARAMETERS: p_obj3 TYPE trobjtype.
SELECT-OPTIONS: s_name3 FOR tadir-obj_name NO INTERVALS.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE.
PARAMETERS: p_obj4 TYPE trobjtype.
SELECT-OPTIONS: s_name4 FOR tadir-obj_name NO INTERVALS.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE.
PARAMETERS: p_obj5 TYPE trobjtype.
SELECT-OPTIONS: s_name5 FOR tadir-obj_name NO INTERVALS.
SELECTION-SCREEN: END OF LINE,
                  END OF BLOCK add.

SELECTION-SCREEN: SKIP,
                  BEGIN OF BLOCK git WITH FRAME TITLE l_tx_git.
PARAMETERS: p_cname  TYPE string DEFAULT 'upload' LOWER CASE,
            p_cemail TYPE string DEFAULT 'upload@localhost' LOWER CASE,
            p_ccomm  TYPE string DEFAULT 'Upload' LOWER CASE.
SELECTION-SCREEN: END OF BLOCK git.

INCLUDE zabapgit_password_dialog.
INCLUDE zabapgit_forms.

INITIALIZATION.
  lcl_password_dialog=>on_screen_init( ).
  l_tx_git = 'GIT Commit'.
  l_tx_add = 'Objects to be added to dependency list'.
  l_tx_com = 'Type         Name'.

AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_output( ).
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.

START-OF-SELECTION.
  PERFORM deps.

FORM deps.

  DATA lo_deps TYPE REF TO zcl_abaplint_deps_git.
  DATA lx_error TYPE REF TO zcx_abapgit_exception.
  DATA lx_error2 TYPE REF TO zcx_abaplint_error.
  DATA lt_additional TYPE zif_abapgit_definitions=>ty_tadir_tt.

  PERFORM add_extra USING lt_additional s_name1[] p_obj1.
  PERFORM add_extra USING lt_additional s_name2[] p_obj2.
  PERFORM add_extra USING lt_additional s_name3[] p_obj3.
  PERFORM add_extra USING lt_additional s_name4[] p_obj4.
  PERFORM add_extra USING lt_additional s_name5[] p_obj5.

  SELECT devclass FROM tdevc INTO TABLE ltb_devc WHERE devclass IN s_devc.
  TRY.
      CREATE OBJECT lo_deps
        EXPORTING
          iv_git_url     = |{ p_git }|
          iv_git_name    = p_cname
          iv_git_email   = p_cemail
          iv_git_comment = p_ccomm
          it_packages    = ltb_devc.
      lo_deps->run( iv_test       = p_test
                    iv_depth      = p_depth
                    it_additional = lt_additional ).
    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
    CATCH zcx_abaplint_error INTO lx_error2.
      MESSAGE ID '00' TYPE 'E' NUMBER '001' WITH lx_error2->message.
  ENDTRY.

ENDFORM.

FORM add_extra USING it_adds TYPE zif_abapgit_definitions=>ty_tadir_tt it_names TYPE ty_names i_obj TYPE trobjtype.

  DATA: ls_adds     TYPE zif_abapgit_definitions=>ty_tadir,
        ls_name     LIKE LINE OF it_names,
        lv_obj_name TYPE sobj_name.

  LOOP AT it_names INTO ls_name.
    CLEAR ls_adds.
    ls_adds-object = i_obj.
    ls_adds-obj_name = ls_name-low.
    lv_obj_name = ls_name-low.
    ls_adds-devclass = zcl_abaplint_deps_find=>determine_package(
      iv_object_type = i_obj
      iv_object_name = lv_obj_name ).
    APPEND ls_adds TO it_adds.
  ENDLOOP.
ENDFORM.
