REPORT zabaplint_dependencies.

TABLES: sscrfields, tdevc.

DATA: ltb_devc TYPE tab_packages.

PARAMETERS: p_git  TYPE text200 OBLIGATORY.
SELECT-OPTIONS: s_devc FOR tdevc-devclass OBLIGATORY.

PARAMETERS: p_depth TYPE i DEFAULT 1,
            p_test  TYPE c AS CHECKBOX.

SELECTION-SCREEN: SKIP,
                  BEGIN OF BLOCK git WITH FRAME TITLE l_title.
PARAMETERS: p_cname  TYPE string DEFAULT 'upload' LOWER CASE,
            p_cemail TYPE string DEFAULT 'upload@localhost' LOWER CASE,
            p_ccomm  TYPE string DEFAULT 'Upload' LOWER CASE.
SELECTION-SCREEN: END OF BLOCK git.

INCLUDE zabapgit_password_dialog.
INCLUDE zabapgit_forms.

INITIALIZATION.
  lcl_password_dialog=>on_screen_init( ).
  l_title = 'GIT Commit'.

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
  DATA lx_error TYPE REF TO zcx_abaplint_error.
  DATA lx_error2 TYPE REF TO zcx_abapgit_exception.

  SELECT devclass FROM tdevc INTO TABLE ltb_devc WHERE devclass IN s_devc.
  TRY.
      CREATE OBJECT lo_deps
        EXPORTING
          iv_git_url     = |{ p_git }|
          iv_git_name    = p_cname
          iv_git_email   = p_cemail
          iv_git_comment = p_ccomm
          iv_packages    = ltb_devc.
      lo_deps->run( iv_test = p_test
                    iv_depth = p_depth
                    is_output = 'X' ).
    CATCH zcx_abaplint_error INTO lx_error.
      MESSAGE ID '00' TYPE 'E' NUMBER '001' WITH lx_error->message.
    CATCH zcx_abapgit_exception INTO lx_error2.
      MESSAGE lx_error2 TYPE 'E'.
  ENDTRY.

ENDFORM.
