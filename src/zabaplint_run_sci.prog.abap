REPORT zabaplint_run_sci NO STANDARD PAGE HEADING.

/usi/cl_auth=>check_tcode( ).
* This report depends on the cross reference generated automatically in SAP
*
* In case it you think there is an issue, run these reports in background:
* - SAPRSEUB for SAP objects
* - SAPRSEUC for Customer objects (Y*/Z*)
*
* Also to ensure that the repository is kept clean, run the abap SAPRSEUJ
* it schedules the jobs required
*
* OSS # 2752795 - Environment analysis progams and classes - use of BAdI definitions
* OSS # 2243139 - REPOSITORY_ENVIRONMENT_ALL - too few hits for enhancement implementations
*
* Note: GPA $RC is used to return status to caller

TABLES: tdevc, tadir.

PARAMETERS: p_chkv TYPE sci_chkv OBLIGATORY.
SELECTION-SCREEN: SKIP.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_obje RADIOBUTTON GROUP g2.
PARAMETERS: p_type TYPE tadir-object,
            p_name TYPE tadir-obj_name.

PARAMETERS: p_devc RADIOBUTTON GROUP g2.
SELECT-OPTIONS: s_devc FOR tdevc-devclass.

PARAMETERS: p_korr RADIOBUTTON GROUP g2.
SELECT-OPTIONS: s_korr FOR tadir-korrnum.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING cx_static_check.

  DATA: li_code_inspector TYPE REF TO zif_abaplint_code_inspector.
  DATA: lt_result     TYPE scit_alvlist.

  DATA: lt_items_sel TYPE zif_abapgit_definitions=>ty_items_tt,
        ls_item      TYPE zif_abapgit_definitions=>ty_item,
        lv_succ      TYPE bool,
        lv_error     TYPE string.

  DATA: lt_packages TYPE tr_devclasses,
        lt_korr     TYPE STANDARD TABLE OF trkorr_old,
        lt_e071     TYPE TABLE OF e071,
        ls_tadir    TYPE tadir.

  FIELD-SYMBOLS: <devc> LIKE tdevc-devclass,
                 <korr> TYPE trkorr_old,
                 <line> TYPE LINE OF scit_alvlist.

  TABLES: e071.

* Validate Check Variant
  zcl_abaplint_code_inspector=>validate_check_variant( p_chkv ).

  CASE abap_true.
    WHEN p_obje.
      CLEAR ls_item.
      ls_item-obj_type = p_type.
      ls_item-obj_name = p_name.
      APPEND ls_item TO lt_items_sel.
    WHEN p_devc.
      SELECT devclass FROM tdevc INTO TABLE lt_packages WHERE devclass IN s_devc.
      LOOP AT lt_packages ASSIGNING <devc>.
        CLEAR ls_item.
        ls_item-obj_type = 'DEVC'.
        ls_item-obj_name = <devc>.
        APPEND ls_item TO lt_items_sel.
      ENDLOOP.
    WHEN p_korr.
      SELECT trkorr FROM e070 INTO TABLE lt_korr WHERE trkorr IN s_korr.
      IF sy-subrc <> 0.
        WRITE: /,/ 'No Transport Requests found'.
        RETURN.
      ENDIF.
      CLEAR ls_item.
      LOOP AT lt_korr ASSIGNING <korr>.
        SELECT * FROM e071 INTO TABLE lt_e071 WHERE trkorr = <korr>.
        LOOP AT lt_e071 INTO e071.
          "Convert to main objects
          CALL FUNCTION 'TR_CHECK_TYPE'
            EXPORTING
              wi_e071              = e071
              iv_translate_objname = 'X'
            IMPORTING
              we_tadir             = ls_tadir.
          IF ls_tadir-object IS NOT INITIAL AND ls_tadir-obj_name IS NOT INITIAL.
            CLEAR ls_item.
            ls_item-obj_type = ls_tadir-object.
            ls_item-obj_name = ls_tadir-obj_name.
            APPEND ls_item TO lt_items_sel.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
  ENDCASE.

* Process Code Inspector
  IF lt_items_sel IS NOT INITIAL.

    "remove duplicates
    SORT lt_items_sel BY obj_type obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_items_sel.

    li_code_inspector = zcl_abaplint_factory=>get_code_inspector( lt_items_sel ).

    lt_result = li_code_inspector->run(
      iv_variant = |{ p_chkv }|
      iv_save    = abap_true ).

    SUMMARY.
    ULINE.
    WRITE: / 'Errors found by Code Inspector'.
    ULINE.
    DETAIL.

    SET PARAMETER ID '$RC' FIELD '0'.
    lv_succ = li_code_inspector->is_successful( ).
    IF lv_succ = abap_true.
      WRITE: /,/ 'No Errors found'.
    ELSE.
      LOOP AT lt_result ASSIGNING <line>.
        CONCATENATE
          <line>-description '-'
          <line>-objname ' Line'
          <line>-line ':'
          <line>-text
          INTO lv_error SEPARATED BY space.
        WRITE: / lv_error.
      ENDLOOP.
      SUMMARY.
      SKIP.
      DATA lv_lines TYPE i.
      lv_lines = lines( lt_result ).
      WRITE: / 'Total errors found', lv_lines.
      SET PARAMETER ID '$RC' FIELD '4'.
    ENDIF.
    "Show objects
    SKIP.
    SUMMARY.
    ULINE.
    WRITE: / 'Objects selected for processing'.
    ULINE.
    DETAIL.
    LOOP AT lt_items_sel INTO ls_item.
      WRITE: / ls_item-obj_type, ls_item-obj_name.
    ENDLOOP.
  ELSE.
    SUMMARY.
    WRITE: /,/ 'No objects found to process'.
  ENDIF.
ENDFORM.
