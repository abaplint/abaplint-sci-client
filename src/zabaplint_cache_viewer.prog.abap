REPORT zabaplint_cache_viewer.

/USI/CL_AUTH=>CHECK_TCODE( ).

TABLES: sscrfields, tadir, zabaplint_cache1.

SELECT-OPTIONS:
  s_pack FOR tadir-devclass,
  s_type FOR tadir-object,
  s_name FOR tadir-obj_name.
PARAMETERS:
  p_file AS CHECKBOX.

SELECTION-SCREEN FUNCTION KEY 1.

DATA:
  gs_item   TYPE zif_abapgit_definitions=>ty_item,
  gt_item   TYPE zif_abapgit_definitions=>ty_items_tt,
  gt_cache1 TYPE STANDARD TABLE OF zabaplint_cache1,
  gs_cache1 LIKE LINE OF gt_cache1,
  gt_cache2 TYPE STANDARD TABLE OF zabaplint_cache2,
  gs_cache2 LIKE LINE OF gt_cache2,
  BEGIN OF gs_file,
    path     TYPE string,
    filename TYPE string,
    len      TYPE i,
    content  TYPE xstring,
  END OF gs_file,
  gv_file TYPE string,
  gt_file TYPE TABLE OF string.

INITIALIZATION.
  sscrfields-functxt_01 = icon_delete && 'Clear Cache'.

AT SELECTION-SCREEN.

  DATA:
    lv_answer TYPE c LENGTH 1,
    lo_cache  TYPE REF TO zcl_abaplint_deps_cache.

  IF sy-ucomm = 'FC01'.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = sy-title
        text_question         = 'Are you sure you want to clear the cache?'
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        default_button        = '2'
        display_cancel_button = 'X'
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc = 0 AND lv_answer = '1'.
      CREATE OBJECT lo_cache.
      lo_cache->clear( ).
      COMMIT WORK.
    ENDIF.
  ENDIF.

START-OF-SELECTION.

  SELECT DISTINCT t~object t~obj_name INTO TABLE gt_item
      FROM tadir AS t JOIN zabaplint_cache1 AS z
      ON t~object = z~obj_type AND t~obj_name = z~obj_name
      WHERE t~pgmid = 'R3TR' AND object IN s_type AND t~obj_name IN s_name AND t~devclass IN s_pack
      ORDER BY t~object t~obj_name.

  SELECT DISTINCT t~object t~obj_name APPENDING TABLE gt_item
      FROM tadir AS t JOIN zabaplint_cache2 AS z
      ON t~object = z~obj_type AND t~obj_name = z~obj_name
      WHERE t~pgmid = 'R3TR' AND object IN s_type AND t~obj_name IN s_name AND t~devclass IN s_pack
      ORDER BY t~object t~obj_name.

  SORT gt_item.
  DELETE ADJACENT DUPLICATES FROM gt_item.

  LOOP AT gt_item INTO gs_item.

    FORMAT COLOR COL_HEADING.
    WRITE: / gs_item-obj_type, gs_item-obj_name.
    SKIP.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Dependencies (Full):'.
    SKIP.
    FORMAT COLOR COL_NORMAL.
    SELECT * FROM zabaplint_cache1 INTO TABLE gt_cache1
        WHERE obj_type = gs_item-obj_type AND obj_name = gs_item-obj_name
        AND minimal = abap_false
        ORDER BY PRIMARY KEY.

    LOOP AT gt_cache1 INTO gs_cache1.
      WRITE: / gs_cache1-ref_obj_type, gs_cache1-ref_obj_name.
    ENDLOOP.
    SKIP.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Dependencies (Minimal):'.
    SKIP.
    FORMAT COLOR COL_NORMAL.
    SELECT * FROM zabaplint_cache1 INTO TABLE gt_cache1
        WHERE obj_type = gs_item-obj_type AND obj_name = gs_item-obj_name
        AND minimal = abap_true
        ORDER BY PRIMARY KEY.

    LOOP AT gt_cache1 INTO gs_cache1.
      WRITE: / gs_cache1-ref_obj_type, gs_cache1-ref_obj_name.
    ENDLOOP.
    SKIP.

    FORMAT COLOR COL_KEY.
    WRITE: / 'Serialized Files:'.
    SKIP.
    FORMAT COLOR COL_NORMAL.
    SELECT * FROM zabaplint_cache2 INTO TABLE gt_cache2
        WHERE obj_type = gs_item-obj_type AND obj_name = gs_item-obj_name
        ORDER BY PRIMARY KEY.

    LOOP AT gt_cache2 INTO gs_cache2.
      WRITE: / gs_cache2-path, AT 40 gs_cache2-filename, AT 120 gs_cache2-len, 'Bytes'.
      IF p_file = abap_true.
        FORMAT INTENSIFIED OFF.
        SKIP.
        gv_file = zcl_abapgit_convert=>xstring_to_string_utf8( gs_cache2-content ).
        SPLIT gv_file AT cl_abap_char_utilities=>newline INTO TABLE gt_file.
        LOOP AT gt_file INTO gv_file.
          WRITE: / gv_file.
        ENDLOOP.
        FORMAT INTENSIFIED ON.
      ENDIF.
    ENDLOOP.
    ULINE.

  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE 'No cache entries found' TYPE 'S'.
  ENDIF.
