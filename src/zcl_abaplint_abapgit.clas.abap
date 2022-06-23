CLASS zcl_abaplint_abapgit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH KEY table_line.

    METHODS fetch_config
      IMPORTING
        !iv_devclass   TYPE devclass
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS list_online
      RETURNING
        VALUE(rt_result) TYPE ty_devclass_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abaplint_abapgit IMPLEMENTATION.


  METHOD fetch_config.

    TRY.
        DATA lt_repos TYPE zif_abapgit_repo_srv=>ty_repo_list.
        lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    DATA lo_repo LIKE LINE OF lt_repos.
    LOOP AT lt_repos INTO lo_repo.
      TRY.
          IF lo_repo->is_offline( ) = abap_true.
            CONTINUE.
          ENDIF.
        CATCH zcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

      IF lo_repo->get_package( ) = iv_devclass.
        DATA lt_files TYPE zif_abapgit_definitions=>ty_files_tt.
        DATA ls_file LIKE LINE OF lt_files.
        " Get most recent files
        lo_repo->refresh( ).
        lt_files = lo_repo->get_files_remote( ).
        READ TABLE lt_files WITH KEY file_path COMPONENTS path = '/' filename = 'abaplint.json' INTO ls_file.
        IF sy-subrc = 0.
          rv_json = zcl_abapgit_convert=>xstring_to_string_utf8( ls_file-data ).
        ENDIF.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD list_online.

    TRY.
        DATA lt_repos TYPE zif_abapgit_repo_srv=>ty_repo_list.
        lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    DATA lo_repo LIKE LINE OF lt_repos.
    LOOP AT lt_repos INTO lo_repo.
      TRY.
          IF lo_repo->is_offline( ) = abap_true.
            CONTINUE.
          ENDIF.
        CATCH zcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

      APPEND lo_repo->get_package( ) TO rt_result.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
