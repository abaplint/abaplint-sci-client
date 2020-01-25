CLASS zcl_abaplint_abapgit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH EMPTY KEY .

    METHODS fetch_config
      IMPORTING
        !iv_devclass   TYPE devclass
      RETURNING
        VALUE(rv_json) TYPE string .
    METHODS list_online
      RETURNING
        VALUE(rt_result) TYPE ty_devclass_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPLINT_ABAPGIT IMPLEMENTATION.


  METHOD fetch_config.

    TRY.
        DATA(lt_repos) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_repos INTO DATA(lo_repo).
      TRY.
          IF lo_repo->is_offline( ) = abap_true.
            CONTINUE.
          ENDIF.
        CATCH zcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

      IF lo_repo->get_package( ) = iv_devclass.
        DATA(lt_files) = lo_repo->get_files_remote( ).
        READ TABLE lt_files WITH KEY path = '/' filename = 'abaplint.json' INTO DATA(ls_file).
        IF sy-subrc = 0.
          rv_json = zcl_abapgit_convert=>xstring_to_string_utf8( ls_file-data ).
        ENDIF.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD list_online.

    TRY.
        DATA(lt_repos) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_repos INTO DATA(lo_repo).
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
