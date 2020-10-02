CLASS zcl_abaplint_configuration DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_packages TYPE STANDARD TABLE OF zabaplint_pack WITH KEY devclass .

    CONSTANTS:
      BEGIN OF c_default,
        ssl_id       TYPE c LENGTH 6 VALUE 'ANONYM',
        http_timeout TYPE i VALUE 6000,
      END OF c_default.

    METHODS read_package
      IMPORTING
        !iv_devclass   TYPE devclass
      RETURNING
        VALUE(rv_json) TYPE string .
    METHODS get_global
      RETURNING
        VALUE(rs_data) TYPE zabaplint_glob_data .
    METHODS set_global
      IMPORTING
        !is_data TYPE zabaplint_glob_data .
    METHODS list_packages
      RETURNING
        VALUE(rt_data) TYPE ty_packages .
    METHODS add_package
      IMPORTING
        !iv_devclass TYPE devclass .
    METHODS remove_package
      IMPORTING
        !iv_devclass TYPE devclass .
    METHODS change_package
      IMPORTING
        !iv_devclass TYPE zabaplint_pack-devclass
        !iv_json     TYPE zabaplint_pack-json .
    CLASS-METHODS find_from_package
      IMPORTING
        !iv_devclass     TYPE devclass
      RETURNING
        VALUE(rv_config) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS find_from_object
      IMPORTING
        !iv_object_type  TYPE trobjtype
        !iv_object_name  TYPE sobj_name
      RETURNING
        VALUE(rv_config) TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abaplint_configuration IMPLEMENTATION.


  METHOD add_package.

    DATA ls_data TYPE zabaplint_pack.

    ASSERT zcl_abapgit_factory=>get_sap_package( iv_devclass )->exists( ) = abap_true.

    ls_data-devclass = iv_devclass.

    INSERT zabaplint_pack FROM ls_data.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD change_package.

    UPDATE zabaplint_pack
      SET json = iv_json
      WHERE devclass = iv_devclass.
    ASSERT sy-dbcnt = 1.

  ENDMETHOD.


  METHOD find_from_object.

    DATA lv_devclass TYPE tadir-devclass.

    SELECT SINGLE devclass FROM tadir
      INTO lv_devclass
      WHERE pgmid = 'R3TR'
      AND object = iv_object_type
      AND obj_name = iv_object_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_config = find_from_package( lv_devclass ).

  ENDMETHOD.


  METHOD find_from_package.

    DATA lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt.
    DATA lt_config TYPE ty_packages.
    DATA lo_abaplint_conf TYPE REF TO zcl_abaplint_configuration.

    lt_packages = zcl_abapgit_factory=>get_sap_package( iv_devclass )->list_superpackages( ).
* todo, cache this in static variable
    CREATE OBJECT lo_abaplint_conf.
    lt_config = lo_abaplint_conf->list_packages( ).

    DATA lv_package LIKE LINE OF lt_packages.
    DATA ls_config LIKE LINE OF lt_config.
    LOOP AT lt_packages INTO lv_package.
      READ TABLE lt_config WITH KEY devclass = lv_package INTO ls_config.
      IF sy-subrc = 0.
        rv_config = ls_config-json.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_global.

    SELECT SINGLE * FROM zabaplint_glob
      INTO CORRESPONDING FIELDS OF rs_data
      WHERE sysid = sy-sysid.

    " set defaults
    IF rs_data-http_timeout IS INITIAL.
      rs_data-http_timeout = c_default-http_timeout.
    ENDIF.
    IF rs_data-ssl_id IS INITIAL.
      rs_data-ssl_id = c_default-ssl_id.
    ENDIF.

  ENDMETHOD.


  METHOD list_packages.

    SELECT * FROM zabaplint_pack
      INTO TABLE rt_data
      ORDER BY PRIMARY KEY.                             "#EC CI_NOWHERE

  ENDMETHOD.


  METHOD read_package.

    SELECT SINGLE json
      FROM zabaplint_pack
      INTO rv_json
      WHERE devclass = iv_devclass.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD remove_package.

    DELETE FROM zabaplint_pack WHERE devclass = iv_devclass.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD set_global.

    DATA: ls_data TYPE zabaplint_glob.

    MOVE-CORRESPONDING is_data TO ls_data.
    ls_data-sysid = sy-sysid.

    MODIFY zabaplint_glob FROM ls_data.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
