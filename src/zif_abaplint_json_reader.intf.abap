INTERFACE zif_abaplint_json_reader
  PUBLIC .

  TYPES:
    BEGIN OF ty_node,
      path TYPE string,
      name TYPE string,
      type TYPE string,
      value TYPE string,
      children TYPE i,
    END OF ty_node.

  TYPES ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH KEY path name.
  TYPES ty_nodes_ts TYPE SORTED TABLE OF ty_node WITH UNIQUE KEY path name.

  METHODS exists
    IMPORTING
      !iv_path TYPE string
    RETURNING
      VALUE(rv_exists) TYPE abap_bool .
  METHODS members
    IMPORTING
      !iv_path TYPE string
    RETURNING
      VALUE(rt_members) TYPE string_table .
  METHODS value
    IMPORTING
      !iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE string .
  METHODS value_boolean
    IMPORTING
      !iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE abap_bool .
  METHODS value_integer
    IMPORTING
      !iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE i .
  METHODS value_number
    IMPORTING
      !iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE f .
  METHODS value_string
    IMPORTING
      !iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE string .
  METHODS sub_section
    IMPORTING
      !iv_path TYPE string
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abaplint_json_reader .

ENDINTERFACE.
