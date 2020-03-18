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

  TYPES tt_nodes TYPE STANDARD TABLE OF ty_node WITH KEY path name.
  TYPES ts_nodes TYPE SORTED TABLE OF ty_node WITH UNIQUE KEY path name.

  methods EXISTS
    importing
      !IV_PATH type STRING
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods MEMBERS
    importing
      !IV_PATH type STRING
    returning
      value(RT_MEMBERS) type STRING_TABLE .
  methods VALUE
    importing
      !IV_PATH type STRING
    returning
      value(RV_VALUE) type STRING .
  methods VALUE_BOOLEAN
    importing
      !IV_PATH type STRING
    returning
      value(RV_VALUE) type ABAP_BOOL .
  methods VALUE_INTEGER
    importing
      !IV_PATH type STRING
    returning
      value(RV_VALUE) type I .
  methods VALUE_NUMBER
    importing
      !IV_PATH type STRING
    returning
      value(RV_VALUE) type f .
  methods VALUE_STRING
    importing
      !IV_PATH type STRING
    returning
      value(RV_VALUE) type STRING .

ENDINTERFACE.
