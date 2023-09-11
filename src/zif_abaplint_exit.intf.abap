INTERFACE zif_abaplint_exit
  PUBLIC.


  METHODS handle_special_abaps
    IMPORTING
      !iv_program_name TYPE programm
    CHANGING
      !cs_files_item   TYPE zif_abapgit_objects=>ty_serialization .
ENDINTERFACE.
