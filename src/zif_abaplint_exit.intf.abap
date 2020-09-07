interface ZIF_ABAPLINT_EXIT
  public .


  methods HANDLE_SPECIAL_ABAPS
    importing
      !IV_PROGRAM_NAME type PROGRAMM
    changing
      !CS_FILES_ITEM type ZCL_ABAPGIT_OBJECTS=>TY_SERIALIZATION .
endinterface.
