*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZABAPLINT_CACHE1................................*
DATA:  BEGIN OF STATUS_ZABAPLINT_CACHE1              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABAPLINT_CACHE1              .
CONTROLS: TCTRL_ZABAPLINT_CACHE1
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABAPLINT_CACHE1              .
TABLES: ZABAPLINT_CACHE1               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
