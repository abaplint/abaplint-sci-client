*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZABAPLINT_GLOB..................................*
DATA:  BEGIN OF STATUS_ZABAPLINT_GLOB                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABAPLINT_GLOB                .
CONTROLS: TCTRL_ZABAPLINT_GLOB
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABAPLINT_GLOB                .
TABLES: ZABAPLINT_GLOB                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
