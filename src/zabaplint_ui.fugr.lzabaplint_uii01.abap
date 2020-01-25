*----------------------------------------------------------------------*
***INCLUDE LZABAPLINT_UII01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  CASE gv_ok_code.
    WHEN 'SAVE'.
      PERFORM save.
    WHEN 'ADD_RAW'.
      PERFORM add_raw.
    WHEN 'ADD_GIT'.
      PERFORM add_git.
    WHEN 'CONFIG'.
      PERFORM call_3000.
    WHEN 'UPDATE_GIT'.
      PERFORM update_git.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_3000 INPUT.

  CASE gv_ok_code.
    WHEN 'TEST'.
      PERFORM test.
    WHEN 'SAVE'.
      PERFORM save_config.
    WHEN 'BACK'.
* todo, dirty data check
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
