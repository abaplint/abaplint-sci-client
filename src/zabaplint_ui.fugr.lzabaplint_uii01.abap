*----------------------------------------------------------------------*
***INCLUDE LZABAPLINT_UII01.
*----------------------------------------------------------------------*

MODULE user_command_2000 INPUT.

  CASE gv_ok_code.
    WHEN 'SAVE'.
      PERFORM save.
      CLEAR gv_ok_code.
    WHEN 'ADD_RAW'.
      PERFORM add_raw.
      CLEAR gv_ok_code.
    WHEN 'DELETE'.
      PERFORM delete.
      CLEAR gv_ok_code.
    WHEN 'ADD_GIT'.
      PERFORM add_git.
      CLEAR gv_ok_code.
    WHEN 'CONFIG'.
      PERFORM call_3000.
      CLEAR gv_ok_code.
    WHEN 'UPDATE_GIT'.
      PERFORM update_git.
      CLEAR gv_ok_code.
    WHEN 'UPDATE_DEF'.
      PERFORM update_with_default_conf.
      CLEAR gv_ok_code.
    WHEN 'DIFF_GIT'.
      PERFORM show_diff USING c_compare_type-git.
      CLEAR gv_ok_code.
    WHEN 'DIFF_DEF'.
      PERFORM show_diff USING c_compare_type-default.
      CLEAR gv_ok_code.
    WHEN 'BACK'.
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_3000 INPUT.

  CASE gv_ok_code.
    WHEN c_drop.
      zcl_abaplint_backend=>display_value_help( EXPORTING i_dropdown = dest_dropdown
                                                CHANGING c_glob_data = zabaplint_glob_data ).
    WHEN 'TEST'.
      PERFORM test.
      CLEAR gv_ok_code.
    WHEN 'PICK'.
      PERFORM pick_3000.
      CLEAR gv_ok_code.
    WHEN 'SAVE'.
      PERFORM save_config.
      CLEAR gv_ok_code.
    WHEN 'BACK'.
      " todo, dirty data check
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_exit_command_3000 INPUT.

  CASE gv_ok_code.
    WHEN 'EXIT' OR 'CANC'.
      " todo, dirty data check
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
