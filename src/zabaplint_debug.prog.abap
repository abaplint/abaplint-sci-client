REPORT zabaplint_debug.

PARAMETERS: p_type TYPE tadir-object OBLIGATORY,
            p_name TYPE tadir-obj_name OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run.

ENDFORM.
