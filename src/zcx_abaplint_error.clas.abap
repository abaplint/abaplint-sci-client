class ZCX_ABAPLINT_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_ABAPLINT_ERROR,
      msgid type symsgid value '00',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'A1',
      attr2 type scx_attrname value 'A2',
      attr3 type scx_attrname value 'A3',
      attr4 type scx_attrname value 'A4',
    end of ZCX_ABAPLINT_ERROR .
  data MESSAGE type STRING read-only .
  data A1 type SYMSGV read-only .
  data A2 type SYMSGV read-only .
  data A3 type SYMSGV read-only .
  data A4 type SYMSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MESSAGE type STRING optional
      !A1 type SYMSGV optional
      !A2 type SYMSGV optional
      !A3 type SYMSGV optional
      !A4 type SYMSGV optional .
  class-methods RAISE
    importing
      !IV_MSG type STRING
    raising
      ZCX_ABAPLINT_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ABAPLINT_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MESSAGE = MESSAGE .
me->A1 = A1 .
me->A2 = A2 .
me->A3 = A3 .
me->A4 = A4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_ABAPLINT_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


METHOD raise.

  DATA:
    BEGIN OF ls_msg,
      a1 LIKE a1,
      a2 LIKE a1,
      a3 LIKE a1,
      a4 LIKE a1,
    END OF ls_msg.

  ls_msg = iv_msg.

  RAISE EXCEPTION type zcx_abaplint_error
    EXPORTING
      message = iv_msg
      a1      = ls_msg-a1
      a2      = ls_msg-a2
      a3      = ls_msg-a3
      a4      = ls_msg-a4.

ENDMETHOD.
ENDCLASS.
