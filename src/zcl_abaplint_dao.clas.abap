class zcl_abaplint_dao definition
  public
  create private .

  public section.

    types: rfcdes_tt type standard table of rfcdes with default key.

    methods: get_rfcs importing i_rfc_type type rfctype_d returning value(r_rfcs) type rfcdes_tt.

    class-methods get_instance returning value(r_instance) type ref to zcl_abaplint_dao.

    data rfc_type type rfctype_d value 'G'.

  protected section.

  private section.

    class-data instance type ref to zcl_abaplint_dao.

endclass.



class ZCL_ABAPLINT_DAO implementation.


  method get_instance.
    if instance is not bound.
      instance = new #(  ).
    endif.
    r_instance = instance.
  endmethod.


  method get_rfcs.

    select rfcdest
               from rfcdes
               into table @data(it_cols)
               where rfctype eq @i_rfc_type.
    r_rfcs = it_cols.
  endmethod.
endclass.
