  METHOD format_date_for_bcb.
    " Formato esperado pela API BCB: MM-DD-YYYY
    r_result = |{ i_date+4(2) }-{ i_date+6(2) }-{ i_date(4) }|.
  ENDMETHOD.