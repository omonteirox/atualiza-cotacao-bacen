  METHOD has_rates.
    IF i_response-value IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.