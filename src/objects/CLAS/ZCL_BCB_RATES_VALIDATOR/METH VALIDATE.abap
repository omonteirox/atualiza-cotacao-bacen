  METHOD validate.
    " Cotação é válida se compra E venda são positivas
    IF i_cotacao-cotacaocompra > 0 AND i_cotacao-cotacaovenda > 0.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.