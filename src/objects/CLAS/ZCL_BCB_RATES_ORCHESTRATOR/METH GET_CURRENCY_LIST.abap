  METHOD get_currency_list.
    " Lista de moedas configuradas para importação
    " Para adicionar novas moedas, basta incluir na lista
    r_result = VALUE #(
      ( currency = 'USD' )
      ( currency = 'EUR' )
    ).
  ENDMETHOD.