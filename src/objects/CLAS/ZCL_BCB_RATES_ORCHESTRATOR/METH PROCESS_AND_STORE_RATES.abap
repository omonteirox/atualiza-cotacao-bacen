  METHOD process_and_store_rates.
    DATA ls_exchange_rate TYPE cl_exchange_rates=>ty_exchange_rate.

    " ========================================
    " COTAÇÃO DIRETA: USD/EUR → BRL
    " Exemplo: 1 USD = 5.17 BRL (cotação de compra)
    " ========================================
    CLEAR ls_exchange_rate.
    ls_exchange_rate-rate_type   = gc_rate_type.
    ls_exchange_rate-from_curr   = i_currency.         " USD ou EUR
    ls_exchange_rate-to_currncy  = gc_base_currency.   " BRL
    ls_exchange_rate-valid_from  = i_date.
    ls_exchange_rate-from_factor = gc_factor.
    ls_exchange_rate-to_factor   = gc_factor.
    ls_exchange_rate-exch_rate   = i_cotacao-cotacaocompra.
    APPEND ls_exchange_rate TO mt_exchange_rates.

    " ========================================
    " COTAÇÃO INDIRETA: BRL → USD/EUR
    " Cotação inversa para conversões BRL → moeda estrangeira
    " ========================================
    CLEAR ls_exchange_rate.
    ls_exchange_rate-rate_type     = gc_rate_type.
    ls_exchange_rate-from_curr     = gc_base_currency.  " BRL
    ls_exchange_rate-to_currncy    = i_currency.        " USD ou EUR
    ls_exchange_rate-valid_from    = i_date.
    ls_exchange_rate-from_factor_v = gc_factor.
    ls_exchange_rate-to_factor_v   = gc_factor.
    ls_exchange_rate-exch_rate_v   = i_cotacao-cotacaocompra.
    APPEND ls_exchange_rate TO mt_exchange_rates.

    " Log de sucesso com detalhes do boletim utilizado
    log_message( i_type       = 'S'
                 i_message    = 'Taxa direta registrada'
                 i_message_v1 = i_currency
                 i_message_v2 = CONV #( gc_base_currency )
                 i_message_v3 = CONV #( i_cotacao-cotacaocompra )
                 i_message_v4 = i_cotacao-tipoboletim ).

    log_message( i_type       = 'S'
                 i_message    = 'Taxa indireta registrada'
                 i_message_v1 = CONV #( gc_base_currency )
                 i_message_v2 = i_currency
                 i_message_v3 = CONV #( i_cotacao-cotacaocompra )
                 i_message_v4 = i_cotacao-tipoboletim ).
  ENDMETHOD.