  METHOD execute_rates_update.
    DATA: lv_effective_date TYPE d.

    " Limpar dados anteriores
    CLEAR: mt_exchange_rates, mt_messages.

    log_message( i_type    = 'I'
                 i_message = 'Iniciando atualização de taxas de câmbio BCB PTAX' ).

    " Obter lista de moedas configuradas
    DATA(lt_currencies) = get_currency_list( ).

    " Processar cada moeda
    LOOP AT lt_currencies INTO DATA(ls_currency).
      log_message( i_type       = 'I'
                   i_message    = 'Buscando cotação'
                   i_message_v1 = ls_currency-currency ).

      " Buscar cotação com fallback para dias anteriores
      DATA ls_cotacao TYPE zif_bcb_ptax_api_client=>ty_bcb_cotacao.
      get_rate_with_fallback(
        EXPORTING
          i_currency       = ls_currency-currency
          i_date           = sy-datum
        IMPORTING
          e_result         = ls_cotacao
          e_effective_date = lv_effective_date ).

      IF ls_cotacao IS NOT INITIAL.
        process_and_store_rates(
          i_currency = ls_currency-currency
          i_cotacao  = ls_cotacao
          i_date     = lv_effective_date ).
      ELSE.
        log_message( i_type       = 'E'
                     i_message    = 'Nenhuma cotação encontrada'
                     i_message_v1 = ls_currency-currency ).
      ENDIF.
    ENDLOOP.

    " Gravar as taxas no sistema (is_update_allowed = abap_true permite re-execução)
    IF mt_exchange_rates IS NOT INITIAL.
      DATA(l_result) = cl_exchange_rates=>put(
        EXPORTING
          exchange_rates    = mt_exchange_rates
          is_update_allowed = abap_true ).
      APPEND LINES OF l_result TO mt_messages.

      " Verificar se houve erro nas mensagens retornadas
      DATA(lv_has_error) = abap_false.
      LOOP AT l_result INTO DATA(ls_result_msg) WHERE type = 'E' OR type = 'A'.
        lv_has_error = abap_true.
        EXIT.
      ENDLOOP.

      IF lv_has_error = abap_false.
        log_message( i_type    = 'S'
                     i_message = 'Taxas gravadas com sucesso no SAP' ).
      ELSE.
        log_message( i_type    = 'E'
                     i_message = 'Erro ao gravar taxas no SAP (verificar mensagens)' ).
      ENDIF.
    ELSE.
      log_message( i_type    = 'W'
                   i_message = 'Nenhuma taxa de câmbio para gravar' ).
    ENDIF.

    log_message( i_type    = 'I'
                 i_message = 'Atualização de taxas finalizada' ).
  ENDMETHOD.