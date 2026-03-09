  METHOD get_rate_with_fallback.
    DATA lv_current_date TYPE d.

    lv_current_date = i_date.
    e_effective_date = i_date.

    " Tentar dia atual e até gc_max_days_back dias anteriores
    DO gc_max_days_back TIMES.
      TRY.
          DATA(ls_response) = mo_ptax_client->fetch_rates_for_date( i_currency = i_currency
                                                                    i_date     = lv_current_date ).

          " Selecionar a melhor cotação
          DATA(ls_best) = mo_rate_selector->select_best_rate( ls_response-value ).

          " Validar cotação selecionada
          IF ls_best IS NOT INITIAL AND mo_rate_validator->validate( ls_best ) = abap_true.
            e_result = ls_best.
            e_effective_date = lv_current_date.

            IF lv_current_date <> i_date.
              log_message( i_type       = 'W'
                           i_message    = 'Usando cotação de data anterior'
                           i_message_v1 = i_currency
                           i_message_v2 = CONV #( lv_current_date ) ).
            ENDIF.

            " Log da cotação selecionada
            log_message( i_type       = 'I'
                         i_message    = 'Cotação selecionada'
                         i_message_v1 = i_currency
                         i_message_v2 = ls_best-tipoboletim
                         i_message_v3 = ls_best-datahoracotacao
                         i_message_v4 = CONV #( ls_best-cotacaocompra ) ).
            RETURN.
          ENDIF.

        CATCH cx_root INTO DATA(lx_exception).
          log_message( i_type       = 'W'
                       i_message    = 'Erro ao buscar cotação'
                       i_message_v1 = i_currency
                       i_message_v2 = CONV #( lv_current_date )
                       i_message_v3 = CONV #( lx_exception->get_text( ) ) ).
      ENDTRY.

      " Ir para o dia útil anterior
      lv_current_date = get_previous_business_day( lv_current_date ).
    ENDDO.
  ENDMETHOD.