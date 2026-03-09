  METHOD zif_bcb_rates_selector~select_best_rate.
    DATA: lv_best_priority TYPE i VALUE 99,
          lv_best_index    TYPE i VALUE 0,
          lv_priority      TYPE i,
          lv_index         TYPE i.

    " Percorrer todas as cotações e selecionar por prioridade
    LOOP AT i_cotacoes INTO DATA(ls_cotacao).
      lv_index = sy-tabix.

      " Determinar prioridade do boletim
      lv_priority = get_boletim_priority( ls_cotacao-tipoboletim ).

      " Selecionar o de maior prioridade (menor número)
      " Em caso de empate (ex: múltiplos intermediários), pegar o último (mais recente)
      IF lv_priority < lv_best_priority.
        lv_best_priority = lv_priority.
        lv_best_index = lv_index.
        r_result = ls_cotacao.
      ELSEIF lv_priority = lv_best_priority AND lv_index > lv_best_index.
        " Mesmo tipo de boletim → pegar o mais recente
        lv_best_index = lv_index.
        r_result = ls_cotacao.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.