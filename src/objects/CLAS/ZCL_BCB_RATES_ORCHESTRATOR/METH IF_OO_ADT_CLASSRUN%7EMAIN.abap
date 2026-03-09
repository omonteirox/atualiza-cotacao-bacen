  METHOD if_oo_adt_classrun~main.
    " Executa a atualização e exibe no console
    execute_rates_update( ).

    " Exibir resultados detalhados no console
    out->write( |========================================| ).
    out->write( |  Atualização de Taxas de Câmbio BCB    | ).
    out->write( |  Data: { sy-datum DATE = USER }        | ).
    out->write( |========================================| ).
    out->write( '' ).

    " Exibir taxas recuperadas
    IF mt_exchange_rates IS NOT INITIAL.
      out->write( |Taxas de câmbio registradas:| ).
      LOOP AT mt_exchange_rates INTO DATA(ls_rate).
        out->write( |  { ls_rate-from_curr } -> { ls_rate-to_currncy }: { ls_rate-exch_rate } ({ ls_rate-valid_from DATE = USER })| ).
      ENDLOOP.
    ELSE.
      out->write( |Nenhuma taxa de câmbio registrada.| ).
    ENDIF.

    out->write( '' ).
    out->write( |--- Mensagens de Log ---| ).
    LOOP AT mt_messages INTO DATA(ls_msg).
      DATA(lv_icon) = SWITCH string( ls_msg-type
        WHEN 'S' THEN '✓'
        WHEN 'E' THEN '✗'
        WHEN 'W' THEN '⚠'
        WHEN 'I' THEN 'ℹ'
        ELSE '?' ).
      out->write( |{ lv_icon } [{ ls_msg-type }] { ls_msg-message } { ls_msg-message_v1 } { ls_msg-message_v2 } { ls_msg-message_v3 } { ls_msg-message_v4 }| ).
    ENDLOOP.
  ENDMETHOD.