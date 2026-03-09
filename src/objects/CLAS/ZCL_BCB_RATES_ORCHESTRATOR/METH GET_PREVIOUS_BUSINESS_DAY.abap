  METHOD get_previous_business_day.
    r_result = i_date - 1.

    " Pular fins de semana
    " ABAP: segunda=1 ... domingo=7 (pode variar conforme calendário do sistema)
    " Abordagem robusta: verificar se caiu em sábado ou domingo


    " Calcular dia da semana usando aritmética modular
    " 01.01.0001 foi uma segunda-feira no calendário ABAP
    DATA(lv_days) = r_result - '00010101'.
    DATA(lv_weekday) = ( lv_days MOD 7 ). " 0=Seg, 1=Ter, ..., 5=Sáb, 6=Dom

    CASE lv_weekday.
      WHEN 5. " Sábado → voltar para Sexta
        r_result = r_result - 1.
      WHEN 6. " Domingo → voltar para Sexta
        r_result = r_result - 2.
    ENDCASE.
  ENDMETHOD.