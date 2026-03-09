  METHOD get_boletim_priority.
    IF i_tipo_boletim CS gc_boletim_fechamento.
      r_result = gc_boletim_priority-fechamento.
    ELSEIF i_tipo_boletim CS gc_boletim_intermediario.
      r_result = gc_boletim_priority-intermediario.
    ELSEIF i_tipo_boletim CS gc_boletim_abertura.
      r_result = gc_boletim_priority-abertura.
    ELSE.
      r_result = gc_boletim_priority-unknown.
    ENDIF.
  ENDMETHOD.