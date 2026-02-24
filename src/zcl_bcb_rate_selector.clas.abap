"! Implementação da lógica de seleção de melhor cotação BCB
"!
"! Seleciona a cotação mais relevante entre os boletins disponíveis.
"! Prioridade: Fechamento PTAX > Intermediário (mais recente) > Abertura.
CLASS zcl_bcb_rate_selector DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_bcb_rate_selector.

    "! Prioridade dos boletins (menor = maior prioridade)
    CONSTANTS:
      BEGIN OF gc_boletim_priority,
        fechamento    TYPE i VALUE 1,
        intermediario TYPE i VALUE 2,
        abertura      TYPE i VALUE 3,
        unknown       TYPE i VALUE 99,
      END OF gc_boletim_priority.

    "! Retorna a prioridade numérica de um tipo de boletim
    "!
    "! @parameter i_tipo_boletim | Tipo do boletim (ex: 'Fechamento PTAX')
    "! @parameter r_result       | Prioridade numérica
    METHODS get_boletim_priority
      IMPORTING i_tipo_boletim  TYPE string
      RETURNING VALUE(r_result) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bcb_rate_selector IMPLEMENTATION.

  METHOD zif_bcb_rate_selector~select_best_rate.
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


  METHOD get_boletim_priority.
    IF i_tipo_boletim CS 'Fechamento'.
      r_result = gc_boletim_priority-fechamento.
    ELSEIF i_tipo_boletim CS 'Intermedi'.
      r_result = gc_boletim_priority-intermediario.
    ELSEIF i_tipo_boletim CS 'Abertura'.
      r_result = gc_boletim_priority-abertura.
    ELSE.
      r_result = gc_boletim_priority-unknown.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
