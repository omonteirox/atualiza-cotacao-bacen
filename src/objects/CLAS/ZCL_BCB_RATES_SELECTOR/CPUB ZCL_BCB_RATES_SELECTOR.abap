"! Implementação da lógica de seleção de melhor cotação BCB
"!
"! Seleciona a cotação mais relevante entre os boletins disponíveis.
"! Prioridade: Fechamento PTAX > Intermediário (mais recente) > Abertura.
CLASS zcl_bcb_rates_selector DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_bcb_rates_selector.

    " Prioridade dos boletins (menor = maior prioridade)
    CONSTANTS:
      BEGIN OF gc_boletim_priority,
        fechamento    TYPE i VALUE 1,
        intermediario TYPE i VALUE 2,
        abertura      TYPE i VALUE 3,
        unknown       TYPE i VALUE 99,
      END OF gc_boletim_priority.

    " Nomes dos boletins da API BCB (valores técnicos, não traduzíveis)
    CONSTANTS:
      gc_boletim_fechamento    TYPE string VALUE 'Fechamento' ##NO_TEXT,
      gc_boletim_intermediario TYPE string VALUE 'Intermedi' ##NO_TEXT,
      gc_boletim_abertura      TYPE string VALUE 'Abertura' ##NO_TEXT.

    "! Retorna a prioridade numérica de um tipo de boletim
    "!
    "! @parameter i_tipo_boletim | Tipo do boletim (ex: 'Fechamento PTAX')
    "! @parameter r_result       | Prioridade numérica
    METHODS get_boletim_priority
      IMPORTING i_tipo_boletim  TYPE string
      RETURNING VALUE(r_result) TYPE i.
