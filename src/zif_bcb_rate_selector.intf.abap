"! Interface para abstração da lógica de seleção de cotação
"!
"! Define o contrato para seleção da melhor cotação entre os boletins
"! disponíveis (Abertura, Intermediário, Fechamento PTAX).
INTERFACE zif_bcb_rate_selector
  PUBLIC.

  "! Seleciona a melhor cotação da lista retornada pela API
  "!
  "! Prioridade: Fechamento PTAX > último Intermediário > Abertura
  "!
  "! @parameter i_cotacoes | Lista de cotações retornadas pela API BCB
  "! @parameter r_result   | Melhor cotação selecionada
  METHODS select_best_rate
    IMPORTING i_cotacoes      TYPE zif_bcb_ptax_client=>ty_bcb_cotacoes
    RETURNING VALUE(r_result) TYPE zif_bcb_ptax_client=>ty_bcb_cotacao.

ENDINTERFACE.
