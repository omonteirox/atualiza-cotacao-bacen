"! Classe responsável pela validação de cotações BCB
"!
"! Verifica se as cotações retornadas pela API possuem valores válidos
"! antes de serem gravadas nas tabelas de câmbio do SAP.
CLASS zcl_bcb_rates_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "! Valida se a cotação tem valores positivos (> 0)
    "!
    "! @parameter i_cotacao  | Cotação a ser validada
    "! @parameter r_result   | ABAP_TRUE se válida, ABAP_FALSE caso contrário
    METHODS validate
      IMPORTING i_cotacao       TYPE zif_bcb_ptax_api_client=>ty_bcb_cotacao
      RETURNING VALUE(r_result) TYPE abap_bool.

    "! Valida se a resposta da API contém cotações
    "!
    "! @parameter i_response | Resposta da API BCB
    "! @parameter r_result   | ABAP_TRUE se contém cotações, ABAP_FALSE caso contrário
    METHODS has_rates
      IMPORTING i_response      TYPE zif_bcb_ptax_api_client=>ty_bcb_response
      RETURNING VALUE(r_result) TYPE abap_bool.
