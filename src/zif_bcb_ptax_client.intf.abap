"! Interface para abstração do cliente HTTP da API PTAX do BCB
"!
"! Permite injeção de dependência e mock nos testes unitários.
"! Implementação real usa Communication Arrangement do SAP BTP.
INTERFACE zif_bcb_ptax_client
  PUBLIC.

  "! Estrutura para deserialização do JSON da API BCB
  TYPES: BEGIN OF ty_bcb_cotacao,
           paridadecompra  TYPE p LENGTH 10 DECIMALS 5,
           paridadevenda   TYPE p LENGTH 10 DECIMALS 5,
           cotacaocompra   TYPE p LENGTH 10 DECIMALS 5,
           cotacaovenda    TYPE p LENGTH 10 DECIMALS 5,
           datahoracotacao TYPE string,
           tipoboletim     TYPE string,
         END OF ty_bcb_cotacao,
         ty_bcb_cotacoes TYPE STANDARD TABLE OF ty_bcb_cotacao WITH EMPTY KEY.

  "! Estrutura do response OData
  TYPES: BEGIN OF ty_bcb_response,
           value TYPE ty_bcb_cotacoes,
         END OF ty_bcb_response.

  "! Busca cotações de uma moeda em uma data específica
  "!
  "! @parameter i_currency | Código da moeda (ex: USD, EUR)
  "! @parameter i_date     | Data da cotação
  "! @parameter r_result   | Resposta com lista de cotações do dia
  "! @raising cx_root      | Erro de comunicação HTTP
  METHODS fetch_rates_for_date
    IMPORTING i_currency      TYPE string
              i_date          TYPE d
    RETURNING VALUE(r_result) TYPE ty_bcb_response
    RAISING   cx_root.

ENDINTERFACE.
