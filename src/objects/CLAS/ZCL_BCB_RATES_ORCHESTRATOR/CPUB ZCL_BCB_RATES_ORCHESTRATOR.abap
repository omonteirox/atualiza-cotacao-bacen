"! Orquestrador de atualização de taxas de câmbio BCB PTAX
"!
"! Classe principal que coordena a busca, seleção, validação e gravação
"! das taxas de câmbio. Implementa interfaces de Job (Application Job)
"! e Console (ADT Classrun) do SAP BTP.
"!
"! Arquitetura Clean Core:
"! - zif_bcb_ptax_api_client   → comunicação HTTP (injetável/mockável)
"! - zif_bcb_rates_selector → lógica de seleção de cotação
"! - zcl_bcb_rates_validator → validação de cotações
"! - ZCL_BCB_RATES_ORCHESTRATOR → este orquestrador
CLASS ZCL_BCB_RATES_ORCHESTRATOR DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_apj_rt_exec_object,
                if_apj_dt_exec_object,
                if_oo_adt_classrun.

    "! Estrutura para configuração de moeda
    TYPES: BEGIN OF ty_currency_config,
             currency TYPE string,
           END OF ty_currency_config,
           ty_currency_configs TYPE STANDARD TABLE OF ty_currency_config WITH EMPTY KEY.

    "! Construtor com injeção de dependência
    "!
    "! @parameter i_ptax_client    | Cliente HTTP (opcional, usa default se não informado)
    "! @parameter i_rate_selector  | Seletor de cotação (opcional, usa default se não informado)
    "! @parameter i_rate_validator | Validador de cotação (opcional, usa default se não informado)
    METHODS constructor
      IMPORTING i_ptax_client    TYPE REF TO zif_bcb_ptax_api_client   OPTIONAL
                i_rate_selector  TYPE REF TO zif_bcb_rates_selector OPTIONAL
                i_rate_validator TYPE REF TO zcl_bcb_rates_validator OPTIONAL.

    "! Método principal de execução
    METHODS execute_rates_update.

    "! Retorna a lista de moedas configuradas para importação
    "! Para adicionar novas moedas, basta incluir na lista
    CLASS-METHODS get_currency_list
      RETURNING VALUE(r_result) TYPE ty_currency_configs.

    "! Formata a data para o padrão da API BCB (MM-DD-YYYY)
    "! Método público para uso nos testes
    CLASS-METHODS format_date_for_bcb
      IMPORTING i_date          TYPE d
      RETURNING VALUE(r_result) TYPE string.
