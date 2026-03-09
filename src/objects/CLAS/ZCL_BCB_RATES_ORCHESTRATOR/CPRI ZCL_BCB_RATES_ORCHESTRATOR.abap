  PRIVATE SECTION.

    "! Tipo de taxa de câmbio padrão (M = Média)
    CONSTANTS gc_rate_type     TYPE cl_exchange_rates=>ty_exchange_rate-rate_type VALUE 'M'.

    "! Moeda base (Real Brasileiro)
    CONSTANTS gc_base_currency TYPE cl_exchange_rates=>ty_exchange_rate-from_curr VALUE 'BRL'.

    "! Número máximo de dias anteriores para buscar cotação (fallback)
    CONSTANTS gc_max_days_back TYPE i VALUE 5.

    "! Fator de taxa fixo
    CONSTANTS gc_factor TYPE i VALUE 1.

    "! Dependências injetáveis
    DATA mo_ptax_client    TYPE REF TO zif_bcb_ptax_api_client.
    DATA mo_rate_selector  TYPE REF TO zif_bcb_rates_selector.
    DATA mo_rate_validator TYPE REF TO zcl_bcb_rates_validator.

    "! Dados recuperados para exibição no console e log
    DATA mt_exchange_rates TYPE cl_exchange_rates=>ty_exchange_rates.
    DATA mt_messages       TYPE cl_exchange_rates=>ty_messages.

    "! Busca cotação com fallback para dias anteriores
    METHODS get_rate_with_fallback
      IMPORTING i_currency       TYPE string
                i_date           TYPE d
      EXPORTING e_result         TYPE zif_bcb_ptax_api_client=>ty_bcb_cotacao
                e_effective_date TYPE d.

    "! Calcula o dia útil anterior (ignora sábado e domingo)
    METHODS get_previous_business_day
      IMPORTING i_date          TYPE d
      RETURNING VALUE(r_result) TYPE d.

    "! Processa e armazena as taxas de câmbio (direta e indireta)
    METHODS process_and_store_rates
      IMPORTING i_currency TYPE string
                i_cotacao  TYPE zif_bcb_ptax_api_client=>ty_bcb_cotacao
                i_date     TYPE d.

    "! Registra mensagem no log
    METHODS log_message
      IMPORTING i_type       TYPE bapi_mtype
                i_message    TYPE string
                i_message_v1 TYPE string OPTIONAL
                i_message_v2 TYPE string OPTIONAL
                i_message_v3 TYPE string OPTIONAL
                i_message_v4 TYPE string OPTIONAL.