"! Orquestrador de atualização de taxas de câmbio BCB PTAX
"!
"! Classe principal que coordena a busca, seleção, validação e gravação
"! das taxas de câmbio. Implementa interfaces de Job (Application Job)
"! e Console (ADT Classrun) do SAP BTP.
"!
"! Arquitetura Clean Core:
"! - zif_bcb_ptax_client   → comunicação HTTP (injetável/mockável)
"! - zif_bcb_rate_selector → lógica de seleção de cotação
"! - zcl_bcb_rate_validator → validação de cotações
"! - zcl_bcb_exchange_rates → este orquestrador
CLASS zcl_bcb_exchange_rates DEFINITION
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
      IMPORTING i_ptax_client    TYPE REF TO zif_bcb_ptax_client   OPTIONAL
                i_rate_selector  TYPE REF TO zif_bcb_rate_selector OPTIONAL
                i_rate_validator TYPE REF TO zcl_bcb_rate_validator OPTIONAL.

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

  PROTECTED SECTION.
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
    DATA mo_ptax_client    TYPE REF TO zif_bcb_ptax_client.
    DATA mo_rate_selector  TYPE REF TO zif_bcb_rate_selector.
    DATA mo_rate_validator TYPE REF TO zcl_bcb_rate_validator.

    "! Dados recuperados para exibição no console e log
    DATA mt_exchange_rates TYPE cl_exchange_rates=>ty_exchange_rates.
    DATA mt_messages       TYPE cl_exchange_rates=>ty_messages.

    "! Busca cotação com fallback para dias anteriores
    METHODS get_rate_with_fallback
      IMPORTING i_currency       TYPE string
                i_date           TYPE d
      EXPORTING e_result         TYPE zif_bcb_ptax_client=>ty_bcb_cotacao
                e_effective_date TYPE d.

    "! Calcula o dia útil anterior (ignora sábado e domingo)
    METHODS get_previous_business_day
      IMPORTING i_date          TYPE d
      RETURNING VALUE(r_result) TYPE d.

    "! Processa e armazena as taxas de câmbio (direta e indireta)
    METHODS process_and_store_rates
      IMPORTING i_currency TYPE string
                i_cotacao  TYPE zif_bcb_ptax_client=>ty_bcb_cotacao
                i_date     TYPE d.

    "! Registra mensagem no log
    METHODS log_message
      IMPORTING i_type       TYPE bapi_mtype
                i_message    TYPE string
                i_message_v1 TYPE string OPTIONAL
                i_message_v2 TYPE string OPTIONAL
                i_message_v3 TYPE string OPTIONAL
                i_message_v4 TYPE string OPTIONAL.
ENDCLASS.



CLASS zcl_bcb_exchange_rates IMPLEMENTATION.

  METHOD constructor.
    " Injeção de dependência: usa instâncias fornecidas ou cria defaults
    mo_ptax_client    = COND #( WHEN i_ptax_client    IS BOUND THEN i_ptax_client
                                ELSE NEW zcl_bcb_ptax_client( ) ).
    mo_rate_selector  = COND #( WHEN i_rate_selector  IS BOUND THEN i_rate_selector
                                ELSE NEW zcl_bcb_rate_selector( ) ).
    mo_rate_validator = COND #( WHEN i_rate_validator IS BOUND THEN i_rate_validator
                                ELSE NEW zcl_bcb_rate_validator( ) ).
  ENDMETHOD.


  METHOD execute_rates_update.
    DATA: lv_effective_date TYPE d.

    " Limpar dados anteriores
    CLEAR: mt_exchange_rates, mt_messages.

    log_message( i_type    = 'I'
                 i_message = 'Iniciando atualização de taxas de câmbio BCB PTAX' ).

    " Obter lista de moedas configuradas
    DATA(lt_currencies) = get_currency_list( ).

    " Processar cada moeda
    LOOP AT lt_currencies INTO DATA(ls_currency).
      log_message( i_type       = 'I'
                   i_message    = 'Buscando cotação'
                   i_message_v1 = ls_currency-currency ).

      " Buscar cotação com fallback para dias anteriores
      DATA ls_cotacao TYPE zif_bcb_ptax_client=>ty_bcb_cotacao.
      get_rate_with_fallback(
        EXPORTING
          i_currency       = ls_currency-currency
          i_date           = sy-datum
        IMPORTING
          e_result         = ls_cotacao
          e_effective_date = lv_effective_date ).

      IF ls_cotacao IS NOT INITIAL.
        process_and_store_rates(
          i_currency = ls_currency-currency
          i_cotacao  = ls_cotacao
          i_date     = lv_effective_date ).
      ELSE.
        log_message( i_type       = 'E'
                     i_message    = 'Nenhuma cotação encontrada'
                     i_message_v1 = ls_currency-currency ).
      ENDIF.
    ENDLOOP.

    " Gravar as taxas no sistema (is_update_allowed = abap_true permite re-execução)
    IF mt_exchange_rates IS NOT INITIAL.
      DATA(l_result) = cl_exchange_rates=>put(
        EXPORTING
          exchange_rates    = mt_exchange_rates
          is_update_allowed = abap_true ).
      APPEND LINES OF l_result TO mt_messages.

      " Verificar se houve erro nas mensagens retornadas
      DATA(lv_has_error) = abap_false.
      LOOP AT l_result INTO DATA(ls_result_msg) WHERE type = 'E' OR type = 'A'.
        lv_has_error = abap_true.
        EXIT.
      ENDLOOP.

      IF lv_has_error = abap_false.
        log_message( i_type    = 'S'
                     i_message = 'Taxas gravadas com sucesso no SAP' ).
      ELSE.
        log_message( i_type    = 'E'
                     i_message = 'Erro ao gravar taxas no SAP (verificar mensagens)' ).
      ENDIF.
    ELSE.
      log_message( i_type    = 'W'
                   i_message = 'Nenhuma taxa de câmbio para gravar' ).
    ENDIF.

    log_message( i_type    = 'I'
                 i_message = 'Atualização de taxas finalizada' ).
  ENDMETHOD.


  METHOD get_currency_list.
    " Lista de moedas configuradas para importação
    " Para adicionar novas moedas, basta incluir na lista
    r_result = VALUE #(
      ( currency = 'USD' )
      ( currency = 'EUR' )
    ).
  ENDMETHOD.


  METHOD format_date_for_bcb.
    " Formato esperado pela API BCB: MM-DD-YYYY
    r_result = |{ i_date+4(2) }-{ i_date+6(2) }-{ i_date(4) }|.
  ENDMETHOD.


  METHOD get_rate_with_fallback.
    DATA: lv_current_date TYPE d.

    lv_current_date = i_date.
    e_effective_date = i_date.

    " Tentar dia atual e até gc_max_days_back dias anteriores
    DO gc_max_days_back TIMES.
      TRY.
          DATA(ls_response) = mo_ptax_client->fetch_rates_for_date(
            i_currency = i_currency
            i_date     = lv_current_date ).

          " Selecionar a melhor cotação
          DATA(ls_best) = mo_rate_selector->select_best_rate( ls_response-value ).

          " Validar cotação selecionada
          IF ls_best IS NOT INITIAL AND mo_rate_validator->validate( ls_best ) = abap_true.
            e_result = ls_best.
            e_effective_date = lv_current_date.

            IF lv_current_date <> i_date.
              log_message( i_type       = 'W'
                           i_message    = 'Usando cotação de data anterior'
                           i_message_v1 = i_currency
                           i_message_v2 = CONV #( lv_current_date ) ).
            ENDIF.

            " Log da cotação selecionada
            log_message( i_type       = 'I'
                         i_message    = 'Cotação selecionada'
                         i_message_v1 = i_currency
                         i_message_v2 = ls_best-tipoboletim
                         i_message_v3 = ls_best-datahoracotacao
                         i_message_v4 = CONV #( ls_best-cotacaocompra ) ).
            RETURN.
          ENDIF.

        CATCH cx_http_dest_provider_error cx_web_http_client_error INTO DATA(lx_exception).
          log_message( i_type       = 'W'
                       i_message    = 'Erro ao buscar cotação'
                       i_message_v1 = i_currency
                       i_message_v2 = CONV #( lv_current_date )
                       i_message_v3 = CONV #( lx_exception->get_text( ) ) ).
      ENDTRY.

      " Ir para o dia útil anterior
      lv_current_date = get_previous_business_day( lv_current_date ).
    ENDDO.
  ENDMETHOD.


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


  METHOD process_and_store_rates.
    DATA ls_exchange_rate TYPE cl_exchange_rates=>ty_exchange_rate.

    " ========================================
    " COTAÇÃO DIRETA: USD/EUR → BRL
    " Exemplo: 1 USD = 5.17 BRL (cotação de compra)
    " ========================================
    CLEAR ls_exchange_rate.
    ls_exchange_rate-rate_type   = gc_rate_type.
    ls_exchange_rate-from_curr   = i_currency.         " USD ou EUR
    ls_exchange_rate-to_currncy  = gc_base_currency.   " BRL
    ls_exchange_rate-valid_from  = i_date.
    ls_exchange_rate-from_factor = gc_factor.
    ls_exchange_rate-to_factor   = gc_factor.
    ls_exchange_rate-exch_rate   = i_cotacao-cotacaocompra.
    APPEND ls_exchange_rate TO mt_exchange_rates.

    " ========================================
    " COTAÇÃO INDIRETA: BRL → USD/EUR
    " Cotação inversa para conversões BRL → moeda estrangeira
    " ========================================
    CLEAR ls_exchange_rate.
    ls_exchange_rate-rate_type     = gc_rate_type.
    ls_exchange_rate-from_curr     = gc_base_currency.  " BRL
    ls_exchange_rate-to_currncy    = i_currency.        " USD ou EUR
    ls_exchange_rate-valid_from    = i_date.
    ls_exchange_rate-from_factor_v = gc_factor.
    ls_exchange_rate-to_factor_v   = gc_factor.
    ls_exchange_rate-exch_rate_v   = i_cotacao-cotacaocompra.
    APPEND ls_exchange_rate TO mt_exchange_rates.

    " Log de sucesso com detalhes do boletim utilizado
    log_message( i_type       = 'S'
                 i_message    = 'Taxa direta registrada'
                 i_message_v1 = i_currency
                 i_message_v2 = CONV #( gc_base_currency )
                 i_message_v3 = CONV #( i_cotacao-cotacaocompra )
                 i_message_v4 = i_cotacao-tipoboletim ).

    log_message( i_type       = 'S'
                 i_message    = 'Taxa indireta registrada'
                 i_message_v1 = CONV #( gc_base_currency )
                 i_message_v2 = i_currency
                 i_message_v3 = CONV #( i_cotacao-cotacaocompra )
                 i_message_v4 = i_cotacao-tipoboletim ).
  ENDMETHOD.


  METHOD log_message.
    APPEND VALUE #(
      type       = i_type
      message    = i_message
      message_v1 = CONV #( i_message_v1 )
      message_v2 = CONV #( i_message_v2 )
      message_v3 = CONV #( i_message_v3 )
      message_v4 = CONV #( i_message_v4 )
    ) TO mt_messages.
  ENDMETHOD.


  METHOD if_apj_dt_exec_object~get_parameters.
    " Sem parâmetros necessários para este job
    et_parameter_def = VALUE #( ).
    et_parameter_val = VALUE #( ).
  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.
    " Executa a atualização via Application Job
    execute_rates_update( ).
  ENDMETHOD.


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

ENDCLASS.
