"! Implementação do cliente HTTP para a API PTAX do BCB
"!
"! Usa Communication Arrangement do SAP BTP para comunicação segura.
"! Inclui mecanismo de retry em caso de falha na API.
CLASS zcl_bcb_ptax_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_bcb_ptax_client.

    "! Communication Arrangement IDs (devem corresponder ao configurado no SAP BTP)
    CONSTANTS gc_comm_scenario TYPE string VALUE 'YY1_AUTOMATIC_RATES'.
    CONSTANTS gc_service_id    TYPE string VALUE 'YY1_ZBCB_PTAX_HTTP_REST'.

    "! Número máximo de tentativas de retry
    CONSTANTS gc_max_retries TYPE i VALUE 3.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Formata a data para o padrão da API BCB (MM-DD-YYYY)
    "!
    "! @parameter i_date     | Data no formato ABAP (YYYYMMDD)
    "! @parameter r_result   | Data no formato BCB (MM-DD-YYYY)
    METHODS format_date_for_bcb
      IMPORTING i_date          TYPE d
      RETURNING VALUE(r_result) TYPE string.

    "! Realiza a chamada HTTP à API BCB
    "!
    "! @parameter i_currency | Código da moeda
    "! @parameter i_date     | Data da cotação
    "! @parameter r_result   | Resposta deserializada da API
    "! @raising cx_root      | Erro de comunicação
    METHODS call_bcb_api
      IMPORTING i_currency      TYPE string
                i_date          TYPE d
      RETURNING VALUE(r_result) TYPE zif_bcb_ptax_client=>ty_bcb_response
      RAISING   cx_root.

ENDCLASS.



CLASS zcl_bcb_ptax_client IMPLEMENTATION.

  METHOD zif_bcb_ptax_client~fetch_rates_for_date.
    DATA: lv_attempt TYPE i.

    " Retry loop para resiliência
    DO gc_max_retries TIMES.
      lv_attempt = lv_attempt + 1.

      TRY.
          r_result = call_bcb_api(
            i_currency = i_currency
            i_date     = i_date ).

          " Sucesso: sair do loop de retry
          RETURN.

        CATCH cx_root INTO DATA(lx_exception).
          IF lv_attempt >= gc_max_retries.
            " Última tentativa falhou: propagar exceção
            RAISE EXCEPTION lx_exception.
          ENDIF.
          " Continua para próxima tentativa
      ENDTRY.
    ENDDO.
  ENDMETHOD.


  METHOD call_bcb_api.
    DATA: lv_url_path TYPE string.

    " Montar o PATH da API (relativo ao host do Communication Arrangement)
    lv_url_path = |CotacaoMoedaDia(moeda=@moeda,dataCotacao=@dataCotacao)?| &&
                  |@moeda='{ i_currency }'&| &&
                  |@dataCotacao='{ format_date_for_bcb( i_date ) }'&| &&
                  |$format=json|.

    " Criar destino via Communication Arrangement
    DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
      comm_scenario  = gc_comm_scenario
      service_id     = gc_service_id ).

    DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
      i_destination = lo_destination ).

    TRY.
        DATA(lo_request) = lo_http_client->get_http_request( ).
        lo_request->set_uri_path( i_uri_path = lv_url_path ).

        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
        DATA(lv_status) = lo_response->get_status( ).

        " Verificar status HTTP
        IF lv_status-code <> 200.
          RAISE EXCEPTION TYPE cx_web_http_client_error
            EXPORTING text_id = cx_web_http_client_error=>http_client_error.
        ENDIF.

        DATA(lv_json) = lo_response->get_text( ).

        " Deserializar JSON
        /ui2/cl_json=>deserialize(
          EXPORTING
            json        = lv_json
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data        = r_result ).

      CLEANUP.
        IF lo_http_client IS BOUND.
          lo_http_client->close( ).
        ENDIF.
    ENDTRY.

    " Fechar o client HTTP
    IF lo_http_client IS BOUND.
      lo_http_client->close( ).
    ENDIF.
  ENDMETHOD.


  METHOD format_date_for_bcb.
    " Formato esperado pela API BCB: MM-DD-YYYY
    r_result = |{ i_date+4(2) }-{ i_date+6(2) }-{ i_date(4) }|.
  ENDMETHOD.

ENDCLASS.
