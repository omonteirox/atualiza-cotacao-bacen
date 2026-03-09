  METHOD zif_bcb_ptax_api_client~fetch_rates_for_date.
    DATA: lv_url_path TYPE string.

    " Montar o PATH da API (relativo ao host do Communication Arrangement)
    lv_url_path = |CotacaoMoedaDia(moeda=@moeda,dataCotacao=@dataCotacao)?| &&
                  |@moeda='{ i_currency }'&| &&
                  |@dataCotacao='{ format_date_for_bcb( i_date ) }'&| &&
                  |$format=json|.

    " Criar destino via Communication Arrangement (released API)
    DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
      comm_scenario  = gc_comm_scenario
      service_id     = gc_service_id ).

    " Criar HTTP client (released API)
    DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
      i_destination = lo_destination ).

    TRY.
        DATA(lo_request) = lo_http_client->get_http_request( ).
        lo_request->set_uri_path( i_uri_path = lv_url_path ).

        " Usar RETRY_EXECUTE nativo do IF_WEB_HTTP_CLIENT
        " O framework gerencia retries automaticamente em caso de erros transientes
        DATA(lo_response) = lo_http_client->retry_execute(
          i_method = if_web_http_client=>get ).

        DATA(lv_status) = lo_response->get_status( ).

        " Verificar status HTTP
        IF lv_status-code <> 200.
          RAISE EXCEPTION TYPE cx_web_http_client_error.
        ENDIF.

        DATA(lv_json) = lo_response->get_text( ).

        " Deserializar JSON usando /ui2/cl_json (released no ABAP Cloud)
        " XCO_CP_JSON é mais Clean Core mas não suporta deserialização direta
        " para estruturas com pretty_name/camel_case como a API BCB requer
        /ui2/cl_json=>deserialize(
          EXPORTING
            json        = lv_json
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data        = r_result ).

      CATCH cx_web_http_client_error INTO DATA(lx_http_error).
        IF lo_http_client IS BOUND.
          TRY.
              lo_http_client->close( ).
            CATCH cx_web_http_client_error ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        RAISE EXCEPTION lx_http_error.
    ENDTRY.

    " Fechar o client HTTP
    IF lo_http_client IS BOUND.
      TRY.
          lo_http_client->close( ).
        CATCH cx_web_http_client_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.