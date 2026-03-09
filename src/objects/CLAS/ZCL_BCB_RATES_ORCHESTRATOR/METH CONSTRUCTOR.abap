  METHOD constructor.
    " Injeção de dependência: usa instâncias fornecidas ou cria defaults
    mo_ptax_client    = COND #( WHEN i_ptax_client    IS BOUND THEN i_ptax_client
                                ELSE NEW zcl_bcb_ptax_api_client( ) ).
    mo_rate_selector  = COND #( WHEN i_rate_selector  IS BOUND THEN i_rate_selector
                                ELSE NEW zcl_bcb_rates_selector( ) ).
    mo_rate_validator = COND #( WHEN i_rate_validator IS BOUND THEN i_rate_validator
                                ELSE NEW zcl_bcb_rates_validator( ) ).
  ENDMETHOD.