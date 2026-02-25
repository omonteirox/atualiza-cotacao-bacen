"! @testing ZCL_BCB_RATES_ORCHESTRATOR
"! @testing zcl_bcb_rate_selector
"! @testing zcl_bcb_rate_validator
"!
"! Testes unitários para as classes de atualização de taxas de câmbio BCB.
"! Usa test doubles (mocks) para isolamento de dependências HTTP.
CLASS ZTCL_BCB_RATES_ORCHESTRATOR DEFINITION
  PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL
  CREATE PUBLIC.

  PRIVATE SECTION.

    " ============================================================
    " SETUP
    " ============================================================
    METHODS setup.

    " ============================================================
    " TESTES: Formatação de Data (ZCL_BCB_RATES_ORCHESTRATOR)
    " ============================================================

    "! Data padrão: 24/02/2026 → 02-24-2026
    METHODS test_format_date_standard    FOR TESTING.
    "! Data com dígitos baixos: 05/01/2026 → 01-05-2026
    METHODS test_format_date_single_dig  FOR TESTING.
    "! Primeiro dia do ano: 01/01/2026 → 01-01-2026
    METHODS test_format_date_new_year    FOR TESTING.
    "! Último dia do ano: 31/12/2025 → 12-31-2025
    METHODS test_format_date_end_year    FOR TESTING.

    " ============================================================
    " TESTES: Seleção de Melhor Cotação (zcl_bcb_rate_selector)
    " ============================================================

    "! Somente Fechamento: deve retornar Fechamento
    METHODS test_select_fechamento_only  FOR TESTING.
    "! Todos os boletins: deve retornar Fechamento PTAX
    METHODS test_select_all_boletins     FOR TESTING.
    "! Somente Intermediários: deve pegar o último
    METHODS test_select_intermediarios   FOR TESTING.
    "! Somente Abertura: deve retornar Abertura
    METHODS test_select_abertura_only    FOR TESTING.
    "! Lista vazia: deve retornar initial
    METHODS test_select_empty_list       FOR TESTING.
    "! Abertura + múltiplos intermediários: pega último intermediário
    METHODS test_select_multi_interm     FOR TESTING.

    " ============================================================
    " TESTES: Prioridade de Boletim (zcl_bcb_rate_selector)
    " ============================================================

    "! Prioridade do Fechamento PTAX = 1
    METHODS test_priority_fechamento     FOR TESTING.
    "! Prioridade do Intermediário = 2
    METHODS test_priority_intermediario  FOR TESTING.
    "! Prioridade da Abertura = 3
    METHODS test_priority_abertura       FOR TESTING.
    "! Prioridade de tipo desconhecido = 99
    METHODS test_priority_unknown        FOR TESTING.

    " ============================================================
    " TESTES: Validação de Cotação (zcl_bcb_rate_validator)
    " ============================================================

    "! Cotação com valores positivos → válida
    METHODS test_validate_rate_valid     FOR TESTING.
    "! Cotação com compra = 0 → inválida
    METHODS test_validate_rate_zero_buy  FOR TESTING.
    "! Cotação com venda = 0 → inválida
    METHODS test_validate_rate_zero_sell FOR TESTING.
    "! Cotação com ambos = 0 → inválida
    METHODS test_validate_rate_both_zero FOR TESTING.
    "! Cotação com valor negativo → inválida
    METHODS test_validate_rate_negative  FOR TESTING.
    "! Response com cotações → has_rates = true
    METHODS test_has_rates_true          FOR TESTING.
    "! Response vazio → has_rates = false
    METHODS test_has_rates_false         FOR TESTING.

    " ============================================================
    " TESTES: Lista de Moedas (ZCL_BCB_RATES_ORCHESTRATOR)
    " ============================================================

    "! Lista contém USD e EUR
    METHODS test_currency_list_content   FOR TESTING.
    "! Lista não está vazia
    METHODS test_currency_list_not_empty FOR TESTING.

    "! ============================================================
    "! Instâncias para teste
    "! ============================================================
    DATA mo_rate_selector  TYPE REF TO zcl_bcb_rate_selector.
    DATA mo_rate_validator TYPE REF TO zcl_bcb_rate_validator.

    "! Helper: cria uma cotação de teste
    METHODS create_test_cotacao
      IMPORTING i_compra        TYPE p
                i_venda         TYPE p
                i_tipo          TYPE string
                i_datahora      TYPE string DEFAULT '2026-02-24 10:00:00.000'
      RETURNING VALUE(r_result) TYPE zif_bcb_ptax_client=>ty_bcb_cotacao.
ENDCLASS.



CLASS ZTCL_BCB_RATES_ORCHESTRATOR IMPLEMENTATION.

  METHOD setup.
    mo_rate_selector  = NEW zcl_bcb_rate_selector( ).
    mo_rate_validator = NEW zcl_bcb_rate_validator( ).
  ENDMETHOD.


  " ========================================
  " Testes de Formatação de Data
  " ========================================

  METHOD test_format_date_standard.
    cl_abap_unit_assert=>assert_equals(
      act = ZCL_BCB_RATES_ORCHESTRATOR=>format_date_for_bcb( '20260224' )
      exp = '02-24-2026'
      msg = 'Formato de data padrão incorreto' ).
  ENDMETHOD.


  METHOD test_format_date_single_dig.
    cl_abap_unit_assert=>assert_equals(
      act = ZCL_BCB_RATES_ORCHESTRATOR=>format_date_for_bcb( '20260105' )
      exp = '01-05-2026'
      msg = 'Formato de data com dígito único incorreto' ).
  ENDMETHOD.


  METHOD test_format_date_new_year.
    cl_abap_unit_assert=>assert_equals(
      act = ZCL_BCB_RATES_ORCHESTRATOR=>format_date_for_bcb( '20260101' )
      exp = '01-01-2026'
      msg = 'Formato de data para ano novo incorreto' ).
  ENDMETHOD.


  METHOD test_format_date_end_year.
    cl_abap_unit_assert=>assert_equals(
      act = ZCL_BCB_RATES_ORCHESTRATOR=>format_date_for_bcb( '20251231' )
      exp = '12-31-2025'
      msg = 'Formato de data para último dia do ano incorreto' ).
  ENDMETHOD.


  " ========================================
  " Testes de Seleção de Melhor Cotação
  " ========================================

  METHOD test_select_fechamento_only.
    DATA(lt_cotacoes) = VALUE zif_bcb_ptax_client=>ty_bcb_cotacoes(
      ( create_test_cotacao( i_compra = '5.17630' i_venda = '5.17690'
                             i_tipo = 'Fechamento PTAX'
                             i_datahora = '2026-02-24 13:04:29.018' ) )
    ).

    DATA(ls_result) = mo_rate_selector->zif_bcb_rate_selector~select_best_rate( lt_cotacoes ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-tipoboletim
      exp = 'Fechamento PTAX'
      msg = 'Deveria selecionar Fechamento PTAX' ).
  ENDMETHOD.


  METHOD test_select_all_boletins.
    DATA(lt_cotacoes) = VALUE zif_bcb_ptax_client=>ty_bcb_cotacoes(
      ( create_test_cotacao( i_compra = '5.10000' i_venda = '5.10060'
                             i_tipo = 'Abertura'
                             i_datahora = '2026-02-24 10:11:24.971' ) )
      ( create_test_cotacao( i_compra = '5.12000' i_venda = '5.12060'
                             i_tipo = 'Intermediário'
                             i_datahora = '2026-02-24 11:09:24.763' ) )
      ( create_test_cotacao( i_compra = '5.14000' i_venda = '5.14060'
                             i_tipo = 'Intermediário'
                             i_datahora = '2026-02-24 12:03:25.131' ) )
      ( create_test_cotacao( i_compra = '5.15210' i_venda = '5.15270'
                             i_tipo = 'Intermediário'
                             i_datahora = '2026-02-24 13:04:29.018' ) )
      ( create_test_cotacao( i_compra = '5.16760' i_venda = '5.16820'
                             i_tipo = 'Fechamento PTAX'
                             i_datahora = '2026-02-24 13:04:29.024' ) )
    ).

    DATA(ls_result) = mo_rate_selector->zif_bcb_rate_selector~select_best_rate( lt_cotacoes ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-tipoboletim
      exp = 'Fechamento PTAX'
      msg = 'Com todos os boletins, deveria selecionar Fechamento PTAX' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-cotacaocompra
      exp = CONV decfloat16( '5.16760' )
      msg = 'Cotação de compra do Fechamento incorreta' ).
  ENDMETHOD.


  METHOD test_select_intermediarios.
    DATA(lt_cotacoes) = VALUE zif_bcb_ptax_client=>ty_bcb_cotacoes(
      ( create_test_cotacao( i_compra = '5.12000' i_venda = '5.12060'
                             i_tipo = 'Intermediário'
                             i_datahora = '2026-02-24 11:09:24.763' ) )
      ( create_test_cotacao( i_compra = '5.14000' i_venda = '5.14060'
                             i_tipo = 'Intermediário'
                             i_datahora = '2026-02-24 12:03:25.131' ) )
      ( create_test_cotacao( i_compra = '5.15210' i_venda = '5.15270'
                             i_tipo = 'Intermediário'
                             i_datahora = '2026-02-24 13:04:29.018' ) )
    ).

    DATA(ls_result) = mo_rate_selector->zif_bcb_rate_selector~select_best_rate( lt_cotacoes ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-cotacaocompra
      exp = CONV decfloat16( '5.15210' )
      msg = 'Deveria selecionar o último Intermediário (mais recente)' ).
  ENDMETHOD.


  METHOD test_select_abertura_only.
    DATA(lt_cotacoes) = VALUE zif_bcb_ptax_client=>ty_bcb_cotacoes(
      ( create_test_cotacao( i_compra = '5.10000' i_venda = '5.10060'
                             i_tipo = 'Abertura'
                             i_datahora = '2026-02-24 10:11:24.971' ) )
    ).

    DATA(ls_result) = mo_rate_selector->zif_bcb_rate_selector~select_best_rate( lt_cotacoes ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-tipoboletim
      exp = 'Abertura'
      msg = 'Deveria selecionar Abertura quando é o único boletim' ).
  ENDMETHOD.


  METHOD test_select_empty_list.
    DATA(lt_cotacoes) = VALUE zif_bcb_ptax_client=>ty_bcb_cotacoes( ).

    DATA(ls_result) = mo_rate_selector->zif_bcb_rate_selector~select_best_rate( lt_cotacoes ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_result
      msg = 'Com lista vazia, resultado deveria ser initial' ).
  ENDMETHOD.


  METHOD test_select_multi_interm.
    DATA(lt_cotacoes) = VALUE zif_bcb_ptax_client=>ty_bcb_cotacoes(
      ( create_test_cotacao( i_compra = '5.10000' i_venda = '5.10060'
                             i_tipo = 'Abertura'
                             i_datahora = '2026-02-24 10:11:24.971' ) )
      ( create_test_cotacao( i_compra = '5.12000' i_venda = '5.12060'
                             i_tipo = 'Intermediário'
                             i_datahora = '2026-02-24 11:09:24.763' ) )
      ( create_test_cotacao( i_compra = '5.14000' i_venda = '5.14060'
                             i_tipo = 'Intermediário'
                             i_datahora = '2026-02-24 12:03:25.131' ) )
    ).

    DATA(ls_result) = mo_rate_selector->zif_bcb_rate_selector~select_best_rate( lt_cotacoes ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-cotacaocompra
      exp = CONV decfloat16( '5.14000' )
      msg = 'Deveria selecionar o último Intermediário (maior prioridade que Abertura)' ).
  ENDMETHOD.


  " ========================================
  " Testes de Prioridade de Boletim
  " ========================================

  METHOD test_priority_fechamento.
    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_selector->get_boletim_priority( 'Fechamento PTAX' )
      exp = 1
      msg = 'Prioridade de Fechamento deveria ser 1' ).
  ENDMETHOD.


  METHOD test_priority_intermediario.
    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_selector->get_boletim_priority( 'Intermediário' )
      exp = 2
      msg = 'Prioridade de Intermediário deveria ser 2' ).
  ENDMETHOD.


  METHOD test_priority_abertura.
    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_selector->get_boletim_priority( 'Abertura' )
      exp = 3
      msg = 'Prioridade de Abertura deveria ser 3' ).
  ENDMETHOD.


  METHOD test_priority_unknown.
    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_selector->get_boletim_priority( 'TipoDesconhecido' )
      exp = 99
      msg = 'Prioridade de tipo desconhecido deveria ser 99' ).
  ENDMETHOD.


  " ========================================
  " Testes de Validação de Cotação
  " ========================================

  METHOD test_validate_rate_valid.
    DATA(ls_cotacao) = create_test_cotacao(
      i_compra = '5.17630' i_venda = '5.17690' i_tipo = 'Fechamento PTAX' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_validator->validate( ls_cotacao )
      exp = abap_true
      msg = 'Cotação com valores positivos deveria ser válida' ).
  ENDMETHOD.


  METHOD test_validate_rate_zero_buy.
    DATA(ls_cotacao) = create_test_cotacao(
      i_compra = '0' i_venda = '5.17690' i_tipo = 'Fechamento PTAX' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_validator->validate( ls_cotacao )
      exp = abap_false
      msg = 'Cotação com compra = 0 deveria ser inválida' ).
  ENDMETHOD.


  METHOD test_validate_rate_zero_sell.
    DATA(ls_cotacao) = create_test_cotacao(
      i_compra = '5.17630' i_venda = '0' i_tipo = 'Fechamento PTAX' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_validator->validate( ls_cotacao )
      exp = abap_false
      msg = 'Cotação com venda = 0 deveria ser inválida' ).
  ENDMETHOD.


  METHOD test_validate_rate_both_zero.
    DATA(ls_cotacao) = create_test_cotacao(
      i_compra = '0' i_venda = '0' i_tipo = 'Fechamento PTAX' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_validator->validate( ls_cotacao )
      exp = abap_false
      msg = 'Cotação com ambos os valores zerados deveria ser inválida' ).
  ENDMETHOD.


  METHOD test_validate_rate_negative.
    DATA(ls_cotacao) = create_test_cotacao(
      i_compra = '-1.00000' i_venda = '5.17690' i_tipo = 'Fechamento PTAX' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_validator->validate( ls_cotacao )
      exp = abap_false
      msg = 'Cotação com valor negativo deveria ser inválida' ).
  ENDMETHOD.


  METHOD test_has_rates_true.
    DATA(ls_response) = VALUE zif_bcb_ptax_client=>ty_bcb_response(
      value = VALUE #(
        ( create_test_cotacao( i_compra = '5.17630' i_venda = '5.17690' i_tipo = 'Abertura' ) )
      )
    ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_validator->has_rates( ls_response )
      exp = abap_true
      msg = 'Response com cotações deveria retornar true' ).
  ENDMETHOD.


  METHOD test_has_rates_false.
    DATA(ls_response) = VALUE zif_bcb_ptax_client=>ty_bcb_response( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_rate_validator->has_rates( ls_response )
      exp = abap_false
      msg = 'Response vazio deveria retornar false' ).
  ENDMETHOD.


  " ========================================
  " Testes de Lista de Moedas
  " ========================================

  METHOD test_currency_list_content.
    DATA(lt_currencies) = ZCL_BCB_RATES_ORCHESTRATOR=>get_currency_list( ).

    DATA(lv_usd_found) = abap_false.
    DATA(lv_eur_found) = abap_false.

    LOOP AT lt_currencies INTO DATA(ls_curr).
      CASE ls_curr-currency.
        WHEN 'USD'. lv_usd_found = abap_true.
        WHEN 'EUR'. lv_eur_found = abap_true.
      ENDCASE.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true(
      act = lv_usd_found
      msg = 'USD deveria estar na lista de moedas' ).

    cl_abap_unit_assert=>assert_true(
      act = lv_eur_found
      msg = 'EUR deveria estar na lista de moedas' ).
  ENDMETHOD.


  METHOD test_currency_list_not_empty.
    cl_abap_unit_assert=>assert_not_initial(
      act = ZCL_BCB_RATES_ORCHESTRATOR=>get_currency_list( )
      msg = 'Lista de moedas não deveria estar vazia' ).
  ENDMETHOD.


  " ========================================
  " Helper Methods
  " ========================================

  METHOD create_test_cotacao.
    r_result = VALUE zif_bcb_ptax_client=>ty_bcb_cotacao(
      paridadecompra  = 1
      paridadevenda   = 1
      cotacaocompra   = i_compra
      cotacaovenda    = i_venda
      datahoracotacao = i_datahora
      tipoboletim     = i_tipo
    ).
  ENDMETHOD.

ENDCLASS.
