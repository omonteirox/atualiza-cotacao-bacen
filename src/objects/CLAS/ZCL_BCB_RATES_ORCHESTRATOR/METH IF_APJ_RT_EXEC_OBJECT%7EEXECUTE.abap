  METHOD if_apj_rt_exec_object~execute.
    " Executa a atualização via Application Job
    execute_rates_update( ).

    " Salvar log e gerenciar status do job
    TRY.
        DATA(lo_log) = cl_bali_log=>create( ).
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(lv_has_error) = abap_false.

        LOOP AT mt_messages INTO DATA(ls_msg).
          DATA lv_severity TYPE if_bali_constants=>ty_severity.

          CASE ls_msg-type.
            WHEN 'E' OR 'A'.
              lv_severity = if_bali_constants=>c_severity_error.
              lv_has_error = abap_true.
            WHEN 'W'.
              lv_severity = if_bali_constants=>c_severity_warning.
            WHEN 'S'.
              lv_severity = if_bali_constants=>c_severity_status.
            WHEN OTHERS.
              lv_severity = if_bali_constants=>c_severity_information.
          ENDCASE.

          DATA(lv_text) = |{ ls_msg-message } { ls_msg-message_v1 } { ls_msg-message_v2 }|.
          CONDENSE lv_text.

          DATA(lo_free_text) = cl_bali_free_text_setter=>create( severity = lv_severity
                                                                 text     = CONV #( lv_text ) ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.

        " Atrela o log estendido ap job
        cl_bali_log_db=>get_instance( )->save_log( log                        = lo_log
                                                   assign_to_current_appl_job = abap_true ).

      CATCH cx_bali_runtime INTO DATA(lx_bali) ##NO_HANDLER. " TODO: variable is assigned but never used (ABAP cleaner)
        " Erro de infraestrutura de log ignorado para não interromper job
    ENDTRY.

    " ATENÇÃO: Framework de Application Job avalia o log automaticamente.
    " Se houver mensagens do tipo 'E' (Error) ou 'A' (Abort),
    " o status da job será definido como Failed.
    " O 'RAISE EXCEPTION' causaria rollback e perda do log.
  ENDMETHOD.