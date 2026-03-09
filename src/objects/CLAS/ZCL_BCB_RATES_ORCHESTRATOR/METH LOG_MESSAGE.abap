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