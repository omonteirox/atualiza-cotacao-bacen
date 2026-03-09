  PRIVATE SECTION.
    "! Formata a data para o padrão da API BCB (MM-DD-YYYY)
    "!
    "! @parameter i_date   | Data no formato ABAP (YYYYMMDD)
    "! @parameter r_result | Data no formato BCB (MM-DD-YYYY)
    METHODS format_date_for_bcb
      IMPORTING i_date          TYPE d
      RETURNING VALUE(r_result) TYPE string.
