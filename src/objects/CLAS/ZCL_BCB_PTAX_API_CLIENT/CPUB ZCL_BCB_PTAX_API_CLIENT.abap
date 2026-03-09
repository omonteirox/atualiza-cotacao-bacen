"! Implementação do cliente HTTP para a API PTAX do BCB
"!
"! Usa Communication Arrangement do SAP BTP para comunicação segura.
"! Utiliza RETRY_EXECUTE nativo do IF_WEB_HTTP_CLIENT para resiliência.
"! Usa XCO_CP_JSON para deserialização (Clean Core compliance).
CLASS zcl_bcb_ptax_api_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_bcb_ptax_api_client.

    "! Communication Arrangement IDs (devem corresponder ao configurado no SAP BTP)
    CONSTANTS gc_comm_scenario TYPE if_com_management=>ty_cscn_id          VALUE 'ZCS_BACEN'.
    CONSTANTS gc_service_id    TYPE if_com_management=>ty_cscn_outb_srv_id VALUE 'Z_BCB_PTAX_HTTP_REST'.
