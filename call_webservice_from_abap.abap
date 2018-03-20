* 1. Aanmaken client proxy vanuit web service naar keuze vanuit SE80.
* 2. Aanmaken logical port vanuit transactie SOAMANAGER.
* 3. Vanuit ABAP rapport proxy class instantieren en methode aanroepen:

DATA: lr_proxy_class  TYPE REF TO zes_co_query_view_data,  <------Gegenereerde Proxy class.
      lo_sys_except   TYPE REF TO cx_ai_system_fault,
      ls_input        TYPE zes_get_query_view_data,
      ls_output       TYPE zes_get_query_view_data_respon,
      lt_parameters   TYPE zes_w3query_tab,
      ls_parameters   TYPE zes_w3query.

TRY.
    CREATE OBJECT lr_proxy_class
      EXPORTING
        logical_port_name = 'Z_QUERY_VIEW_DATA'.   <--------Aanroep logical port.

  CATCH cx_ai_system_fault.
  CATCH cx_ai_application_fault.
ENDTRY.

ls_input-infoprovider         = 'ZRDA_M01'.
ls_input-query                = 'ZRDA_M01_Q7001'.
ls_parameters-name            = 'ZSO_INST'.
ls_parameters-value           = '871'.
APPEND ls_parameters TO ls_input-parameter-item.

TRY.
    CALL METHOD lr_proxy_class->get_query_view_data	<---------Aanroep method uit proxy class.
      EXPORTING
        input  = ls_input
      IMPORTING
        output = ls_output.

  CATCH cx_ai_system_fault INTO lo_sys_except.
    WRITE:/ 'System fault occurred:', lo_sys_except->code, lo_sys_except->errortext.
  CATCH cx_ai_application_fault.
ENDTRY.
