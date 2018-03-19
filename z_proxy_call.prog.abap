*&---------------------------------------------------------------------*
*& Report z_proxy_call
*&---------------------------------------------------------------------*
*& Author       : Aris van Mazijk (DEVELOPER)
*& Date         : 19-03-2018
*& Description  : ABAP OO Report with multiple proxy calls (TEST)
*& Marker       :
*& Transport    : NPLK900023.
*&---------------------------------------------------------------------*
REPORT z_proxy_call.

INCLUDE z_proxy_calls_screen.
INCLUDE z_proxy_calls_class.
*&---------------------------------------------------------------------*
*& AT-SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
  lcl_proxy_call=>select_file( ).

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  CONSTANTS cn_max_days TYPE i VALUE 31.

* Input checks.
  IF ( pa_local EQ abap_true AND so_date IS INITIAL ) OR
     ( so_date-low IS NOT INITIAL AND so_date-high IS INITIAL ) OR
     ( so_date-low IS INITIAL AND so_date-high IS NOT INITIAL ) OR
     ( so_date-high - so_date-low ) > cn_max_days.
    MESSAGE ID 'MO' TYPE 'I' NUMBER '001' WITH 'Geef datumperiode van max. 1 maand op'. "#EC CI_USE_WANTED
  ELSEIF ( pa_local EQ abap_true AND pa_file IS INITIAL ).
    MESSAGE ID 'MO' TYPE 'I' NUMBER '001' WITH 'Geef bestandslocatie & naam op'. "#EC CI_USE_WANTED
  ELSE.
    NEW lcl_proxy_call( ).
  ENDIF.
