*&---------------------------------------------------------------------*
*& Report  Z_DELETE_LINK_DPF
*&---------------------------------------------------------------------*
*& Author       : Aris van Mazijk (DEVELOPER) for Softwaris.
*& Date         : 20-03-2018
*& Description  : Report to delete broken links to documents from DPF
*& Marker       :
*& Transport    : NPLK900025
*&---------------------------------------------------------------------*
REPORT z_dpf_delete_link.

INCLUDE z_dpf_delete_link_screen.
INCLUDE z_dpf_delete_link_class.

DATA lo_dpd_delete_link TYPE REF TO z_dpf_delete_link.

*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT lo_dpd_delete_link.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

  IF ( so_pernr IS INITIAL AND pa_all EQ abap_false ).
    lo_dpd_delete_link->show_message(
    EXPORTING iv_num = '014' ).
    EXIT.
  ELSEIF ( so_pernr IS NOT INITIAL AND pa_all EQ abap_true ).
    lo_dpd_delete_link->show_message(
    EXPORTING iv_num = '015' ).
    EXIT.
  ENDIF.

* Select files.
  lo_dpd_delete_link->select_files( ).

* research broken links to documents.
  lo_dpd_delete_link->retrieve_links( ).

  IF pa_dir EQ abap_true.
*   Delete broken links.
    lo_dpd_delete_link->process_links( ).
*   Toon resultaat.
    IF sy-batch EQ abap_false.
      lo_dpd_delete_link->show_links( EXPORTING iv_fieldcat = 'RESULTS' ).
    ENDIF.
  ELSE.
*   Show broken links
    lo_dpd_delete_link->show_links( iv_fieldcat = 'SHOW' ).
  ENDIF.
