*&---------------------------------------------------------------------*
*& Report  ZHR_ASR_DELETE_DOCUMENTS
*&
*&---------------------------------------------------------------------*
*& Author       : Aris van Mazijk (DEVELOPER)
*& Date         : 20-03-2018
*& Description  : Report to delete docs from DPF
*& Marker       :
*& Transport    : NPLK900025
*&---------------------------------------------------------------------*
REPORT z_delete_doc_from_dpf.

INCLUDE z_delete_doc_from_dpf_screen.
INCLUDE z_delete_doc_from_dpf_class. "Local class definition.
INCLUDE z_delete_doc_from_dpf_alv.

TABLES pernr.

DATA lo_delete_documents  TYPE REF TO lcl_delete_documents.

*----------------------------------------------------------------------*
* INITIALIZATION.                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT lo_delete_documents.

*----------------------------------------------------------------------*
* START-OF-SELECTION.                                                  *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  GET pernr.

  lo_delete_documents->get_files( EXPORTING ip_pernr = pernr-pernr ).

*----------------------------------------------------------------------*
* END-OF-SELECTION.                                                  *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  lo_delete_documents->get_documents( EXCEPTIONS no_documents = 1 ).

  IF sy-subrc EQ 1.
    lcl_delete_documents=>show_message( iv_num = '002' ).
    EXIT.
  ENDIF.

  IF p_dir EQ abap_true.
    lo_delete_documents->process_documents( ).
    lo_delete_documents->show_result( ).
  ELSE.
    lo_delete_documents->show_documents( ).
  ENDIF.
