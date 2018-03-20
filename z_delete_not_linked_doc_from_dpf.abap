*&---------------------------------------------------------------------*
*& Report  Z_DELETE_NOT_LINKED_DOC_FROM_DPF
*&
*&---------------------------------------------------------------------*
*& Author       : Aris van Mazijk (DEVELOPER) for Softwaris.
*& Date         : 20-03-2018
*& Description  : Report to delete files not linked to a DPF anymore
*& Marker       :
*& Transport    : NPLK900025
*&---------------------------------------------------------------------*
report z_dpf_delete_files.

include z_dpf_delete_files_screen.
include z_dpf_delete_files_class.
include z_dpf_delete_files_alv.

data lo_dpf_delete_files type ref to z_dpf_delete_files.

*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
initialization.

  create object lo_dpf_delete_files.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
start-of-selection.

* Select DPF's.
  lo_dpf_delete_files->select_files( exceptions no_files = 1 error_files = 2 ).

  if sy-subrc eq 1.
    lo_dpf_delete_files->show_message( iv_num = '000').
    exit.
  elseif sy-subrc eq 2.
    lo_dpf_delete_files->show_message( iv_num = '001').
    exit.
  endif.

* Select document links.
  lo_dpf_delete_files->select_document_links( exceptions no_links = 1 error_links = 2 ).

  if sy-subrc eq 1.
    lo_dpf_delete_files->show_message( iv_num = '004').
    exit.
  elseif sy-subrc eq 2.
    lo_dpf_delete_files->show_message( iv_num = '005').
    exit.
  endif.

* Select documents.
  lo_dpf_delete_files->select_documents( exceptions no_docs = 1 ).

  if sy-subrc eq 1.
    lo_dpf_delete_files->show_message( iv_num = '002').
    exit.
  endif.

*---------------------------------------------------------------------*
* END-OF-SELECTION
*---------------------------------------------------------------------*
end-of-selection.

* Compare DPD documents with content server.
  lo_dpf_delete_files->compare_files( exceptions no_files = 1 ).

  if sy-subrc eq 1.
    lo_dpf_delete_files->show_message( iv_num = '006').
    exit.
  endif.

  if pa_dir eq abap_true.

*   Delete documents.
    lo_dpf_delete_files->process_files( ).

*   Show results on screen.
    lo_dpf_delete_files->show_result( ).

  else.
*   Show to be deleted documents on screen.
    lo_dpf_delete_files->show_files( ).
  endif.
