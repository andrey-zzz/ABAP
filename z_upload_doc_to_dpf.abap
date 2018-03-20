*&---------------------------------------------------------------------*
*& Report  Z_UPLOAD_DOC_TO_DPF
*&
*&---------------------------------------------------------------------*
*& Author       : Aris van Mazijk (DEVELOPER) for Softwaris.
*& Date         : 20-03-2018
*& Description  : Upload documents to DPF
*& Marker       :
*& Transport    : NPLK900025
*&---------------------------------------------------------------------*
report  z_upload_doc_to_dpf.

include z_upload_doc_to_dpf_screen.
include z_upload_doc_to_dpf_class.

data lo_dpf_upload type ref to z_dpf_upload.

*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
initialization.

  create object lo_dpf_upload.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
start-of-selection.

  if pa_batch eq abap_true.

*   Submit this report as job.
    lo_dpf_upload->process_in_background( ).

  else.

*   Read metadata file.
    lo_dpf_upload->read_metadata( ).

*   Check if metadata is present.
    check lo_dpf_upload->gt_meta is not initial.

*   Process documents in batch.
    lo_dpf_upload->process_documents( ).

    if pa_test ne abap_true.
*     Write error log to server.
      lo_dpf_upload->write_log( ).
    else.
*     Write log to screen.
      lo_dpf_upload->write_result( ).
    endif.

  endif.
