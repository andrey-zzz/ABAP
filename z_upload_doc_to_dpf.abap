*&---------------------------------------------------------------------*
*& Report  ZHR_DPD_UPLOAD_FILES
*&
*&---------------------------------------------------------------------*
*& Author       : Aris van Mazijk (DEVELOPER) for Softwaris.
*& Date         : 20-03-2018
*& Description  : Report to upload files to DPF
*& Marker       :
*& Transport    : ECOK939291
*&---------------------------------------------------------------------*
REPORT z_upload_doc_to_dpf.

INCLUDE z_upload_doc_to_dpf_screen.
INCLUDE z_upload_doc_to_dpf_class.

DATA lo_dpd_upload_files TYPE REF TO z_dpd_upload_files.
DATA ls_return           TYPE bapiret2.

*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT lo_dpd_upload_files.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

* Selecteer DPD van medewerker.
  lo_dpd_upload_files->select_dpd( EXCEPTIONS no_dpd = 1 ).
  IF sy-subrc NE 0.
    lo_dpd_upload_files->show_message( iv_num = '016').
    EXIT.
  ENDIF.

* Selecteer bestanden van lokale schijf.
  lo_dpd_upload_files->select_files( EXCEPTIONS error_select   = 1
                                                error_upload   = 2
                                                error_filename = 3
                                                no_files       = 4 ).
  CASE sy-subrc.
    WHEN '1'.
      lo_dpd_upload_files->show_message( iv_num = '021').
      EXIT.
    WHEN '2'.
      lo_dpd_upload_files->show_message( iv_num = '022').
      EXIT.
    WHEN '3'.
      lo_dpd_upload_files->show_message( iv_num = '023').
      EXIT.
    WHEN '4'.
      lo_dpd_upload_files->show_message( iv_num = '024').
      EXIT.
    WHEN OTHERS.
      "Verder gaan.
  ENDCASE.

*---------------------------------------------------------------------*
* END-OF-SELECTION
*---------------------------------------------------------------------*
END-OF-SELECTION.

  IF pa_test NE abap_true.
*   Upload bestanden naar DPD.
    lo_dpd_upload_files->add_files_to_dpd( EXCEPTIONS dpd_locked = 1 ).

    IF sy-subrc EQ 0.
      lo_dpd_upload_files->show_result( ).
    ELSE.
      lo_dpd_upload_files->show_message( iv_num = '025').
    ENDIF.

  ELSE.
*   Laat up-te-loaden bestanden zien.
    lo_dpd_upload_files->show_files( ).
  ENDIF.
