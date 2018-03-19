*&---------------------------------------------------------------------*
*& Report  Z_DOWNLOAD_DOC_FROM_DPF
*&---------------------------------------------------------------------*
*& Author       : Aris van Mazijk (DEVELOPER)
*& Date         : 19-03-2018
*& Description  : Report for downloading docs from DPF
*& Marker       :
*& Transport    : NPLKK900022
*&---------------------------------------------------------------------*
REPORT z_download_doc_from_dpf.

INCLUDE z_download_doc_from_dpf_screen.
INCLUDE z_download_doc_from_dpf_class.

DATA lo_dpd_download_files TYPE REF TO zhr_dpd_download_files.

*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT lo_dpd_download_files.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

* Selecteer DPD's and bestanden.
  lo_dpd_download_files->select_files( EXCEPTIONS no_dpd   = 1
                                                  no_files = 2 ).

  IF sy-subrc EQ 1.
    lo_dpd_download_files->show_message( iv_num = '016').
    EXIT.
  ELSEIF sy-subrc EQ 2.
    lo_dpd_download_files->show_message( iv_num = '018').
    EXIT.
  ENDIF.

*---------------------------------------------------------------------*
* END-OF-SELECTION
*---------------------------------------------------------------------*
END-OF-SELECTION.

  IF pa_test NE abap_true.
*   Download bestanden in ZIP file.
    lo_dpd_download_files->download_files( EXCEPTIONS error_location = 1
                                                      error_download = 2 ).

    IF sy-subrc EQ 1.
      lo_dpd_download_files->show_message( iv_num = '019').
      EXIT.
    ELSEIF sy-subrc EQ 2.
      lo_dpd_download_files->show_message( iv_num = '020').
      EXIT.
    ENDIF.

*   Laat resultaat zien op scherm.
    lo_dpd_download_files->show_result( ).

  ELSE.
*   Laat te downloaden bestanden zien op scherm.
    lo_dpd_download_files->show_files( ).
  ENDIF.
