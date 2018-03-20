*&---------------------------------------------------------------------*
*& Report TEXTFILE_UPLOAD
*&---------------------------------------------------------------------*
*& Author       : Aris van Mazijk (DEVELOPER)
*& Date         : 20-03-2018
*& Description  : Upload and process text file
*& Marker       :
*& Transport    :
*&---------------------------------------------------------------------*
REPORT textfile_upload.

INCLUDE z_upload_text_file_bapi_process_screen.
INCLUDE z_upload_text_file_bapi_process_class.

DATA lcl_textfile_upload TYPE REF TO textfile_upload.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.

* Select textfile file from GUI.
  pa_file = textfile_upload=>select_file( ).

*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.

* Initialize local class.
  CREATE OBJECT lcl_textfile_upload.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

* Read selected textfile file into system.
  lcl_textfile_upload->file_upload( ).

* Process textfile file into structures.
  lcl_textfile_upload->file_process( ).

* Post header & line items wih BAPI
  lcl_textfile_upload->document_post( ).

* Display result of posting.
  lcl_textfile_upload->display_result( ).
