*&---------------------------------------------------------------------*
*& Include          Z_UPLOAD_TEXT_FILE_BAPI_PROCESS_SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS pa_file TYPE localfile.
SELECTION-SCREEN SKIP.
PARAMETERS pa_test TYPE boolean AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.
