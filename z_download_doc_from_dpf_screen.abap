*&---------------------------------------------------------------------*
*&  Include           Z_DOWNLOAD_DOC_FROM_DPF_SCREEN
*&---------------------------------------------------------------------*
TABLES p0001.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS so_pernr FOR p0001-pernr.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS pa_test TYPE boolean AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.
