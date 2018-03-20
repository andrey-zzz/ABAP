*&---------------------------------------------------------------------*
*&  Include           Z_UPLOAD_DOC_TO_DPF
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS pa_pernr TYPE persno.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS pa_anch TYPE bapianchor DEFAULT 'ZZZZ'.
PARAMETERS pa_rms  TYPE bapisrmdoc-rmsid DEFAULT 'ASR_DPF'.
PARAMETERS pa_sps  TYPE bapisrmdoc-spsid DEFAULT 'ZASR_SPS_DPD_DOCUMENT'.
PARAMETERS pa_test TYPE boolean AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.
