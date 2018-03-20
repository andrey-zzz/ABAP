*&---------------------------------------------------------------------*
*&  Include           Z_DELETE_DOC_FROM_DPF
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.
PARAMETERS p_anch   TYPE bapianchor.
PARAMETERS p_name   TYPE string.
PARAMETERS p_dir    TYPE boole_d AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b01.
