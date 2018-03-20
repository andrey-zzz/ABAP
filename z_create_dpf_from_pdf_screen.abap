*&---------------------------------------------------------------------*
*&  Include           Z_CREATE_DPF_FROM_PDF_SCREEN
*&---------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.
parameters pa_meta type rlgrap-filename obligatory default '\\Sapfil01\sap\ECC\ECO\HRM\HRM0800_Pdossier\metadata.csv'.
selection-screen end of block b1.
selection-screen begin of block b2 with frame title text-002.
parameters pa_dir type rlgrap-filename obligatory default '\\Sapfil01\sap\ECC\ECO\HRM\HRM0800_Pdossier\'.
selection-screen end of block b2.
selection-screen begin of block b3 with frame title text-003.
parameters pa_anch type bapianchor default 'ZZZZ'.
parameters pa_rms  type bapisrmdoc-rmsid default 'ASR_DPF'.
parameters pa_sps  type bapisrmdoc-spsid default 'ZASR_SPS_DPD_DOCUMENT'.
parameters pa_mime type bapimimetype default 'application/pdf'.
parameters pa_test type boolean as checkbox default 'X'.
parameters pa_batch type boolean as checkbox default 'X'.
selection-screen end of block b3.
