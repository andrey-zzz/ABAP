*&---------------------------------------------------------------------*
*& Include          Z_PROXY_CALLS_SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1.                  "#EC CI_USE_WANTED
SELECT-OPTIONS: so_date FOR sy-datum.                "#EC CI_USE_WANTED
SELECTION-SCREEN SKIP.                               "#EC CI_USE_WANTED
PARAMETERS: pa_local RADIOBUTTON GROUP r1,           "#EC CI_USE_WANTED
            pa_file  TYPE dxfilename,                "#EC CI_USE_WANTED
            pa_ftp   RADIOBUTTON GROUP r1 DEFAULT 'X'. "#EC CI_USE_WANTED
SELECTION-SCREEN END OF BLOCK b1.                    "#EC CI_USE_WANTED
