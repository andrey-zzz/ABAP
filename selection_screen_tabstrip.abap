*&---------------------------------------------------------------------*
*&  Include           Z_TABSTRIP_REPORT_SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS pa_pernr TYPE persno OBLIGATORY.
PARAMETERS pa_begda TYPE datum  OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS mon1 TYPE sollz.
PARAMETERS tue1 TYPE sollz.
PARAMETERS wed1 TYPE sollz.
PARAMETERS thu1 TYPE sollz.
PARAMETERS fri1 TYPE sollz.
PARAMETERS sat1 TYPE sollz.
PARAMETERS sun1 TYPE sollz.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF SCREEN 100.

SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETERS mon2 TYPE sollz.
PARAMETERS tue2 TYPE sollz.
PARAMETERS wed2 TYPE sollz.
PARAMETERS thu2 TYPE sollz.
PARAMETERS fri2 TYPE sollz.
PARAMETERS sat2 TYPE sollz.
PARAMETERS sun2 TYPE sollz.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF SCREEN 200.

SELECTION-SCREEN BEGIN OF SCREEN 300 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME.
PARAMETERS mon3 TYPE sollz.
PARAMETERS tue3 TYPE sollz.
PARAMETERS wed3 TYPE sollz.
PARAMETERS thu3 TYPE sollz.
PARAMETERS fri3 TYPE sollz.
PARAMETERS sat3 TYPE sollz.
PARAMETERS sun3 TYPE sollz.
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN END OF SCREEN 300.

SELECTION-SCREEN BEGIN OF SCREEN 400 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME.
PARAMETERS mon4 TYPE sollz.
PARAMETERS tue4 TYPE sollz.
PARAMETERS wed4 TYPE sollz.
PARAMETERS thu4 TYPE sollz.
PARAMETERS fri4 TYPE sollz.
PARAMETERS sat4 TYPE sollz.
PARAMETERS sun4 TYPE sollz.
SELECTION-SCREEN END OF BLOCK b5.
SELECTION-SCREEN END OF SCREEN 400.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK mytab FOR 9 LINES,
                  TAB (20) button1 USER-COMMAND push1,
                  TAB (20) button2 USER-COMMAND push2,
                  TAB (20) button3 USER-COMMAND push3,
                  TAB (20) button4 USER-COMMAND push4
                  DEFAULT SCREEN 100,
                  END OF BLOCK mytab.
