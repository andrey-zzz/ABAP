*&---------------------------------------------------------------------*
*& Report  ZANDBAK_ARIS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zandbak_aris.

TYPES: BEGIN OF ttab,
        rec(1000) TYPE c,
       END OF ttab.

DATA lt_itab   TYPE TABLE OF ttab.
DATA ls_itab   TYPE ttab.
DATA lt_idat   TYPE TABLE OF t100.
DATA ls_idat   TYPE t100.
DATA file_str  TYPE string.
DATA lv_regex  TYPE string VALUE '"'.
DATA lv_length TYPE i.

*--------------------------------------------------*
* selection screen design
*-------------------------------------------------*
PARAMETERS: p_file TYPE localfile.

*--------------------------------------------------*
* at selection screen for field
*-------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
    CHANGING
      file_name = p_file.

*--------------------------------------------------*
* start of selection
*-------------------------------------------------*
START-OF-SELECTION.

  file_str = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = file_str
    TABLES
      data_tab                = lt_itab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

*--------------------------------------------------*
* process and display output
*-------------------------------------------------*
  LOOP AT lt_itab INTO ls_itab.
    CLEAR ls_idat.

    ls_idat-sprsl = 'E'.
    ls_idat-arbgb = 'Z_SELF_SERVICE'.

    SPLIT ls_itab-rec AT ',' INTO
    ls_idat-msgnr
    ls_idat-text.

    lv_length = strlen( ls_idat-msgnr ).

    IF lv_length < 3.
      SHIFT ls_idat-msgnr RIGHT DELETING TRAILING space.
      OVERLAY ls_idat-msgnr WITH '000'.
    ENDIF.
    REPLACE ALL OCCURRENCES OF REGEX lv_regex IN ls_idat-text WITH ''.
    APPEND ls_idat TO lt_idat.
  ENDLOOP.

  INSERT t100 FROM TABLE lt_idat.

  COMMIT WORK.

  CLEAR ls_idat.

  LOOP AT lt_idat INTO ls_idat.
    WRITE:/ ls_idat-sprsl, ls_idat-arbgb, ls_idat-msgnr, ls_idat-text.
    CLEAR ls_idat.
  ENDLOOP.
