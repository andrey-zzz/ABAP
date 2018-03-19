*&---------------------------------------------------------------------*
*& Report  Z_UPLOAD_CSV_GUI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_upload_csv_gui.

TYPES: BEGIN OF ttab,
        rec(1000) TYPE c,
       END OF ttab,

       BEGIN OF tdat,
        fld1(5)   TYPE c,
        fld2(5)   TYPE c,
        fld3(10)  TYPE c,
        fld4(1)   TYPE c,
        fld5(2)   TYPE c,
        fld6(50)  TYPE c,
       END OF tdat.

DATA lt_itab  TYPE STANDARD TABLE OF ttab.
DATA lt_idat  TYPE STANDARD TABLE OF znmwr_cust.
DATA file_str TYPE string.

FIELD-SYMBOLS: <ls_itab> TYPE ttab.
FIELD-SYMBOLS: <ls_idat> TYPE znmwr_cust.

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
  LOOP AT lt_itab ASSIGNING <ls_itab>.
    APPEND INITIAL LINE TO lt_idat ASSIGNING <ls_idat>.
    SPLIT <ls_itab>-rec AT ';' INTO
    <ls_idat>-grpid
    <ls_idat>-semid
    <ls_idat>-gsval
    <ls_idat>-con_sign
    <ls_idat>-con_option
    <ls_idat>-extext.
  ENDLOOP.

  INSERT znmwr_cust FROM TABLE lt_idat.
