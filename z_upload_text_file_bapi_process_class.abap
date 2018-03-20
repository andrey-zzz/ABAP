*&---------------------------------------------------------------------*
*& Include          Z_UPLOAD_TEXT_FILE_BAPI_PROCESS_CLASS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS textfile_upload DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS textfile_upload DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS select_file RETURNING VALUE(rv_file) TYPE localfile.

    METHODS file_upload.
    METHODS file_process.
    METHODS document_post.
    METHODS display_result.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_file,
             line TYPE string,
           END OF ty_file,

           tt_file      TYPE TABLE OF ty_file,
           tt_accountgl TYPE TABLE OF bapiacgl09 WITH DEFAULT KEY,
           tt_currency  TYPE TABLE OF bapiaccr09 WITH DEFAULT KEY,
           tt_results   TYPE TABLE OF bapiret2 WITH DEFAULT KEY.

    DATA textfile_file    TYPE tt_file.
    DATA documentheader    TYPE bapiache09.
    DATA accountgl         TYPE TABLE OF bapiacgl09.
    DATA currency          TYPE TABLE OF bapiaccr09.
    DATA results           TYPE TABLE OF bapiret2.

    CONSTANTS co_upload_title TYPE string VALUE 'Select textfile file'.
    CONSTANTS co_default_ext  TYPE string VALUE 'TXT'.

    METHODS set_documentheader  IMPORTING iv_documentheader        TYPE bapiache09.
    METHODS set_accountgl       IMPORTING iv_accountgl             TYPE tt_accountgl.
    METHODS set_currency        IMPORTING iv_currency              TYPE tt_currency.
    METHODS set_textfile_file   IMPORTING iv_textfile_file        TYPE tt_file.
    METHODS set_results         IMPORTING iv_results               TYPE tt_results.
    METHODS get_documentheader  RETURNING VALUE(rv_documentheader) TYPE bapiache09.
    METHODS get_accountgl       RETURNING VALUE(rv_accountgl)      TYPE tt_accountgl.
    METHODS get_currency        RETURNING VALUE(rv_currency)       TYPE tt_currency.
    METHODS get_results         RETURNING VALUE(rv_results)        TYPE tt_results.
    METHODS add_leading_zeros   CHANGING cv_value TYPE any.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS textfile_upload DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS textfile_upload IMPLEMENTATION.

  METHOD display_result.

    DATA lt_return  TYPE TABLE OF bapiret2.
    DATA lv_type    TYPE string.
    DATA lv_message TYPE string.

    FIELD-SYMBOLS <ls_return> TYPE bapiret2.

    lt_return = get_results( ).

    LOOP AT lt_return ASSIGNING <ls_return>.

      CLEAR lv_type.
      CLEAR lv_message.

      CASE <ls_return>-type.
        WHEN 'E'.
          lv_type = 'Error'.
        WHEN 'W'.
          lv_type = 'Warning'.
        WHEN 'S'.
          lv_type = 'Success'.
        WHEN OTHERS.
      ENDCASE.

      CONCATENATE lv_type '-' <ls_return>-message INTO lv_message
      SEPARATED BY space.
      WRITE:/ lv_message.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_leading_zeros.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cv_value
      IMPORTING
        output = cv_value.

  ENDMETHOD.

  METHOD document_post.

    DATA lt_accountgl       TYPE TABLE OF bapiacgl09.
    DATA lt_currency        TYPE TABLE OF bapiaccr09.
    DATA ls_documentheader  TYPE bapiache09.
    DATA lt_return          TYPE TABLE OF bapiret2.

    lt_accountgl      = get_accountgl( ).
    lt_currency       = get_currency( ).
    ls_documentheader = get_documentheader( ).

    CALL FUNCTION 'ZBAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_documentheader
        testrun        = pa_test
      TABLES
        accountgl      = lt_accountgl
        currencyamount = lt_currency
        return         = lt_return.

    set_results( iv_results = lt_return ).

  ENDMETHOD.

  METHOD file_process.

    DATA ls_documentheader TYPE bapiache09.
    DATA lt_accountgl      TYPE TABLE OF bapiacgl09.
    DATA lt_currency       TYPE TABLE OF bapiaccr09.
    DATA lv_date(8)        TYPE c.
    DATA lv_counter        TYPE i.

    FIELD-SYMBOLS <ls_textfile_file> TYPE ty_file.
    FIELD-SYMBOLS <ls_accountgl>      TYPE bapiacgl09.
    FIELD-SYMBOLS <ls_currency>       TYPE bapiaccr09.

    LOOP AT textfile_file ASSIGNING <ls_textfile_file>.

*     Process header
      IF <ls_textfile_file>-line(1) EQ '1'.

        ls_documentheader-bus_act     = 'RFBU'.
        ls_documentheader-username    = sy-uname.
        ls_documentheader-comp_code   = <ls_textfile_file>-line+7(4).
        ls_documentheader-fisc_year   = <ls_textfile_file>-line+1(4).
        ls_documentheader-fis_period  = <ls_textfile_file>-line+5(2).
        ls_documentheader-doc_type    = 'ZW'.

        CLEAR lv_date.
        CONCATENATE <ls_textfile_file>-line+15(4) <ls_textfile_file>-line+13(2)
        <ls_textfile_file>-line+11(2) INTO lv_date.

        WRITE lv_date TO ls_documentheader-doc_date.
        WRITE lv_date TO ls_documentheader-pstng_date.

*     Process line item.
      ELSEIF <ls_textfile_file>-line(1) EQ '2'.

        lv_counter = lv_counter + 1.

        APPEND INITIAL LINE TO lt_accountgl ASSIGNING <ls_accountgl>.
        <ls_accountgl>-itemno_acc = lv_counter.

        <ls_accountgl>-gl_account = <ls_textfile_file>-line+19(9).
        add_leading_zeros( CHANGING cv_value = <ls_accountgl>-gl_account ).

        CONCATENATE <ls_textfile_file>-line+47(4) <ls_textfile_file>-line+51(20)
        INTO <ls_accountgl>-item_text SEPARATED BY space.
        <ls_accountgl>-comp_code  = <ls_textfile_file>-line+7(4).
        <ls_accountgl>-fis_period = <ls_textfile_file>-line+5(2).
        <ls_accountgl>-fisc_year  = <ls_textfile_file>-line+1(4).
        <ls_accountgl>-alloc_nmbr = <ls_textfile_file>-line+114(12).

        <ls_accountgl>-costcenter = <ls_textfile_file>-line+37(10).
        add_leading_zeros( CHANGING cv_value = <ls_accountgl>-costcenter ).

        CLEAR lv_date.
        CONCATENATE <ls_textfile_file>-line+15(4) <ls_textfile_file>-line+13(2)
        <ls_textfile_file>-line+11(2) INTO lv_date.

        WRITE lv_date TO <ls_accountgl>-pstng_date.

        APPEND INITIAL LINE TO lt_currency ASSIGNING <ls_currency>.
        <ls_currency>-itemno_acc  = lv_counter.
        <ls_currency>-currency    = <ls_textfile_file>-line+91(3).
        <ls_currency>-amt_doccur  = <ls_textfile_file>-line+94(13).

      ENDIF.

    ENDLOOP.

    set_documentheader( ls_documentheader ).
    set_accountgl( lt_accountgl ).
    set_currency( lt_currency ).

  ENDMETHOD.

  METHOD file_upload.

    DATA lv_filestring TYPE string.
    DATA lt_filetab    TYPE tt_file.

    lv_filestring = pa_file.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = lv_filestring
      CHANGING
        data_tab                = lt_filetab
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
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.

    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ELSE.
      set_textfile_file( lt_filetab[] ).
    ENDIF.

  ENDMETHOD.

  METHOD select_file.

    DATA lv_rc                TYPE i.
    DATA lt_filetable         TYPE filetable.

    FIELD-SYMBOLS <ls_file>   TYPE file_table.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = co_upload_title
        default_extension       = co_default_ext
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ELSE.
      READ TABLE lt_filetable ASSIGNING <ls_file> INDEX 1.
      IF sy-subrc EQ 0.
        rv_file = <ls_file>.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD set_documentheader.

    documentheader = iv_documentheader.

  ENDMETHOD.

  METHOD set_accountgl.

    accountgl = iv_accountgl.

  ENDMETHOD.

  METHOD set_currency.

    currency = iv_currency.

  ENDMETHOD.

  METHOD set_textfile_file.

    textfile_file[] = iv_textfile_file[].

  ENDMETHOD.

  METHOD get_documentheader.

    rv_documentheader = documentheader.

  ENDMETHOD.

  METHOD get_accountgl.

    rv_accountgl[] = accountgl[].

  ENDMETHOD.

  METHOD get_currency.

    rv_currency[] = currency[].

  ENDMETHOD.

  METHOD get_results.

    rv_results = results.

  ENDMETHOD.

  METHOD set_results.

    results = iv_results.

  ENDMETHOD.

ENDCLASS.
