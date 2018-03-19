*&---------------------------------------------------------------------*
*& Include          Z_PROXY_CALLS_CLASS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Klassedefinitie
*&---------------------------------------------------------------------*
CLASS lcl_proxy_call DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_dates,
             low  TYPE sy-datum,
             high TYPE sy-datum,
           END OF ty_dates,
           dates_tab TYPE STANDARD TABLE OF ty_dates.

    METHODS constructor.
    CLASS-METHODS select_file.

  PRIVATE SECTION.

    CONSTANTS gc_delta      TYPE char5     VALUE 'DELTA'.
    CONSTANTS gc_full       TYPE char5     VALUE 'FULL'.
    CONSTANTS gc_object     TYPE balobj_d  VALUE 'ZINTERFACE'.
    CONSTANTS gc_subobject  TYPE balsubobj VALUE 'PROXY_CALLS'.
    CONSTANTS gc_msg_start  TYPE symsgno   VALUE '001'.
    CONSTANTS gc_msgid      TYPE symsgid   VALUE 'ZMC_PROXY_CALL'.
    CONSTANTS gc_probclass  TYPE balprobcl VALUE '5'.
    CONSTANTS gc_msg_succes TYPE symsgty   VALUE 'S'.
    CONSTANTS gc_msg_error  TYPE symsgty   VALUE 'E'.
    CONSTANTS gc_msg_end    TYPE symsgno   VALUE '002'.

    TYPES:BEGIN OF ty_kaart,
            line TYPE string,
          END OF ty_kaart.

    DATA int_query_data TYPE zxi_get_all_coupled_cards__tab.
    DATA ins_kaart_data TYPE zxi_kaart_out1.
    DATA inv_aantal     TYPE i.
    DATA inv_filename   TYPE string.
    DATA inv_batch      TYPE i.
    DATA inv_datum      TYPE sy-datum.
    DATA inv_tijd       TYPE sy-uzeit.
    DATA inv_methode    TYPE char5.
    DATA ino_logger      TYPE REF TO cl_sbal_logger.

    METHODS call_iss_service  IMPORTING ip_dates  TYPE ty_dates.
    METHODS data_to_output.
    METHODS write_local_file.
    METHODS call_ftp_interface.
    METHODS process_pakbon.
    METHODS set_query_data    IMPORTING ip_query_data TYPE zxi_get_all_coupled_cards__tab.
    METHODS get_query_data    EXPORTING ep_query_data TYPE zxi_get_all_coupled_cards__tab.
    METHODS set_kaart_data    IMPORTING ip_kaart_data TYPE zxi_kaart_out1.
    METHODS get_kaart_data    EXPORTING ep_kaart_data TYPE zxi_kaart_out1.
    METHODS set_aantal        IMPORTING ip_aantal     TYPE string.
    METHODS get_aantal        EXPORTING ep_aantal     TYPE string.
    METHODS set_filename      IMPORTING ip_name       TYPE string.
    METHODS get_filename      EXPORTING ep_name       TYPE string.
    METHODS maak_filename.
    METHODS get_tabel.
    METHODS set_tabel.
    METHODS get_methode       EXPORTING ep_methode TYPE char5.
    METHODS set_methode       IMPORTING ip_methode TYPE char5.
    METHODS get_batch         EXPORTING ep_batch   TYPE i.
    METHODS set_batch         IMPORTING ip_batch   TYPE i.
    METHODS get_datum         EXPORTING ep_datum   TYPE sy-datum.
    METHODS set_datum         IMPORTING ip_datum   TYPE sy-datum.
    METHODS get_tijd          EXPORTING ep_tijd    TYPE sy-uzeit.
    METHODS set_tijd          IMPORTING ip_tijd    TYPE sy-uzeit.
    METHODS start_log.
    METHODS end_log.
    METHODS get_dates         EXPORTING ep_dates  TYPE dates_tab.
    METHODS log_message       IMPORTING ip_msgid  TYPE symsgid   DEFAULT gc_msgid
                                        ip_msgno  TYPE symsgno
                                        ip_probcl TYPE balprobcl DEFAULT gc_probclass
                                        ip_msgty  TYPE symsgty   DEFAULT gc_msg_succes
                                        ip_msgv1  TYPE string    OPTIONAL
                                        ip_msgv2  TYPE string    OPTIONAL
                                        ip_msgv3  TYPE string    OPTIONAL
                                        ip_msgv4  TYPE string    OPTIONAL.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Klasse implementatie
*&---------------------------------------------------------------------*
CLASS lcl_proxy_call IMPLEMENTATION.

  METHOD constructor.

    me->start_log( ).
    me->get_dates( IMPORTING ep_dates = DATA(lt_dates) ).

    LOOP AT lt_dates ASSIGNING FIELD-SYMBOL(<ls_dates>).
*     Lees tabelparameters.
      me->get_tabel( ).
*     Query service aanroepen.
      me->call_iss_service( ip_dates = <ls_dates> ).
*     Query data naar output converteren.
      me->data_to_output( ).
*     Lokaal of via FTP wegschrijven?
      IF pa_local = abap_true.
        me->write_local_file( ).
      ELSE.
*       Maak bestandsnaam op.
        me->maak_filename( ).
*       Roep databestand interface aan.
        me->call_ftp_interface( ).
*       Roep pakbon interface aan.
        me->process_pakbon( ).
*       Schrijf tabelparameters weg.
        me->set_tabel( ).
      ENDIF.
    ENDLOOP.

    me->end_log( ).

  ENDMETHOD.

  METHOD call_iss_service.

    DATA lv_startdate     TYPE string.
    DATA lv_enddate       TYPE string.

*   SAP PI interface aanroepen (SOAP->FTP).
    DO 3 TIMES.
      TRY.
          DATA(lo_query_proxy) = NEW zxi_co_proxy_calls_quer( ).
          lo_query_proxy->proxy_calls_query_out( EXPORTING output = VALUE #( get_all_coupled_cards_request-begin_date = ip_dates-low
                                                                                     get_all_coupled_cards_request-end_date   = ip_dates-high )
                                                         IMPORTING input  = DATA(ls_response) ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        CATCH cx_ai_system_fault INTO DATA(lx_exception).
          me->log_message( ip_msgno = '018' ip_msgty = gc_msg_error ip_msgv1 = lx_exception->get_text( ) ).
      ENDTRY.

      IF ls_response-get_all_coupled_cards_response-coupled_cards-coupled_card IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

    me->log_message( ip_msgno = '017' ).

    lv_startdate = ip_dates-low.
    lv_enddate = ip_dates-high.

    IF ls_response-get_all_coupled_cards_response-coupled_cards-coupled_card IS INITIAL.
      me->log_message( ip_msgno = '006' ip_msgty = gc_msg_error ip_msgv1 = lv_startdate ip_msgv2 = lv_enddate ).
    ELSE.
      me->set_query_data( ls_response-get_all_coupled_cards_response-coupled_cards-coupled_card ).
    ENDIF.

  ENDMETHOD.

  METHOD data_to_output.

    DATA lt_kaart_data    TYPE zxi_kaart_out_kaarten_tab.
    DATA ls_kaart_output  TYPE zxi_kaart_out1.
    DATA lv_aantal        TYPE i.

    get_query_data( IMPORTING ep_query_data = DATA(lt_query_data) ).

    CLEAR lv_aantal.

*   Query data naar proxy output datatype converteren.
    LOOP AT lt_query_data ASSIGNING FIELD-SYMBOL(<fs_query_data>).
      APPEND INITIAL LINE TO lt_kaart_data ASSIGNING FIELD-SYMBOL(<fs_kaart_data>).
      <fs_kaart_data>-engraved_id  = <fs_query_data>-engraved_id.
      <fs_kaart_data>-person_id    = <fs_query_data>-person_id.
      <fs_kaart_data>-coupled_date = <fs_query_data>-coupled_date.
      lv_aantal = lv_aantal + 1.
    ENDLOOP.

    INSERT INITIAL LINE INTO lt_kaart_data ASSIGNING <fs_kaart_data> INDEX 1.
    <fs_kaart_data>-engraved_id  = |ENGRAVED_ID|.
    <fs_kaart_data>-person_id    = |PERSON_ID|.
    <fs_kaart_data>-coupled_date = |KOPPEL_DATUM|.

    ls_kaart_output-kaart_out-kaarten = lt_kaart_data.

    me->set_kaart_data( ip_kaart_data = ls_kaart_output ).
    me->set_aantal( ip_aantal = CONV string( lv_aantal ) ).
    me->log_message( ip_msgno = '008' ).

  ENDMETHOD.

  METHOD write_local_file.

    DATA lv_filename TYPE string.
    DATA lt_kaarten  TYPE TABLE OF ty_kaart.

    me->get_kaart_data( IMPORTING ep_kaart_data = DATA(ls_kaart) ).

*   Huidig data in CSV bestand gieten.
    LOOP AT ls_kaart-kaart_out-kaarten ASSIGNING FIELD-SYMBOL(<ls_kaart>).
      APPEND INITIAL LINE TO lt_kaarten ASSIGNING FIELD-SYMBOL(<ls_line>).
      CONCATENATE <ls_kaart>-engraved_id <ls_kaart>-person_id <ls_kaart>-coupled_date
      INTO <ls_line>-line SEPARATED BY '|'.
    ENDLOOP.

    lv_filename = pa_file.

*   CSV bestand naar front-end wegschrijven.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename = lv_filename
      TABLES
        data_tab = lt_kaarten
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc <> 0.
      me->log_message( ip_msgno = '003' ip_msgty = gc_msg_error ).
    ELSE.
      me->log_message( ip_msgno = '004' ).
    ENDIF.

  ENDMETHOD.

  METHOD call_ftp_interface.

    me->get_kaart_data( IMPORTING ep_kaart_data = DATA(ls_kaart) ).
    me->get_filename( IMPORTING ep_name = DATA(lv_filename) ).

    APPEND INITIAL LINE TO ls_kaart-kaart_out-bestandsnaam
    ASSIGNING FIELD-SYMBOL(<fs_bestandsnaam>).
    <fs_bestandsnaam>-filename = lv_filename.

*   SAP PI interface aanroepen (SOAP->FTP).
    TRY.
        DATA(lo_kaart_proxy) = NEW zxi_co_proxy_calls_out( ).
        lo_kaart_proxy->proxy_calls_out( ls_kaart ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      CATCH cx_ai_system_fault INTO DATA(lx_exception).
        me->log_message( ip_msgno = '011' ip_msgty = gc_msg_error ip_msgv1 = lx_exception->get_text( ) ).
    ENDTRY.

    me->log_message( ip_msgno = '010' ).

  ENDMETHOD.

  METHOD process_pakbon.

    DATA ls_pakbon       TYPE zxi_pakbon_out1.
    DATA ls_line         TYPE zxi_pakbon_out_lines.
    DATA lv_batch_txt(5) TYPE c.

    me->get_datum( IMPORTING ep_datum = DATA(lv_datum) ).
    me->get_tijd( IMPORTING ep_tijd = DATA(lv_tijd) ).
    CONCATENATE lv_datum lv_tijd INTO DATA(lv_datumtijd).

    me->get_filename( IMPORTING ep_name = DATA(lv_filename) ).
    APPEND INITIAL LINE TO ls_pakbon-pakbon_out-bestandsnaam
    ASSIGNING FIELD-SYMBOL(<fs_bestandsnaam>).
    <fs_bestandsnaam>-filename = lv_filename.

    me->get_batch( IMPORTING ep_batch = DATA(lv_batch) ).
    lv_batch_txt = lv_batch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_batch_txt
      IMPORTING
        output = lv_batch_txt.

    CONCATENATE 'Batchnummer' 'DatumExport' 'AantalBestanden' 'AantalRijen' INTO ls_line-line
    SEPARATED BY '|'.
    APPEND ls_line TO ls_pakbon-pakbon_out-lines.

    me->get_aantal( IMPORTING ep_aantal = DATA(lv_aantal) ).

    CONCATENATE lv_batch_txt lv_datumtijd '1' lv_aantal INTO ls_line-line
    SEPARATED BY '|'.
    APPEND ls_line TO ls_pakbon-pakbon_out-lines.

*   SAP PI interface aanroepen (SOAP->FTP).
    TRY.
        DATA(lo_pakbon_proxy) = NEW zxi_co_pakbon_out( ).
        lo_pakbon_proxy->pakbon_out( ls_pakbon ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      CATCH cx_ai_system_fault INTO DATA(lx_exception).
        me->log_message( ip_msgno = '012' ip_msgty = gc_msg_error ip_msgv1 = lx_exception->get_text( ) ).
    ENDTRY.

    me->log_message( ip_msgno = '013' ).

  ENDMETHOD.

  METHOD set_query_data.

    int_query_data = ip_query_data.

  ENDMETHOD.

  METHOD get_query_data.

    ep_query_data = int_query_data.

  ENDMETHOD.

  METHOD select_file.

    DATA lv_filtmp TYPE dxlpath.

    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = 'P'
      IMPORTING
        o_path          = lv_filtmp.

    pa_file = lv_filtmp.

  ENDMETHOD.

  METHOD set_kaart_data.

    ins_kaart_data = ip_kaart_data.

  ENDMETHOD.

  METHOD get_kaart_data.

    ep_kaart_data = ins_kaart_data.

  ENDMETHOD.

  METHOD set_aantal.

    inv_aantal = ip_aantal.

  ENDMETHOD.

  METHOD get_aantal.

    ep_aantal = inv_aantal.

  ENDMETHOD.

  METHOD set_filename.

    inv_filename = ip_name.

  ENDMETHOD.

  METHOD get_filename.

    ep_name = inv_filename.

  ENDMETHOD.

  METHOD maak_filename.

    DATA lv_batch_txt(5) TYPE c.

    me->get_methode( IMPORTING ep_methode = DATA(lv_methode) ).
    me->get_batch( IMPORTING ep_batch = DATA(lv_batch) ).

    lv_batch_txt = lv_batch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_batch_txt
      IMPORTING
        output = lv_batch_txt.

    CONCATENATE lv_batch_txt '_ISS_ProxyCalls_' lv_methode '_' sy-datum '_'
    sy-uzeit INTO DATA(lv_filename).

    me->set_filename( ip_name = lv_filename ).
    me->set_datum( ip_datum = sy-datum ).
    me->set_tijd( ip_tijd = sy-uzeit ).
    me->log_message( ip_msgno = '009' ).

  ENDMETHOD.

  METHOD get_tabel.

    SELECT MAX( batch ), MAX( datum )
      FROM zgekoppeld
      INTO (@DATA(lv_batch_oud), @DATA(lv_datum)). "#EC CI_SEL_NESTED "#EC CI_NOWHERE

    IF sy-subrc = 0.
      DATA(lv_batch) = lv_batch_oud + 1.
    ELSE.
      lv_datum = sy-datum.
      lv_batch = 1.
    ENDIF.

    set_batch( ip_batch = lv_batch ).

    me->log_message( ip_msgno = '005' ).
    me->log_message( ip_msgno = '016' ip_msgv1 = CONV string( lv_batch_oud )
                                      ip_msgv2 = CONV string( lv_datum ) ).

  ENDMETHOD.

  METHOD set_tabel.

    DATA ls_gekoppeld TYPE zgekoppeld.

    get_batch( IMPORTING ep_batch = ls_gekoppeld-batch ).
    get_datum( IMPORTING ep_datum = ls_gekoppeld-datum ).
    INSERT INTO zgekoppeld VALUES ls_gekoppeld.     "#EC CI_IMUD_NESTED

    IF sy-subrc = 0.
      me->log_message( ip_msgno = '014' ip_msgv1 = CONV string( ls_gekoppeld-batch )
                                        ip_msgv2 = CONV string( ls_gekoppeld-datum ) ).
    ELSE.
      me->log_message( ip_msgno = '015' ).
    ENDIF.

  ENDMETHOD.

  METHOD set_methode.

    inv_methode = ip_methode.

  ENDMETHOD.

  METHOD get_methode.

    ep_methode = inv_methode.

  ENDMETHOD.

  METHOD set_batch.

    inv_batch = ip_batch.

  ENDMETHOD.

  METHOD get_batch.

    ep_batch = inv_batch.

  ENDMETHOD.

  METHOD set_datum.

    inv_datum = ip_datum.

  ENDMETHOD.

  METHOD get_datum.

    ep_datum = inv_datum.

  ENDMETHOD.

  METHOD set_tijd.

    inv_tijd = ip_tijd.

  ENDMETHOD.

  METHOD get_tijd.

    ep_tijd = inv_tijd.

  ENDMETHOD.

  METHOD start_log.

    IF ino_logger IS NOT INITIAL.
      RETURN.
    ENDIF.

    ino_logger = NEW #( i_category    = gc_object
                       i_subcategory = gc_subobject ).

    IF ino_logger IS NOT INITIAL.
      me->log_message( ip_msgno = gc_msg_start ).
    ENDIF.

  ENDMETHOD.

  METHOD log_message.

    IF ino_logger IS INITIAL.
      me->start_log( ).
    ENDIF.

    IF ino_logger IS NOT INITIAL.
      ino_logger->if_logger~add_message( i_msgid  = ip_msgid
                                        i_msgno  = ip_msgno
                                        i_probcl = ip_probcl
                                        i_msgty  = ip_msgty
                                        i_msgv1  = ip_msgv1
                                        i_msgv2  = ip_msgv2
                                        i_msgv3  = ip_msgv3
                                        i_msgv4  = ip_msgv4 ).
    ENDIF.

  ENDMETHOD.

  METHOD end_log.

    IF ino_logger IS NOT INITIAL.
      me->log_message( ip_msgno = gc_msg_end ).
      ino_logger->if_logger~finalize( ).
    ENDIF.

  ENDMETHOD.

  METHOD get_dates.

    DATA lv_enddate   TYPE sy-datum.
    DATA lv_startdate TYPE sy-datum VALUE '20120101'.

    FREE ep_dates.

*   Bepaal of het een periode betreft of de hele mikmak vanaf 01.01.2012.
    IF so_date IS NOT INITIAL.
      APPEND INITIAL LINE TO ep_dates ASSIGNING FIELD-SYMBOL(<ls_date>).
      <ls_date>-low  = so_date-low.
      <ls_date>-high = so_date-high.
      set_methode( ip_methode = gc_delta ).
    ELSE.
      WHILE lv_startdate LE sy-datum.

        CALL FUNCTION 'SN_LAST_DAY_OF_MONTH'
          EXPORTING
            day_in       = lv_startdate
          IMPORTING
            end_of_month = lv_enddate.

        APPEND INITIAL LINE TO ep_dates ASSIGNING FIELD-SYMBOL(<ls_dates>).
        <ls_dates>-high = lv_enddate.
        <ls_dates>-low  = lv_startdate.

        lv_startdate = lv_enddate + 1.
      ENDWHILE.
      set_methode( ip_methode = gc_full ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
