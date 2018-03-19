*&---------------------------------------------------------------------*
*&  Include           Z_DOWNLOAD_DOC_FROM_DPF_CLASS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS z_download_doc_from_dpf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_download_doc_from_dpf DEFINITION.

  PUBLIC SECTION.

    METHODS select_files   EXCEPTIONS no_dpd
                                      no_files.
    METHODS download_files EXCEPTIONS error_location
                                      error_download.
    METHODS show_files.
    METHODS show_result.
    METHODS show_message   IMPORTING iv_num TYPE symsgno.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_content,
             name    TYPE string,
             size    TYPE i,
             content TYPE bapidoccontentab,
             text    TYPE string,
           END OF ty_content.

    CONSTANTS gc_propname        TYPE bapipropna VALUE 'SRM_DOCUMENT_ID'.
    CONSTANTS gc_fname_dpf_rms   TYPE asr_sgpid  VALUE 'DPF_RMS'.
    CONSTANTS gc_fname_dpf_etype TYPE asr_sgpid  VALUE 'DPF_ETYPE'.
    CONSTANTS gc_doc_id          TYPE bapipropna VALUE 'DOC_ID'.

    DATA gt_reslist              TYPE TABLE OF bapidoctab.
    DATA gt_content              TYPE TABLE OF ty_content.

    METHODS select_dpds      EXCEPTIONS no_dpd.
    METHODS select_dpd_files EXCEPTIONS no_files.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS z_download_doc_from_dpf IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_download_doc_from_dpf IMPLEMENTATION.

  METHOD show_message.

    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel = 'Foutmelding'
        msgid = 'ZHCM_DPD'
        msgty = 'E'
        msgno = iv_num.

  ENDMETHOD.

  METHOD select_dpd_files.

    DATA lt_element     TYPE TABLE OF bapisrmrec_element.
    DATA lt_el_id       TYPE TABLE OF bapipropelement.
    DATA ls_return      TYPE bapiret2.
    DATA lv_objid       TYPE bapisrmrec-guid.
    DATA lv_docclass    TYPE bapisrmrec-docclass.
    DATA lt_content     TYPE bapidoccontentab.
    DATA lt_components  TYPE bapidoccompt.

    FIELD-SYMBOLS <ls_reslist>   TYPE bapidoctab.
    FIELD-SYMBOLS <ls_element>   TYPE bapisrmrec_element.
    FIELD-SYMBOLS <ls_el_id>     TYPE bapipropelement.
    FIELD-SYMBOLS <ls_content>   TYPE ty_content.
    FIELD-SYMBOLS <ls_component> TYPE bapidoccomp.

    LOOP AT gt_reslist ASSIGNING <ls_reslist>.

      CLEAR: lt_element, lt_el_id.
      CALL FUNCTION 'BAPI_RECORD_GETELEMENTS'
        EXPORTING
          objectid               = <ls_reslist>-objectid
          documentclass          = <ls_reslist>-docclass
        IMPORTING
          return                 = ls_return
        TABLES
          element                = lt_element
          element_identification = lt_el_id.

      LOOP AT lt_element ASSIGNING <ls_element> WHERE type EQ 'I'.

        READ TABLE lt_el_id ASSIGNING <ls_el_id>
        WITH KEY element_id = <ls_element>-element_id
                 name       = gc_doc_id.

        IF sy-subrc EQ 0.

          lv_docclass = <ls_el_id>-value(6).
          lv_objid    = <ls_el_id>-value+10.

          CLEAR: lt_content, lt_components.
          CALL FUNCTION 'SRM_DOCUMENT_CHECKOUT_VIA_TAB'
            EXPORTING
              objectid        = lv_objid
              documentclass   = lv_docclass
            IMPORTING
              return          = ls_return
            TABLES
              components      = lt_components
              bin_content     = lt_content
            EXCEPTIONS
              internal_error  = 1
              parameter_error = 2
              not_authorized  = 3
              doc_not_found   = 4
              OTHERS          = 5.

          IF sy-subrc <> 0.
            "Ga verder met volgende document.
          ELSE.
            READ TABLE lt_components ASSIGNING <ls_component> INDEX 1.
            IF sy-subrc EQ 0.
              APPEND INITIAL LINE TO gt_content ASSIGNING <ls_content>.
              IF sy-subrc EQ 0.
                <ls_content>-name    = <ls_component>-comp_id.
                <ls_content>-size    = <ls_component>-comp_size.
                <ls_content>-content = lt_content.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF gt_content IS INITIAL.
      RAISE no_files.
    ENDIF.

  ENDMETHOD.

  METHOD select_dpds.

    DATA lvc_rmsid     TYPE bapirmsid.
    DATA lvc_recspsid  TYPE bapispsid.
    DATA ls_return     TYPE bapiret2.
    DATA lt_propsel    TYPE TABLE OF bapipropqy.
    DATA lt_reslist    TYPE TABLE OF bapidoctab.
    DATA lvc_value     TYPE asr_sgpvl.

    FIELD-SYMBOLS <ls_propsel> TYPE bapipropqy.

*   Haal ID's op.
    CLEAR lvc_value.
    SELECT SINGLE value FROM t5asrsettings
    INTO lvc_value
    WHERE fname = gc_fname_dpf_rms.
    IF sy-subrc = 0.
      lvc_rmsid = lvc_value.
    ENDIF.

    CLEAR lvc_value.
    SELECT SINGLE value FROM t5asrsettings
    INTO lvc_value
    WHERE fname = gc_fname_dpf_etype.
    IF sy-subrc = 0.
      lvc_recspsid = lvc_value.
    ENDIF.

    APPEND INITIAL LINE TO lt_propsel ASSIGNING <ls_propsel>.
    <ls_propsel>-propname   = gc_propname.
    <ls_propsel>-sign       = so_pernr-sign.
    <ls_propsel>-option     = so_pernr-option.
    <ls_propsel>-propval_lo = so_pernr-low.
    <ls_propsel>-propval_hi = so_pernr-high.

*   Haal geselecteerde dossiers op.
    CALL FUNCTION 'BAPI_RECORD_GETLIST'
      EXPORTING
        rms_id             = lvc_rmsid
        sps_id             = lvc_recspsid
        max_hits           = 100000
      IMPORTING
        return             = ls_return
      TABLES
        property_selection = lt_propsel
        resulting_list     = lt_reslist.

    IF lt_reslist IS INITIAL.
      RAISE no_dpd.
      EXIT.
    ELSE.
      gt_reslist = lt_reslist.
    ENDIF.

  ENDMETHOD.

  METHOD select_files.

*   Selecteer DPD dossiers.
    select_dpds( EXCEPTIONS no_dpd = 1 ).
    IF sy-subrc NE 0.
      RAISE no_dpd.
    ENDIF.

*   Haal bestanden op.
    select_dpd_files( EXCEPTIONS no_files = 1 ).
    IF sy-subrc NE 0.
      RAISE no_files.
    ENDIF.

  ENDMETHOD.

  METHOD download_files.

    DATA lo_zip           TYPE REF TO cl_abap_zip.
    DATA lv_xstring       TYPE xstring.
    DATA lv_xstring_zip   TYPE xstring.
    DATA lv_size          TYPE i.
    DATA lv_filesize      TYPE i.
    DATA lt_content       TYPE bapidoccontentab.
    DATA lv_filename      TYPE string.
    DATA lv_path          TYPE string.
    DATA lv_filepath      TYPE string.

    FIELD-SYMBOLS <ls_content> TYPE ty_content.

    CHECK gt_content IS NOT INITIAL.

    CREATE OBJECT lo_zip.

    LOOP AT gt_content ASSIGNING <ls_content>.

      CLEAR lv_xstring.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = <ls_content>-size
        IMPORTING
          buffer       = lv_xstring
        TABLES
          binary_tab   = <ls_content>-content
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.

      IF sy-subrc EQ 0.
        lo_zip->add( name    = <ls_content>-name
                     content = lv_xstring ).

        <ls_content>-text = 'Document toegevoegd'.
      ELSE.
        "Ga verder met volgende document.
        <ls_content>-text = 'Fout bij converteren document, niet toegevoegd'.
      ENDIF.

    ENDLOOP.

    lv_xstring_zip = lo_zip->save( ).

    IF lv_xstring_zip IS NOT INITIAL.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_xstring_zip
        IMPORTING
          output_length = lv_size
        TABLES
          binary_tab    = lt_content.

      cl_gui_frontend_services=>file_save_dialog(
        EXPORTING
          window_title         = 'Selecteer locatie'
          file_filter = '(*.zip)|*.zip|'
        CHANGING
          filename             = lv_filename
          path                 = lv_path
          fullpath             = lv_filepath
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4 ).

      IF sy-subrc <> 0.
        RAISE error_location.
      ENDIF.

      cl_gui_frontend_services=>gui_download(
            EXPORTING
              bin_filesize              = lv_size
              filename                  = lv_filepath
              filetype                  = 'BIN'
            IMPORTING
              filelength                = lv_filesize
            CHANGING
              data_tab                  = lt_content
            EXCEPTIONS
              file_write_error          = 1
              no_batch                  = 2
              gui_refuse_filetransfer   = 3
              invalid_type              = 4
              no_authority              = 5
              unknown_error             = 6
              header_not_allowed        = 7
              separator_not_allowed     = 8
              filesize_not_allowed      = 9
              header_too_long           = 10
              dp_error_create           = 11
              dp_error_send             = 12
              dp_error_write            = 13
              unknown_dp_error          = 14
              access_denied             = 15
              dp_out_of_memory          = 16
              disk_full                 = 17
              dp_timeout                = 18
              file_not_found            = 19
              dataprovider_exception    = 20
              control_flush_error       = 21
              not_supported_by_gui      = 22
              error_no_gui              = 23
              OTHERS                    = 24
                 ).

      IF sy-subrc <> 0.
        RAISE error_download.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD show_files.

    FIELD-SYMBOLS <ls_content> TYPE ty_content.

    WRITE:/ 'Inhoud ZIP bestand:'.

    LOOP AT gt_content ASSIGNING <ls_content>.

      WRITE:/ <ls_content>-name.

    ENDLOOP.

  ENDMETHOD.

  METHOD show_result.

    FIELD-SYMBOLS <ls_content> TYPE ty_content.

    WRITE:/ 'Bestanden die zijn verwerkt:'.

    LOOP AT gt_content ASSIGNING <ls_content>.

      WRITE:/ <ls_content>-name, <ls_content>-text.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
