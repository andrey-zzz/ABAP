*&---------------------------------------------------------------------*
*&  Include           Z_UPLOAD_DOC_TO_DPF_CLASS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS z_dpd_upload_files DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_dpd_upload_files DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_content,
             name     TYPE string,
             filesize TYPE i,
             content  TYPE bapidoccontentab,
             mime     TYPE bapimimetype,
             text     TYPE string,
           END OF ty_content.

    DATA gt_content TYPE TABLE OF ty_content.

    METHODS select_dpd        EXCEPTIONS no_dpd.
    METHODS select_files      EXCEPTIONS error_select
                                         error_upload
                                         error_filename
                                         no_files.
    METHODS add_files_to_dpd  EXCEPTIONS dpd_locked.
    METHODS show_files.
    METHODS show_result.
    METHODS show_message      IMPORTING iv_num TYPE symsgno.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS gc_propname        TYPE bapipropna       VALUE 'SRM_DOCUMENT_ID'.
    CONSTANTS gc_fname_dpf_rms   TYPE asr_sgpid        VALUE 'DPF_RMS'.
    CONSTANTS gc_fname_dpf_etype TYPE asr_sgpid        VALUE 'DPF_ETYPE'.
    CONSTANTS gc_rms             TYPE bapisrmdoc-rmsid VALUE 'ASR_DPF'.
    CONSTANTS gc_sps             TYPE bapisrmdoc-spsid VALUE 'ZASR_SPS_DPD_DOCUMENT'.

    DATA gt_reslist              TYPE TABLE OF bapidoctab.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS z_dpd_upload_files IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_dpd_upload_files IMPLEMENTATION.

  METHOD show_message.

    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel = 'Foutmelding'
        msgid = 'ZHCM_DPD'
        msgty = 'E'
        msgno = iv_num.

  ENDMETHOD.

  METHOD show_files.

    FIELD-SYMBOLS <ls_content> TYPE ty_content.

    WRITE:/ 'Bestanden die worden toegevoegd:'.

    LOOP AT gt_content ASSIGNING <ls_content>.

      WRITE:/ <ls_content>-name, <ls_content>-text.

    ENDLOOP.

  ENDMETHOD.

  METHOD show_result.

    FIELD-SYMBOLS <ls_content> TYPE ty_content.

    WRITE:/ 'Bestanden die zijn verwerkt:'.

    LOOP AT gt_content ASSIGNING <ls_content>.

      WRITE:/ <ls_content>-name, <ls_content>-text.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_files_to_dpd.

    DATA lv_guid             TYPE sysuuid_c32.
    DATA lv_docid            TYPE bapidocid.
    DATA ls_return           TYPE bapiret2.
    DATA lv_objectid         TYPE bapiguid.
    DATA lv_doc_class        TYPE bapidclass.
    DATA lt_components       TYPE bapidoccompt.
    DATA lv_anch             TYPE asr_anchor_no.
    DATA lv_doctype          TYPE zhcm_dpd_doc_type.
    DATA lt_properties       TYPE bapipropt.
    DATA lt_sp_poid          TYPE bapipropt.
    DATA ls_doc_id           TYPE sdokobject.
    DATA lv_ref_doc          TYPE string.
    DATA lv_rec_id           TYPE bapiguid.
    DATA lv_rec_class        TYPE bapidclass.
    DATA lv_el_descr         TYPE bapisrmrec-el_descr.
    DATA lv_filename         TYPE bapisrmdoc-descr.
    DATA lv_lock             TYPE boolean.

    FIELD-SYMBOLS <ls_reslist>    TYPE bapidoctab.
    FIELD-SYMBOLS <ls_content>    TYPE ty_content.
    FIELD-SYMBOLS <ls_components> TYPE bapidoccomp.
    FIELD-SYMBOLS <ls_property>   TYPE bapiproptb.
    FIELD-SYMBOLS <ls_sp_poid>    TYPE bapiproptb.

    READ TABLE gt_reslist ASSIGNING <ls_reslist> INDEX 1.
    IF sy-subrc EQ 0.

      lv_rec_id     = <ls_reslist>-objectid.
      lv_rec_class  = <ls_reslist>-docclass.

      lv_anch = pa_anch.

      SELECT SINGLE doctype FROM zhcm_dpd_doctypm
        INTO lv_doctype
       WHERE anchor EQ lv_anch.

*     Controleer of DPD geblokkeerd is.
      DO 3 TIMES.
        CALL FUNCTION 'ENQUEUE_ESDOKOBJ'
          EXPORTING
            mode_sdok_enq  = 'E'
            objid          = lv_rec_id
            _scope         = '3'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc <> 0.
          lv_lock = abap_false.
          WAIT UP TO 2 SECONDS.
        ELSE.
          lv_lock = abap_true.
          EXIT.
        ENDIF.
      ENDDO.

      IF lv_lock EQ abap_false.
        RAISE dpd_locked.
      ENDIF.

      LOOP AT gt_content ASSIGNING <ls_content> WHERE mime IS NOT INITIAL.

*       Creeer unieke GUID.
        CLEAR: lv_guid, lv_docid, lv_filename.
        CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
          RECEIVING
            uuid = lv_guid.

        lv_docid    = lv_guid.
        lv_filename = <ls_content>-name.

*       Creer nieuw document.
        CLEAR: lv_objectid, lv_doc_class.
        CALL FUNCTION 'SRM_DOCUMENT_CREATE'
          EXPORTING
            rms_id            = gc_rms
            sps_id            = gc_sps
            documentid        = lv_docid
            description       = lv_filename
          IMPORTING
            return            = ls_return
            objectid          = lv_objectid
            documentclass     = lv_doc_class
          EXCEPTIONS
            internal_error    = 1
            parameter_error   = 2
            doc_id_not_unique = 3
            not_authorized    = 4
            customizing_error = 5
            OTHERS            = 6.

        IF sy-subrc NE 0.

*         Wegens foutmelding(en) wijzigingen terugdraaien.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            IMPORTING
              return = ls_return.

          <ls_content>-text = 'Fout bij creeren document'.
          CONTINUE.
        ENDIF.

        CLEAR lt_components.
        APPEND INITIAL LINE TO lt_components ASSIGNING <ls_components>.
        <ls_components>-mimetype    = <ls_content>-mime.
        <ls_components>-comp_id     = lv_filename.
        <ls_components>-comp_size   = <ls_content>-filesize.
        <ls_components>-binary_flag = abap_true.

*       Transfer content naar document.
        CALL FUNCTION 'SRM_DOCUMENT_CHECKIN_VIA_TAB'
          EXPORTING
            objectid        = lv_objectid
            documentclass   = lv_doc_class
          IMPORTING
            return          = ls_return
          TABLES
            components      = lt_components
            bin_content     = <ls_content>-content
          EXCEPTIONS
            internal_error  = 1
            parameter_error = 2
            not_authorized  = 3
            doc_not_found   = 4
            yet_locked      = 5
            OTHERS          = 6.

        IF sy-subrc NE 0.

*         Wegens foutmelding(en) wijzigingen terugdraaien.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            IMPORTING
              return = ls_return.

          <ls_content>-text = 'Fout bij overdracht bestandsinhoud'.
          CONTINUE.
        ENDIF.

        CLEAR lt_properties.

        APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_property>.
        <ls_property>-name  = 'SRM_DOCUMENT_ID'.
        <ls_property>-value = lv_docid.

        APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_property>.
        <ls_property>-name  = 'DESCRIPTION'.
        <ls_property>-value = lv_filename.

        APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_property>.
        <ls_property>-name  = 'ASR_PERSONNEL_NUMBER'.
        <ls_property>-value = pa_pernr.

        APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_property>.
        <ls_property>-name  = 'ASR_DOCUMENT_TYPE'.
        <ls_property>-value = 'Z_ASR'.

        APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_property>.
        <ls_property>-name  = 'ZDPD_DOCTYPE'.
        <ls_property>-value = lv_doctype.

*       Eigenschappen op document zetten.
        CALL FUNCTION 'SRM_DOCUMENT_CHANGEPROPERTIES'
          EXPORTING
            objectid          = lv_objectid
            documentclass     = lv_doc_class
          IMPORTING
            return            = ls_return
          TABLES
            properties        = lt_properties
          EXCEPTIONS
            internal_error    = 1
            parameter_error   = 2
            not_authorized    = 3
            doc_not_found     = 4
            variant_not_found = 5
            version_not_found = 6
            yet_locked        = 7
            OTHERS            = 8.

        IF sy-subrc NE 0.
          <ls_content>-text = 'Fout bij zetten attributen document, wel toegevoegd'.
        ENDIF.

        CLEAR: ls_doc_id, lv_ref_doc.
        ls_doc_id-class = lv_doc_class.
        ls_doc_id-objid = lv_objectid.
        lv_ref_doc = ls_doc_id.

        CLEAR lt_sp_poid.

        APPEND INITIAL LINE TO lt_sp_poid ASSIGNING <ls_sp_poid>.
        <ls_sp_poid>-name  = 'DOC_ID'.
        <ls_sp_poid>-value = lv_ref_doc.

        APPEND INITIAL LINE TO lt_sp_poid ASSIGNING <ls_sp_poid>.
        <ls_sp_poid>-name  = 'VARIANT'.
        <ls_sp_poid>-value = '00000000'.
        CONDENSE <ls_sp_poid>-value.

        APPEND INITIAL LINE TO lt_sp_poid ASSIGNING <ls_sp_poid>.
        <ls_sp_poid>-name  = 'VERSION'.
        <ls_sp_poid>-value = '00000000'.
        CONDENSE <ls_sp_poid>-value.

        CLEAR lv_el_descr.
        lv_el_descr = <ls_content>-name.

*       Link document met DPD.
        CALL FUNCTION 'SRM_RECORD_ADDELEMENT'
          EXPORTING
            objectid               = lv_rec_id
            documentclass          = lv_rec_class
            sps_id                 = gc_sps
            anchor                 = pa_anch
            description            = lv_el_descr
          IMPORTING
            return                 = ls_return
          TABLES
            element_sp_poid        = lt_sp_poid
          EXCEPTIONS
            anchor_not_found       = 1
            not_authorized         = 2
            parameter_error        = 3
            container_not_found    = 4
            container_is_locked    = 5
            max_number_of_elements = 6
            poid_is_wrong          = 7
            internal_error         = 8
            record_is_frozen       = 9
            OTHERS                 = 10.

        IF sy-subrc NE 0.

*         Wegens foutmelding(en) wijzigingen terugdraaien.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            IMPORTING
              return = ls_return.

          <ls_content>-text = 'Fout bij linken document met DPD'.
          CONTINUE.
        ENDIF.

        CLEAR lt_properties.

        APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_property>.
        <ls_property>-name  = 'SRM_DOCUMENT_ID'.
        <ls_property>-value = pa_pernr.

        APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_property>.
        <ls_property>-name  = 'ASR_PERSONNEL_NUMBER'.
        <ls_property>-value = pa_pernr.

*        APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_property>.
*        <ls_property>-name  = 'ZCONVERSIE'.
*        <ls_property>-value = 'X'.

*       Eigenschappen worden door bovenstaande FB verwijderd en moeten opnieuw
*       gezet worden.
        CALL FUNCTION 'SRM_RECORD_CHANGEPROPERTIES'
          EXPORTING
            objectid            = lv_rec_id
            documentclass       = lv_rec_class
          TABLES
            properties          = lt_properties
          EXCEPTIONS
            container_is_locked = 1
            not_authorized      = 2
            parameter_error     = 3
            container_not_found = 4
            container_corrupt   = 5
            internal_error      = 6
            record_is_frozen    = 7
            invalid_srm_state   = 8
            OTHERS              = 9.

        IF sy-subrc NE 0.
          <ls_content>-text = 'Fout bij wijzigen attributen document, wel toegevoegd'.
        ENDIF.

*       Deblokkeer DPD.
        CALL FUNCTION 'DEQUEUE_ESDOKOBJ'
          EXPORTING
            mode_sdok_enq = 'E'
            objid         = lv_rec_id
            _scope        = '3'.

*       Wijzigingen doorvoeren.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = ls_return.

        IF <ls_content>-text IS INITIAL.
          <ls_content>-text = 'Document toegevoegd'.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD select_dpd.

    DATA ls_reslist    TYPE bapidoctab.

*   Get digital personnel file from user.
    CALL FUNCTION 'ZHR_DPD_GETRECORD'
      EXPORTING
        i_pernr   = pa_pernr
      IMPORTING
        e_reslist = ls_reslist
      EXCEPTIONS
        not_found = 1
        OTHERS    = 3.

    IF ls_reslist IS NOT INITIAL.
      APPEND ls_reslist TO gt_reslist.
    ELSE.
      RAISE no_dpd.
    ENDIF.

  ENDMETHOD.

  METHOD select_files.

    DATA lv_rc            TYPE i.
    DATA lt_files         TYPE filetable.
    DATA lt_filecontent   TYPE bapidoccontentab.
    DATA lv_filename      TYPE string.
    DATA lt_content       TYPE TABLE OF ty_content.
    DATA lv_filesize      TYPE i.
    DATA lv_file          TYPE string.
    DATA lv_fileext(50)   TYPE c.
    DATA lv_extension(10) TYPE c.

    FIELD-SYMBOLS <ls_files>    TYPE file_table.
    FIELD-SYMBOLS <ls_content>  TYPE ty_content.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = 'Bestanden selecteren'
        multiselection          = 'X'
      CHANGING
        file_table              = lt_files
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    IF sy-subrc <> 0.
      RAISE error_select.
    ENDIF.

    LOOP AT lt_files ASSIGNING <ls_files>.

      lv_filename = <ls_files>-filename.

      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename                = lv_filename
          filetype                = 'BIN'
        IMPORTING
          filelength              = lv_filesize
*         header                  =
        CHANGING
          data_tab                = lt_filecontent
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
        RAISE error_upload.
      ENDIF.

      CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
        EXPORTING
          full_name     = lv_filename
        IMPORTING
          stripped_name = lv_file
        EXCEPTIONS
          x_error       = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
        RAISE error_filename.
      ENDIF.

      APPEND INITIAL LINE TO lt_content ASSIGNING <ls_content>.
      <ls_content>-name     = lv_file.
      <ls_content>-filesize = lv_filesize.
      <ls_content>-content  = lt_filecontent.

      CLEAR lv_fileext.
      lv_fileext = lv_file.

      CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
        EXPORTING
          filename  = lv_fileext
        IMPORTING
          extension = lv_extension.

      CASE lv_extension.
        WHEN 'JPEG' OR 'JPG'.
          <ls_content>-mime     = 'image/jpeg'.
        WHEN 'XLS' OR 'XLSX'.
          <ls_content>-mime     = 'application/msexcel'.
        WHEN 'PDF'.
          <ls_content>-mime     = 'application/pdf'.
        WHEN 'GIF'.
          <ls_content>-mime     = 'image/gif'.
        WHEN 'BMP'.
          <ls_content>-mime     = 'image/bmp'.
        WHEN 'PNG'.
          <ls_content>-mime     = 'text/x-doctype'.
        WHEN 'TXT'.
          <ls_content>-mime     = 'text/plain'.
        WHEN 'DOC' OR 'DOCX'.
          <ls_content>-mime     = 'application/msword'.
        WHEN 'RTF'.
          <ls_content>-mime     = 'application/rtf'.
        WHEN OTHERS.
          <ls_content>-text     = 'Extensie wordt niet ondersteund, of bestandsnaam te lang'.
      ENDCASE.

    ENDLOOP.

    IF lt_content IS NOT INITIAL.
      gt_content = lt_content.
    ELSE.
      RAISE no_files.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
