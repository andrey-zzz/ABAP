*&---------------------------------------------------------------------*
*&  Include           Z_UPLOAD_DOC_TO_DPF
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS z_dpf_upload DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class z_dpf_upload definition.

  public section.

    constants:
      co_text_no_file         type string           value 'Fout bij lezen PDF van server',
      co_text_err_content     type string           value 'Fout bij conversie PDF naar binary',
      co_text_err_create      type string           value 'Fout bij aanleggen dossier',
      co_text_err_record      type string           value 'Fout bij ophalen dossier',
      co_text_err_document    type string           value 'Fout bij creeren document',
      co_text_err_transfer    type string	          value 'Fout bij overzetten inhoud PDF',
      co_text_err_properties  type string           value 'Fout bij zetten eigenschappen van document',
      co_text_err_link        type string           value 'Fout bij linken document aan dossier',
      co_text_err_change_prop	type string	          value 'Fout bij aanpassen eigenschappen',
      co_text_err_lock        type string           value 'Dossier gelocked door ander proces',
      co_text_err_log         type string           value 'Fout bij downloaden log file',
      co_text_test            type string           value 'PDF bestand word opgeslagen in dossier',
      co_text_err_property    type string           value 'Archief map mag niet meer gevuld worden'.

    types: begin of ty_meta,
      file type bapisrmdoc-descr,
      pernr type pernr_d,
      end of ty_meta,

      begin of ty_log,
      file    type char25,
      pernr   type char8,
      message type char45,
      end of ty_log,

      tt_meta type table of ty_meta,
      tt_log  type table of ty_log.

    data:
      gt_meta type tt_meta,
      gt_log  type tt_log.

    methods:
    process_documents,
    write_log,
    write_result,
    read_metadata,
    process_in_background.

  protected section.

  private section.

    methods:
    read_document importing iv_file     type rlgrap-filename
                  exporting et_content  type bapidoccontentab
                            ev_length   type i
                            es_log      type ty_log,
    save_document importing iv_pernr    type pernr_d
                            iv_length   type i
                            it_content  type bapidoccontentab
                            iv_filename type bapisrmdoc-descr
                  exporting es_log      type ty_log.

endclass.                    "zhr_dpd_inlezen DEFINITION

*----------------------------------------------------------------------*
*       CLASS z_dpf_upload IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class z_dpf_upload implementation.

  method process_in_background.

    data lv_jobname  type btcjob value 'Z_DPF_UPLOAD'.
    data lv_jobcount type btcjobcnt.

*   Open job.
    call function 'JOB_OPEN'
      exporting
        jobname  = lv_jobname
      importing
        jobcount = lv_jobcount.

*   Submit report to job.
    submit zhr_dpd_inlezen with pa_meta = pa_meta
                           with pa_dir  = pa_dir
                           with pa_test = pa_test
                           with pa_batch = abap_false
                           via job lv_jobname number lv_jobcount
                           user sy-uname and return.

*   Schedule and close job.
    call function 'JOB_CLOSE'
      exporting
        jobcount  = lv_jobcount
        jobname   = lv_jobname
        sdlstrtdt = sy-datum
        sdlstrttm = sy-uzeit.

  endmethod.                    "process_in_background

  method write_result.

    field-symbols <ls_log> type ty_log.

    write:/ 'Bestandsnaam             ', 'Pers.nr ', 'Resultaat'.

    loop at gt_log assigning <ls_log>.

      write:/ <ls_log>-file, <ls_log>-pernr, <ls_log>-message.

    endloop.

  endmethod.                    "write_result

  method read_metadata.

    data lv_line     type string.
    data ls_meta     type ty_meta.
    data lt_meta     type tt_meta.

    open dataset pa_meta for input in text mode encoding default
    with native linefeed.

    if sy-subrc ne 0.
*     Write message to log.
      call function 'ZHR_DPD_WRITE_LOG'
        exporting
          iv_msgno = '018'.

      return.
    else.

      clear gt_meta.

      do.
        clear: lv_line, ls_meta.
        read dataset pa_meta into lv_line.
        if sy-subrc ne 0.
          exit.
        else.
          split lv_line at ';' into
          ls_meta-file
          ls_meta-pernr.

*         Convert PERNR to correct format.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = ls_meta-pernr
            importing
              output = ls_meta-pernr.

          append ls_meta to lt_meta.
        endif.
      enddo.

      close dataset pa_meta.

    endif.

    gt_meta = lt_meta.

  endmethod.                    "read_metadata

  method write_log.

    data lv_path type rlgrap-filename.
    data lv_line type string.
    data ls_log  type ty_log.

    field-symbols <ls_log> type ty_log.

    concatenate pa_dir '\log.csv' into lv_path.

    delete dataset lv_path.

    open dataset lv_path for output in text mode encoding default.

    if sy-subrc ne 0.
*     Write message to log.
      call function 'ZHR_DPD_WRITE_LOG'
        exporting
          iv_msgno = '017'.

      return.
    else.

      loop at gt_log assigning <ls_log>.

        clear lv_line.

        concatenate <ls_log>-file <ls_log>-pernr <ls_log>-message
        into lv_line separated by ';'.

        transfer lv_line to lv_path.

      endloop.

      close dataset lv_path.

    endif.

  endmethod.                    "write_log

  method process_documents.

    data lv_file    type rlgrap-filename.
    data lt_content type bapidoccontentab.
    data lv_length  type i.
    data lt_log     type tt_log.
    data ls_log     type ty_log.
    data ls_return  type bapiret2.

    field-symbols <ls_meta> type ty_meta.

    loop at gt_meta assigning <ls_meta>.

      free lv_file.
      free ls_log.
      free lv_length.
      free lt_content.
      free ls_return.

      concatenate pa_dir <ls_meta>-file into lv_file.

*     Read document from file location.
      me->read_document( exporting iv_file    = lv_file
                         importing et_content = lt_content
                                   ev_length  = lv_length
                                   es_log     = ls_log ).

*     Log error messages.
      if ls_log-message is not initial.
        ls_log-file  = <ls_meta>-file.
        ls_log-pernr = <ls_meta>-pernr.
        append ls_log to lt_log.
        continue.
      endif.

      if pa_test ne abap_true.
*       Save document in DPD.
        me->save_document( exporting iv_pernr    = <ls_meta>-pernr
                                     iv_length   = lv_length
                                     it_content  = lt_content
                                     iv_filename = <ls_meta>-file
                           importing es_log      = ls_log ).

*       Log error messages.
        if ls_log-message is not initial.
          ls_log-file  = <ls_meta>-file.
          ls_log-pernr = <ls_meta>-pernr.
          append ls_log to lt_log.

*         Undo changes done to either document or personnel file.
          call function 'BAPI_TRANSACTION_ROLLBACK'
            exporting
              wait   = 'X'
            importing
              return = ls_return.

          continue.
        else.
*         Commit changes done to either document or personnel file.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait   = 'X'
            importing
              return = ls_return.

*         Delete PDF file from directory if success.
*          DELETE DATASET lv_file.
        endif.

      else.
        ls_log-file     = <ls_meta>-file.
        ls_log-pernr    = <ls_meta>-pernr.
        ls_log-message  = co_text_test.
        append ls_log to lt_log.
      endif.

    endloop.

    gt_log = lt_log.

  endmethod.                    "process_documents

  method read_document.

    data ls_content   type xstring.
    data lt_content   type bapidoccontentab.
    data lv_length    type i.

    open dataset iv_file for input in binary mode.

    if sy-subrc ne 0.
      es_log-message = co_text_no_file.
      return.
    else.

      read dataset iv_file into ls_content.

      if sy-subrc eq 0.

        call function 'SCMS_XSTRING_TO_BINARY'
          exporting
            buffer        = ls_content
          importing
            output_length = lv_length
          tables
            binary_tab    = lt_content.

      else.
        es_log-message = co_text_err_content.
        return.
      endif.
    endif.

    close dataset iv_file.

    et_content = lt_content.
    ev_length  = lv_length.

  endmethod.                    "read_document

  method save_document.

    data lv_pernr            type pernr_d.
    data ls_reslist          type bapidoctab.
    data lv_rec_id           type bapiguid.
    data lv_rec_class        type bapidclass.
    data ls_return_enq       type bapireturn1.
    data lv_ok               type boolean.
    data lv_lock             type boolean.
    data lv_length           type i.
    data lt_content          type bapidoccontentab.
    data lv_guid             type sysuuid_c32.
    data lv_docid            type bapidocid.
    data ls_return           type bapiret2.
    data lv_objectid         type bapiguid.
    data lv_doc_class        type bapidclass.
    data lt_components       type bapidoccompt.
    data lt_properties       type bapipropt.
    data ls_doc_id           type sdokobject.
    data lt_sp_poid          type bapipropt.
    data ls_sp_poid          type bapiproptb.
    data lv_ref_doc          type string.
    data lv_el_descr         type bapisrmrec-el_descr.
    data lv_filename         type bapisrmdoc-descr.
    data lv_doctype          type zhcm_dpd_doc_type.
    data lv_anch             type asr_anchor_no.

    field-symbols <ls_property>   type bapiproptb.
    field-symbols <ls_components> type bapidoccomp.

    lv_pernr    = iv_pernr.
    lv_length   = iv_length.
    lt_content  = it_content.
    lv_filename = iv_filename.

*   Get digital personnel file from user.
    call function 'ZHR_DPD_GETRECORD'
      exporting
        i_pernr   = lv_pernr
      importing
        e_reslist = ls_reslist
      exceptions
        not_found = 1
        others    = 3.

    if sy-subrc eq 0.
      lv_rec_id     = ls_reslist-objectid.
      lv_rec_class  = ls_reslist-docclass.
    else.

      call function 'HR_EMPLOYEE_ENQUEUE'
        exporting
          number = lv_pernr
        importing
          return = ls_return_enq.

      if ls_return_enq-type ne 'E'.

*       Create new personnel file for user.
        call function 'ZHR_DPD_CREATE_DOSSIER'
          exporting
            iv_pernr = lv_pernr
          importing
            iv_ok    = lv_ok.

        call function 'HR_EMPLOYEE_DEQUEUE'
          exporting
            number = lv_pernr
          importing
            return = ls_return_enq.

        if lv_ok ne abap_true.
          es_log-message = co_text_err_create.
          return.
        endif.
      endif.
    endif.

    if ls_reslist is initial.

*     Get digital personnel file from user.
      call function 'ZHR_DPD_GETRECORD'
        exporting
          i_pernr   = lv_pernr
        importing
          e_reslist = ls_reslist
        exceptions
          not_found = 1
          others    = 3.

      if sy-subrc ne 0.
        es_log-message = co_text_err_record.
        return.
      else.
        lv_rec_id     = ls_reslist-objectid.
        lv_rec_class  = ls_reslist-docclass.
      endif.
    endif.

*   Check if record doesn't have ZCONVERSIE property.
    call function 'SRM_RECORD_GETPROPERTIES'
      exporting
        objectid            = lv_rec_id
        documentclass       = lv_rec_class
      importing
        return              = ls_return
      tables
        properties          = lt_properties
      exceptions
        container_is_locked = 1
        not_authorized      = 2
        parameter_error     = 3
        container_not_found = 4
        container_corrupt   = 5
        internal_error      = 6
        customizing_error   = 7
        others              = 8.

    if sy-subrc <> 0.

    else.

      read table lt_properties assigning <ls_property>
      with key name  = 'ZCONVERSIE'
               value = abap_true.

      if sy-subrc eq 0.
        es_log-message = co_text_err_property.
        return.
      endif.
    endif.

    clear lv_lock.

*   Check if personnel file is locked by other proces.
    do 10 times.
      call function 'ENQUEUE_ESDOKOBJ'
        exporting
          mode_sdok_enq  = 'E'
          objid          = lv_rec_id
          _scope         = '3'
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.

      if sy-subrc <> 0.
        lv_lock = abap_false.
        wait up to 3 seconds.
      else.
        lv_lock = abap_true.
        exit.
      endif.
    enddo.

    if lv_lock eq abap_true.

*     Create unique GUID.
      call method cl_system_uuid=>if_system_uuid_static~create_uuid_c32
        receiving
          uuid = lv_guid.

      lv_docid = lv_guid.

*     Create new document.
      call function 'SRM_DOCUMENT_CREATE'
        exporting
          rms_id            = pa_rms
          sps_id            = pa_sps
          documentid        = lv_docid
          description       = lv_filename
        importing
          return            = ls_return
          objectid          = lv_objectid
          documentclass     = lv_doc_class
        exceptions
          internal_error    = 1
          parameter_error   = 2
          doc_id_not_unique = 3
          not_authorized    = 4
          customizing_error = 5
          others            = 6.

      if sy-subrc ne 0.
        es_log-message = co_text_err_document.
        return.
      endif.

      append initial line to lt_components assigning <ls_components>.
      <ls_components>-mimetype    = pa_mime.
      <ls_components>-comp_id     = lv_filename.
      <ls_components>-comp_size   = lv_length.
      <ls_components>-binary_flag = abap_true.

*     Transfer content to newly created document.
      call function 'SRM_DOCUMENT_CHECKIN_VIA_TAB'
        exporting
          objectid        = lv_objectid
          documentclass   = lv_doc_class
        importing
          return          = ls_return
        tables
          components      = lt_components
          bin_content     = lt_content
        exceptions
          internal_error  = 1
          parameter_error = 2
          not_authorized  = 3
          doc_not_found   = 4
          yet_locked      = 5
          others          = 6.

      if sy-subrc ne 0.
        es_log-message = co_text_err_transfer.
        return.
      endif.

      lv_anch = pa_anch.

      select single doctype from zhcm_dpd_doctype
        into lv_doctype
       where anchor eq lv_anch.

      free lt_properties.

      append initial line to lt_properties assigning <ls_property>.
      <ls_property>-name  = 'SRM_DOCUMENT_ID'.
      <ls_property>-value = lv_docid.

      append initial line to lt_properties assigning <ls_property>.
      <ls_property>-name  = 'DESCRIPTION'.
      <ls_property>-value = lv_filename.

      append initial line to lt_properties assigning <ls_property>.
      <ls_property>-name  = 'ASR_PERSONNEL_NUMBER'.
      <ls_property>-value = lv_pernr.

      append initial line to lt_properties assigning <ls_property>.
      <ls_property>-name  = 'ASR_DOCUMENT_TYPE'.
      <ls_property>-value = 'Z_ASR'.

      append initial line to lt_properties assigning <ls_property>.
      <ls_property>-name  = 'ZDPD_DOCTYPE'.
      <ls_property>-value = lv_doctype.

*     Add properties to document.
      call function 'SRM_DOCUMENT_CHANGEPROPERTIES'
        exporting
          objectid          = lv_objectid
          documentclass     = lv_doc_class
        importing
          return            = ls_return
        tables
          properties        = lt_properties
        exceptions
          internal_error    = 1
          parameter_error   = 2
          not_authorized    = 3
          doc_not_found     = 4
          variant_not_found = 5
          version_not_found = 6
          yet_locked        = 7
          others            = 8.

      if sy-subrc ne 0.
        es_log-message = co_text_err_properties.
        return.
      endif.

      lv_el_descr = lv_filename.

      ls_doc_id-class = lv_doc_class.
      ls_doc_id-objid = lv_objectid.
      lv_ref_doc = ls_doc_id.

      ls_sp_poid-name   = 'DOC_ID'.
      ls_sp_poid-value  = lv_ref_doc.
      append ls_sp_poid to lt_sp_poid.

      ls_sp_poid-name   = 'VARIANT'.
      ls_sp_poid-value  = '00000000'.
      condense ls_sp_poid-value.
      append ls_sp_poid to lt_sp_poid.

      ls_sp_poid-name   = 'VERSION'.
      ls_sp_poid-value  = '00000000'.
      condense ls_sp_poid-value.
      append ls_sp_poid to lt_sp_poid.

*     Link document to personnel file.
      call function 'SRM_RECORD_ADDELEMENT'
        exporting
          objectid               = lv_rec_id
          documentclass          = lv_rec_class
          sps_id                 = pa_sps
          anchor                 = pa_anch
          description            = lv_el_descr
        importing
          return                 = ls_return
        tables
          element_sp_poid        = lt_sp_poid
        exceptions
          anchor_not_found       = 1
          not_authorized         = 2
          parameter_error        = 3
          container_not_found    = 4
          container_is_locked    = 5
          max_number_of_elements = 6
          poid_is_wrong          = 7
          internal_error         = 8
          record_is_frozen       = 9
          others                 = 10.

      if sy-subrc ne 0.
        es_log-message = co_text_err_link.
        return.
      endif.

      free lt_properties.

      append initial line to lt_properties assigning <ls_property>.
      <ls_property>-name  = 'SRM_DOCUMENT_ID'.
      <ls_property>-value = lv_pernr.

      append initial line to lt_properties assigning <ls_property>.
      <ls_property>-name  = 'ASR_PERSONNEL_NUMBER'.
      <ls_property>-value = lv_pernr.

      append initial line to lt_properties assigning <ls_property>.
      <ls_property>-name  = 'ZCONVERSIE'.
      <ls_property>-value = 'X'.

*     For an unknown reason the above process deletes some important properties
*     from the personnel file, so we need to add them here...again.
      call function 'SRM_RECORD_CHANGEPROPERTIES'
        exporting
          objectid            = lv_rec_id
          documentclass       = lv_rec_class
        tables
          properties          = lt_properties
        exceptions
          container_is_locked = 1
          not_authorized      = 2
          parameter_error     = 3
          container_not_found = 4
          container_corrupt   = 5
          internal_error      = 6
          record_is_frozen    = 7
          invalid_srm_state   = 8
          others              = 9.

      if sy-subrc ne 0.
        es_log-message = co_text_err_change_prop.
        return.
      endif.

*     De-block personnel file.
      call function 'DEQUEUE_ESDOKOBJ'
        exporting
          mode_sdok_enq = 'E'
          objid         = lv_rec_id
          _scope        = '3'.

    else.
      es_log-message = co_text_err_lock.
      return.
    endif.

  endmethod.                    "save_document

endclass.                    "z_dpf_upload IMPLEMENTATION
