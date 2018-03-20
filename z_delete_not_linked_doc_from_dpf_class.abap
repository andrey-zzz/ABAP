*&---------------------------------------------------------------------*
*&  Include           Z_DELETE_NOT_LINKED_DOC_FROM_DPF_CLASS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS z_dpf_delete_files DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class z_dpf_delete_files definition.

  public section.

    types: begin of ty_links,
      pernr type pernr_d,
      guid  type bapiguid,
      class type bapidclass,
      user  type sy-uname,
      date  type sy-datum,
      time  type sy-timlo,
      descr type bapidescr,
      result type char25,
    end of ty_links,

    begin of ty_so_pernr,
      sign(1)   type c,
      option(2) type c,
      low       type pernr_d,
      high      type pernr_d,
      end of ty_so_pernr.

    class-data gt_not_in_dpd      type table of ty_links.
    class-data gt_result          type table of ty_links.
    class-data gt_link_type       type ty_links.

    class-methods delete_document importing is_delete type ty_links
                                  exceptions error_del.
    class-methods show_message    importing iv_num type symsgno.

    methods select_files          exceptions no_files error_files.
    methods select_documents      exceptions no_docs.
    methods compare_files         exceptions no_files.
    methods select_document_links exceptions no_links error_links.
    methods show_files.
    methods show_result.
    methods process_files.

  protected section.

  private section.

    constants gc_file_propname    type bapipropna value 'SRM_DOCUMENT_ID'.
    constants gc_doc_propname     type bapipropna value 'ASR_PERSONNEL_NUMBER'.
    constants gc_doc_username     type bapipropna value 'SRM_LAST_CHANGED_BY'.
    constants gc_doc_datetime     type bapipropna value 'SRM_LAST_CHANGED_AT'.
    constants gc_fname_dpf_rms    type bapirmsid  value 'ASR_DPF'.
    constants gc_fname_dpf_sps    type bapispsid  value 'ASR_SPS_RECORD'.
    constants gc_fname_dpf_doc    type bapispsid  value 'ZASR_SPS_PERSFILE_DOCUMENT'.

    data gt_persfiles             type table of bapidoctab.
    data gt_documents             type table of bapidoctab.
    data gt_links                 type table of ty_links.

endclass.                    "zhr_dpd_delete_files DEFINITION

*----------------------------------------------------------------------*
*       CLASS z_dpf_delete_files IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class z_dpf_delete_files implementation.

  method process_files.

    data lt_result type table of ty_links.

    field-symbols <ls_delete>  type ty_links.
    field-symbols <ls_result>  type ty_links.

    loop at gt_not_in_dpd assigning <ls_delete>.

*     Verwijder document.
      delete_document( exporting is_delete = <ls_delete>
                       exceptions error_del = 1 ).

      append initial line to lt_result assigning <ls_result>.
      <ls_result>-pernr = <ls_delete>-pernr.
      <ls_result>-guid  = <ls_delete>-guid.
      <ls_result>-class = <ls_delete>-class.
      <ls_result>-descr = <ls_delete>-descr.

      if sy-subrc eq 0.
        <ls_result>-result = 'Document verwijderd'.
      else.
        <ls_result>-result = 'Fout bij verwijderen'.
      endif.

    endloop.

    gt_result = lt_result.

    clear gt_not_in_dpd.

  endmethod.                    "process_files

  method delete_document.

    data ls_return type bapiret2.

    call function 'SRM_DOCUMENT_DELETE'
      exporting
        objectid        = is_delete-guid
        documentclass   = is_delete-class
        whole_document  = 'X'
      importing
        return          = ls_return
      exceptions
        internal_error  = 1
        parameter_error = 2
        not_authorized  = 3
        doc_not_found   = 4
        yet_locked      = 5
        others          = 6.

    if sy-subrc <> 0.
      raise error_del.
    endif.

  endmethod.                    "delete_document

  method select_document_links.

    data ls_return         type bapiret2.
    data lt_element_tmp    type table of bapisrmrec_element.
    data lt_elementid_tmp  type table of bapipropelement.
    data lt_links          type table of ty_links.
    data lt_persfiles      type table of bapidoctab.

    field-symbols <ls_persfiles>  type bapidoctab.
    field-symbols <ls_element>    type bapisrmrec_element.
    field-symbols <ls_elementid>  type bapipropelement.
    field-symbols <ls_links>      type ty_links.

    lt_persfiles = gt_persfiles.

    loop at lt_persfiles assigning <ls_persfiles>.

*     Haal alle elementen van DPD op.
      clear: lt_element_tmp, lt_elementid_tmp.
      call function 'SRM_RECORD_GETELEMENTS'
        exporting
          objectid               = <ls_persfiles>-objectid
          documentclass          = <ls_persfiles>-docclass
        importing
          return                 = ls_return
        tables
          element                = lt_element_tmp
          element_identification = lt_elementid_tmp
        exceptions
          not_authorized         = 1
          parameter_error        = 2
          container_not_found    = 3
          internal_error         = 4
          others                 = 5.

      if sy-subrc <> 0.
        delete gt_persfiles where objectid   = <ls_persfiles>-objectid
                              and docclass   = <ls_persfiles>-docclass
                              and documentid = <ls_persfiles>-documentid
                              and descr      = <ls_persfiles>-descr.
      else.

        loop at lt_element_tmp assigning <ls_element> where type eq 'I'.

          read table lt_elementid_tmp assigning <ls_elementid>
          with key element_id = <ls_element>-element_id
                   name = 'DOC_ID'.

          if sy-subrc eq 0.
            append initial line to lt_links assigning <ls_links>.
            <ls_links>-guid  = <ls_elementid>-value+10.
            <ls_links>-class = <ls_elementid>-value(9).
            <ls_links>-descr = <ls_element>-description.
          endif.

        endloop.

      endif.

    endloop.

    if lt_links is not initial.
      gt_links = lt_links.
    else.
      raise no_links.
    endif.

  endmethod.                    "select_document_links

  method compare_files.

    data lt_properties     type table of bapiproptb.
    data lv_pernr          type pernr_d.
    data ls_return         type bapiret2.
    data lt_not_in_dpd     type table of ty_links.

    field-symbols <ls_properties> type bapiproptb.
    field-symbols <ls_documents>  type bapidoctab.
    field-symbols <ls_not_in_dpd> type ty_links.

    loop at gt_documents assigning <ls_documents>.

      read table gt_links transporting no fields
      with key guid  = <ls_documents>-objectid
               class = <ls_documents>-docclass
               descr = <ls_documents>-descr.

      if sy-subrc eq 4.

*       Haal personeelsnummer van document op.
        call function 'SRM_RECORD_GETPROPERTIES'
          exporting
            objectid            = <ls_documents>-objectid
            documentclass       = <ls_documents>-docclass
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
          "Proceed.
        endif.

        clear lv_pernr.
        read table lt_properties assigning <ls_properties> with key name = gc_doc_propname.
        if sy-subrc eq 0.
          lv_pernr = <ls_properties>-value.
        endif.

        if ( <ls_documents>-descr is not initial and lv_pernr is not initial ).
          append initial line to lt_not_in_dpd assigning <ls_not_in_dpd>.
          <ls_not_in_dpd>-pernr = lv_pernr.
          <ls_not_in_dpd>-guid  = <ls_documents>-objectid.
          <ls_not_in_dpd>-class = <ls_documents>-docclass.
          <ls_not_in_dpd>-descr = <ls_documents>-descr.

          read table lt_properties assigning <ls_properties> with key name = gc_doc_username.
          if sy-subrc eq 0.
            <ls_not_in_dpd>-user = <ls_properties>-value.
          endif.

          read table lt_properties assigning <ls_properties> with key name = gc_doc_datetime.
          if sy-subrc eq 0.
            <ls_not_in_dpd>-date = <ls_properties>-value.
          endif.

          read table lt_properties assigning <ls_properties> with key name = gc_doc_datetime.
          if sy-subrc eq 0.
            <ls_not_in_dpd>-time = <ls_properties>-value+8.
          endif.
        endif.

      endif.

    endloop.

    if lt_not_in_dpd is not initial.
      sort lt_not_in_dpd by pernr.
      gt_not_in_dpd = lt_not_in_dpd.
    else.
      raise no_files.
    endif.

  endmethod.                    "compare_files

  method select_documents.

    data ls_return         type bapiret2.
    data lt_documents      type table of bapidoctab.
    data lt_documents_tmp  type table of bapidoctab.
    data lt_properties     type table of bapipropqy.

    field-symbols <ls_properties> type bapipropqy.
    field-symbols <ls_persfile>   type bapidoctab.

    loop at gt_persfiles assigning <ls_persfile>.

      clear lt_properties.
      append initial line to lt_properties assigning <ls_properties>.
      <ls_properties>-propname    = gc_doc_propname.
      <ls_properties>-sign        = 'I'.
      <ls_properties>-option      = 'EQ'.
      <ls_properties>-propval_lo  = <ls_persfile>-documentid.

      call function 'SRM_DOCUMENT_GETLIST'
        exporting
          rms_id             = gc_fname_dpf_rms
          sps_id             = gc_fname_dpf_doc
          max_hits           = 100000
        importing
          return             = ls_return
        tables
          property_selection = lt_properties
          resulting_list     = lt_documents_tmp
        exceptions
          not_authorized     = 1
          internal_error     = 2
          parameter_error    = 3
          others             = 4.

      if sy-subrc <> 0.
      else.
        append lines of lt_documents_tmp to lt_documents.
      endif.

    endloop.

    if lt_documents is initial.
      raise no_docs.
    else.
      gt_documents = lt_documents.
    endif.

  endmethod.                    "select_documents

  method show_message.

    call function 'POPUP_DISPLAY_MESSAGE'
      exporting
        titel = 'Foutmelding'
        msgid = 'ZHR_DPD'
        msgty = 'E'
        msgno = iv_num.

  endmethod.                    "show_message

  method select_files.

    data ls_return     type bapiret2.
    data lt_persfiles  type table of bapidoctab.
    data lt_properties type table of bapipropqy.

    field-symbols <ls_properties> type bapipropqy.
    field-symbols <ls_persfile>   type bapidoctab.
    field-symbols <ls_pernr>      type ty_so_pernr.

    if so_pernr is initial.

      call function 'SRM_RECORD_GETLIST'
        exporting
          rms_id          = gc_fname_dpf_rms
          sps_id          = gc_fname_dpf_sps
          max_hits        = 100000
        importing
          return          = ls_return
        tables
          resulting_list  = lt_persfiles
        exceptions
          not_authorized  = 1
          internal_error  = 2
          parameter_error = 3
          others          = 4.

      if sy-subrc <> 0.
        raise error_files.
      endif.

    else.

      loop at so_pernr assigning <ls_pernr>.

        append initial line to lt_properties assigning <ls_properties>.
        <ls_properties>-propname    = gc_file_propname.
        <ls_properties>-sign        = <ls_pernr>-sign.
        <ls_properties>-option      = <ls_pernr>-option.
        <ls_properties>-propval_lo  = <ls_pernr>-low.
        <ls_properties>-propval_hi  = <ls_pernr>-high.

      endloop.

      call function 'SRM_RECORD_GETLIST'
        exporting
          rms_id             = gc_fname_dpf_rms
          sps_id             = gc_fname_dpf_sps
          max_hits           = 100000
        importing
          return             = ls_return
        tables
          property_selection = lt_properties
          resulting_list     = lt_persfiles
        exceptions
          not_authorized     = 1
          internal_error     = 2
          parameter_error    = 3
          others             = 4.

      if sy-subrc <> 0.
        raise error_files.
      endif.

    endif.

    if lt_persfiles is initial.
      raise no_files.
    else.
      gt_persfiles = lt_persfiles.
    endif.

  endmethod.                    "select_files

  method show_files.

    call screen 100.

  endmethod.                    "show_files

  method show_result.

    call screen 100.

  endmethod.                    "show_result

endclass.                    "z_dpf_delete_files IMPLEMENTATION
