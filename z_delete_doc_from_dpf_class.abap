*&---------------------------------------------------------------------*
*&  Include           Z_DELETE_DOC_FROM_DPF_CLASS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_delete_documents DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_delete_documents DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA gt_documents      TYPE TABLE OF zhr_dpd_delete_documents.
    CLASS-DATA gt_result         TYPE TABLE OF zhr_dpd_delete_documents.

    METHODS get_files       IMPORTING ip_pernr TYPE pernr_d.
    METHODS get_documents   EXCEPTIONS no_documents.
    METHODS show_documents.
    METHODS process_documents.
    METHODS show_result.

    CLASS-METHODS delete_document IMPORTING is_file  TYPE zhr_dpd_delete_documents
                                  EXPORTING ep_subrc TYPE sy-subrc.
    CLASS-METHODS show_message    IMPORTING iv_num   TYPE symsgno.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS gc_fname_dpf_rms    TYPE bapirmsid  VALUE 'ASR_DPF'.
    CONSTANTS gc_fname_dpf_doc    TYPE bapispsid  VALUE 'ASR_SPS_PERSFILE_DOCUMENT'.
    CONSTANTS gc_pernr_propname   TYPE bapipropna VALUE 'ASR_PERSONNEL_NUMBER'.
    CONSTANTS gc_name_propname    TYPE bapipropna VALUE 'DESCRIPTION'.

    DATA gt_files TYPE TABLE OF zhr_dpd_delete_documents.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_delete_documents DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_delete_documents IMPLEMENTATION.

  METHOD get_files.

    DATA message_handler TYPE REF TO if_hrbas_message_handler.
    DATA message_list    TYPE REF TO cl_hrbas_message_list.
    DATA lv_ok           TYPE boole_d.
    DATA ls_files_tmp    TYPE bapidoctab.
    DATA ls_files        TYPE zhr_dpd_delete_documents.

    CREATE OBJECT message_list.
    message_handler ?= message_list.

    CALL METHOD cl_hrasr00_dpf_utilities=>search_dpf
      EXPORTING
        pernr           = ip_pernr
        message_handler = message_handler
      IMPORTING
        is_ok           = lv_ok
        record          = ls_files_tmp.

    IF ls_files_tmp-documentid IS NOT INITIAL.
      CLEAR ls_files.
      ls_files-pernr = ip_pernr.
      MOVE-CORRESPONDING ls_files_tmp TO ls_files.
      APPEND ls_files TO gt_files.
    ENDIF.

    FREE message_list.
    FREE message_handler.

  ENDMETHOD.

  METHOD get_documents.

    DATA lt_properties     TYPE TABLE OF bapipropqy.
    DATA ls_return         TYPE bapiret2.
    DATA lt_docs_tmp       TYPE TABLE OF bapidoctab.
    DATA ls_documents      TYPE zhr_dpd_delete_documents.
    DATA ls_message        TYPE bapiret2.
    DATA lt_elements       TYPE TABLE OF bapisrmrec_element.
    DATA lt_element_ids    TYPE TABLE OF bapisrmrec_element_ident.
    DATA lt_element_props  TYPE TABLE OF bapipropelement.
    DATA lv_valid          TYPE boole_d.

    FIELD-SYMBOLS <ls_files>      TYPE zhr_dpd_delete_documents.
    FIELD-SYMBOLS <ls_properties> TYPE bapipropqy.
    FIELD-SYMBOLS <ls_docs_tmp>   TYPE bapidoctab.

    FIELD-SYMBOLS <ls_elements>      LIKE LINE OF lt_elements.
    FIELD-SYMBOLS <ls_element_ids>   LIKE LINE OF lt_element_ids.
    FIELD-SYMBOLS <ls_element_props> LIKE LINE OF lt_element_props.

    IF p_name IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_properties>.
      <ls_properties>-propname    = gc_name_propname.
      <ls_properties>-sign        = 'I'.
      <ls_properties>-option      = 'CP'.
      <ls_properties>-propval_lo  = p_name.
    ENDIF.

    LOOP AT gt_files ASSIGNING <ls_files>.

      DELETE lt_properties WHERE propname EQ gc_pernr_propname.
      APPEND INITIAL LINE TO lt_properties ASSIGNING <ls_properties>.
      <ls_properties>-propname    = gc_pernr_propname.
      <ls_properties>-sign        = 'I'.
      <ls_properties>-option      = 'EQ'.
      <ls_properties>-propval_lo  = <ls_files>-documentid.

      FREE lt_docs_tmp.
      CALL FUNCTION 'SRM_DOCUMENT_GETLIST'
        EXPORTING
          rms_id             = gc_fname_dpf_rms
          sps_id             = gc_fname_dpf_doc
          max_hits           = 100000
        IMPORTING
          return             = ls_return
        TABLES
          property_selection = lt_properties
          resulting_list     = lt_docs_tmp
        EXCEPTIONS
          not_authorized     = 1
          internal_error     = 2
          parameter_error    = 3
          OTHERS             = 4.

      IF sy-subrc NE 0.
      ELSEIF sy-subrc EQ 0 AND lt_docs_tmp IS NOT INITIAL.

        LOOP AT lt_docs_tmp ASSIGNING <ls_docs_tmp>.
          CLEAR ls_documents.
          ls_documents-pernr = <ls_files>-pernr.
          MOVE-CORRESPONDING <ls_docs_tmp> TO ls_documents.
          APPEND ls_documents TO gt_documents.
        ENDLOOP.

*        IF p_anch IS NOT INITIAL.
*
*          CALL FUNCTION 'SRM_RECORD_GETELEMENTS'
*            EXPORTING
*              objectid               = <ls_files>-objectid
*              documentclass          = <ls_files>-docclass
*            IMPORTING
*              return                 = ls_message
*            TABLES
*              element                = lt_elements
*              element_identification = lt_element_ids
*              element_properties     = lt_element_props
*            EXCEPTIONS
*              not_authorized         = 1
*              parameter_error        = 2
*              container_not_found    = 3
*              internal_error         = 4
*              OTHERS                 = 5.
*
*          LOOP AT lt_element_props ASSIGNING <ls_element_props> WHERE name  = 'ANCHOR'
*                                                                  AND value = p_anch.
*            CLEAR lv_valid.
**            CLEAR ls_documenten.
*            LOOP AT lt_element_ids ASSIGNING <ls_element_ids>
*                                   WHERE element_id = <ls_element_props>-element_id.
*              CASE <ls_element_ids>-name.
*                WHEN 'DOC_ID'.
*                  lv_valid = abap_true.
*                  ls_documenten-document_class = <ls_element_ids>-value+0(10).
*                  ls_documenten-document_id  = <ls_element_ids>-value+10(32).
*                WHEN 'VARIANT'.
*                  ls_documenten-variant = <ls_element_ids>-value.
*                WHEN 'VERSION'.
*                  ls_documenten-version = <ls_element_ids>-value.
*                WHEN OTHERS.
*              ENDCASE.
*            ENDLOOP.   "AT lt_element_ids
*            IF lv_valid = abap_true.
**           Document gevonden. Opslaan in documententabel
*              READ TABLE lt_elements WITH KEY element_id = <ls_element_props>-element_id ASSIGNING <ls_elements>.
*              IF sy-subrc = 0.
*                SPLIT <ls_elements>-description AT '_' INTO  lv_tmp1 lv_tmp2 lv_jaar.   "BESLUITVERLOF_<JAAR>.<EXT>
*                IF lv_tmp1 = 'verlof' AND lv_tmp2 = 'attachment' AND lv_jaar = iv_jaar.
*                  ls_documenten-naam = <ls_elements>-description.
*                  ls_documenten-jaar = iv_jaar.
*                  ls_documenten-pernr = iv_pernr.
*                  APPEND ls_documenten TO me->gt_documenten.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDLOOP.
*
*        ENDIF.
      ENDIF.
    ENDLOOP.

    IF gt_documents IS INITIAL.
      RAISE no_documents.
    ENDIF.

  ENDMETHOD.

  METHOD delete_document.

    DATA ls_return TYPE bapiret2.

    CALL FUNCTION 'SRM_DOCUMENT_DELETE'
      EXPORTING
        objectid        = is_file-objectid
        documentclass   = is_file-docclass
        whole_document  = 'X'
      IMPORTING
        return          = ls_return
      EXCEPTIONS
        internal_error  = 1
        parameter_error = 2
        not_authorized  = 3
        doc_not_found   = 4
        yet_locked      = 5
        OTHERS          = 6.

    IF sy-subrc <> 0.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = ls_return.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = ls_return.

    ENDIF.

    ep_subrc = sy-subrc.

  ENDMETHOD.

  METHOD show_documents.

    CALL SCREEN 100.

  ENDMETHOD.

  METHOD process_documents.

    DATA lv_subrc  TYPE sy-subrc.
    DATA ls_result TYPE zhr_dpd_delete_documents.

    FIELD-SYMBOLS <ls_file> TYPE zhr_dpd_delete_documents.

    LOOP AT gt_documents ASSIGNING <ls_file>.

      lcl_delete_documents=>delete_document( EXPORTING is_file = <ls_file>
                                             IMPORTING ep_subrc = lv_subrc ).

      IF lv_subrc EQ 0.
        MOVE-CORRESPONDING <ls_file> TO ls_result.
        ls_result-result = 'Verwijderd'.
        APPEND ls_result TO lcl_delete_documents=>gt_result.
      ELSE.
        MOVE-CORRESPONDING <ls_file> TO ls_result.
        ls_result-result = 'Fout'.
        APPEND ls_result TO lcl_delete_documents=>gt_result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD show_result.

    CALL SCREEN 101.

  ENDMETHOD.

  METHOD show_message.

    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel = 'Foutmelding'
        msgid = 'ZHR_DPD'
        msgty = 'E'
        msgno = iv_num.

  ENDMETHOD.

ENDCLASS.
