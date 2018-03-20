*&---------------------------------------------------------------------*
*&  Include           Z_DELETE_LINK_DPF_CLASS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS z_dpf_delete_link DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_dpf_delete_link DEFINITION.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF ty_element,
        empl      TYPE bapidescr,
        object_id TYPE bapisrmrec-guid,
        docclass  TYPE bapisrmrec-docclass,
        element   TYPE bapielementid,
        name      TYPE char50,
        text      TYPE char25,
      END OF ty_element,

      BEGIN OF ty_result,
        empl      TYPE bapidescr,
        object_id TYPE bapisrmrec-guid,
        docclass  TYPE bapisrmrec-docclass,
        name      TYPE char50,
        text      TYPE char25,
      END OF ty_result.

    DATA:
      gc_alv      TYPE REF TO cl_gui_alv_grid,
      gt_elements TYPE TABLE OF ty_element,
      gt_results  TYPE TABLE OF ty_result,
      gv_process  TYPE boole_d.

    METHODS:
      select_files,
      retrieve_links,
      process_links,
      show_links         IMPORTING iv_fieldcat TYPE char10,
      constructor,
      delete_link        IMPORTING is_element  TYPE ty_element,
      show_message       IMPORTING iv_num      TYPE symsgno.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS:
      gc_propname        TYPE bapipropna VALUE 'SRM_DOCUMENT_ID',
      gc_fname_dpf_rms   TYPE asr_sgpid  VALUE 'DPF_RMS',
      gc_fname_dpf_etype TYPE asr_sgpid  VALUE 'DPF_ETYPE',
      gc_message_text    TYPE char25     VALUE 'Document niet gevonden',
      gc_message_del     TYPE char25     VALUE 'Link document verwijderd',
      gc_message_error   TYPE char25     VALUE 'Fout bij verwijderen link'.

    DATA:
      gt_reslist TYPE TABLE OF bapidoctab.

    METHODS:
      build_fieldcat    IMPORTING iv_fieldcat TYPE char10
                        EXPORTING et_fieldcat TYPE lvc_t_fcat,
      exclude_functions EXPORTING et_exclude  TYPE ui_functions,
      get_layout        IMPORTING iv_fieldcat TYPE char10
                        EXPORTING es_layout   TYPE lvc_s_layo.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS z_dpf_delete_link IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS z_dpf_delete_link IMPLEMENTATION.

  METHOD show_message.

    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel = 'Foutmelding'
        msgid = 'ZHCM_DPD'
        msgty = 'E'
        msgno = iv_num.

  ENDMETHOD.                    "show_message

  METHOD constructor.

    CLEAR gt_elements.
    CLEAR gt_results.
    CLEAR gc_alv.
    CLEAR gv_process.

  ENDMETHOD.

  METHOD select_files.

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

    IF pa_all EQ abap_true.

*     Haal alle dossiers op.
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

    ELSE.

      APPEND INITIAL LINE TO lt_propsel ASSIGNING <ls_propsel>.
      <ls_propsel>-propname   = gc_propname.
      <ls_propsel>-sign       = so_pernr-sign.
      <ls_propsel>-option     = so_pernr-option.
      <ls_propsel>-propval_lo = so_pernr-low.
      <ls_propsel>-propval_hi = so_pernr-high.

*     Haal geselecteerde dossiers op.
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

    ENDIF.

    IF lt_reslist IS INITIAL.
      show_message( EXPORTING iv_num = '016' ).
      EXIT.
    ELSE.
      gt_reslist = lt_reslist.
    ENDIF.

  ENDMETHOD.

  METHOD retrieve_links.

    DATA lt_element   TYPE TABLE OF bapisrmrec_element.
    DATA lt_el_id     TYPE TABLE OF bapipropelement.
    DATA ls_return    TYPE bapiret2.
    DATA lt_elements  TYPE TABLE OF ty_element.

    DATA lv_objid     TYPE  bapisrmrec-guid.
    DATA lv_docclass  TYPE  bapisrmrec-docclass.

    FIELD-SYMBOLS <ls_reslist>  TYPE bapidoctab.
    FIELD-SYMBOLS <ls_element>  TYPE bapisrmrec_element.
    FIELD-SYMBOLS <ls_el_id>    TYPE bapipropelement.
    FIELD-SYMBOLS <ls_elements> TYPE ty_element.

    LOOP AT gt_reslist ASSIGNING <ls_reslist>.

      CLEAR lt_element.
      CLEAR lt_el_id.

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
                 name       = 'DOC_ID'.

        IF sy-subrc EQ 0.

          lv_docclass = <ls_el_id>-value(6).
          lv_objid    = <ls_el_id>-value+10.

          CALL FUNCTION 'SRM_RECORD_EXISTENCECHECK'
            EXPORTING
              objectid        = lv_objid
              documentclass   = lv_docclass
            EXCEPTIONS
              not_authorized  = 1
              internal_error  = 2
              parameter_error = 3
              not_found       = 4
              OTHERS          = 5.

          IF sy-subrc EQ 4.
            APPEND INITIAL LINE TO lt_elements ASSIGNING <ls_elements>.
            <ls_elements>-empl      = <ls_reslist>-descr.
            <ls_elements>-object_id = <ls_reslist>-objectid.
            <ls_elements>-docclass  = <ls_reslist>-docclass.
            <ls_elements>-element   = <ls_el_id>-element_id.
            <ls_elements>-name      = <ls_element>-description.
            <ls_elements>-text      = gc_message_text.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    IF lt_elements IS INITIAL.
      show_message( EXPORTING iv_num = '017' ).
      EXIT.
    ELSE.
      gt_elements = lt_elements.
    ENDIF.

  ENDMETHOD.

  METHOD process_links.

    FIELD-SYMBOLS <ls_elements>  TYPE ty_element.

    LOOP AT gt_elements ASSIGNING <ls_elements>.
*     Verwijder alle dode linkjes.
      delete_link( EXPORTING is_element = <ls_elements> ).
    ENDLOOP.

  ENDMETHOD.

  METHOD show_links.

    DATA lc_container TYPE REF TO cl_gui_custom_container.
    DATA lt_fieldcat  TYPE lvc_t_fcat.
    DATA lt_exclude   TYPE ui_functions.
    DATA ls_layout    TYPE lvc_s_layo.

    IF gc_alv IS NOT BOUND.

      CREATE OBJECT gc_alv
        EXPORTING
          i_parent = cl_gui_container=>screen0.

    ENDIF.

    build_fieldcat( EXPORTING iv_fieldcat = iv_fieldcat
                    IMPORTING et_fieldcat = lt_fieldcat ).

    exclude_functions( IMPORTING et_exclude = lt_exclude ).

    get_layout( EXPORTING iv_fieldcat = iv_fieldcat
                IMPORTING es_layout   = ls_layout ).

    IF iv_fieldcat EQ 'SHOW'.

      CALL METHOD gc_alv->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding          = lt_exclude
          is_layout                     = ls_layout
        CHANGING
          it_outtab                     = gt_elements
          it_fieldcatalog               = lt_fieldcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      CALL SCREEN 100.

    ELSEIF iv_fieldcat EQ 'RESULTS'.

      CALL METHOD gc_alv->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding          = lt_exclude
          is_layout                     = ls_layout
        CHANGING
          it_outtab                     = gt_results
          it_fieldcatalog               = lt_fieldcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      CALL SCREEN 200.

    ENDIF.

  ENDMETHOD.

  METHOD build_fieldcat.

    DATA lt_fieldcat  TYPE lvc_t_fcat.

    FIELD-SYMBOLS <ls_fieldcat> TYPE lvc_s_fcat.

    IF iv_fieldcat EQ 'SHOW'.

      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'EMPL'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Medewerker'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'OBJECT_ID'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Object ID'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'DOCCLASS'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Document class'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'ELEMENT'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Element ID'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'NAME'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Bestandsnaam'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'TEXT'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Melding'.

    ELSEIF iv_fieldcat EQ 'RESULTS'.

      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'EMPL'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Medewerker'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'OBJECT_ID'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Object ID'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'DOCCLASS'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Document class'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'NAME'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Bestandsnaam'.
      APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
      <ls_fieldcat>-fieldname   = 'TEXT'.
      <ls_fieldcat>-tabname     = 'GT_ELEMENTS'.
      <ls_fieldcat>-col_opt     = 'X'.
      <ls_fieldcat>-coltext     = 'Melding'.

    ENDIF.

    et_fieldcat = lt_fieldcat.

  ENDMETHOD.

  METHOD exclude_functions.

    DATA lt_exclude TYPE ui_functions.

    FIELD-SYMBOLS <ls_exclude> TYPE ui_func.

    APPEND INITIAL LINE TO lt_exclude ASSIGNING <ls_exclude>.
    <ls_exclude> = cl_gui_alv_grid=>mc_fc_sort_asc.
    APPEND INITIAL LINE TO lt_exclude ASSIGNING <ls_exclude>.
    <ls_exclude> = cl_gui_alv_grid=>mc_fc_sort_dsc.

    et_exclude = lt_exclude.

  ENDMETHOD.

  METHOD get_layout.

    DATA ls_layout TYPE lvc_s_layo.

    IF iv_fieldcat EQ 'SHOW'.
      ls_layout-sel_mode = 'A'.
    ENDIF.

    es_layout = ls_layout.

  ENDMETHOD.

  METHOD delete_link.

    DATA lt_return    TYPE TABLE OF bapiret2.
    DATA lt_results   TYPE TABLE OF ty_result.
    DATA lt_ident_pos TYPE TABLE OF bapirecpos.

    FIELD-SYMBOLS <ls_results>   TYPE ty_result.
    FIELD-SYMBOLS <ls_ident_pos> TYPE bapirecpos.

    APPEND INITIAL LINE TO lt_ident_pos ASSIGNING <ls_ident_pos>.
    <ls_ident_pos>-rec_nodeid = is_element-element.

    CALL FUNCTION 'BAPI_RECORD_DELETEELEMENTS'
      EXPORTING
        objectid          = is_element-object_id
        documentclass     = is_element-docclass
      TABLES
        elem_ident_recpos = lt_ident_pos
        return            = lt_return.

    APPEND INITIAL LINE TO gt_results ASSIGNING <ls_results>.
    <ls_results>-empl       = is_element-empl.
    <ls_results>-object_id  = is_element-object_id.
    <ls_results>-docclass   = is_element-docclass.
    <ls_results>-name       = is_element-name.

    READ TABLE lt_return WITH KEY type = 'E'
    TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      <ls_results>-text       = gc_message_error.
    ELSE.
      <ls_results>-text       = gc_message_del.
    ENDIF.

    CLEAR lt_ident_pos.

  ENDMETHOD.

ENDCLASS.
