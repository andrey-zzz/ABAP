*&---------------------------------------------------------------------*
*&  Include           Z_DELETE_DOC_FROM_DPF_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUILD_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE build_alv OUTPUT.

  DATA ls_layout    TYPE lvc_s_layo.
  DATA lt_fieldcat  TYPE lvc_t_fcat.
  DATA lcl_alv      TYPE REF TO cl_gui_alv_grid.

  FIELD-SYMBOLS <ls_fieldcat> TYPE lvc_s_fcat.

  IF lcl_alv IS NOT BOUND.

    CREATE OBJECT lcl_alv
      EXPORTING
        i_parent = cl_gui_container=>screen0.

*   ALV layout aanpassen.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-sel_mode   = 'A'.
    ls_layout-zebra      = 'X'.

*   Veldcatalogus aanpassen.
    APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'PERNR'.
    <ls_fieldcat>-col_opt   = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'DESCR'.
    <ls_fieldcat>-col_opt   = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'OBJECTID'.
    <ls_fieldcat>-no_out    = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'DOCCLASS'.
    <ls_fieldcat>-no_out    = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'DOCUMENTID'.
    <ls_fieldcat>-no_out    = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'RESULT'.
    <ls_fieldcat>-no_out    = 'X'.

    CALL METHOD lcl_alv->set_table_for_first_display
      EXPORTING
        i_structure_name = 'ZHR_DPD_DELETE_DOCUMENTS'
        is_layout        = ls_layout
      CHANGING
        it_outtab        = lcl_delete_documents=>gt_documents
        it_fieldcatalog  = lt_fieldcat.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA lv_ucomm   TYPE sy-ucomm.
  DATA lt_row_no  TYPE lvc_t_roid.
  DATA lv_ok      TYPE boole_d.
  DATA ls_return  TYPE bapiret2.
  DATA lv_subrc   TYPE sy-subrc.

  FIELD-SYMBOLS <ls_row_no>   TYPE lvc_s_roid.
  FIELD-SYMBOLS <ls_delete>   TYPE zhr_dpd_delete_documents.

  lv_ucomm = sy-ucomm.

  CASE lv_ucomm.

    WHEN 'BACK'.

      CLEAR lcl_delete_documents=>gt_result.
      CLEAR lcl_delete_documents=>gt_documents.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.

      CLEAR lcl_delete_documents=>gt_result.
      CLEAR lcl_delete_documents=>gt_documents.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.

      CLEAR lcl_delete_documents=>gt_result.
      CLEAR lcl_delete_documents=>gt_documents.
      LEAVE TO SCREEN 0.

    WHEN 'DELETE'.

      IF lcl_delete_documents=>gt_documents IS NOT INITIAL.

        lcl_alv->get_selected_rows( IMPORTING et_row_no = lt_row_no ).

        LOOP AT lt_row_no ASSIGNING <ls_row_no>.

          READ TABLE lcl_delete_documents=>gt_documents ASSIGNING <ls_delete>
          INDEX <ls_row_no>-row_id.
          IF sy-subrc EQ 0.

            lcl_delete_documents=>delete_document( EXPORTING is_file = <ls_delete>
                                                   IMPORTING ep_subrc = lv_subrc ).

            IF sy-subrc EQ 0.
              DELETE lcl_delete_documents=>gt_documents INDEX <ls_row_no>-row_id.
            ELSE.
              lcl_delete_documents=>show_message( iv_num = '001' ).
            ENDIF.

          ENDIF.

        ENDLOOP.

        lcl_alv->refresh_table_display( ).

      ENDIF.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS '0101'.
  SET TITLEBAR '101'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  DATA lv_ucom TYPE sy-ucomm.

  lv_ucom = sy-ucomm.

  CASE lv_ucom.

    WHEN 'BACK'.

      CLEAR lcl_delete_documents=>gt_result.
      CLEAR lcl_delete_documents=>gt_documents.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.

      CLEAR lcl_delete_documents=>gt_result.
      CLEAR lcl_delete_documents=>gt_documents.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.

      CLEAR lcl_delete_documents=>gt_result.
      CLEAR lcl_delete_documents=>gt_documents.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUILD_ALV_RESULT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE build_alv_result OUTPUT.

  DATA ls_layout_res    TYPE lvc_s_layo.
  DATA lt_fieldcat_res  TYPE lvc_t_fcat.
  DATA lcl_alv_res      TYPE REF TO cl_gui_alv_grid.

  FIELD-SYMBOLS <ls_fieldcat_res> TYPE lvc_s_fcat.

  IF lcl_alv_res IS NOT BOUND.

    CREATE OBJECT lcl_alv_res
      EXPORTING
        i_parent = cl_gui_container=>screen0.

*   ALV layout aanpassen.
    ls_layout_res-cwidth_opt = 'X'.
*    ls_layout_res-sel_mode   = 'A'.
    ls_layout_res-zebra      = 'X'.

*   Veldcatalogus aanpassen.
    APPEND INITIAL LINE TO lt_fieldcat_res ASSIGNING <ls_fieldcat_res>.
    <ls_fieldcat_res>-fieldname = 'PERNR'.
    <ls_fieldcat_res>-col_opt   = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat_res ASSIGNING <ls_fieldcat_res>.
    <ls_fieldcat_res>-fieldname = 'DESCR'.
    <ls_fieldcat_res>-col_opt   = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat_res ASSIGNING <ls_fieldcat_res>.
    <ls_fieldcat_res>-fieldname = 'RESULT'.
    <ls_fieldcat_res>-col_opt   = 'X'.
    <ls_fieldcat_res>-coltext   = 'Resultaat'.
    APPEND INITIAL LINE TO lt_fieldcat_res ASSIGNING <ls_fieldcat_res>.
    <ls_fieldcat_res>-fieldname = 'OBJECTID'.
    <ls_fieldcat_res>-no_out    = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat_res ASSIGNING <ls_fieldcat_res>.
    <ls_fieldcat_res>-fieldname = 'DOCCLASS'.
    <ls_fieldcat_res>-no_out    = 'X'.
    APPEND INITIAL LINE TO lt_fieldcat_res ASSIGNING <ls_fieldcat_res>.
    <ls_fieldcat_res>-fieldname = 'DOCUMENTID'.
    <ls_fieldcat_res>-no_out    = 'X'.

    CALL METHOD lcl_alv_res->set_table_for_first_display
      EXPORTING
        i_structure_name = 'ZHR_DPD_DELETE_DOCUMENTS'
        is_layout        = ls_layout_res
      CHANGING
        it_outtab        = lcl_delete_documents=>gt_result
        it_fieldcatalog  = lt_fieldcat_res.

  ENDIF.

ENDMODULE.
