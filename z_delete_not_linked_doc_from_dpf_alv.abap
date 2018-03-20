*&---------------------------------------------------------------------*
*&  Include           Z_DELETE_NOT_LINKED_DOC_FROM_DPF_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status '0100'.
  set titlebar '100'.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BUILD_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module build_alv output.

  data ls_layout    type lvc_s_layo.
  data lt_fieldcat  type lvc_t_fcat.
  data lcl_alv      type ref to cl_gui_alv_grid.

  field-symbols <ls_fieldcat> type lvc_s_fcat.

  if lcl_alv is not bound.

    create object lcl_alv
      exporting
        i_parent = cl_gui_container=>screen0.

*   ALV layout aanpassen.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-sel_mode   = 'A'.
    ls_layout-zebra      = 'X'.

*   Laat resultaat zien.
    if zhr_dpd_delete_files=>gt_result is initial.

*     Veldcatalogus aanpassen.
      append initial line to lt_fieldcat assigning <ls_fieldcat>.
      <ls_fieldcat>-fieldname = 'DATE'.
      <ls_fieldcat>-coltext   = 'Datum laatste bewerking'.
      append initial line to lt_fieldcat assigning <ls_fieldcat>.
      <ls_fieldcat>-fieldname = 'TIME'.
      <ls_fieldcat>-coltext   = 'Tijd laatste bewerking'.
      append initial line to lt_fieldcat assigning <ls_fieldcat>.
      <ls_fieldcat>-fieldname = 'USER'.
      <ls_fieldcat>-coltext   = 'Laatste bewerker'.
      append initial line to lt_fieldcat assigning <ls_fieldcat>.
      <ls_fieldcat>-fieldname = 'RESULT'.
      <ls_fieldcat>-coltext   = 'Resultaat'.

      call method lcl_alv->set_table_for_first_display
        exporting
          i_structure_name = 'ZHR_DPD_DELETE_FILES'
          is_layout        = ls_layout
        changing
          it_outtab        = zhr_dpd_delete_files=>gt_not_in_dpd
          it_fieldcatalog  = lt_fieldcat.

    else.

*     Veldcatalogus aanpassen.
      append initial line to lt_fieldcat assigning <ls_fieldcat>.
      <ls_fieldcat>-fieldname = 'DATE'.
      <ls_fieldcat>-coltext   = 'Datum laatste bewerking'.
      append initial line to lt_fieldcat assigning <ls_fieldcat>.
      <ls_fieldcat>-fieldname = 'TIME'.
      <ls_fieldcat>-coltext   = 'Tijd laatste bewerking'.
      append initial line to lt_fieldcat assigning <ls_fieldcat>.
      <ls_fieldcat>-fieldname = 'USER'.
      <ls_fieldcat>-coltext   = 'Laatste bewerker'.
      append initial line to lt_fieldcat assigning <ls_fieldcat>.
      <ls_fieldcat>-fieldname = 'RESULT'.
      <ls_fieldcat>-coltext   = 'Resultaat'.

      call method lcl_alv->set_table_for_first_display
        exporting
          i_structure_name = 'ZHR_DPD_DELETE_FILES'
          is_layout        = ls_layout
        changing
          it_outtab        = zhr_dpd_delete_files=>gt_result
          it_fieldcatalog  = lt_fieldcat.

    endif.
  endif.

endmodule.                 " BUILD_ALV  OUTPUT
