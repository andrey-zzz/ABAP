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
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA lv_ucomm    TYPE sy-ucomm.
  DATA lt_rows     TYPE lvc_t_row.

  FIELD-SYMBOLS <ls_row>      TYPE lvc_s_row.
  FIELD-SYMBOLS <ls_elements> TYPE lo_dpd_delete_link->ty_element.

  lv_ucomm = sy-ucomm.

  CASE lv_ucomm.

    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.

      LEAVE TO SCREEN 0.

    WHEN 'PROCESS'.

      CALL METHOD lo_dpd_delete_link->gc_alv->get_selected_rows
        IMPORTING
          et_index_rows = lt_rows.

      LOOP AT lt_rows ASSIGNING <ls_row>.

        READ TABLE lo_dpd_delete_link->gt_elements ASSIGNING <ls_elements> INDEX <ls_row>-index.
        IF sy-subrc EQ 0.
          lo_dpd_delete_link->delete_link( EXPORTING is_element = <ls_elements> ).
        ENDIF.

      ENDLOOP.

*     Toon resultaat.
      lo_dpd_delete_link->show_links( EXPORTING iv_fieldcat = 'RESULTS' ).

    WHEN 'OTHERS'.
      "Doe niets.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA lv_ucom    TYPE sy-ucomm.

  lv_ucom = sy-ucomm.

  CASE lv_ucom.

    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.

      LEAVE TO SCREEN 0.

    WHEN 'OTHERS'.
      "Doe niets.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '200'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           Z_DELETE_LINK_DPF_SCREEN
*&---------------------------------------------------------------------*
TABLES p0001.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS so_pernr FOR p0001-pernr.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS pa_all TYPE boolean AS CHECKBOX.
PARAMETERS pa_dir TYPE boolean AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.
