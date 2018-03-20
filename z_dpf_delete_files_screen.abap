*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  data lt_row_no  type lvc_t_roid.
  data lv_ok      type boole_d.
  data ls_return  type bapiret2.

  field-symbols <ls_row_no>   type lvc_s_roid.
  field-symbols <ls_delete>   like zhr_dpd_delete_files=>gt_link_type.

  case sy-ucomm.

    when 'BACK'.

      clear zhr_dpd_delete_files=>gt_not_in_dpd.
      clear zhr_dpd_delete_files=>gt_result.

      leave to screen 0.

    when 'EXIT'.

      clear zhr_dpd_delete_files=>gt_not_in_dpd.
      clear zhr_dpd_delete_files=>gt_result.

      leave to screen 0.

    when 'CANCEL'.

      clear zhr_dpd_delete_files=>gt_not_in_dpd.
      clear zhr_dpd_delete_files=>gt_result.

      leave to screen 0.

    when 'DELETE'.

      if zhr_dpd_delete_files=>gt_not_in_dpd is not initial.

        lcl_alv->get_selected_rows( importing et_row_no = lt_row_no ).

        loop at lt_row_no assigning <ls_row_no>.

          read table zhr_dpd_delete_files=>gt_not_in_dpd assigning <ls_delete>
          index <ls_row_no>-row_id.
          if sy-subrc eq 0.

            if <ls_delete>-result ne 'Document verwijderd'.

              zhr_dpd_delete_files=>delete_document( exporting is_delete = <ls_delete>
                                                     exceptions error_del = 1 ).
              if sy-subrc eq 0.

                call function 'BAPI_TRANSACTION_COMMIT'
                  exporting
                    wait   = 'X'
                  importing
                    return = ls_return.

                <ls_delete>-result = 'Document verwijderd'.

              else.

                call function 'BAPI_TRANSACTION_ROLLBACK'
                  importing
                    return = ls_return.

                <ls_delete>-result = 'Fout bij verwijderen'.

              endif.

            else.
              zhr_dpd_delete_files=>show_message( iv_num = '008').
            endif.
          endif.

        endloop.

        lcl_alv->refresh_table_display( ).

      else.
        zhr_dpd_delete_files=>show_message( iv_num = '007').
      endif.

    when others.

  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
