LOOP AT lt_ff ASSIGNING <ls_ff>.
CONCATENATE 'LV_' <ls_ff>-fieldname INTO lv_fieldname.
ASSIGN (lv_fieldname) TO <lv_field>.
IF sy-subrc = 0 AND <lv_field> IS ASSIGNED.
<lv_field> = <ls_ff>-fieldvalue.
ENDIF.
ENDLOOP.
