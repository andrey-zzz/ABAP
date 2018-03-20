  DO 12 TIMES VARYING dat-dar FROM ls_p0041-dar01 NEXT ls_p0041-dar02.
    IF dat-dar EQ lv_dar.
      lv_index = sy-index.
      CONCATENATE 'ME->GS_P0041-DAR' lv_index INTO lv_dar_field.
      ASSIGN (lv_dar_field) TO <fs_dar>.
      CONCATENATE 'ME->GS_P0041-DAT' lv_index INTO lv_dat_field.
      ASSIGN (lv_dat_field) TO <fs_dat>.
      <fs_dar> = lv_dar.
      <fs_dat> = me->gv_hire_date.
      EXIT.
    ENDIF.
  ENDDO.
