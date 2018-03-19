    DATA lv_path TYPE rlgrap-filename VALUE '/data/test_aris.csv'.
    DATA lv_line	TYPE string.

    FIELD-SYMBOLS <ls_data> TYPE ty_data.

    OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc NE 0.

    ELSE.

      LOOP AT gt_data ASSIGNING <ls_data>.

        CONCATENATE <ls_data>-file <ls_data>-pernr <ls_data>-message
        INTO lv_line SEPARATED BY ','.

        TRANSFER lv_line TO lv_path.

      ENDLOOP.

      CLOSE DATASET lv_path.

    ENDIF.
