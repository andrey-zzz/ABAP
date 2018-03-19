  data: lv_line     type string,
        lwa_bupa    type ty_file,
        lwa_output  type ty_output.

  open dataset pa_file for input in text mode encoding default
  with native linefeed.

  if sy-subrc ne 0.
    lwa_output-message = 'Fout bij openen van bestand'.
    append lwa_output to gt_output.
  else.

    clear gt_bupa.

    do.
      clear: lv_line, lwa_bupa.
      read dataset pa_file into lv_line.
      if sy-subrc ne 0.
        exit.
      else.
        move lv_line to lwa_bupa.
        append lwa_bupa to gt_bupa.
      endif.
    enddo.

    close dataset pa_file.

  endif.
