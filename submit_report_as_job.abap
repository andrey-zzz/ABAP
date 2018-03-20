    DATA lv_jobname  TYPE btcjob VALUE 'Z_DPF_UPLOAD'.
    DATA lv_jobcount TYPE btcjobcnt.

*   Open job.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname  = lv_jobname
      IMPORTING
        jobcount = lv_jobcount.

*   Submit report to job.
    SUBMIT z_dpf_upload WITH pa_meta = pa_meta
                        WITH pa_dir  = pa_dir
                        WITH pa_test = pa_test
                        VIA JOB lv_jobname NUMBER lv_jobcount
                        USER sy-uname AND RETURN.

*   Schedule and close job.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount  = lv_jobcount
        jobname   = lv_jobname
        sdlstrtdt = sy-datum
        sdlstrttm = sy-uzeit.
