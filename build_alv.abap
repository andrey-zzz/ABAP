   DATA lc_alv       TYPE REF TO cl_gui_alv_grid.
   DATA lc_container TYPE REF TO cl_gui_custom_container.
   DATA lt_fieldcat  TYPE lvc_t_fcat.
   DATA lt_exclude   TYPE ui_functions.

    CREATE OBJECT lc_alv
      EXPORTING
        i_parent = cl_gui_container=>screen0.

    me->build_fieldcat( IMPORTING et_fieldcat = lt_fieldcat ).

    me->exclude_toolbar( IMPORTING et_exclude = lt_exclude ).

    CALL METHOD lc_alv->set_table_for_first_display
      EXPORTING
*       is_layout                     = ls_layout
        it_toolbar_excluding          = lt_exclude
      CHANGING
        it_outtab                     = gt_resultlist
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL SCREEN 100.
