REPORT shared_buffer.

TYPES: BEGIN OF ty_crm,
        kernwaarde_code TYPE char2,
        hoeveelheid     TYPE char16,
        bedrag          TYPE char16,
       END OF ty_crm.

DATA: lt_crm  TYPE STANDARD TABLE OF zkernwaarde_code,
      ls_crm  TYPE zkernwaarde_code,
      lt_crm2 TYPE STANDARD TABLE OF zkernwaarde_code,
      ls_crm2 TYPE zkernwaarde_code.

ls_crm-kernwaarde_code = '02'.
ls_crm-hoeveelheid     = '1'.
ls_crm-bedrag          = '500'.

APPEND ls_crm TO lt_crm.

EXPORT lt_crm TO SHARED BUFFER zkernwaarde_code(ar) FROM ls_crm ID 'ARIS1'.

IMPORT lt_crm2 FROM SHARED BUFFER zkernwaarde_code(ar) TO ls_crm2 ID 'ARIS1'.
