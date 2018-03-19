
* 1. Autorisatieobject aan SU21 toevoegen (Object hoort in klasse).
* 2 Onderstaande code aan ABAP toevoegen:

  AUTHORITY-CHECK OBJECT 'Z_UBC_TEST'
           ID 'ACTVT'   FIELD '35'
           ID 'PROGRAM' DUMMY
           ID 'P_GROUP' FIELD 'ZUBC'.

  IF sy-subrc NE 0.
    cp_check = 'X'.
  ENDIF.
