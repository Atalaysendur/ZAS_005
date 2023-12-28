*&---------------------------------------------------------------------*
*& Include          ZAS_005_I_MAILPROCESSING_CLS
*&---------------------------------------------------------------------*

CLASS lcl_main DEFINITION CREATE PRIVATE FINAL.

  PUBLIC SECTION.
    METHODS: get_data.
    METHODS start_of_selection.
    METHODS end_of_selection.
    METHODS sending_mail.

    CLASS-METHODS: create_instance RETURNING VALUE(ro_main) TYPE REF TO lcl_main.

  PRIVATE SECTION.
    DATA: lt_mailcntnt TYPE TABLE OF  zas_005_s_mailcontent.
*    DATA: lt_bk1 TYPE TABLE OF  zas_005_s_bk1.
    DATA: lS_bk1 TYPE  zas_005_s_bk1.
    CLASS-DATA: mo_main TYPE REF TO lcl_main.

ENDCLASS.


CLASS lcl_main IMPLEMENTATION.

  METHOD create_instance.
    IF mo_main IS INITIAL.
      mo_main = NEW #( ).
    ENDIF.
    ro_main = mo_main.
  ENDMETHOD.

  METHOD get_data.

    SELECT vbeln,
           posnr,
           matnr,
           ntgew,
           brgew,
           gewei
      FROM lips
      WHERE vbeln EQ @P_vbeln
      INTO TABLE @lt_mailcntnt.


    SELECT SINGLE vbeln,
           mail_sender,
           mail_receiver,
           cc,
           bcc
      FROM zas_005_t_bk1
      WHERE vbeln EQ @P_vbeln
      INTO CORRESPONDING FIELDS OF @lS_bk1.



  ENDMETHOD.

  METHOD sending_mail.


    DATA: Lo_gbt       TYPE REF TO cl_gbt_multirelated_service,
          Lo_bcs       TYPE REF TO cl_bcs,
          Lo_doc_bcs   TYPE REF TO cl_document_bcs,
          Lo_recipient TYPE REF TO if_recipient_bcs,
          Lt_soli      TYPE TABLE OF  soli,
          Ls_soli      TYPE soli,
          Lv_status    TYPE bcs_rqst,
          lo_sender    TYPE REF TO if_sender_bcs,
          Lv_content   TYPE string,
          Lv_table     TYPE string.
    DATA: Lv_attachment_size TYPE sood-objlen,
          Lt_att_content_hex TYPE solix_tab,
          Lv_att_content     TYPE string,
          Lv_att_line        TYPE string,
          lo_exc             TYPE REF TO cx_root,
          cc                 TYPE xfeld,
          bcc                TYPE xfeld.



    CREATE OBJECT Lo_gbt.

    DATA: LT_lINES  TYPE TABLE OF tline,
          ls_header TYPE  thead.


    CONDENSE:lv_table.

    CALL FUNCTION 'READ_TEXT' "SO10 Standart test'i okur
      EXPORTING
        id                      = 'ST'
        language                = 'T'
        name                    = 'ZAS_005_STX_MAIL'
        object                  = 'TEXT'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
      IMPORTING
        header                  = ls_header
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = LT_lINES
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'SET_TEXTSYMBOL' "Dynamic alanları program içerisindeki parametreler ile değiştirir
      EXPORTING
        header  = ls_header
        name    = 'LV_TABLE'
        value   = lv_table "string
*       value_length = 9999 "mandatory
        replace = 'X'.
    CALL FUNCTION 'TEXT_SYMBOL_REPLACE' "Dynamic alanları program içerisindeki parametreler ile değiştirir
      EXPORTING
        endline         = 99999
        header          = ls_header
        replace_program = 'X'
        replace_text    = 'X'
      TABLES
        lines           = LT_lINES.

    LOOP AT LT_lINES INTO DATA(ls_lines).
      CONCATENATE lv_content ls_lines-tdline INTO lv_content.
    ENDLOOP.

    LOOP AT lt_mailcntnt INTO DATA(ls_mailcntnt).
      CONDENSE:lv_table.

      lv_table = lv_table &&   '      <tr>                                         '
                              &&   '           <td>' && ls_mailcntnt-vbeln && ' </td>  '
                              &&   '           <td>' && ls_mailcntnt-posnr && ' </td>  '
                              &&   '           <td>' && ls_mailcntnt-matnr && ' </td>  '
                              &&   '           <td>' && ls_mailcntnt-ntgew && '</td>   '
                              &&   '           <td>' && ls_mailcntnt-brgew && '</td>   '
                              &&   '           <td>' && ls_mailcntnt-gewei && '</td>   '
                              &&   '      </tr>                                        '.
    ENDLOOP.

    REPLACE ALL OCCURRENCES OF 'LV_TABLE' IN lv_content WITH lv_table. "TDLINE ALANI sadece 132 karakter olduğu için replace atılır.

    TRY.
        lt_soli = cl_document_bcs=>string_to_soli( lv_content ).

        CALL METHOD lo_gbt->set_main_html
          EXPORTING
            content = lt_soli.


        lo_doc_bcs = cl_document_bcs=>create_from_multirelated(
                       i_subject          =  'Teslimat Maili'
                       i_multirel_service = lo_gbt ).


        LOOP AT lt_mailcntnt INTO ls_mailcntnt.

          DATA(lv_ntgew) = CONV string( ls_mailcntnt-ntgew ).
          DATA(lv_brgew) = CONV string( ls_mailcntnt-brgew ).

          IF sy-tabix EQ 1.
            lv_att_content = lv_att_line.
            CONCATENATE
                            'Teslimat Numarası'
                            'Kalem Numarası'
                            'Malzeme Numarası'
                            'Net Ağırlık'
                            'Brüt Ağırlık'
                            'Birim'
              INTO lv_att_line
              SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
          ELSE.

            CONCATENATE
              ls_mailcntnt-vbeln
              ls_mailcntnt-posnr
              ls_mailcntnt-matnr
              lv_ntgew
              lv_brgew
              ls_mailcntnt-gewei
              INTO lv_att_line
              SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
          ENDIF.
          CONCATENATE lv_att_content
                      lv_att_line
                      INTO lv_att_content
                      SEPARATED BY cl_abap_char_utilities=>newline.
        ENDLOOP.



        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   =   lv_att_content
            iv_codepage =  '4103'
            iv_add_bom  =   'X'
          IMPORTING
            et_solix    =  lt_att_content_hex
            ev_size     =  lv_attachment_size
        ).

        lo_doc_bcs->add_attachment(
          EXPORTING
            i_attachment_type     =  'xls'                  " Document Class for Attachment
            i_attachment_subject  =  'Teslimat'      " Attachment Title
            i_attachment_size     =  lv_attachment_size     " Size of Document Content
            i_att_content_hex     =  lt_att_content_hex     " Content (Binary)
        ).

        lo_recipient = cl_cam_address_bcs=>create_internet_address(
                         i_address_string = ls_bk1-mail_receiver ).

*
        lo_sender = cl_cam_address_bcs=>create_internet_address( ls_bk1-mail_sender ). "değişecek
        lo_sender = cl_sapuser_bcs=>create( sy-uname ).

        lo_bcs = cl_bcs=>create_persistent( ).
        lo_bcs->set_document( i_document = lo_doc_bcs ).
        lo_bcs->set_sender( i_sender  =  lo_sender ).
        lo_bcs->add_recipient( i_recipient  =  lo_recipient ).


        lo_bcs->add_recipient(
       EXPORTING
         i_recipient  = lo_recipient
         i_express    = abap_true
         i_copy       = cc
         i_blind_copy = bcc
         i_no_forward = ' '
     ).
*
        lv_status = 'N'.
        CALL METHOD lo_bcs->set_status_attributes
          EXPORTING
            i_requested_status = lv_status.
*
        lo_bcs->send( ).

        COMMIT WORK.

      CATCH cx_bcs INTO lo_exc.
        MESSAGE lo_exc->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.

      CATCH cx_gbt_mime INTO lo_exc.
        MESSAGE lo_exc->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.

      CATCH cx_bcom_mime INTO lo_exc.
        MESSAGE lo_exc->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    IF sy-subrc EQ 0.
      MESSAGE 'Mail Başarılı Bir Şekilde Gönderildi!' TYPE 'I'.
    ELSE.
      MESSAGE 'Mail Gönderilemedi.' TYPE 'I'.
    ENDIF.


  ENDMETHOD.

  METHOD start_of_selection.
    mo_main->get_data( ).
  ENDMETHOD.

  METHOD end_of_selection.
    IF lS_bk1 IS INITIAL.
      MESSAGE 'Teslimat Numarasına Ait kalem bulunamadı.' TYPE 'I'.
    ELSEIF lt_mailcntnt IS INITIAL.
      MESSAGE 'Gönderilecek mail bilgileri bulunamadı' TYPE 'I'.
    ELSE.
      mo_main->sending_mail( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
