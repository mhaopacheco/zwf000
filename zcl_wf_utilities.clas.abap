class ZCL_WF_UTILITIES definition
  public
  final
  create public .

public section.

  class-methods CREATE_EVENT
    importing
      !ID_OBJTYP type SWO_OBJTYP
      !ID_OBJKEY type SWO_TYPEID
      !ID_EVENT type SWO_EVENT
      !ID_COMMIT type XFELD default 'X'
    exporting
      !ED_SUBRC type SYSUBRC .
  class-methods CREATE_EVENT_CL
    importing
      !ID_EVENT type SIBFEVENT
      !ID_OBJKEY type SIBFINSTID
      !ID_OBJTYPE type SIBFTYPEID
      !IO_CONTAINER type ref to IF_SWF_IFS_PARAMETER_CONTAINER optional
    exporting
      !ED_SUBRC type SYSUBRC .
  class-methods CREATE_SHORTCUT
    importing
      !ID_TRANSACTION type TCODE
      !ID_RECIPIENT_USER_ID type SYUNAME
    exporting
      !ED_CONTENT type STRING
    changing
      !CT_SHORTCUT_PARAM type ZTTWF_SHORTCURT_PAR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WF_UTILITIES IMPLEMENTATION.


METHOD create_event.

  DATA :
*        lit_container TYPE STANDARD TABLE OF   swr_cont ,
*         lst_container LIKE LINE OF lit_container ,
        lst_swr  TYPE swr_struct .

*  DATA : ln_event TYPE swr_struct-event_id .

*  lst_container-element = 'Costos' .
*  lst_container-VALUE = us_zswf_001_areas-costos .
*  APPEND lst_container TO lit_container .

  lst_swr-object_typ = id_objtyp .
  lst_swr-object_key = id_objkey .
  lst_swr-event = id_event .
*  lst_swr-commitflag = 'X' .
*  lst_swr-commitflag = 'X' . XFELD

  CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
  EXPORTING
    object_type       = lst_swr-object_typ
    object_key        = lst_swr-object_key
    event             = lst_swr-event
    commit_work       = id_commit
    event_language    = sy-langu
    language          = sy-langu
    user              = sy-uname
*     IFS_XML_CONTAINER =
  IMPORTING
    return_code       = ed_subrc
*    event_id          = ed_return_event
*  TABLES
*    input_container   = lit_container
*     MESSAGE_LINES     =
*     MESSAGE_STRUCT    =
    .

*  IF NOT ed_event IS INITIAL .
*    MESSAGE i000(zwf) .
**    LEAVE TO SCREEN 0 .
*  ELSE .
*    MESSAGE e004(zwf) WITH id_event id_objtyp .
*  ENDIF .

ENDMETHOD.


METHOD create_event_cl.

*  DATA :
*      lv_objtype          TYPE sibftypeid ,
*      lv_event            TYPE sibfevent ,
*      lv_objkey           TYPE sibfinstid .

  TRY .

*      lv_objtype = zcl_pp001=>ms_typeid .
*      lv_event   = 'CREATED' .
*      lv_objkey = me->solpre .

      CALL METHOD cl_swf_evt_event=>raise
        EXPORTING
          im_objcateg = cl_swf_evt_event=>mc_objcateg_cl
          im_objtype  = id_objtype
          im_event    = id_event
          im_objkey   = id_objkey
          im_event_container = io_container .

      COMMIT WORK.

  CATCH cx_swf_evt_exception .
    ED_SUBRC = 4 .
  ENDTRY .

ENDMETHOD.


METHOD CREATE_SHORTCUT.

*** Declaration for shortcut content
  DATA: PARAMETER TYPE text255,
        v_pernr(12) TYPE C.
  DATA: v_tcode TYPE tcode.

  " Structure's *******************************************************
  DATA: ls_shortcut_param     LIKE LINE OF ct_shortcut_param.

* Check if transaction code is available
  CLEAR v_tcode .
  SELECT SINGLE tcode FROM tstc
  INTO v_tcode
  WHERE tcode EQ id_transaction .

  IF v_tcode IS INITIAL.
    MESSAGE 'Enter a valid transaction' TYPE 'E' DISPLAY LIKE 'A'.
    EXIT.
  ENDIF.

* Populate the parameters to be passed to the shortcut
  IF NOT ct_shortcut_param[] IS INITIAL.
    CLEAR PARAMETER.
    LOOP AT ct_shortcut_param INTO ls_shortcut_param.
      CONCATENATE PARAMETER ls_shortcut_param-fieldname '='
        ls_shortcut_param-fieldvalue ';'
      INTO PARAMETER.
    ENDLOOP.
  ENDIF.

*** create the shortcut content for the required transaction
  CALL FUNCTION 'SWN_CREATE_SHORTCUT'
  EXPORTING
    i_transaction           = id_transaction
    i_parameter             = PARAMETER
    i_sysid                 = sy-sysid
    i_client                = sy-mandt
    i_user                  = id_recipient_user_id
    i_language              = sy-langu
    i_windowsize            = 'Normal window'
  IMPORTING
    shortcut_string         = ed_content
  EXCEPTIONS
    inconsistent_parameters = 1
    OTHERS                  = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDMETHOD.
ENDCLASS.
