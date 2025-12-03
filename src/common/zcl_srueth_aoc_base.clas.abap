CLASS zcl_srueth_aoc_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_vec2i,
             x TYPE i,
             y TYPE i,
           END OF ts_vec2i.


    METHODS constructor
      IMPORTING
        iv_verbose TYPE abap_bool DEFAULT abap_false.
    "! <p class="shorttext synchronized" lang="de">Einen Tag ausführen</p>
    "!
    "! @parameter iv_day | <p class="shorttext synchronized" lang="de"></p>
    "! @raising zcx_srueth_aoc_exception | <p class="shorttext synchronized" lang="de"></p>
    METHODS run
      IMPORTING
        iv_day TYPE zdte_srueth_aoc_day
      RAISING
        zcx_srueth_aoc_exception.

    "! <p class="shorttext synchronized" lang="de">Datei nach Wunsch des Nutzers lesen</p>
    "!
    "! @parameter rt_file_contents | <p class="shorttext synchronized" lang="de">Dateiinhalte</p>
    CLASS-METHODS open_file
      RETURNING VALUE(rt_file_contents) TYPE ztt_srueth_string
      RAISING   zcx_srueth_aoc_exception.

    METHODS get_char_xy
      IMPORTING
        is_coord       TYPE ts_vec2i
      RETURNING
        VALUE(rv_char) TYPE char1.

  PROTECTED SECTION.
    DATA mv_day_meth_prefix TYPE string.
    DATA mv_verbose TYPE abap_bool.
    DATA mt_input TYPE ztt_srueth_string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_srueth_aoc_base IMPLEMENTATION.
  METHOD constructor.
    mv_day_meth_prefix = 'DAY_'.
    mv_verbose         = iv_verbose.
  ENDMETHOD.

  METHOD run.
    DATA: lt_input TYPE ztt_srueth_string.

    mt_input = open_file( ).

    DATA(lv_meth) = |{ mv_day_meth_prefix }{ iv_day }|.

    TRY.
        CALL METHOD (lv_meth).
      CATCH cx_sy_dyn_call_error INTO DATA(lr_dyn_call_ex).
        RAISE EXCEPTION TYPE zcx_srueth_aoc_exception
          EXPORTING
            iv_message = |Methode { lv_meth } der Klasse { cl_abap_classdescr=>get_class_name( me ) } konnte nicht aufgerufen werden|.
    ENDTRY.
  ENDMETHOD.

  METHOD open_file.
    DATA: lv_rc             TYPE i
        , lv_start_dir      TYPE string
        , lt_files          TYPE filetable
        , lv_filepath       TYPE string
        , lt_file_contents  TYPE TABLE OF string
        , ls_acfg           TYPE ztb_srueth_acfg
        .

    SELECT SINGLE *
      FROM ztb_srueth_acfg
      INTO ls_acfg
      WHERE usr = sy-uname.
    IF sy-subrc = 0 AND ls_acfg-last_open_path IS NOT INITIAL.
      lv_start_dir = ls_acfg-last_open_path.
    ENDIF.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        multiselection    = abap_false
        initial_directory = lv_start_dir
      CHANGING
        file_table        = lt_files
        rc                = lv_rc
      EXCEPTIONS
        OTHERS            = 1
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_srueth_aoc_exception
        EXPORTING
          iv_message = |Fehler beim auswählen der Input-Datei|.
    ENDIF.

    IF lines( lt_files ) < 1.
      RAISE EXCEPTION TYPE zcx_srueth_aoc_exception
        EXPORTING
          iv_message = |Keine Datei ausgewählt!|.
    ENDIF.

    lv_filepath = lt_files[ 1 ].

    ls_acfg-last_open_path = lv_filepath.
    UPDATE ztb_srueth_acfg FROM ls_acfg.
    IF sy-subrc <> 0.
      WRITE: 'Error while updating last config'.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename            = lv_filepath
        filetype            = 'ASC'
        has_field_separator = abap_false
      CHANGING
        data_tab            = rt_file_contents
      EXCEPTIONS
        OTHERS              = 1
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_srueth_aoc_exception
        EXPORTING
          iv_message = |Datei konnte nicht gelesen werden!|.
    ENDIF.
  ENDMETHOD.

  METHOD get_char_xy.
    DATA(lv_line) = mt_input[ is_coord-y + 1 ].
    rv_char = lv_line+is_coord-x(1).
  ENDMETHOD.
ENDCLASS.
