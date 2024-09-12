*&---------------------------------------------------------------------*
*& Report zpg_srueth_aoc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_srueth_aoc.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_y2023 RADIOBUTTON GROUP year DEFAULT 'X'
            , p_y2024 RADIOBUTTON GROUP year
            .
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_d01 RADIOBUTTON GROUP day DEFAULT 'X'
            , p_d02 RADIOBUTTON GROUP day
            , p_d03 RADIOBUTTON GROUP day
            , p_d04 RADIOBUTTON GROUP day
            , p_d05 RADIOBUTTON GROUP day
            , p_d06 RADIOBUTTON GROUP day
            , p_d07 RADIOBUTTON GROUP day
            , p_d08 RADIOBUTTON GROUP day
            , p_d09 RADIOBUTTON GROUP day
            , p_d10 RADIOBUTTON GROUP day
            , p_d11 RADIOBUTTON GROUP day
            , p_d12 RADIOBUTTON GROUP day
            , p_d13 RADIOBUTTON GROUP day
            , p_d14 RADIOBUTTON GROUP day
            , p_d15 RADIOBUTTON GROUP day
            , p_d16 RADIOBUTTON GROUP day
            , p_d17 RADIOBUTTON GROUP day
            , p_d18 RADIOBUTTON GROUP day
            , p_d19 RADIOBUTTON GROUP day
            , p_d20 RADIOBUTTON GROUP day
            , p_d21 RADIOBUTTON GROUP day
            , p_d22 RADIOBUTTON GROUP day
            , p_d23 RADIOBUTTON GROUP day
            , p_d24 RADIOBUTTON GROUP day
            , p_d25 RADIOBUTTON GROUP day
            .
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
  PARAMETERS: p_verb TYPE zdte_srueth_aoc_verbose DEFAULT abap_true AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b03.

*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
  PERFORM aa_initialization.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  PERFORM aa_start_of_selection.

END-OF-SELECTION.

FORM aa_initialization.
  DATA: ls_cfg TYPE ztb_srueth_acfg.

  SELECT SINGLE *
    FROM ztb_srueth_acfg
    INTO ls_cfg
    WHERE usr = sy-uname.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF ls_cfg-last_sel_day IS NOT INITIAL.
    p_d01 = space.
    PERFORM set_day
      USING ls_cfg-last_sel_day.
  ENDIF.

  IF ls_cfg-last_sel_year IS NOT INITIAL.
    p_y2023 = space.
    PERFORM set_year
      USING ls_cfg-last_sel_year.
  ENDIF.
ENDFORM.

FORM aa_start_of_selection.
  DATA: lv_day      TYPE zdte_srueth_aoc_day
      , lv_year     TYPE zdte_srueth_aoc_year
      , lv_clsname  TYPE seoclsname
      , lr_aoc_year TYPE REF TO zcl_srueth_aoc_base
      .

  PERFORM save_options.

  PERFORM get_year
    CHANGING lv_year.

  lv_clsname = |ZCL_SRUETH_AOC_{ lv_year }|.

  TRY.
      CREATE OBJECT lr_aoc_year
        TYPE (lv_clsname)
        EXPORTING
          iv_verbose = p_verb.
    CATCH cx_sy_create_object_error INTO DATA(lr_create_obj_ex).
      WRITE: |Objekt der Klasse "{ lv_clsname }" konnte nicht erstellt werden! ({ lr_create_obj_ex->get_longtext( ) })|.
      RETURN.
  ENDTRY.

  PERFORM get_day
    CHANGING lv_day.

  TRY.
      lr_aoc_year->run( iv_day = lv_day ).
    CATCH zcx_srueth_aoc_exception INTO DATA(lr_aoc_ex).
      WRITE: lr_aoc_ex->get_message( ).
  ENDTRY.
ENDFORM.

FORM get_year CHANGING cv_year TYPE zdte_srueth_aoc_year.
  IF p_y2023 = abap_true.
    cv_year = 2023.
  ELSEIF p_y2024 = abap_true.
    cv_year = 2024.
  ELSE.
    " TODO: Error, shouldn't happen
  ENDIF.
ENDFORM.

FORM set_year USING iv_year TYPE zdte_srueth_aoc_year.
  CASE iv_year.
    WHEN 2023.
      p_y2023 = 'X'.
    WHEN 2024.
      p_y2024 = 'X'.
    WHEN OTHERS.
      WRITE: |Invalid year "{ iv_year }"!|.
  ENDCASE.
ENDFORM.

FORM get_day CHANGING cv_day TYPE zdte_srueth_aoc_day.
  IF p_d01 = abap_true.
    cv_day = 01.
  ELSEIF p_d02 = abap_true.
    cv_day = 02.
  ELSEIF p_d03 = abap_true.
    cv_day = 03.
  ELSEIF p_d04 = abap_true.
    cv_day = 04.
  ELSEIF p_d05 = abap_true.
    cv_day = 05.
  ELSEIF p_d06 = abap_true.
    cv_day = 06.
  ELSEIF p_d07 = abap_true.
    cv_day = 07.
  ELSEIF p_d08 = abap_true.
    cv_day = 08.
  ELSEIF p_d09 = abap_true.
    cv_day = 09.
  ELSEIF p_d10 = abap_true.
    cv_day = 10.
  ELSEIF p_d11 = abap_true.
    cv_day = 11.
  ELSEIF p_d12 = abap_true.
    cv_day = 12.
  ELSEIF p_d13 = abap_true.
    cv_day = 13.
  ELSEIF p_d14 = abap_true.
    cv_day = 14.
  ELSEIF p_d15 = abap_true.
    cv_day = 15.
  ELSEIF p_d16 = abap_true.
    cv_day = 16.
  ELSEIF p_d17 = abap_true.
    cv_day = 17.
  ELSEIF p_d18 = abap_true.
    cv_day = 18.
  ELSEIF p_d19 = abap_true.
    cv_day = 19.
  ELSEIF p_d20 = abap_true.
    cv_day = 20.
  ELSEIF p_d21 = abap_true.
    cv_day = 21.
  ELSEIF p_d22 = abap_true.
    cv_day = 22.
  ELSEIF p_d23 = abap_true.
    cv_day = 23.
  ELSEIF p_d24 = abap_true.
    cv_day = 24.
  ELSEIF p_d25 = abap_true.
    cv_day = 25.
  ELSE.
    " TODO: Error, shouldn't happen
  ENDIF.
ENDFORM.

FORM set_day USING iv_day TYPE zdte_srueth_aoc_day.
  CASE iv_day.
    WHEN 01.
      p_d01 = 'X'.
    WHEN 02.
      p_d02 = 'X'.
    WHEN 03.
      p_d03 = 'X'.
    WHEN 04.
      p_d04 = 'X'.
    WHEN 05.
      p_d05 = 'X'.
    WHEN 06.
      p_d06 = 'X'.
    WHEN 07.
      p_d07 = 'X'.
    WHEN 08.
      p_d08 = 'X'.
    WHEN 09.
      p_d09 = 'X'.
    WHEN 10.
      p_d10 = 'X'.
    WHEN 11.
      p_d11 = 'X'.
    WHEN 12.
      p_d12 = 'X'.
    WHEN 13.
      p_d13 = 'X'.
    WHEN 14.
      p_d14 = 'X'.
    WHEN 15.
      p_d15 = 'X'.
    WHEN 16.
      p_d16 = 'X'.
    WHEN 17.
      p_d17 = 'X'.
    WHEN 18.
      p_d18 = 'X'.
    WHEN 19.
      p_d19 = 'X'.
    WHEN 20.
      p_d20 = 'X'.
    WHEN 21.
      p_d21 = 'X'.
    WHEN 22.
      p_d22 = 'X'.
    WHEN 23.
      p_d23 = 'X'.
    WHEN 24.
      p_d24 = 'X'.
    WHEN 25.
      p_d25 = 'X'.
    WHEN OTHERS.
      WRITE: |Day { iv_day } is not valid|.
  ENDCASE.
ENDFORM.

FORM save_options.
  DATA: lv_day  TYPE zdte_srueth_aoc_day
      , lv_year TYPE zdte_srueth_aoc_year
      .

  PERFORM get_day
    CHANGING lv_day.

  PERFORM get_year
    CHANGING lv_year.

  SELECT COUNT( * )
    FROM ztb_srueth_acfg
    INTO @DATA(lv_cnt).

  IF sy-subrc = 0.
    UPDATE ztb_srueth_acfg
      SET last_sel_verbose = p_verb
          last_sel_day     = lv_day
          last_sel_year    = lv_year
      WHERE usr = sy-uname.
    IF sy-subrc <> 0.
      WRITE: 'Error writing options to DB!'.
    ENDIF.
    RETURN.
  ENDIF.

  INSERT INTO ztb_srueth_acfg
    VALUES @( VALUE ztb_srueth_acfg(
      usr              = sy-uname
      last_sel_verbose = p_verb
      last_sel_day     = lv_day
      last_sel_year    = lv_year
    ) ).
  IF sy-subrc <> 0.
    WRITE: 'Error writing options to DB!'.
  ENDIF.
ENDFORM.
