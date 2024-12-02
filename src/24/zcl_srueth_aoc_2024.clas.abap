CLASS zcl_srueth_aoc_2024 DEFINITION
  PUBLIC
  INHERITING FROM zcl_srueth_aoc_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="de">Tag #01</p>
    "!
    METHODS day_01.


    "! <p class="shorttext synchronized" lang="de">Tag #02</p>
    "!
    METHODS day_02.

  PROTECTED SECTION.
    TYPES: BEGIN OF ts_day_02_line,
            numbers TYPE ztt_srueth_int4,
           END OF ts_day_02_line.

    TYPES: ts_day_02_input TYPE TABLE OF ts_day_02_line.


    METHODS day_01_part_1
      IMPORTING it_list_1 TYPE ztt_srueth_int4
                it_list_2 TYPE ztt_srueth_int4.
    METHODS day_01_part_2
      IMPORTING it_list_1 TYPE ztt_srueth_int4
                it_list_2 TYPE ztt_srueth_int4.

    METHODS day_02_part_1
      IMPORTING
        it_input TYPE ts_day_02_input.
    METHODS day_02_part_2
      IMPORTING
        it_input TYPE ts_day_02_input.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_srueth_aoc_2024 IMPLEMENTATION.
  METHOD day_01.
    DATA: lv_tmp_str_1 TYPE string,
          lv_tmp_str_2 TYPE string,
          lv_tmp_i_1   TYPE i,
          lv_tmp_i_2   TYPE i,
          lt_list_1    TYPE TABLE OF i,
          lt_list_2    TYPE TABLE OF i.

    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_line>).
      SPLIT <lv_line> AT space INTO lv_tmp_str_1 lv_tmp_str_2.

      CONDENSE: lv_tmp_str_1, lv_tmp_str_2.

      lv_tmp_i_1 = lv_tmp_str_1.
      APPEND lv_tmp_i_1 TO lt_list_1.

      lv_tmp_i_1 = lv_tmp_str_2.
      APPEND lv_tmp_i_1 TO lt_list_2.
    ENDLOOP.

    SORT lt_list_1 ASCENDING.
    SORT lt_list_2 ASCENDING.

    day_01_part_1(
      EXPORTING it_list_1 = lt_list_1
                it_list_2 = lt_list_2
    ).
    day_01_part_2(
      EXPORTING it_list_1 = lt_list_1
                it_list_2 = lt_list_2
    ).
  ENDMETHOD.

  METHOD day_01_part_1.
    DATA: lv_tmp_i_1   TYPE i,
          lv_tmp_i_2   TYPE i,
          lv_tabix     TYPE i,
          lv_res       TYPE i.

    DO lines( it_list_1 ) TIMES.
      lv_tabix = sy-index.

      READ TABLE it_list_1 INTO lv_tmp_i_1 INDEX lv_tabix.
      IF sy-subrc <> 0.
        WRITE: |DAY01: Can't read index { lv_tabix } of Table 1|.
        RETURN.
      ENDIF.

      READ TABLE it_list_2 INTO lv_tmp_i_2 INDEX lv_tabix.
      IF sy-subrc <> 0.
        WRITE: |DAY01: Can't read index { lv_tabix } of Table 2|.
        RETURN.
      ENDIF.

      SUBTRACT lv_tmp_i_2 FROM lv_tmp_i_1.
      lv_tmp_i_1 = abs( lv_tmp_i_1 ).

      ADD lv_tmp_i_1 TO lv_res.
    ENDDO.

    WRITE: |Part 01: { lv_res }|, /.
  ENDMETHOD.

  METHOD day_01_part_2.
    DATA: lv_similar_lines TYPE i,
          lv_res           TYPE i.

    LOOP AT it_list_1 ASSIGNING FIELD-SYMBOL(<ls_num_1>).
      " Get lines with same number from other table
      lv_similar_lines = REDUCE i( INIT x = 0 FOR wa IN it_list_2 WHERE ( TABLE_LINE = <ls_num_1> ) NEXT x = x + 1 ).

      lv_res += <ls_num_1> * lv_similar_lines.
    ENDLOOP.


    WRITE: |Part 02: { lv_res }|, /.
  ENDMETHOD.

  METHOD day_02.
    DATA: lt_line      TYPE TABLE OF string,
          lt_line_nums TYPE TABLE OF ts_day_02_line.

    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<ls_line>).
      SPLIT <ls_line> AT space INTO TABLE lt_line.

      APPEND INITIAL LINE TO lt_line_nums ASSIGNING FIELD-SYMBOL(<ls_line_nums>).

      LOOP AT lt_line ASSIGNING FIELD-SYMBOL(<ls_num_c>).
        APPEND CONV i( <ls_num_c> ) TO <ls_line_nums>-numbers.
      ENDLOOP.
    ENDLOOP.

    day_02_part_1( lt_line_nums ).
    day_02_part_2( lt_line_nums ).
  ENDMETHOD.

  METHOD day_02_part_1.
    DATA: lv_index              TYPE i,
          lv_diff               TYPE i,
          lv_last_level         TYPE i,
          lv_increasing         TYPE abap_bool,
          lv_increasing_current TYPE abap_bool,
          lv_end_report         TYPE abap_bool,
          lv_safe_reports       TYPE i.

    LOOP AT it_input ASSIGNING FIELD-SYMBOL(<ls_report>).
      CLEAR: lv_last_level, lv_increasing,
             lv_increasing_current, lv_end_report.

      LOOP AT <ls_report>-numbers ASSIGNING FIELD-SYMBOL(<lv_level>).
        lv_index = sy-tabix.
        CLEAR: lv_diff.

        " Checking. TODO: Entfernen
        IF <lv_level> = 0.
          WRITE: 'Found 0!!!', /.
        ENDIF.

        IF lv_last_level IS INITIAL.
          lv_last_level = <lv_level>.
          CONTINUE.
        ENDIF.

        lv_increasing_current = xsdbool( <lv_level> > lv_last_level ).

        " We are on Nr. 2. There are no previous inc- or decreasings.
        IF lv_index = 2.
          lv_increasing = lv_increasing_current.
        ENDIF.

        IF lv_increasing_current <> lv_increasing.
          " Unsafe report
          lv_end_report = abap_true.
          EXIT.
        ENDIF.

        lv_diff = abs( <lv_level> - lv_last_level ).

        IF lv_diff < 1 OR lv_diff > 3.
          lv_end_report = abap_true.
          EXIT.
        ENDIF.

        lv_last_level = <lv_level>.
      ENDLOOP.

      IF lv_end_report = abap_true.
        CONTINUE.
      ENDIF.

      ADD 1 TO lv_safe_reports.
    ENDLOOP.


    WRITE: |Part 01: { lv_safe_reports } Reports are safe|, /.
  ENDMETHOD.

  METHOD day_02_part_2.
  ENDMETHOD.
ENDCLASS.
