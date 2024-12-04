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

    "! <p class="shorttext synchronized" lang="de">Tag #03</p>
    "!
    METHODS day_03.


    "! <p class="shorttext synchronized" lang="de">Tag #04</p>
    "!
    METHODS day_04.

  PROTECTED SECTION.
    TYPES: BEGIN OF ts_day_02_line,
             numbers TYPE ztt_srueth_int4,
           END OF ts_day_02_line.

    TYPES: ts_day_02_input TYPE TABLE OF ts_day_02_line.

    TYPES: BEGIN OF ts_vec2i,
            x TYPE i,
            y TYPE i,
           END OF ts_vec2i.

    CONSTANTS: gc_day_04_xmas_len TYPE i VALUE 4.


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

    METHODS day_03_part_1
      IMPORTING
        iv_input TYPE string.
    METHODS day_03_part_2
      IMPORTING
        iv_input TYPE string.

    METHODS day_04_part_1.
    METHODS day_04_part_2.

    METHODS day_04_find_xmas_from
      IMPORTING
        iv_x            TYPE i
        iv_y            TYPE i
        iv_size_x       TYPE i
        iv_size_y       TYPE i
        iv_delta_x      TYPE i
        iv_delta_y      TYPE i
      RETURNING
        VALUE(rv_found) TYPE abap_bool.

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
    DATA: lv_index              TYPE i,
          lv_diff               TYPE i,
          lv_last_level         TYPE i,
          lv_increasing         TYPE abap_bool,
          lv_increasing_current TYPE abap_bool,
          lv_end_report         TYPE abap_bool,
          lv_safe_reports       TYPE i,
          ls_report_copy        TYPE ts_day_02_line.

    LOOP AT it_input ASSIGNING FIELD-SYMBOL(<ls_report>).
      DO ( lines( <ls_report>-numbers ) + 1 ) TIMES.
        CLEAR: lv_last_level, lv_increasing,
               lv_increasing_current, lv_end_report.

        ls_report_copy = <ls_report>.

        IF sy-index <> 1.
          DELETE ls_report_copy-numbers INDEX ( sy-index - 1 ).
        ENDIF.

        LOOP AT ls_report_copy-numbers ASSIGNING FIELD-SYMBOL(<lv_level>).
          lv_index = sy-tabix.
          CLEAR: lv_diff.

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
        EXIT.
      ENDDO.
    ENDLOOP.

    WRITE: |Part 02: { lv_safe_reports } Reports are safe|, /.
  ENDMETHOD.

  METHOD day_03.
    DATA: lv_joined_input TYPE string.

    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<ls_line>).
      CONCATENATE lv_joined_input <ls_line> INTO lv_joined_input.
    ENDLOOP.

    day_03_part_1( lv_joined_input ).
    day_03_part_2( lv_joined_input ).
  ENDMETHOD.

  METHOD day_03_part_1.
    DATA: lv_tmp_number1 TYPE i,
          lv_tmp_number2 TYPE i,
          lv_result      TYPE i.

    FIND ALL OCCURRENCES OF PCRE 'mul\((\d{1,3}),(\d{1,3})\)'
      IN iv_input
      RESULTS DATA(lt_rgx_res).

    LOOP AT lt_rgx_res ASSIGNING FIELD-SYMBOL(<ls_match>).
      READ TABLE <ls_match>-submatches ASSIGNING FIELD-SYMBOL(<ls_sm1>) INDEX 1.
      IF sy-subrc <> 0.
        WRITE: |Part 1: No submatches!|, /.
        RETURN.
      ENDIF.

      READ TABLE <ls_match>-submatches ASSIGNING FIELD-SYMBOL(<ls_sm2>) INDEX 2.
      IF sy-subrc <> 0.
        WRITE: |Part 1: No second submatch|, /.
        RETURN.
      ENDIF.

      lv_tmp_number1 = iv_input+<ls_sm1>-offset(<ls_sm1>-length).
      lv_tmp_number2 = iv_input+<ls_sm2>-offset(<ls_sm2>-length).

      lv_result += lv_tmp_number1 * lv_tmp_number2.
    ENDLOOP.

    WRITE: |Part 1: Result: { lv_result }|, /.
  ENDMETHOD.

  METHOD day_03_part_2.
    DATA: lv_disabled    TYPE abap_bool VALUE abap_false,
          lv_tmp_str     TYPE string,
          lv_tmp_number1 TYPE i,
          lv_tmp_number2 TYPE i,
          lv_result      TYPE i.

    FIND ALL OCCURRENCES OF PCRE 'mul\((\d{1,3}),(\d{1,3})\)'
      IN iv_input
      RESULTS DATA(lt_rgx_res).

    FIND ALL OCCURRENCES OF PCRE `(do\(\)|don't\(\))`
      IN iv_input
      RESULTS DATA(lt_rgx_do_dont).

    SORT lt_rgx_do_dont BY offset ASCENDING.

    LOOP AT lt_rgx_res ASSIGNING FIELD-SYMBOL(<ls_match>).
      READ TABLE <ls_match>-submatches ASSIGNING FIELD-SYMBOL(<ls_sm1>) INDEX 1.
      IF sy-subrc <> 0.
        WRITE: |Part 1: No submatches!|, /.
        RETURN.
      ENDIF.

      READ TABLE <ls_match>-submatches ASSIGNING FIELD-SYMBOL(<ls_sm2>) INDEX 2.
      IF sy-subrc <> 0.
        WRITE: |Part 1: No second submatch|, /.
        RETURN.
      ENDIF.

      LOOP AT lt_rgx_do_dont ASSIGNING FIELD-SYMBOL(<ls_do_dont_match>)
                             WHERE offset < <ls_match>-offset.
        lv_tmp_str = iv_input+<ls_do_dont_match>-offset(<ls_do_dont_match>-length).

        IF lv_tmp_str = 'do()'.
          lv_disabled = abap_false.
        ELSEIF lv_tmp_str = 'don''t()'.
          lv_disabled = abap_true.
        ENDIF.
      ENDLOOP.

      IF lv_disabled = abap_true.
        CONTINUE.
      ENDIF.

      lv_tmp_number1 = iv_input+<ls_sm1>-offset(<ls_sm1>-length).
      lv_tmp_number2 = iv_input+<ls_sm2>-offset(<ls_sm2>-length).

      lv_result += lv_tmp_number1 * lv_tmp_number2.
    ENDLOOP.

    WRITE: |Part 2: Result: { lv_result }|, /.
  ENDMETHOD.

  METHOD day_04.
    DATA: lv_last_strlen TYPE i.

    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<ls_line>).
      IF lv_last_strlen IS INITIAL.
        lv_last_strlen = strlen( <ls_line> ).
        CONTINUE.
      ENDIF.

      IF lv_last_strlen <> strlen( <ls_line> ).
        WRITE: 'Different line lengths. Invalid input!', /.
        RETURN.
      ENDIF.

      lv_last_strlen = strlen( <ls_line> ).
    ENDLOOP.

    day_04_part_1( ).
    day_04_part_2( ).
  ENDMETHOD.

  METHOD day_04_part_1.
    DATA: lv_size_x     TYPE i,
          lv_size_y     TYPE i,
          lv_idx_x      TYPE i,
          lv_idx_y      TYPE i,
          lv_result     TYPE i,
          lv_xmas_found TYPE abap_bool,
          lt_directions TYPE TABLE OF ts_vec2i.

    FIELD-SYMBOLS: <ls_line> TYPE string.


    lv_size_x = lines( mt_input ).
    lv_size_y = strlen( mt_input[ 1 ] ).

    lt_directions = VALUE #(
      ( x = 0 y = 1 ) " N
      ( x = 1 y = 1 ) " NE
      ( x = 1 y = 0 ) " E
      ( x = 1 y = -1 ) " SE
      ( x = 0 y = -1 ) " S
      ( x = -1 y = -1 ) " SW
      ( x = -1 y = 0 ) " W
      ( x = -1 y = 1 ) " NW
    ).

    DO lv_size_y TIMES.
      lv_idx_y = sy-index - 1.
      DO lv_size_x TIMES.
        lv_idx_x = sy-index - 1.

        " Check all directions
        LOOP AT lt_directions ASSIGNING FIELD-SYMBOL(<ls_direction>).
          lv_xmas_found = day_04_find_xmas_from(
            iv_x       = lv_idx_x
            iv_y       = lv_idx_y
            iv_size_x  = lv_size_x
            iv_size_y  = lv_size_y
            iv_delta_x = <ls_direction>-x
            iv_delta_y = <ls_direction>-y
          ).

          IF lv_xmas_found = abap_true.
            ADD 1 TO lv_result.
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDDO.

    WRITE: |Part 01: Found 'XMAS' { lv_result } times.|, /.
  ENDMETHOD.

  METHOD day_04_part_2.
  ENDMETHOD.

  METHOD day_04_find_xmas_from.
    DATA: lv_x_end   TYPE i,
          lv_y_end   TYPE i,
          lv_idx_x   TYPE i,
          lv_idx_y   TYPE i,
          lv_str     TYPE string,
          lv_str_tmp TYPE string.

    rv_found = abap_false.

    lv_x_end = iv_x + ( iv_delta_x * gc_day_04_xmas_len ).
    lv_y_end = iv_y + ( iv_delta_y * gc_day_04_xmas_len ).

    IF lv_x_end > iv_size_x
    OR lv_x_end < -1
    OR lv_y_end > iv_size_y
    OR lv_y_end < -1.
      RETURN.
    ENDIF.

    lv_idx_x = iv_x.
    lv_idx_y = iv_y.

    DO gc_day_04_xmas_len TIMES.

      lv_str_tmp = mt_input[ lv_idx_y + 1 ].
      lv_str = |{ lv_str }{ lv_str_tmp+lv_idx_x(1) }|.

      IF lv_str NA 'XS'.
        RETURN.
      ENDIF.

      ADD iv_delta_x TO lv_idx_x.
      ADD iv_delta_y TO lv_idx_y.
    ENDDO.

    IF lv_str = 'XMAS'.
      rv_found = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
