CLASS zcl_srueth_aoc_2025 DEFINITION
  PUBLIC
  INHERITING FROM zcl_srueth_aoc_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS day_01.
    METHODS day_02.
    METHODS day_03.
    METHODS day_04.
    METHODS day_05.
    METHODS day_06.

  PROTECTED SECTION.
    METHODS day03_get_largest_joltage
      IMPORTING iv_bank           TYPE string
                iv_num_batteries  TYPE i
                iv_offset         TYPE i DEFAULT 0
      CHANGING  ct_batteries      TYPE ztt_srueth_aoc_int4
      RETURNING VALUE(rv_joltage) TYPE int8.

    METHODS day04_can_forklift_access
      IMPORTING is_position          TYPE ts_vec2i
                is_size              TYPE ts_vec2i
      RETURNING VALUE(rv_can_access) TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_srueth_aoc_2025 IMPLEMENTATION.
  METHOD day_01.
    DATA: lv_direction   TYPE string,
          lv_dial        TYPE i,
          lv_number      TYPE i,
          lv_solution_p1 TYPE i,
          lv_solution_p2 TYPE i.

    " Dial starts at 50.
    lv_dial = 50.

    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_line>).
      lv_direction = <lv_line>(1).
      lv_number = <lv_line>+1.

      DO lv_number TIMES.
        IF lv_direction = 'R'.
          ADD 1 TO lv_dial.
        ELSEIF lv_direction = 'L'.
          SUBTRACT 1 FROM lv_dial.
        ENDIF.

        lv_dial = lv_dial MOD 100.

        IF lv_dial = 0.
          ADD 1 TO lv_solution_p2.
        ENDIF.
      ENDDO.

      IF lv_dial = 0.
        ADD 1 TO lv_solution_p1.
      ENDIF.
    ENDLOOP.

    WRITE: |Part 1: Solution: { lv_solution_p1 }|, /.
    WRITE: |Part 2: Solution: { lv_solution_p2 }|, /.
  ENDMETHOD.

  METHOD day_02.
    DATA: lv_lower_txt           TYPE string,
          lv_upper_txt           TYPE string,
          lv_counter             TYPE int8,
          lv_char_counter        TYPE int8,
          lv_char                TYPE c,
          lv_number              TYPE int8,
          lv_number_txt          TYPE string,
          lv_number_txt_len      TYPE int8,
          lv_number_txt_len_half TYPE int8,
          lv_first_half          TYPE string,
          lv_second_half         TYPE string,
          lv_pattern_buf         TYPE string,
          lv_tmp_chunk           TYPE string,
          lv_chunk_offset        TYPE i,
          lv_pattern_buf_len     TYPE i,
          lv_num_chunks          TYPE i,
          lv_is_invalid_id       TYPE abap_bool,
          lv_solution_p1         TYPE int8,
          lv_solution_p2         TYPE int8,
          lt_ranges              TYPE tt_i64_range,
          lt_text_ranges         TYPE TABLE OF string.

    " Convert input to ranges
    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_line>).
      CLEAR: lt_text_ranges.

      SPLIT <lv_line> AT ',' INTO TABLE lt_text_ranges.

      LOOP AT lt_text_ranges ASSIGNING FIELD-SYMBOL(<lv_range_text>).
        SPLIT <lv_range_text> AT '-' INTO lv_lower_txt lv_upper_txt.

        APPEND VALUE #( low = lv_lower_txt high = lv_upper_txt ) TO lt_ranges.
      ENDLOOP.
    ENDLOOP.

    " Loop over ranges
    LOOP AT lt_ranges ASSIGNING FIELD-SYMBOL(<ls_range>).
      lv_counter = <ls_range>-low.

      " Loop over every number in the range
      WHILE lv_counter LE <ls_range>-high.
        CLEAR: lv_pattern_buf, lv_char_counter.

        lv_number = lv_counter.
        lv_number_txt = lv_number.
        CONDENSE lv_number_txt.

        lv_number_txt_len = strlen( lv_number_txt ).

        " If the number is halfable compare first and second half.
        IF lv_number_txt_len MOD 2 EQ 0.
          lv_number_txt_len_half = lv_number_txt_len DIV 2.

          lv_first_half  = lv_number_txt(lv_number_txt_len_half).
          lv_second_half = lv_number_txt+lv_number_txt_len_half.

          IF lv_first_half EQ lv_second_half.
            " This is an 'invalid' ID.
            ADD lv_number TO lv_solution_p1.
          ENDIF.
        ENDIF.

        " Loop over all chars in a number for part 2
        DO lv_number_txt_len TIMES.
          CLEAR: lv_chunk_offset.

          lv_char = lv_number_txt+lv_char_counter(1).
          CONCATENATE lv_pattern_buf lv_char INTO lv_pattern_buf.

          lv_pattern_buf_len = lv_char_counter + 1.

          IF lv_number_txt_len EQ lv_pattern_buf_len.
            EXIT.
          ENDIF.

          " Can we divide the number text into buffer-len sized chunks?
          IF lv_number_txt_len MOD lv_pattern_buf_len EQ 0.
            lv_num_chunks = lv_number_txt_len DIV lv_pattern_buf_len.

            " If all 'chunks' match we need to add the number to the solution
            lv_is_invalid_id = abap_true.
            DO lv_num_chunks TIMES.
              lv_tmp_chunk = lv_number_txt+lv_chunk_offset(lv_pattern_buf_len).

              IF lv_pattern_buf NE lv_tmp_chunk.
                lv_is_invalid_id = abap_false.
                EXIT.
              ENDIF.

              ADD lv_pattern_buf_len TO lv_chunk_offset.
            ENDDO.

            IF lv_is_invalid_id = abap_true.
              ADD lv_number TO lv_solution_p2.
              EXIT.
            ENDIF.
          ENDIF.

          ADD 1 TO lv_char_counter.
        ENDDO.

        ADD 1 TO lv_counter.
      ENDWHILE.
    ENDLOOP.

    WRITE: |Part 1: Solution: { lv_solution_p1 }|, /.
    WRITE: |Part 2: Solution: { lv_solution_p2 }|, /.
  ENDMETHOD.

  METHOD day_03.
    DATA: lv_joltage               TYPE int8,
          lv_total_joltage_p1      TYPE i,
          lv_total_joltage_p2      TYPE int8,
          lt_batteries             TYPE ztt_srueth_aoc_int4.

    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_bank>).
      CLEAR: lt_batteries.

      lv_joltage = day03_get_largest_joltage(
        EXPORTING iv_bank          = <lv_bank>
                  iv_num_batteries = 2
        CHANGING  ct_batteries     = lt_batteries
      ).
      ADD lv_joltage TO lv_total_joltage_p1.

      CLEAR: lt_batteries.

      lv_joltage = day03_get_largest_joltage(
        EXPORTING iv_bank          = <lv_bank>
                  iv_num_batteries = 12
        CHANGING  ct_batteries     = lt_batteries
      ).
      ADD lv_joltage TO lv_total_joltage_p2.
    ENDLOOP.


    WRITE: |Part 1: Solution: { lv_total_joltage_p1 }|, /.
    WRITE: |Part 2: Solution: { lv_total_joltage_p2 }|, /.
  ENDMETHOD.

  METHOD day03_get_largest_joltage.
    DATA: lv_do_times    TYPE i,
          lv_offset      TYPE i,
          lv_battery     TYPE i,
          lv_max_battery TYPE i,
          lv_max_offset  TYPE i.

    CLEAR: rv_joltage.

    lv_do_times = strlen( iv_bank ) - iv_num_batteries - iv_offset + lines( ct_batteries ) + 1.

    DO lv_do_times TIMES.
      lv_offset = sy-index - 1 + iv_offset.

      lv_battery = iv_bank+lv_offset(1).

      IF lv_battery > lv_max_battery.
        lv_max_battery = lv_battery.
        lv_max_offset  = lv_offset.
      ENDIF.
    ENDDO.

    APPEND lv_max_battery TO ct_batteries.

    IF lines( ct_batteries ) GE iv_num_batteries.
      LOOP AT ct_batteries ASSIGNING FIELD-SYMBOL(<lv_battery>).
        rv_joltage = rv_joltage * 10 + <lv_battery>.
      ENDLOOP.

      RETURN.
    ENDIF.

    rv_joltage = day03_get_largest_joltage(
      EXPORTING iv_bank          = iv_bank
                iv_num_batteries = iv_num_batteries
                iv_offset        = lv_max_offset + 1
      CHANGING  ct_batteries     = ct_batteries
    ).
  ENDMETHOD.

  METHOD day_04.
    DATA: lv_char             TYPE c,
          lv_can_access       TYPE abap_bool,
          lv_solution_p1      TYPE i,
          lv_solution_p2      TYPE i,
          ls_size             TYPE ts_vec2i,
          ls_position         TYPE ts_vec2i,
          lt_remove_positions TYPE TABLE OF ts_vec2i.

    ls_size-x = strlen( mt_input[ 1 ] ).
    ls_size-y = lines( mt_input ).

    " Part 01: Just check once for accessability
    DO ls_size-y TIMES.
      ls_position-y = sy-index - 1.
      DO ls_size-y TIMES.
        ls_position-x = sy-index - 1.

        lv_char = get_char_xy( ls_position ).

        IF lv_char = '@'.
          " Check neighbors. If there are fewer than four
          " rolls in the neighboring positions then it can be accessed
          lv_can_access = day04_can_forklift_access(
            is_position = ls_position
            is_size     = ls_size
          ).

          IF lv_can_access = abap_true.
            ADD 1 TO lv_solution_p1.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDDO.

    " Part 02: Check until no rolls are removed no more.
    DO.
      CLEAR: lt_remove_positions.

      DO ls_size-y TIMES.
        ls_position-y = sy-index - 1.
        DO ls_size-y TIMES.
          ls_position-x = sy-index - 1.

          lv_char = get_char_xy( ls_position ).

          IF lv_char = '@'.
            " Check neighbors. If there are fewer than four
            " rolls in the neighboring positions then it can be accessed
            lv_can_access = day04_can_forklift_access(
              is_position = ls_position
              is_size     = ls_size
            ).

            IF lv_can_access = abap_true.
              APPEND ls_position TO lt_remove_positions.
            ENDIF.
          ENDIF.
        ENDDO.
      ENDDO.

      IF lines( lt_remove_positions ) = 0.
        EXIT.
      ENDIF.

      LOOP AT lt_remove_positions ASSIGNING FIELD-SYMBOL(<ls_remove_position>).
        " Remove roll
        set_char_xy(
          is_coord = <ls_remove_position>
          iv_value = '.'
        ).

        ADD 1 TO lv_solution_p2.
      ENDLOOP.
    ENDDO.



    WRITE: |Part 01: Solution: { lv_solution_p1 }|, /.
    WRITE: |Part 02: Solution: { lv_solution_p2 }|, /.
  ENDMETHOD.

  METHOD day04_can_forklift_access.
    DATA: lv_char          TYPE c,
          lv_num_neighbors TYPE i,
          ls_new_position  TYPE ts_vec2i,
          lt_check_deltas  TYPE TABLE OF ts_vec2i.

    lt_check_deltas = VALUE #(
      ( x = 0  y =  1 ) " Up
      ( x = 1  y =  1 ) " Up Right
      ( x = 1  y =  0 ) " Right
      ( x = 1  y = -1 ) " Down Right
      ( x = 0  y = -1 ) " Down
      ( x = -1 y = -1 ) " Down Left
      ( x = -1 y =  0 ) " Left
      ( x = -1 y =  1 ) " Up Left
    ).

    LOOP AT lt_check_deltas ASSIGNING FIELD-SYMBOL(<ls_check_delta>).
      " Bounds checks
      IF is_position-x = 0 AND <ls_check_delta>-x < 0.
        CONTINUE.
      ENDIF.
      IF is_position-y = 0 AND <ls_Check_delta>-y < 0.
        CONTINUE.
      ENDIF.
      IF is_position-x >= is_size-x - 1 AND <ls_check_delta>-x > 0.
        CONTINUE.
      ENDIF.
      IF is_position-y >= is_size-y - 1 AND <ls_check_delta>-y > 0.
        CONTINUE.
      ENDIF.

      ls_new_position-x = is_position-x + <ls_check_delta>-x.
      ls_new_position-y = is_position-y + <ls_check_delta>-y.

      lv_char = get_char_xy( ls_new_position ).

      IF lv_char = '@'.
        ADD 1 TO lv_num_neighbors.
      ENDIF.
    ENDLOOP.

    IF lv_num_neighbors < 4.
      rv_can_access = abap_true.
    ELSE.
      rv_can_access = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD day_05.
    DATA: lv_tmp_low             TYPE string,
          lv_tmp_high            TYPE string,
          lv_done_reading_ranges TYPE abap_bool,
          lv_is_fresh            TYPE abap_bool,
          lv_num_ids             TYPE int8,
          lv_solution_p1         TYPE int8,
          lv_solution_p2         TYPE int8,
          lv_highest_id          TYPE int8,
          lt_fresh_id_ranges     TYPE tt_i64_range,
          lt_available_ids       TYPE TABLE OF int8.

    FIELD-SYMBOLS: <ls_id_range> TYPE ts_i64_range.

    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_line>).
      CLEAR: lv_tmp_low, lv_tmp_high.

      IF <lv_line> = ''.
        lv_done_reading_ranges = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_done_reading_ranges = abap_false.
        SPLIT <lv_line> AT '-' INTO lv_tmp_low lv_tmp_high.

        APPEND INITIAL LINE TO lt_fresh_id_ranges ASSIGNING <ls_id_range>.
        <ls_id_range>-low  = lv_tmp_low.
        <ls_id_range>-high = lv_tmp_high.
      ELSE.
        APPEND CONV int8( <lv_line> ) TO lt_available_ids.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_available_ids ASSIGNING FIELD-SYMBOL(<lv_avl_id>).
      CLEAR: lv_is_fresh.

      LOOP AT lt_fresh_id_ranges ASSIGNING <ls_id_range>.
        IF <lv_avl_id> GE <ls_id_range>-low AND <lv_avl_id> LE <ls_id_range>-high.
          lv_is_fresh = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_is_fresh = abap_true.
        ADD 1 TO lv_solution_p1.
      ENDIF.
    ENDLOOP.

    SORT lt_fresh_id_ranges BY low.

    LOOP AT lt_fresh_id_ranges ASSIGNING <ls_id_range>.
      " Check if the low value is less than the highest seen id.
      " If yes, we need to skip/modify some numbers so we don't count them multiple times.
      IF <ls_id_range>-low LE lv_highest_id.
        " Check if the high value is higher than the highest seen id.
        IF <ls_id_range>-high GT lv_highest_id.
          " Adjust low-value to the last highest id. Shortening the range.
          <ls_id_range>-low = lv_highest_id + 1.
        ELSE.
          " If the high-value is also lower than the highest id,
          " we can completely skip the range, since we already counted
          " those IDs.
          CONTINUE.
        ENDIF.
      ENDIF.

      " WARN: With the code above, there may still be the possibility to construct a 'broken'
      "       range. Since we modify the low-value without checking afterwards if it is still
      "       lower than the high-value.

      lv_num_ids = ( <ls_id_range>-high - <ls_id_range>-low ) + 1.
      ADD lv_num_ids TO lv_solution_p2.

      lv_highest_id = <ls_id_range>-high.
    ENDLOOP.

    WRITE: |Part 01: Solution: { lv_solution_p1 }|, /.
    WRITE: |Part 02: Solution: { lv_solution_p2 }|, /.
  ENDMETHOD.

  METHOD day_06.
    TYPES: BEGIN OF ts_day06_problem,
             operation TYPE c LENGTH 1,
             numbers   TYPE ztt_srueth_int8,
           END OF ts_day06_problem.

    DATA: lv_last_len    TYPE i,
          lv_number      TYPE int8,
          lv_index       TYPE i,
          lv_solution_p1 TYPE int8,
          lt_lines       TYPE TABLE OF ztt_srueth_string,
          lt_line        TYPE ztt_srueth_string,
          lt_problems    TYPE TABLE OF ts_day06_problem.

    FIELD-SYMBOLS: <lv_string>  TYPE string,
                   <lv_number>  TYPE int8,
                   <ls_problem> TYPE ts_day06_problem,
                   <lt_line>    TYPE ztt_srueth_string.

    " Transform inputs to table of columns/cells
    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_line>).
      CLEAR: lt_line.

      SPLIT <lv_line> AT space INTO TABLE lt_line.

      LOOP AT lt_line ASSIGNING <lv_string>.
        " Trim string.
        CONDENSE <lv_string>.

        " If the string is empty...
        IF <lv_string> IS INITIAL.
          " ...remove this from the lt_line itab.
          DELETE lt_line.
        ENDIF.
      ENDLOOP.

      APPEND lt_line TO lt_lines.
    ENDLOOP.

    " Fill problems array with initial entries.
    DO lines( lt_lines[ 1 ] ) TIMES.
      APPEND INITIAL LINE TO lt_problems.
    ENDDO.

    " Convert table of columns/cells to table of problems.
    LOOP AT lt_lines ASSIGNING <lt_line>.
      " Sanity-check number of problems is the same on each line.
      IF sy-tabix <> 1 AND lv_last_len <> lines( <lt_line> ).
        MESSAGE |Line { sy-tabix }: Length doesn't match previous. { lines( <lt_line> ) } <> { lv_last_len }| TYPE 'E'.
      ENDIF.

      LOOP AT <lt_line> ASSIGNING <lv_string>.
        lv_index = sy-tabix.

        READ TABLE lt_problems INDEX lv_index ASSIGNING <ls_problem>.
        IF sy-subrc <> 0.
          MESSAGE |Couldn't read index { lv_index } of problems itab.| TYPE 'E'.
        ENDIF.

        TRY.
            lv_number = <lv_string>.
            APPEND lv_number TO <ls_problem>-numbers.
          CATCH cx_sy_conversion_error.
            " It was not a number, so i'm assuming it's the math operation.
            <ls_problem>-operation = <lv_string>.
        ENDTRY.
      ENDLOOP.

      lv_last_len = lines( <lt_line> ).
    ENDLOOP.

    " Calculate the solutions to the problems.
    LOOP AT lt_problems ASSIGNING <ls_problem>.
      CLEAR: lv_number.

      LOOP AT <ls_problem>-numbers ASSIGNING <lv_number>.
        IF sy-tabix = 1.
          lv_number = <lv_number>.
          CONTINUE.
        ENDIF.

        CASE <ls_problem>-operation.
          WHEN '*'.
            lv_number *= <lv_number>.
          WHEN '+'.
            lv_number += <lv_number>.
        ENDCASE.
      ENDLOOP.

      ADD lv_number TO lv_solution_p1.
    ENDLOOP.

    WRITE: |Part 01: Solution: { lv_solution_p1 }|, /.
  ENDMETHOD.
ENDCLASS.
