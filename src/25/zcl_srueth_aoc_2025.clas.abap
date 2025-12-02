CLASS zcl_srueth_aoc_2025 DEFINITION
  PUBLIC
  INHERITING FROM zcl_srueth_aoc_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS day_01.
    METHODS day_02.

  PROTECTED SECTION.
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
    TYPES: BEGIN OF tt_i64_range,
             low  TYPE int8,
             high TYPE int8,
           END OF tt_i64_range.

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
          lt_ranges              TYPE TABLE OF tt_i64_range,
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
ENDCLASS.
