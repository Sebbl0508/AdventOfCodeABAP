CLASS zcl_srueth_aoc_2023 DEFINITION
  PUBLIC
  INHERITING FROM zcl_srueth_aoc_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="de">Tag #01</p>
    METHODS day_01.

    "! <p class="shorttext synchronized" lang="de">Tag #02</p>
    "!
    METHODS day_02.

    METHODS day_01_part_1.
    METHODS day_01_part_2.

    METHODS day_02_part_1.
    METHODS day_02_part_2.

    METHODS find_numbers_by_regex
      IMPORTING iv_line           TYPE string
      RETURNING VALUE(rt_numbers) TYPE ztt_srueth_string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SRUETH_AOC_2023 IMPLEMENTATION.


METHOD day_01_part_1.
  DATA: lv_char    TYPE c LENGTH 1
    , lv_first_num TYPE c LENGTH 1
    , lv_last_num  TYPE c LENGTH 1
    , lv_number    TYPE i
    , lv_result    TYPE i
    , lt_digits    TYPE TABLE OF c.
  .

  LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_line>).
    CLEAR: lv_char, lv_first_num, lv_last_num, lv_number, lt_digits.

    " Loop over chars in current line
    DO strlen( <lv_line> ) TIMES.
      DATA(lv_offset) = sy-index - 1.

      lv_char = <lv_line>+lv_offset(1).

      " Is this a digit?
      IF lv_char CA zcl_srueth_aoc_constants=>mc_digits.
        APPEND lv_char TO lt_digits.
      ENDIF.
    ENDDO.

    IF lt_digits IS INITIAL.
      IF mv_verbose = abap_true.
        WRITE: |Keine Nummer gefunden in Zeile gefunden! ("{ <lv_line> }")|.
      ENDIF.
      CONTINUE.
    ENDIF.

    lv_first_num = lt_digits[ 1 ].
    lv_last_num  = lt_digits[ lines( lt_digits ) ].

    IF mv_verbose = abap_true.
      WRITE: |Found "{ lv_first_num }{ lv_last_num }" for line "{ <lv_line> }"|, /.
    ENDIF.

    lv_number = |{ lv_first_num }{ lv_last_num }|.
    ADD lv_number TO lv_result.
  ENDLOOP.

  WRITE: 'Part 1 Result: ', lv_result, /.
ENDMETHOD.


METHOD day_02_part_1.
  TYPES: BEGIN OF ts_cube_set,
           game       TYPE i,
           set        TYPE i,
           cube_count TYPE i,
           color      TYPE string,
         END OF ts_cube_set.

  DATA: ls_cube_set     TYPE ts_cube_set,
        lv_game_id      TYPE i,
        lv_set_idx      TYPE i,
        lv_tmp_str      TYPE string,
        lt_cube_sets    TYPE TABLE OF ts_cube_set,
        lt_splits       TYPE TABLE OF string,
        lt_cube_split   TYPE TABLE OF string,
        lt_cube_split2  TYPE TABLE OF string.

  LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<ls_line>).
    CLEAR: ls_cube_set
         , lt_splits
         , lt_cube_split
         , lt_cube_split2
         .

    SPLIT <ls_line> AT ':' INTO TABLE lt_splits.

    " Set game ID
    lv_tmp_str = lt_splits[ 1 ].
    ls_cube_set-game = lv_tmp_str+4.

    lv_tmp_str = lt_splits[ 2 ].
    CLEAR lt_splits.
    SPLIT lv_tmp_str AT ';' INTO TABLE lt_splits.

    " LT_SPLITS is now split by sets
    LOOP AT lt_splits ASSIGNING FIELD-SYMBOL(<ls_set>).
      ls_cube_set-set = sy-index.

      CONDENSE <ls_set>.
      SPLIT <ls_set> AT ',' INTO TABLE lt_cube_split.

      LOOP AT lt_cube_split ASSIGNING FIELD-SYMBOL(<ls_cube>).
        SPLIT <ls_cube> AT space INTO TABLE lt_cube_split2.


      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD day_01_part_2.
  DATA: lv_result    TYPE i
      , lv_first_num TYPE i
      , lv_last_num  TYPE i
      , lv_tmp_num   TYPE i
      .

  LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_line>).
    DATA(lt_numbers) = find_numbers_by_regex( <lv_line> ).

    IF lines( lt_numbers ) = 0.
      IF mv_verbose = abap_true.
        WRITE: |Found no numbers in line! ("{ <lv_line> }")|, /.
      ENDIF.
      CONTINUE.
    ENDIF.

    " First number
    TRY.
      lv_first_num = zcl_srueth_aoc_utils=>word_digit_to_digit( lt_numbers[ 1 ] ).
    CATCH zcx_srueth_aoc_exception.
      lv_first_num = lt_numbers[ 1 ].
    ENDTRY.

    " Last number
    TRY.
      lv_last_num = zcl_srueth_aoc_utils=>word_digit_to_digit( lt_numbers[ lines( lt_numbers ) ] ).
    CATCH zcx_srueth_aoc_exception.
      lv_last_num = lt_numbers[ lines( lt_numbers ) ].
    ENDTRY.

    IF mv_verbose = abap_true.
      WRITE: |Found "{ lv_first_num }{ lv_last_num }" for line "{ <lv_line> }"|, /.
    ENDIF.

    lv_tmp_num = |{ lv_first_num }{ lv_last_num }|.
    ADD lv_tmp_num TO lv_result.
  ENDLOOP.

  WRITE: 'Part 2 Result:', lv_result, /.
ENDMETHOD.


METHOD find_numbers_by_regex.
  FIND ALL OCCURRENCES OF PCRE zcl_srueth_aoc_constants=>mc_digits_pcre
    IN iv_line
    RESULTS DATA(lt_rgx_res).

  SORT lt_rgx_res BY offset ASCENDING.

  LOOP AT lt_rgx_res ASSIGNING FIELD-SYMBOL(<ls_rgx_match>).
    IF <ls_rgx_match>-length = 0 OR <ls_rgx_match>-offset < 0.
      LOOP AT <ls_rgx_match>-submatches ASSIGNING FIELD-SYMBOL(<ls_rgx_submatch>).
        IF <ls_rgx_submatch>-length <> 0 OR <ls_rgx_submatch>-offset >= 0.
          APPEND iv_line+<ls_rgx_submatch>-offset(<ls_rgx_submatch>-length) TO rt_numbers.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      APPEND iv_line+<ls_rgx_match>-offset(<ls_rgx_match>-length) TO rt_numbers.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD day_02.
  day_02_part_1( ).
  day_02_part_2( ).
ENDMETHOD.


METHOD day_02_part_2.
ENDMETHOD.


METHOD day_01.
  day_01_part_1( ).
  day_01_part_2( ).
ENDMETHOD.
ENDCLASS.
