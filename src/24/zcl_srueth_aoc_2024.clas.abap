CLASS zcl_srueth_aoc_2024 DEFINITION
  PUBLIC
  INHERITING FROM zcl_srueth_aoc_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="de">Tag #01</p>
    "!
    METHODS day_01.

    METHODS day_01_part_1
      IMPORTING it_list_1 TYPE ztt_srueth_int4
                it_list_2 TYPE ztt_srueth_int4.
    METHODS day_01_part_2
      IMPORTING it_list_1 TYPE ztt_srueth_int4
                it_list_2 TYPE ztt_srueth_int4.

  PROTECTED SECTION.
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
ENDCLASS.
