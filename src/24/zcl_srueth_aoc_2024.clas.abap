CLASS zcl_srueth_aoc_2024 DEFINITION
  PUBLIC
  INHERITING FROM zcl_srueth_aoc_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="de">Tag #01</p>
    "!
    METHODS day_01.

    METHODS day_01_part_1.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_srueth_aoc_2024 IMPLEMENTATION.
  METHOD day_01.
    day_01_part_1( ).
  ENDMETHOD.

  METHOD day_01_part_1.
    DATA: lv_tmp_str_1 TYPE string,
          lv_tmp_str_2 TYPE string,
          lv_tmp_i_1   TYPE i,
          lv_tmp_i_2   TYPE i,
          lv_tabix     TYPE i,
          lt_list_1    TYPE TABLE OF i,
          lt_list_2    TYPE TABLE OF i,
          lv_res       TYPE i.

    LOOP AT mt_input ASSIGNING FIELD-SYMBOL(<lv_line>).
      SPLIT <lv_line> AT space INTO lv_tmp_str_1 lv_tmp_str_2.

      CONDENSE: lv_tmp_str_1, lv_tmp_str_2.

      lv_tmp_i_1 = lv_tmp_str_1.
      APPEND lv_tmp_i_1 TO lt_list_1.

      lv_tmp_i_1 = lv_tmp_str_2.
      APPEND lv_tmp_i_1 TO lt_list_2.
    ENDLOOP.

    SORT lt_list_1.
    SORT lt_list_2.

    DO lines( lt_list_1 ) TIMES.
      lv_tabix = sy-index.

      READ TABLE lt_list_1 INTO lv_tmp_i_1 INDEX lv_tabix.
      IF sy-subrc <> 0.
        WRITE: |DAY01: Can't read index { lv_tabix } of Table 1|.
        RETURN.
      ENDIF.

      READ TABLE lt_list_2 INTO lv_tmp_i_2 INDEX lv_tabix.
      IF sy-subrc <> 0.
        WRITE: |DAY01: Can't read index { lv_tabix } of Table 2|.
        RETURN.
      ENDIF.

      SUBTRACT lv_tmp_i_2 FROM lv_tmp_i_1.
      lv_tmp_i_1 = abs( lv_tmp_i_1 ).

      ADD lv_tmp_i_1 TO lv_res.
    ENDDO.

    WRITE: |Part 01: { lv_res }|.
  ENDMETHOD.
ENDCLASS.
