CLASS zcl_srueth_aoc_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS word_digit_to_digit
      IMPORTING
        iv_word TYPE string
      RETURNING VALUE(rv_digit) TYPE i
      RAISING zcx_srueth_aoc_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SRUETH_AOC_UTILS IMPLEMENTATION.


METHOD word_digit_to_digit.
  CASE iv_word.
    WHEN 'one'.
      rv_digit = 1.
    WHEN 'two'.
      rv_digit = 2.
    WHEN 'three'.
      rv_digit = 3.
    WHEN 'four'.
      rv_digit = 4.
    WHEN 'five'.
      rv_digit = 5.
    WHEN 'six'.
      rv_digit = 6.
    WHEN 'seven'.
      rv_digit = 7.
    WHEN 'eight'.
      rv_digit = 8.
    WHEN 'nine'.
      rv_digit = 9.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zcx_srueth_aoc_exception
        EXPORTING
          iv_message = |Word "{ iv_word }" is not a digit!|.
  ENDCASE.
ENDMETHOD.
ENDCLASS.
