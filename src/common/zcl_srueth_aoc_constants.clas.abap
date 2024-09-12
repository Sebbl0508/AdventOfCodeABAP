CLASS zcl_srueth_aoc_constants DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS mc_digits TYPE string VALUE '0123456789'.
    CONSTANTS mc_digits_pcre TYPE string VALUE '(?=(one|two|three|four|five|six|seven|eight|nine)|(1|2|3|4|5|6|7|8|9))'.

    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_srueth_aoc_constants IMPLEMENTATION.
  METHOD class_constructor.
  ENDMETHOD.
ENDCLASS.
