CLASS zcx_srueth_aoc_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_message TYPE string.

    METHODS get_message
      RETURNING VALUE(rv_message) TYPE string.
  PROTECTED SECTION.
    DATA mv_message TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_srueth_aoc_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(  ).

    mv_message = iv_message.
  ENDMETHOD.

  METHOD get_message.
    rv_message = mv_message.
  ENDMETHOD.
ENDCLASS.
