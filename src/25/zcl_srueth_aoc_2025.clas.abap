CLASS zcl_srueth_aoc_2025 DEFINITION
  PUBLIC
  INHERITING FROM zcl_srueth_aoc_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS day_01.

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
ENDCLASS.
