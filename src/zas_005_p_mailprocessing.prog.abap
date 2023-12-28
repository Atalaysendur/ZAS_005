*&---------------------------------------------------------------------*
*& Report ZAS_005_P_MAILPROCESSING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZAS_005_P_MAILPROCESSING.


*INCLUDE ZAS_005_P_MAILPROCESSING_TOP.
INCLUDE ZAS_005_I_MAILPROCESSING_TOP.
*INCLUDE ZAS_005_P_MAILPROCESSING_CLS.
INCLUDE ZAS_005_I_MAILPROCESSING_CLS.


INITIALIZATION.
  go_main = lcl_main=>create_instance( ).

START-OF-SELECTION.
  go_main->start_of_selection( ).

END-OF-SELECTION.
  go_main->end_of_selection( ).
