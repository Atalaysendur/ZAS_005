*&---------------------------------------------------------------------*
*& Include          ZAS_005_I_MAILPROCESSING_TOP
*&---------------------------------------------------------------------*

CLASS lcl_main DEFINITION DEFERRED.
DATA: go_main TYPE REF TO lcl_main.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_vbeln TYPE vbeln_vl.
SELECTION-SCREEN END OF BLOCK blk1.
