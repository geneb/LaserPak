'様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
'     Program to extract commented lines from a BASIC program saved in ASCII
'     text format.  UNDOC will remove all lines where the comment symbol "'"
'     is in the first column; but, will preserve $INCLUDE: metacommands.

'     (c) 1991, by R. F. Holcombe
'様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
   COLOR 7, 1: CLS

'---Set up files
   LOCATE 10, 10
   LINE INPUT "Name of file to un-document: "; docfile$
   LOCATE 12, 10
   LINE INPUT "     Name of file to create: "; undocfile$
   OPEN "i", 1, docfile$
   OPEN "o", 2, undocfile$

'---Process the source file
   DO
      IF EOF(1) THEN EXIT DO
      LINE INPUT #1, inline$
      IF LEFT$(inline$, 10) = "'$INCLUDE:" OR LEFT$(inline$, 1) <> "'" THEN PRINT #2, inline$
   LOOP
   CLOSE

