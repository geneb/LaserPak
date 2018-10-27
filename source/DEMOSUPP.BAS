DECLARE FUNCTION DemoChoice% (choice$())

'                  Parameter List: DEMOSUPP.BAS

''  choice$() - array containing descriptions of choices available

FUNCTION DemoChoice% (choice$())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DemoChoice%: Function that returns the choice made from a menu; used
'               only for the demo programs.

'  choice$()    - array containing descriptions of choices available
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Set up the menu window
   numchoices% = UBOUND(choice$)
   COLOR 7, 1: CLS
   maxlen% = 32
   FOR i% = 1 TO numchoices%           'Find longest string
      IF LEN(choice$(i%)) + 1 > maxlen% THEN maxlen% = LEN(choice$(i%)) + 1
   NEXT i%
   wide% = maxlen% + 5
   x% = (80 - wide% - 2) \ 2 + 1       'Compute starting coordinates of
   y% = (19 - numchoices%) \ 2         'upper left corner of the window

'---Draw the window
   COLOR 0, 7
   LOCATE y%, x%
   PRINT "ี" + STRING$(wide%, "อ") + "ธ"
   LOCATE CSRLIN, x%
   PRINT "ณ" + STRING$(wide%, " ") + "ณ";
   t$ = "Select The Demo You Wish To Run"
   LOCATE CSRLIN, 40 - LEN(t$) \ 2
   PRINT t$
   LOCATE CSRLIN, x%
   PRINT "ร" + STRING$(wide%, "ฤ") + "ด"
   lite% = 0
   FOR choice% = 1 TO numchoices%
      LOCATE CSRLIN, x%
      PRINT "ณ" + STRING$(wide%, " ") + "ณ";
      GOSUB display.choice
   NEXT choice%
   LOCATE CSRLIN, x%
   PRINT "ร" + STRING$(wide%, "ฤ") + "ด"
   LOCATE CSRLIN, x%
   PRINT "ณ" + STRING$(wide%, " ") + "ณ";
   t$ = "Press the appropriate number or"
   LOCATE CSRLIN, 40 - LEN(t$) \ 2
   PRINT t$
   LOCATE CSRLIN, x%
   PRINT "ณ" + STRING$(wide%, " ") + "ณ";
   t$ = "use the cursor keys and [Enter]"
   LOCATE CSRLIN, 40 - LEN(t$) \ 2
   PRINT t$
   LOCATE CSRLIN, x%
   PRINT "ณ" + STRING$(wide%, " ") + "ณ";
   t$ = "select.  Any other key to exit."
   LOCATE CSRLIN, 40 - LEN(t$) \ 2
   PRINT t$
   LOCATE CSRLIN, x%
   PRINT "ิ" + STRING$(wide%, "อ") + "พ";

'---Activate selection process
   lite% = 1
   choice% = 1
   GOSUB display.choice
   DO
      a$ = INKEY$
      IF LEN(a$) = 1 THEN              'Normal key code
         a% = ASC(a$)
         IF a% <> 13 THEN choice% = a% - 48  'If enter key, retain current
                           'choice, otherwise convert ASCII code to number
         EXIT DO
      ELSEIF LEN(a$) = 2 THEN          'Extended key code
         a% = ASC(RIGHT$(a$, 1))
         IF a% = 72 THEN               'Cursor up
            lite% = 0: GOSUB display.choice
            IF choice% = 1 THEN choice% = numchoices% ELSE choice% = choice% - 1
            lite% = 1: GOSUB display.choice
         ELSEIF a% = 80 THEN           'Cursor down
            lite% = 0: GOSUB display.choice
            IF choice% = numchoices% THEN choice% = 1 ELSE choice% = choice% + 1
            lite% = 1: GOSUB display.choice
         END IF
      END IF
   LOOP

   COLOR 7, 1: CLS
   IF choice% < 1 OR choice% > numchoices% THEN END   'If not a valid choice,
                                                      'then we are finished

   DemoChoice% = choice%
EXIT FUNCTION

display.choice:                                 
   IF lite% THEN COLOR 7, 1 ELSE COLOR 0, 7
   LOCATE y% + 2 + choice%, x% + 1
   PRINT USING " ##. "; choice%;
   PRINT choice$(choice%); SPACE$(wide% - 5 - LEN(choice$(choice%)))
RETURN

END FUNCTION

