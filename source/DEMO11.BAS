'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO11.BAS: The LaserCard & W-2 Form
             

'             (c) 1991, by R. F. Holcombe

'  IMPORTANT: The demos in this module use the laser printer,
'             make sure that it is on-line when needed.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

DECLARE SUB Demo1 ()
DECLARE SUB Demo2 ()
DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserpak.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'The LaserCard
   CALL Demo1
RETURN

demo.2:     'The W-2 Form
   CALL Demo2
RETURN
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(2)
      choice$(1) = "The LaserCard"
      choice$(2) = "The W-2 Form"

DO
   choice% = DemoChoice%(choice$())    'Select the demo to run

   LOCATE 2, 10
   PRINT TIME$; "  "; choice$(choice%); " demo commencing."
   bytecount& = 0                      'Initialize the number of bytes sent
   CALL PutCount(bytecount&)           'Save it in DOS buffer
                        'Execute the selected demo

   ON choice% GOSUB demo.1, demo.2

   CALL GetCount(bytecount&)           'Find out how many bytes were sent
   LOCATE CSRLIN, 10                   '  and, display it.
   IF bytecount& > 0 THEN PRINT TIME$; "  Demo over.  Bytes sent to printer/file:"; bytecount&
   LOCATE 20, 15: PRINT "Press any key to continue."
   DO WHILE INKEY$ = "": LOOP
LOOP
RETURN

SUB Demo1

   filename$ = "lpdemo_5"
   cardfile$ = "lzrcard"
   screenfile$ = "cc1a.img"
   laserfile$ = "laser2.img"
   maskfile$ = "crdmask1.img"

'---Create the title below the card
   s$ = "copy crdname1.img cname1.img"
   SHELL s$
   s$ = "copy crdname1.img name1.img"
   SHELL s$                                     'Do "Laser" first
   CALL ScreenImage("cname1.img", screenfile$)  'Screen with the user pattern
   CALL ScreenImage("name1.img", "screen25")    'Now, create gray background
   CALL MergeImage("cname1.img", "name1.img", 0, 0)   'Merge the two
   s$ = "copy crdname2.img cname2.img"
   SHELL s$
   s$ = "copy crdname2.img name2.img"
   SHELL s$
   CALL ScreenImage("cname2.img", screenfile$)  'Now, do "Card"
   CALL ScreenImage("name2.img", "screen25")
   CALL MergeImage("cname2.img", "name2.img", 0, 0)

'---Create the card
   handle% = OpenFile("lpdemo_5")
   bytes& = DoPrint(PosCursor$(2, 2) + Resolution$(300))
   CALL PrintImage(cardfile$, 0)
   bytes& = DoPrint(PosCursor(3.8, 2.2) + HorzLine(2.3, 5))
   bytes& = DoPrint(FixedSpacing + StdFont)
   bytes& = DoPrint(PosCursor(3, 2.4) + BoldText("Exp.", 10))
   bytes& = DoPrint(PosCursor(3.15, 2.4) + BoldText("1/80", 10))
   
   bytes& = DoPrint(PosCursor(4.15, 2.3))
   CALL PrintImage("cname1.img", 0)
   bytes& = DoPrint(PosCursor(4.15, 3.65))
   CALL PrintImage("cname2.img", 0)

   bytes& = DoPrint(PosCursor(5, 2.5) + PushCursor + "* it's worthless")
   bytes& = DoPrint(PopCursor + LineFeed(6) + PushCursor + "* it's useless")
   bytes& = DoPrint(PopCursor + LineFeed(6) + PushCursor + "* it's valueless")
   bytes& = DoPrint(PopCursor + LineFeed(6) + PushCursor + "* it's expired")
   bytes& = DoPrint(PopCursor + LineFeed(6) + PushCursor + "* it's a waste of natural resources")
   bytes& = DoPrint(PopCursor + LineFeed(6) + PushCursor + "* it's a blatant example of consumer exploitation")
   bytes& = DoPrint(PopCursor + LineFeed(6) + PushCursor + "* it's non-bio-degradeable")
   bytes& = DoPrint(PopCursor + LineFeed(6) + PushCursor + "* it's ugly, and")
   bytes& = DoPrint(PopCursor + LineFeed(6) + PushCursor + "* worst of all, it's corny, but...")
   bytes& = DoPrint(PopCursor + LineFeed(6) + LineFeed(6) + MoveCursor(0, -1) + "...don't even consider ordering software without it!")
   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)

EXIT SUB

'     These subroutines were used in the initial creation of the LaserCard
'     They are not used in this demo, because it is time consuming to create
'     the card from scratch, and requires several additional bitmap files.
'     The code is retained here so you can see how it was done.  Some detours
'     to PC Paintbrush+ were made to do some pixel editing.  This is one of
'     the more powerful features of LaserPak.  Do not attempt to run these
'     subroutines - several files they require are not provided.

'trim.image:
'   CALL FineTrimImage("cc1.img", screenfile$)         'Trim the image
'   CALL TrimImage("cardnam1.img", "crdname1.img")
'   CALL TrimImage("cardnam2.img", "crdname2.img")
'RETURN

'create.bitmap:
'   CALL InitialFile(cardfile$)
'   f% = FREEFILE
'   OPEN "b", #f%, cardfile$
'   numlines% = 600                        '2 inches high
'   numdots% = 1000                        '3 1/3 inches wide
'   numbytes% = (numdots% + 7) \ 8           'Bitmap width, in bytes
'   init$ = STRING$(numbytes%, 255)        'Initialize to all black
'   ptr& = 1
'   FOR i% = 1 TO numlines%
'      PUT #f%, ptr&, init$
'      ptr& = ptr& + numbytes%
'   NEXT i%
'   CLOSE f%
'   CALL AddBitmapHeader(cardfile$, numdots%, numlines%)
'RETURN

'first.screen:
'   CALL ScreenImage(cardfile$, screenfile$)
'RETURN

'second.screen:
'   CALL ScreenImage(cardfile$, "screen12")
'RETURN

'first.knock:
'   yoffset% = .5 * 300 + 3
'   xoffset% = .25 * 300 + 3
'   CALL KnockOut(cardfile$, laserfile$, yoffset%, xoffset%)
'RETURN

'make.mask:
'   maskfile$ = "crdmask1.img"
'   CALL InitialFile(maskfile$)
'   f% = FREEFILE
'   OPEN "b", #f%, maskfile$
'   numlines% = 600                        '2 inches high
'   numdots% = 1000                        '3 1/3 inches wide
'   numbytes% = (numdots% + 7) \ 8           'Bitmap width, in bytes
'   init1$ = STRING$(numbytes%, 255)        'Initialize to all black
'   init2$ = CHR$(128) + STRING$(numbytes% - 2, 0) + CHR$(1)
'   init3$ = CHR$(128) + STRING$(3, 0) + STRING$(numbytes% - 8, 255) + STRING$(3, 0) + CHR$(1)
'   ptr& = 1
'   FOR i% = 1 TO numlines%
'      IF i% = 1 OR i% = numlines% THEN
'         init$ = init1$
'      ELSEIF i% <= 32 OR i% > numlines% - 32 THEN
'         init$ = init2$
'      ELSE
'         init$ = init3$
'      END IF
'      PUT #f%, ptr&, init$
'      ptr& = ptr& + numbytes%
'   NEXT i%
'   CLOSE f%
'   CALL AddBitmapHeader(maskfile$, numdots%, numlines%)
'RETURN

END SUB

SUB Demo2

   filename$ = "lpdemo_6"
   handle% = OpenFile(filename$)
   y! = 1: x! = 1: high! = 4.16: wide! = 6.4

   bytes& = DoPrint(PosCursor(y!, x!) + Rectangle(high!, wide!, 3))
   bytes& = DoPrint(PosCursor(y! - .2, x! - .2) + Rectangle(high! + .8, wide! + .4, 3))
   bytes& = DoPrint(PosCursor(y! - .2, x! - .2) + GrayScale(high! + .8, .2, 5))
   bytes& = DoPrint(PosCursor(y! - .2, x! + wide!) + GrayScale(high! + .8, .2, 5))
   bytes& = DoPrint(PosCursor(y! + high!, x! - .2) + GrayScale(.6, wide! + .4, 5))
   bytes& = DoPrint(PosCursor(y! - .2, x! - .2) + GrayScale(.2, wide! + .2, 5))
   bytes& = DoPrint(PosCursor(y! + .33, x!) + HorzLine(wide!, 1))
   bytes& = DoPrint(PosCursor(y! + 1.33, x!) + HorzLine(wide!, 1))
   bytes& = DoPrint(PosCursor(y! + 2, x!) + HorzLine(wide!, 1))
   bytes& = DoPrint(PosCursor(y! + 3.16, x!) + HorzLine(wide!, 1))
   bytes& = DoPrint(PosCursor(y! + 3.5, x!) + HorzLine(wide!, 1))
   bytes& = DoPrint(PosCursor(y! + .33, x! + 3.33) + VertLine(high! - .33, 1))
   bytes& = DoPrint(PosCursor(y! + .66, x! + 4.9) + VertLine(high! - 1.32, 1))
   bytes& = DoPrint(PosCursor(y! + 1.33, x! + 1.6) + VertLine(.67, 1))

   bytes& = DoPrint(PosCursor(y! + .66, x! + 3.33) + HorzLine(wide! - 3.33, 1))
   bytes& = DoPrint(PosCursor(y! + 1, x! + 3.33) + HorzLine(wide! - 3.33, 1))
   bytes& = DoPrint(PosCursor(y! + 1.66, x! + 3.33) + HorzLine(wide! - 3.33, 1))
   bytes& = DoPrint(PosCursor(y! + 2.33, x! + 3.33) + HorzLine(wide! - 3.33, 1))
   bytes& = DoPrint(PosCursor(y! + 1.66, x!) + HorzLine(1.6, 1))
   bytes& = DoPrint(PosCursor(y! + 1 + 1 / 60, x! + 3.33 + 1 / 60) + Rectangle(.33 - 1 / 30, wide! - 3.33 - 1 / 30, 10))
   bytes& = DoPrint(MoveCursor(0, 1.55) + VertLine(.3, 10))

   bytes& = DoPrint(PosCursor(y!, x! + 1.3) + VertLine(.33, 1))
   bytes& = DoPrint(PosCursor(y!, x! + 2.2) + VertLine(.33, 1))
   bytes& = DoPrint(PosCursor(y! + 3.16, x! + 1.6) + VertLine(.34, 1))
   bytes& = DoPrint(PosCursor(y! + 3.5, x! + 1.1) + VertLine(.66, 1))
   bytes& = DoPrint(PosCursor(y! + 3.5, x! + 2.2) + VertLine(.66, 1))
   bytes& = DoPrint(PosCursor(y! + 3.5, x! + 4.35) + VertLine(.66, 1))
   bytes& = DoPrint(PosCursor(y! + 3.5, x! + 5.45) + VertLine(.66, 1))

   CALL PutLineTint(20)
   bytes& = DoPrint(PosCursor(y! + 1.66, x! + 1.6) + HorzLine(1.73, 1))
   bytes& = DoPrint(PosCursor(y! + 3.83, x!) + HorzLine(wide!, 1))
   CALL PutLineTint(100)
   bytes& = DoPrint(PosCursor(y! + 3.33, x!) + HPPattern(.17, wide!, 3))
   bytes& = DoPrint(PosCursor(y! + 3.16, x! + .2) + HPPattern(.2, 1.4, 3))
   bytes& = DoPrint(PosCursor(y! + 3.16, x! + 1.8) + HPPattern(.2, 1.53, 3))
   bytes& = DoPrint(PosCursor(y! + 3.16, x! + 3.53) + HPPattern(.2, 1.37, 3))
   bytes& = DoPrint(PosCursor(y! + 3.16, x! + 5.1) + HPPattern(.2, 1.3, 3))

'---Box 6 checkoff boxes
   FOR i% = 1 TO 8
      xx! = x! + 3.45 + (i% - 1) * (.4 - .1 / 7)
      bytes& = DoPrint(PosCursor(y! + .54, xx!) + Rectangle(.1, .1, 1))
   NEXT i%

'---Box text
   bytes& = DoPrint(FixedSpacing + CompFont)
   bytes& = DoPrint(PosCursor(y! + .1, x! + .07) + BoldText("1", 17) + "  Control number")
   bytes& = DoPrint(PosCursor(y! + .33 + .1, x! + .07) + BoldText("2", 17) + "  Employer's name, address, and ZIP code")
   bytes& = DoPrint(PosCursor(y! + 1.33 + .1, x! + .07) + BoldText("3", 17) + "  Employer's ID number")
   bytes& = DoPrint(PosCursor(y! + 1.66 + .1, x! + .07) + BoldText("5", 17) + "  Employee's SS number")
   bytes& = DoPrint(PosCursor(y! + 2 + .1, x! + .07 - 1 / 17) + BoldText("19", 17) + "   Employee's name, address and ZIP code")
   bytes& = DoPrint(PosCursor(y! + 3.16 + .1, x! + .07 - 1 / 17) + BoldText("20", 17))
   bytes& = DoPrint(PosCursor(y! + 3.16 + .1, x! + 1.6 + .07 - 1 / 17) + BoldText("21", 17))
   bytes& = DoPrint(PosCursor(y! + 1.33 + .1, x! + 1.6 + .07 - 1 / 17) + BoldText("4", 17) + "   Employer's state ID number")
   bytes& = DoPrint(PosCursor(y! + 3.16 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText("22", 17))
   bytes& = DoPrint(PosCursor(y! + .33 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText(" 6", 17) + "  Stat. Dec.  Pens. Legal   942   Sub-  Defer. Void")
   bytes& = DoPrint(PosCursor(y! + .43 + .08, x! + 3.33 + .07 - 1 / 17) + "  Emp.        plan  rep.    emp.  total comp.")
   bytes& = DoPrint(PosCursor(y! + .66 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText(" 7", 17) + "   Allocated tips")
   bytes& = DoPrint(PosCursor(y! + 1 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText(" 9", 17) + "   Federal income tax...")
   bytes& = DoPrint(PosCursor(y! + 1.33 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText("11", 17) + "   Social security tax...")
   bytes& = DoPrint(PosCursor(y! + 1.66 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText("13", 17) + "   Social security tips")
   bytes& = DoPrint(PosCursor(y! + 2 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText("15", 17) + "   Dependent care benefits")
   bytes& = DoPrint(PosCursor(y! + 2.33 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText("17", 17) + "")
   bytes& = DoPrint(PosCursor(y! + .66 + .1, x! + 4.9 + .07 - 1 / 17) + BoldText(" 8", 17) + "   Advance EIC payment")
   bytes& = DoPrint(PosCursor(y! + 1 + .1, x! + 4.9 + .07 - 1 / 17) + BoldText("10", 17) + "   Wages, tips, other...")
   bytes& = DoPrint(PosCursor(y! + 1.33 + .1, x! + 4.9 + .07 - 1 / 17) + BoldText("12", 17) + "   Social security wages")
   bytes& = DoPrint(PosCursor(y! + 1.66 + .1, x! + 4.9 + .07 - 1 / 17) + BoldText("14", 17) + "   Nonqualified plans")
   bytes& = DoPrint(PosCursor(y! + 2 + .1, x! + 4.9 + .07 - 1 / 17) + BoldText("16", 17) + "   Fringe benefits incl...")
   bytes& = DoPrint(PosCursor(y! + 2.33 + .1, x! + 4.9 + .07 - 1 / 17) + BoldText("18", 17) + "   Other")
   bytes& = DoPrint(PosCursor(y! + 3.5 + .1, x! + 3.33 + .07 - 1 / 17) + BoldText("27", 17) + "   Local imcome..")
   bytes& = DoPrint(PosCursor(y! + 3.16 + .1, x! + 4.9 + .07 - 1 / 17) + BoldText("23", 17) + "")
   bytes& = DoPrint(PosCursor(y! + 3.5 + .1, x! + .07 - 1 / 17) + BoldText("24", 17) + "   State income...")
   bytes& = DoPrint(PosCursor(y! + 3.5 + .1, x! + 1.1 + .07 - 1 / 17) + BoldText("25", 17) + "   State wages...")
   bytes& = DoPrint(PosCursor(y! + 3.5 + .1, x! + 2.2 + .07 - 1 / 17) + BoldText("26", 17) + "    Name of state")
   bytes& = DoPrint(PosCursor(y! + 3.5 + .1, x! + 4.35 + .07 - 1 / 17) + BoldText("28", 17) + "   Local wages...")
   bytes& = DoPrint(PosCursor(y! + 3.5 + .1, x! + 5.45 + .07 - 1 / 17) + BoldText("29", 17) + "   Locality")

'---Heading
   t$ = "Dept. of the Treasury-Internal Revenue Service"
   bytes& = DoPrint(PosCursor(y! + 4.3, x! + 6.4) + FlushRight(t$, 17) + t$)
   bytes& = DoPrint(PosCursor(y! + 4.55, x!) + "Form")
   bytes& = DoPrint(StdFont + BoldText(" W-2 Wage and Tax Statement 1990", 10))

'---Fill in the blanks
   bytes& = DoPrint(PosCursor(y! + .3, x! + .2) + PushCursor + "00007" + PopCursor)
   bytes& = DoPrint(LineFeed(6))
   bytes& = DoPrint(LineFeed(6))
   bytes& = DoPrint(PushCursor + "National Software, Inc." + PopCursor + LineFeed(6))
   bytes& = DoPrint(PushCursor + "123 Industrial Blvd." + PopCursor + LineFeed(6))
   bytes& = DoPrint(PushCursor + "Brentwood, TN  37027" + PopCursor + LineFeed(6))
   bytes& = DoPrint(PosCursor(y! + 1.3, x! + 3.5) + "595.99")
   bytes& = DoPrint(PosCursor(y! + 1.3, x! + 5.1) + "3549.42")
   bytes& = DoPrint(PosCursor(y! + 1.63, x! + 3.5) + "293.96")
   bytes& = DoPrint(PosCursor(y! + 1.63, x! + 5.1) + "3549.42")
   bytes& = DoPrint(PosCursor(y! + 1.63, x! + .2) + PushCursor + "56-4444450      252033311-0" + PopCursor + LineFeed(6))
   bytes& = DoPrint(LineFeed(6))
   bytes& = DoPrint(PushCursor + "555-77-9999" + PopCursor + LineFeed(6))
   bytes& = DoPrint(LineFeed(6))
   bytes& = DoPrint(PushCursor + "Hacker, Ima P." + PopCursor + LineFeed(6))
   bytes& = DoPrint(PushCursor + "2222 Maple Avenue" + PopCursor + LineFeed(6))
   bytes& = DoPrint(PushCursor + "Blue Bell, PA  19422" + PopCursor + LineFeed(6))
   bytes& = DoPrint(PosCursor(y! + 3.8, x! + .2) + " 225.39    3549.42       PA")

'---Close it out
   bytes& = DoPrint(EjectPage)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)

END SUB

