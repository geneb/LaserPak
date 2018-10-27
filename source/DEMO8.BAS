'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO8.BAS: Demonstration of procedures contained the module: GRAPHS.BAS
             

'             (c) 1991, by R. F. Holcombe

'  IMPORTANT: All of the demos in this module use the laser printer,
'             make sure that it is on-line when needed.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
DECLARE SUB Demo1 ()
DECLARE SUB Demo2 ()
DECLARE SUB Demo3 ()
DECLARE SUB Demo4 ()
DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserpak.dec'
'$INCLUDE: 'fonts.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'Download the demo font
   CALL Demo1
RETURN

demo.2:     'Mathematical Plotting
   CALL Demo2
RETURN

demo.3:     'Scalable symbol library
   CALL Demo3
RETURN

demo.4:     'Scales example
   CALL Demo4
RETURN

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(4)
      choice$(1) = "Download Demonstration Font"
      choice$(2) = "Mathematical Plotting"
      choice$(3) = "Scalable Symbol Library"
      choice$(4) = "Example of Scales"

DO
   choice% = DemoChoice%(choice$())    'Select the demo to run

   LOCATE 2, 10
   PRINT TIME$; "  "; choice$(choice%); " demo commencing."
   bytecount& = 0                      'Initialize the number of bytes sent
   CALL PutCount(bytecount&)           'Save it in DOS buffer
                        'Execute the selected demo

   ON choice% GOSUB demo.1, demo.2, demo.3, demo.4

   CALL GetCount(bytecount&)           'Find out how many bytes were sent
   LOCATE CSRLIN, 10                   '  and, display it.
   IF bytecount& > 0 THEN PRINT TIME$; "  Demo over.  Bytes sent to printer/file:"; bytecount&
   LOCATE 20, 15: PRINT "Press any key to continue."
   DO WHILE INKEY$ = "": LOOP
LOOP
RETURN

SUB Demo1

   fontfile$ = "demofont"
   CALL FontCopy(fontfile$, 1, SPACE$(4096), 0)

END SUB

SUB Demo2

   fontfile$ = "demofont"
   filename$ = "lpdemo_3"
   handle% = OpenFile%(filename$)
   REDIM pstab%(256)
   CALL CreatePSTable(fontfile$, pstab%())

   majorincs% = 5: minorincs% = 10: inc! = 1: tick! = .075: wt% = 5
   start$ = " .1000": incvalue$ = ".001": cpi! = 15
   gridwide! = 5: gridhigh! = 5: majorwt% = 3: minorwt% = 1

'---Create grid
   bytes& = DoPrint&(PosCursor$(2, 2) + Proportional$)
   tint% = 50: CALL PutLineTint(tint%)
   bytes& = DoPrint&(HorzGrid$(majorincs%, minorincs%, inc!, gridhigh!, majorwt%, minorwt%))
   bytes& = DoPrint&(PosCursor$(2, 2))
   bytes& = DoPrint&(VertGrid$(majorincs%, minorincs%, inc!, gridwide!, majorwt%, minorwt%))
   tint% = 100: CALL PutLineTint(tint%)
   bytes& = DoPrint&(PosCursor$(2, 2))
   bytes& = DoPrint&(HorzGrid$(majorincs%, 2, inc!, gridhigh!, majorwt%, minorwt%))
   bytes& = DoPrint&(PosCursor$(2, 2))
   bytes& = DoPrint&(VertGrid$(majorincs%, 2, inc!, gridwide!, majorwt%, minorwt%))
   bytes& = DoPrint&(PosCursor$(2, 2) + GrayScale$(5, 5, 5))
   bytes& = DoPrint&(PosCursor$(2, 2) + Rectangle$(5, 5, 5))

'---Title major divisions
   REDIM title$(5)
      title$(0) = "40"
      title$(1) = "50"
      title$(2) = "60"
      title$(3) = "70"
      title$(4) = "80"
      title$(5) = "90"
   bytes& = DoPrint&(PosCursor$(7.25, 2))
   bytes& = DoPrint&(HorzTitlesP$(title$(), majorincs%, inc!, pstab%()))
   start$ = " .80": incvalue$ = "-.1"
   REDIM title$(5)
      title$(0) = "0.50"
      title$(1) = "0.40"
      title$(2) = "0.30"
      title$(3) = "0.20"
      title$(4) = "0.10"
      title$(5) = "0.00"
   bytes& = DoPrint&(PosCursor$(2.07, 1.85))
   rightjustify% = -1
   bytes& = DoPrint&(VertTitlesP$(title$(), majorincs%, inc!, rightjustify%, pstab%()))

'---Titles
   title$ = "Microprocessor Efficiency"
   bytes& = DoPrint&(PosCursor$(7.6, 4.5) + CenterTextP$(title$, pstab%()) + title$)
   title$ = "Coprocessor"
   bytes& = DoPrint&(PosCursor$(4.5, 1.35) + CenterTextP$(title$, pstab%()) + title$)
   title$ = "Usage"
   bytes& = DoPrint&(PosCursor$(4.68, 1.35) + CenterTextP$(title$, pstab%()) + title$)
   title$ = "PROCESSOR EFFICIENCY CURVE"
   bytes& = DoPrint&(PosCursor$(8, 4.5) + CenterTextP$(title$, pstab%()))
   REDIM text$(1)
   text$(1) = title$
   text$(1) = InsertBoldCode$(text$(1), text$(1))
   lpi! = 6: txtwidth! = 4: indent! = 0: justify% = 0
   CALL FormatTextP(text$(), lpi!, txtwidth!, indent!, justify%, pstab%())

'---Plot data
   REDIM y!(4), x!(4)
      y!(1) = 6: x!(1) = 2.5
      y!(2) = 4: x!(2) = 3.5
      y!(3) = 3: x!(3) = 5
      y!(4) = 2.4: x!(4) = 6.5
   linewt% = 3: symbol% = 12: wide! = .08: symbolwt% = 1
   CALL PlotLine(y!(), x!(), linewt%, symbol%, wide!, symbolwt%)

'---Border,
   bytes& = DoPrint&(PosCursor$(.5, .5) + Rectangle$(10, 7.4, 10))
   bytes& = DoPrint&(PosCursor$(.54, .54) + Rectangle$(9.92, 7.32, 3))
   title$ = "- Figure 1 -"
   bytes& = DoPrint&(PosCursor$(10.3, 4.25) + CenterTextP$(title$, pstab%()) + title$)


   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)

END SUB

SUB Demo3

'---Set up
   filename$ = "demofile"
   handle% = OpenFile%(filename$)

'---Print symbols
   wide! = .07
   wt% = 2
   y! = 1
   x! = 2
   FOR symbol% = 1 TO 20
      CALL Symbols(y!, x!, wide!, symbol%, wt%)
      bytes& = DoPrint&(PosCursor$(y!, 3) + LTRIM$(STR$(symbol%)))
      CALL Symbols(y!, x! + 2, wide! * 4, symbol%, wt% * 2)
      y! = y! + .4
   NEXT symbol%

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)

END SUB

SUB Demo4

'---Set up
   filename$ = "lpdemo_2"
   handle% = OpenFile%(filename$)

'---Draw holder
   bytes& = DoPrint&(PosCursor$(2, 3) + VertLine$(5, 5))
   bytes& = DoPrint&(PosCursor$(2, 5) + VertLine$(5, 5))
   y! = 2: x! = 4: r! = 1: startangle! = 0: endangle! = 180: aspect! = .75: wt% = 5
   CALL Pies(y!, x!, r!, startangle!, endangle!, aspect!, wt%)
   y! = 7: startangle! = 180: endangle! = 360: aspect! = 1.5
   CALL Pies(y!, x!, r!, startangle!, endangle!, aspect!, wt%)

'---Draw thermometer
   CALL Symbols(7, 4, .5, 20, 2)
   CALL Symbols(2, 4, .2, 12, 2)
   bytes& = DoPrint&(PosCursor$(2, 3.9) + GrayScale$(5, .2, 100))
   majorincs% = 26: minorincs% = 2: tick! = -.3: inc! = 4.33 / 26: wt% = 2
   bytes& = DoPrint&(PosCursor$(2.21, 3.9))
   bytes& = DoPrint&(VertScale$(majorincs%, minorincs%, inc!, tick!, wt%))
   majorincs% = 10: minorincs% = 5: tick! = .3: inc! = 3 / 10: wt% = 2
   bytes& = DoPrint&(PosCursor$(3, 4.1))
   bytes& = DoPrint&(VertScale$(majorincs%, minorincs%, inc!, tick!, wt%))

'---Add the titles
   bytes& = DoPrint&(PosCursor$(2.23, 3.5) + FixedSpacing$ + CompFont$)
   majorincs% = 13: inc! = 4.33 / 13: wt% = 2: cpi! = 15: rightjustify% = -1
   REDIM title$(13)
   FOR i% = 0 TO 13: title$(i%) = LTRIM$(STR$(260 - i% * 20)): NEXT i%
   bytes& = DoPrint&(VertTitles$(title$(), majorincs%, inc!, rightjustify%, cpi!))
   majorincs% = 10: inc! = 3 / 10: rightjustify% = 0
   REDIM title$(10)
   FOR i% = 0 TO 10: title$(i%) = LTRIM$(STR$(100 - i% * 10)): NEXT i%
   bytes& = DoPrint&(PosCursor$(3.03, 4.5))
   bytes& = DoPrint&(VertTitles$(title$(), majorincs%, inc!, rightjustify%, cpi!))
   title$ = "Fahrenheit    Centigrade"
   bytes& = DoPrint&(PosCursor$(7.5, 4) + CenterText$(title$, cpi!) + title$)


'---Finish up
   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)

END SUB

