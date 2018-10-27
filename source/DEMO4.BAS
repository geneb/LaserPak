'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO4.BAS:     Program to demonstrate the printing of proportionally
'                 spaced text routines contained in the module: FONTS.BAS

'                 (c) 1991, by R. F. Holcombe

'  IMPORTANT:     All the demos in this module use the laser printer,
'                 make sure that it is on-line.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserlib.dec'
'$INCLUDE: 'laserfns.dec'
'$INCLUDE: 'lzrtext.dec'
'$INCLUDE: 'fonts.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'Downloading a proportional font
   fontfile$ = "demofont"                    'Font file to use
   fontidnum% = ExtractID%(fontfile$)        'Retrieve font I.D. number
   LPrinter% = 1: buffer$ = SPACE$(4096)      'Set up parms
   PRINT : PRINT "         Font I.D.:"; fontidnum%
   PRINT : PRINT "         Downloading font."
   CALL FontCopy(fontfile$, LPrinter%, buffer$, fontidnum%)  'Download the font
   PRINT : PRINT "         Font no."; fontidnum%; " is now resident."
RETURN

demo.2:     'Proportional font basics
   fontfile$ = "demofont"                    'Font file to use
   fontidnum% = ExtractID%(fontfile$)        'Retrieve font I.D. number
   filename$ = "LPT1"
   handle% = OpenFile(filename$)
            'Create a PS table from the font file, and print using LeftTextP$
            'Notice that character spacing is now controlled by the width of
            'the space character pstab%(32), in the PS table.
   REDIM pstab%(256)
   CALL CreatePSTable(fontfile$, pstab%())
   t$ = "This is a proportional font, printed using LeftTextP$."
   bytes& = DoPrint&(PosCursor$(2, 1))
   bytes& = DoPrint&(LeftTextP$(t$, pstab%()))
            'Let's print some bold, centered, and underlined text
   t$ = "This is Bold"
   bytes& = DoPrint&(PosCursor$(2.5, 1))
   bytes& = DoPrint&(BoldTextP$(t$, pstab%()))
   t$ = "This is Centered"
   bytes& = DoPrint&(PosCursor$(2.75, 4.25) + VertLine$(.5, 1))
   bytes& = DoPrint&(PosCursor$(3, 4.25))
   bytes& = DoPrint&(CenterTextP$(t$, pstab%()) + t$)

   t$ = "This underlined text"
   bytes& = DoPrint&(PosCursor$(3.5, 1) + UnderlineP$(t$, "underlined"))


   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
RETURN

demo.3:     'Saving and retrieving a proportional font spacing table
   fontfile$ = "demofont"                    'Font file to use
   pstabfile$ = "demofont.pst"               'PS table file
   fontidnum% = ExtractID%(fontfile$)        'Retrieve font I.D. number
   REDIM pstab%(256)
   CALL CreatePSTable(fontfile$, pstab%()) 'Make the PS table
   CALL SavePSTable(pstabfile$, pstab%())  'Save it to the file

'---Now let's create the PS table both ways and see which is faster
'   We'll run 10 repetitions so we can see the difference with more precision

   PRINT
   PRINT "         Creating PS table from font file: "; TIME$
   FOR i% = 1 TO 10
      CALL CreatePSTable(fontfile$, pstab%())          'Make the PS table
   NEXT i%
   PRINT "               Finished creating PS table: "; TIME$
   PRINT
   PRINT "          Loading PS table from font file: "; TIME$
   FOR i% = 1 TO 10
      CALL LoadPSTable(pstabfile$, pstab%())           'Load the PS table
   NEXT i%
   PRINT "                Finished loading PS table: "; TIME$

RETURN
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(3)
      choice$(1) = "Downloading a Proportional Font (DO THIS FIRST!!!)"
      choice$(2) = "Proportional Font Basics"
      choice$(3) = "Saving a Proportional Font Spacing Table"
DO
   choice% = DemoChoice%(choice$())    'Select the demo to run

   LOCATE 2, 10
   PRINT TIME$; "  "; choice$(choice%); " demo commencing."
   bytecount& = 0                      'Initialize the number of bytes sent
   CALL PutCount(bytecount&)           'Save it in DOS buffer
                        'Execute the selected demo

   ON choice% GOSUB demo.1, demo.2, demo.3

   CALL GetCount(bytecount&)           'Find out how many bytes were sent
   LOCATE CSRLIN, 10                   '  and, display it.
   IF bytecount& > 0 THEN PRINT TIME$; "  Demo over.  Bytes sent to printer/file:"; bytecount&
   LOCATE 20, 15: PRINT "Press any key to continue."
   DO WHILE INKEY$ = "": LOOP
LOOP
RETURN


