'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO9.BAS: Yearly sales chart
             

'             (c) 1991, by R. F. Holcombe

'  IMPORTANT: All of the demos in this module use the laser printer,
'             make sure that it is on-line when needed.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

DECLARE SUB Demo1 ()
DECLARE SUB Demo2 ()
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

demo.2:     'Company growth example
   CALL Demo2
RETURN
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(2)
      choice$(1) = "Download Demonstration Font"
      choice$(2) = "Company Growth Example"

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

   fontfile$ = "demofont"
   CALL FontCopy(fontfile$, 1, SPACE$(4096), 0)

END SUB

SUB Demo2

   fontfile$ = "demofont"
   filename$ = "lpdemo_4"
   handle% = OpenFile%(filename$)
   REDIM pstab%(256)
   CALL CreatePSTable(fontfile$, pstab%())

   bytes& = DoPrint&(PosCursor$(2, 1.5) + Rectangle$(3, 5, 10))
   bytes& = DoPrint&(Resolution$(300))
   CALL PrintPCX("revtitle.pcx", 0)
   bytes& = DoPrint&(PosCursor$(5.3, 3.1))
   CALL PrintImage("laser2.img", 0)
'---First year
   bytes& = DoPrint&(PosCursor$(3.85, 2.25))
   CALL PrintPCX("sales1.pcx", 0)
   bytes& = DoPrint&(PosCursor$(2.9, 1.5) + HPPattern$(.95, 2, 5))
   bytes& = DoPrint&(PosCursor$(3.85, 1.5) + HPPattern$(.25, .75, 5))
   bytes& = DoPrint&(PosCursor$(4.1, 1.5) + HPPattern$(.9, .5, 5))
   bytes& = DoPrint&(PosCursor$(3.85, 3) + HPPattern$(1.15, .5, 5))
   bytes& = DoPrint&(PosCursor$(4.1, 2.75) + GrayScale$(.9, .25, 100))
'---Second year
   bytes& = DoPrint&(PosCursor$(3.35, 3.75))
   CALL PrintPCX("sales2.pcx", 0)
   bytes& = DoPrint&(PosCursor$(2, 3.5) + HPPattern$(1.6, .25, 5))
   bytes& = DoPrint&(PosCursor$(2, 3.75) + HPPattern$(1.35, .75, 5))
   bytes& = DoPrint&(PosCursor$(2, 4.5) + HPPattern$(3, .5, 5))
   bytes& = DoPrint&(PosCursor$(3.6, 4.25) + GrayScale$(1.4, .25, 100))
'---Third year
   bytes& = DoPrint&(PosCursor$(2.5, 5.25))
   CALL PrintPCX("sales3.pcx", 0)
   bytes& = DoPrint&(PosCursor$(2, 5) + HPPattern$(.75, .25, 5))
   bytes& = DoPrint&(PosCursor$(2, 5.25) + HPPattern$(.5, .75, 5))
   bytes& = DoPrint&(PosCursor$(2, 6) + HPPattern$(3, .5, 5))
   bytes& = DoPrint&(PosCursor$(2.75, 5.75) + GrayScale$(2.25, .25, 100))
'---Label years
   year$ = "1991"
   bytes& = DoPrint&(PosCursor$(4.93, 2.375) + CenterTextP$(year$, pstab%()) + year$)
   year$ = "1992"
   bytes& = DoPrint&(PosCursor$(4.93, 3.875) + CenterTextP$(year$, pstab%()) + year$)
   year$ = "1993"
   bytes& = DoPrint&(PosCursor$(4.93, 5.375) + CenterTextP$(year$, pstab%()) + year$)

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)

END SUB

