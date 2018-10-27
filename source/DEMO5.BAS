'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO5.BAS: Demonstration of procedures contained the module: PATTERNS.BAS.
             

'             (c) 1991, by R. F. Holcombe

'  IMPORTANT: All the demos in this module use the laser printer,
'             make sure that it is on-line.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserlib.dec'
'$INCLUDE: 'laserfns.dec'
'$INCLUDE: 'patterns.dec'
'$INCLUDE: 'figures.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'Filling of Circle, Ellipse & Other Shapes
   xhairs! = 1.9                    'Width of cross hairs
   ho! = -.15: vo! = .08            'Offsets to adjust printer registration
   hm! = .5: vm! = 2                'Offsets to move on the page
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)
   filename$ = "LPT1"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Initial$)

               'First, let's do just shading
   y! = 2: x! = 1.5: r! = .5: shade% = 50: pattern% = 0
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillCircle(y!, x!, r!, shade%, pattern%)                  'position
               'Now, let's do just a pattern
   y! = 2: x! = 3: r! = .5: aspect! = 1.3: shade% = 0: pattern% = 5
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1))
   CALL FillEllipse(y!, x!, r!, aspect!, shade%, pattern%)
               'Here, we do both
   y! = 2: x! = 4.5: high! = 1: wide! = 1: shade% = 20: pattern% = 6
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1))
   CALL FillRectangle(y!, x!, high!, wide!, shade%, pattern%)
               'Want it to be solid, just use 100% shade
   y! = 2: x! = 6: high! = 1: wide! = 1: shade% = 100: pattern% = 0
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1))
   CALL FillTriangle(y!, x!, high!, wide!, shade%, pattern%)
               'Let's try the crescent
   y! = 4: x! = 1.5: r! = .5: aspect! = 1.4: crescent% = 50
   shade% = 50: pattern% = 0
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1))
   CALL FillCrescent(y!, x!, r!, aspect!, crescent%, shade%, pattern%)
               'And, the diamond
   y! = 4: x! = 3: high! = 1: wide! = 1: shade% = 0: pattern% = 5
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1))
   CALL FillDiamond(y!, x!, high!, wide!, shade%, pattern%)

   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
RETURN

demo.2:     'Filling of Trapezoids
   ho! = -.15: vo! = .08            'Offsets to adjust printer registration
   hm! = .5: vm! = 2                'Offsets to move on the page
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)
   filename$ = "LPT1"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Initial$)

               'Horizontal traps first
   y! = 2: x! = 2
   high! = 1: wide! = 1: lean! = .4: stretch! = .4: shade% = 10: pattern% = 6
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(.5, 1))
   CALL FillTrap(y!, x!, high!, wide!, lean!, shade%, pattern%)
   CALL Lines(y! - high! - .05, x!, y! - high! - .3, x!, 1)
   CALL Lines(y! - high! - .05, x! + lean!, y! - high! - .3, x! + lean!, 1)
   CALL Lines(y! - high! - .175, x!, y! - high! - .175, x! + lean! + .3, 1)
   bytes& = DoPrint(PosCursor(y! - high! - .15, x! + lean! + .35) + "lean!")

               'Let's lean the other way
   y! = 2: x! = 4
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(.5, 1))
   CALL FillTrap(y!, x!, high!, wide!, -lean!, shade%, pattern%)
               'Vertical traps
   shade% = 100: pattern% = 0
   y! = 4: x! = 2
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(.5, 1))
   CALL FillTrap2(y!, x!, high!, wide!, stretch!, shade%, pattern%)
   CALL Lines(y!, x! + wide! + .05, y!, x! + wide! + .35, 1)
   CALL Lines(y! + stretch!, x! + wide! + .05, y! + stretch!, x! + wide! + .35, 1)
   CALL Lines(y! - .3, x! + wide! + .175, y! + stretch!, x! + wide! + .175, 1)
   CALL Lines(y! - .3, x! + wide! - .15, y! - .3, x! + wide! + .175, 1)
   bytes& = DoPrint(PosCursor(y! - .25, x! + wide! - .95) + "stretch!")
               'Let's stretch the other way
   y! = 4: x! = 4
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(.5, 1))
   CALL FillTrap2(y!, x!, high!, wide!, -stretch!, shade%, pattern%)

   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
RETURN

demo.3:     'Filling of Corner & Fillet
'---Let's start with the corners
   xhairs! = 1.9                    'Width of cross hairs
   ho! = -.15: vo! = .08            'Offsets to adjust printer registration
   hm! = .5: vm! = 1                'Offsets to move on the page
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)
   filename$ = "LPT1"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Initial$)

'---First let's just look at the possibilities
   high! = 1: wide! = 1: shade% = 40: pattern% = 0
   y! = 2: x! = 2       'Lower left corner
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillCorner(y!, x!, high!, wide!, shade%, pattern%)
   y! = 1: x! = 2       'Upper left corner
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillCorner(y!, x!, -high!, wide!, shade%, pattern%)
   y! = 2: x! = 3       'Lower right corner
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillCorner(y!, x!, high!, -wide!, shade%, pattern%)
   y! = 1: x! = 3       'Upper right corner
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillCorner(y!, x!, -high!, -wide!, shade%, pattern%)

'---Now, a semi-practical application
   tint% = 15
   hm! = 4: vm! = 1                       'Let's use the offsets to move this
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)   'out of the way
   high! = .1: wide! = .1: shade% = tint%: pattern% = 0
   y! = 2: x! = 2       'Lower left corner
   CALL FillCorner(y!, x!, high!, wide!, shade%, pattern%)
   y! = 1: x! = 2       'Upper left corner
   CALL FillCorner(y!, x!, -high!, wide!, shade%, pattern%)
   y! = 2: x! = 3       'Lower right corner
   CALL FillCorner(y!, x!, high!, -wide!, shade%, pattern%)
   y! = 1: x! = 3       'Upper right corner
   CALL FillCorner(y!, x!, -high!, -wide!, shade%, pattern%)

   CALL PutLineTint(BYVAL tint%)
   y1! = .9: x1! = 1.9: y2! = 2.1: x2! = 3.1
   bytes& = DoPrint&(PosCursor$(y1!, x1!))
   bytes& = DoPrint&(Rectangle$(y2! - y1!, x2! - x1!, 30))

   tint% = 100
   CALL PutLineTint(BYVAL tint%)
   y1! = .8: x1! = 1.8: y2! = 2.2: x2! = 3.2
   bytes& = DoPrint&(PosCursor$(y1!, x1!))
   bytes& = DoPrint&(Rectangle$(y2! - y1!, x2! - x1!, 30))

'---Now, we'll do the fillets
   xhairs! = 1.9                    'Width of cross hairs
   hm! = .5: vm! = 5                'Offsets to move on the page
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)
   tint% = 15                       'Let's make the crosshairs dotted
   CALL PutLineTint(BYVAL tint%)

'---First let's just look at the possibilities
   high! = 1: wide! = 1: shade% = 40: pattern% = 0
   y! = 2: x! = 2       'Lower left corner
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillFillet(y!, x!, high!, wide!, shade%, pattern%)
   y! = 1: x! = 2       'Upper left corner
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillFillet(y!, x!, -high!, wide!, shade%, pattern%)
   y! = 2: x! = 3       'Lower right corner
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillFillet(y!, x!, high!, -wide!, shade%, pattern%)
   y! = 1: x! = 3       'Upper right corner
   bytes& = DoPrint(PosCursor$(y!, x!) + CrossHairs$(xhairs!, 1)) 'Show x,y
   CALL FillFillet(y!, x!, -high!, -wide!, shade%, pattern%)

'---Now, a semi-practical application
   tint% = 100
   CALL PutLineTint(BYVAL tint%)
   hm! = 4: vm! = 5                       'Let's use the offsets to move this
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)   'out of the way
   high! = .1: wide! = .1: shade% = tint%: pattern% = 0
   y! = 2: x! = 2       'Lower left corner
   CALL FillFillet(y!, x!, high!, wide!, shade%, pattern%)
   y! = 1: x! = 2       'Upper left corner
   CALL FillFillet(y!, x!, -high!, wide!, shade%, pattern%)
   y! = 2: x! = 3       'Lower right corner
   CALL FillFillet(y!, x!, high!, -wide!, shade%, pattern%)
   y! = 1: x! = 3       'Upper right corner
   CALL FillFillet(y!, x!, -high!, -wide!, shade%, pattern%)
                        'Put a box around it
   y1! = .9: x1! = 1.9: y2! = 2.1: x2! = 3.1
   bytes& = DoPrint&(PosCursor$(y1!, x1!))
   bytes& = DoPrint&(Rectangle$(y2! - y1!, x2! - x1!, 30))
                        'And, another box
   tint% = 15
   CALL PutLineTint(BYVAL tint%)
   y1! = .8: x1! = 1.8: y2! = 2.2: x2! = 3.2
   bytes& = DoPrint&(PosCursor$(y1!, x1!))
   bytes& = DoPrint&(Rectangle$(y2! - y1!, x2! - x1!, 30))

   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
RETURN

demo.4:     'Three Dimensional Bar Example
   ho! = -.15: vo! = .08            'Offsets to adjust printer registration
   vm! = 0: hm! = 0
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)
   filename$ = "LPT1"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Initial$)

   wide! = .3: high! = 2.5: stretch! = .2: shade% = 20: pattern% = 0
   dx! = .6: dy! = dx! * stretch! / wide!

   y! = 2: x! = 2          'First bar
   CALL FillTrap2(y!, x!, high!, wide!, stretch!, shade%, pattern%)
   CALL FillDiamond(y!, x! + wide!, stretch! * 2, wide! * 2, shade% * 2.5, pattern%)
   CALL FillTrap2(y! + stretch!, x! + wide!, high!, wide!, -stretch!, shade% * 5, pattern%)
                           'Second bar
   y! = y! + dy! + 1.3: x! = x! + dx!: high! = 1.2
   CALL FillTrap2(y!, x!, high!, wide!, stretch!, shade%, pattern%)
   CALL FillDiamond(y!, x! + wide!, stretch! * 2, wide! * 2, shade% * 2.5, pattern%)
   CALL FillTrap2(y! + stretch!, x! + wide!, high!, wide!, -stretch!, shade% * 5, pattern%)
                           'Third bar
   y! = y! + dy! - .3: x! = x! + dx!: high! = 1.5
   CALL FillTrap2(y!, x!, high!, wide!, stretch!, shade%, pattern%)
   CALL FillDiamond(y!, x! + wide!, stretch! * 2, wide! * 2, shade% * 2.5, pattern%)
   CALL FillTrap2(y! + stretch!, x! + wide!, high!, wide!, -stretch!, shade% * 5, pattern%)
                           'Background
   y! = 4.5: x! = 3.5
   CALL FillDiamond(y!, x!, dx! * 4, wide! * 12, 5, 0)

   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
RETURN

demo.5:     'Pie Chart Example
   ho! = -.15: vo! = .08            'Offsets to adjust printer registration
   hm! = .5: vm! = 2                'Offsets to move on the page
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)
   filename$ = "LPT1"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Initial$)

   y! = 4: x! = 4: r! = 1.5: aspect! = .6
   startangle! = 300: endangle! = 240: shade% = 50: pattern% = 0
   CALL FillPie(y!, x!, r!, startangle!, endangle!, aspect!, shade%, pattern%)
   shade% = 10
   CALL FillPie(y! + .1, x! + .1, r!, startangle!, endangle!, aspect!, shade%, pattern%)
   y! = 4.1
   startangle! = 240: endangle! = 300
   shade% = 100: pattern% = 0
   CALL FillPie(y!, x!, 1.5, startangle!, endangle!, aspect!, shade%, pattern%)
   shade% = 10
   CALL FillPie(y! + .1, x! + .1, r!, startangle!, endangle!, aspect!, shade%, pattern%)

   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
RETURN

demo.6:     'User pattern example
   ho! = 0: vo! = 0            'Offsets to adjust printer registration
   hm! = 0: vm! = 0            'Offsets to move on the page
   CALL PutOffsets((vo! + vm!) * 720, (ho! + hm!) * 720)
   filename$ = "LPT1:BIN"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Initial$)
   patternfile$ = "moose.img"
   reso% = 300
   y1! = 1
   x1! = 1
   y2! = 3
   x2! = 3
   reverse% = -1
   CALL UserRectangle(y1!, x1!, y2!, x2!, patternfile$, reso%, reverse%)

   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
RETURN
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(6)
      choice$(1) = "Filling of Circle, Ellipse, & Other Shapes"
      choice$(2) = "Filling of Trapezoids"
      choice$(3) = "Filling of Corner & Fillet"
      choice$(4) = "Three Dimensional Bar Example"
      choice$(5) = "Pie Chart Example"
      choice$(6) = "User Pattern Fill Example"
DO
   choice% = DemoChoice%(choice$())    'Select the demo to run

   LOCATE 2, 10
   PRINT TIME$; "  "; choice$(choice%); " demo commencing."
   bytecount& = 0                      'Initialize the number of bytes sent
   CALL PutCount(bytecount&)           'Save it in DOS buffer
                        'Execute the selected demo

   ON choice% GOSUB demo.1, demo.2, demo.3, demo.4, demo.5, demo.6

   CALL GetCount(bytecount&)           'Find out how many bytes were sent
   LOCATE CSRLIN, 10                   '  and, display it.
   IF bytecount& > 0 THEN PRINT TIME$; "  Demo over.  Bytes sent to printer/file:"; bytecount&
   LOCATE 20, 15: PRINT "Press any key to continue."
   DO WHILE INKEY$ = "": LOOP
LOOP
RETURN


