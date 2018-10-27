'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO2.BAS:     Program to demonstrate some of the more advanced graphical
'                 procedures, dealing with figures drawn with lines and con-
'                 tained in the module: FIGURES.BAS.

'                 (c) 1991, by R. F. Holcombe

'  IMPORTANT:     All the demos in this module use the laser printer,
'                 make sure that it is on-line.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserlib.dec'
'$INCLUDE: 'laserfns.dec'
'$INCLUDE: 'figures.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'Full Circle and Ellipses
   filename$ = "LPT1"                        'Decide where it send it
   handle% = OpenFile%(filename$)            'Do initial set up
   bytes& = DoPrint&(Initial$)               'Clear margins

   y! = 2: x! = 1.7: r! = 1: wt% = 3         'Circle parameters
   CALL Circles(y!, x!, r!, wt%)             'Make the call
   x! = x! + 2.5: aspect! = 1.5              'Move over and specify aspect
   CALL Ellipse(y!, x!, r!, aspect!, wt%)    'Call the ellipse
   x! = x! + 2.5: aspect! = .75              'Now let's stretch in the
   CALL Ellipse(y!, x!, r!, aspect!, wt%)    'Horizontal direction

   bytes& = DoPrint&(EjectPage$)             'Hand us the paper
   CLOSE handle%                             'Say goodbye
RETURN

demo.2:     'Arcs and Pies
   filename$ = "LPT1"                        'Decide where it send it
   handle% = OpenFile%(filename$)            'Do initial set up
   bytes& = DoPrint&(Initial$)               'Clear margins

   y! = 2: x! = 1.7: r! = 1: wt% = 3
   startangle! = 30: endangle! = 285
   CALL Arcs(y!, x!, r!, startangle!, endangle!, wt%)
   x! = x! + 2.5: aspect! = 1.5
   CALL Pies(y!, x!, r!, startangle!, endangle!, aspect!, wt%)
   x! = x! + 2.5: aspect! = .75
   CALL Pies(y!, x!, r!, endangle!, startangle!, aspect!, wt%)

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
RETURN

demo.3:     'Slices, two ways; and, radial lines
   filename$ = "demofile"                    'Decide where it send it
   handle% = OpenFile%(filename$)            'Do initial set up
   bytes& = DoPrint&(Initial$)               'Clear margins

   y! = 2: x! = 4: r! = 1: wt% = 3
   startangle! = 300: endangle! = 225
   aspect! = .7

'---One way to do it
   CALL Pies(y!, x!, r!, startangle!, endangle!, aspect!, wt%)
   CALL RadialLine(y!, x!, r!, startangle!, aspect!, endy!, endx!)
   CALL DiagLine(y!, x!, endy!, endx!, wt%)
   CALL RadialLine(y!, x!, r!, endangle!, aspect!, endy!, endx!)
   CALL DiagLine(y!, x!, endy!, endx!, wt%)

'---Another way to do it
   y! = y! + 5
   CALL Slices(y!, x!, r!, -startangle!, -endangle!, aspect!, wt%)

'---Explode the last slice
   midangle! = endangle! + (startangle! - endangle!) / 2
   CALL RadialLine(y!, x!, .1, midangle!, aspect!, endy!, endx!)
   CALL Slices(endy!, endx!, r!, -endangle!, -startangle!, aspect!, wt%)

'---Maybe we should tag it
   CALL RadialLine(endy!, endx!, r! / 2, midangle!, aspect!, endy1!, endx1!)
   CALL RadialLine(endy!, endx!, 1.25 * r!, midangle!, aspect!, endy2!, endx2!)
   CALL DiagLine(endy1!, endx1!, endy2!, endx2!, wt%)
   bytes& = DoPrint&(PosCursor$(endy2!, endx2!) + HorzLine$(.25, wt%))
   bytes& = DoPrint&(PosCursor$(endy2! + .05, endx2! + .27) + "Profit")

'---That should be enough to get the point across
   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%

   CALL FontCopy(filename$, 1, SPACE$(4096), 0)

RETURN

demo.4:     '3-D Bars
   CALL PutLineTint(50)                      'Let's tint the top line
   filename$ = "LPT1"                        'Decide where it send it
   handle% = OpenFile%(filename$)            'Do initial set up

'---Construct a 3-Bar from it's elements
   y! = 3: x! = 2: high! = 1.5: wide1! = .2: wide2! = .3
   angle1! = 40: angle2! = 20: wt% = 3

   CALL RightSideBar(y!, x!, high!, wide1!, angle1!, wt%)
   CALL LeftSideBar(y!, x! - .1, high!, wide2!, angle2!, wt%)
   CALL TopBar3D(y! - .05, x! - .05, high!, wide1!, wide2!, angle1!, angle2!, wt%)

'---Pull it together
   y! = 3: x! = 4.25: high! = 1.5: wide1! = .2: wide2! = .3
   angle1! = 40: angle2! = 20: wt% = 3

   CALL RightSideBar(y!, x!, high!, wide1!, angle1!, wt%)
   CALL LeftSideBar(y!, x!, high!, wide2!, angle2!, wt%)
   CALL TopBar3D(y!, x!, high!, wide1!, wide2!, angle1!, angle2!, wt%)

'---Now, we'll do it the easy way
   y! = 3: x! = 6.5: high! = 1.5: wide1! = .2: wide2! = .3
   angle1! = 40: angle2! = 20: wt% = 3
   CALL Bar3D(y!, x!, high!, wide1!, wide2!, angle1!, angle2!, wt%)

'---Here is why we have procedures for the individual pieces
   CALL PutLineTint(100)                      'Make the lines black
   y! = 7: x! = 4.25: high! = .75: wide1! = .2: wide2! = .3
   angle1! = 40: angle2! = 20: wt% = 3
   CALL Bar3D(y!, x!, high!, wide1!, wide2!, angle1!, angle2!, wt%)
   high! = 1.5
   r! = wide1! / (COS(angle1! * .017453))
   CALL RadialLine(y!, x!, r!, angle1!, 1, endy!, endx!)
   dy! = DiagOffset!(wide1!, angle1!)
   CALL RightSideBar(y! - dy!, x! + wide1!, high!, wide1!, angle1!, wt%)
   CALL LeftSideBar(y! - .75 - dy!, x! + wide1!, high! - .75, wide2!, angle2!, wt%)
   CALL BotBar3D(y! - dy! - 1, x! + wide1!, wide1!, wide2!, angle1!, angle2!, wt%)
   CALL TopBar3D(y! - dy!, x! + wide1!, high!, wide1!, wide2!, angle1!, angle2!, wt%)
   high! = .5
   CALL RightSideBar(y! - dy! * 2, x! + wide1! * 2, high!, wide1!, angle1!, wt%)
   dy2! = DiagOffset(wide1!, angle2!)
   y1! = y! - dy! * 3 - high!: x1! = x! + wide1! * 3
   y2! = y1! - dy2!: x2! = x1! - wide1!
   CALL DiagLine(y1!, x1!, y2!, x2!, wt%)


   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%

RETURN
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(4)
      choice$(1) = "Full Circles and Ellipses"
      choice$(2) = "Arcs and Pies"
      choice$(3) = "Two Solutions for Pie Charts"
      choice$(4) = "Three Dimensional Bars - Two Ways"
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


