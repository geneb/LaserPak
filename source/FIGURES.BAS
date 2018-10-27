'$INCLUDE: 'laserpak.dec'

'                  Parameter List: FIGURES.BAS
'-----------------------------------------------------------------------------
''  angle! - angle between the bottom and horiz., in degrees
''  angle1! - angle between the right side bottom and horiz., in degrees
''  angle2! - angle between the left side bottom and horiz., in degrees
''  aspect! - ratio of the horizontal radius to the vertical radius
''  endangle! - terminating angle of an arc, in degrees
''  endx! - x coordinate of end of radial line, in inches
''  endy! - y coordinate of end of radial line, in inches
''  high! - height, in inches
''  r! - radius, in inches
''  startangle! - beginning angle of an arc, in degrees
''  wide! - width, in inches
''  wide1! - width of right side, in inches
''  wide2! - width of left side, in inches
''  wt% - line weight, in dots
''  x! - x coordinate, in inches
''  x1! - x coordinate of start point, in inches
''  x2! - x coordinate of end point, in inches
''  y! - y coordinate, in inches
''  y1! - y coordinate of start point, in inches
''  y2! - y coordinate of end point, in inches

SUB Arcs (y!, x!, r!, startangle!, endangle!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Arcs:       Draws an arc of any number of degrees from 0 to 360.

'  y!          - y coordinate, in inches
'  x!          - x coordinate, in inches
'  r!          - radius, in inches
'  startangle! - beginning angle of an arc, in degrees
'  endangle!   - terminating angle of an arc, in degrees
'  wt%         - line weight, in dots

'  Externals:  FillDriver
'              GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Constants
   pi! = 3.14159
   woff! = (wt% - 1) / 600
   rad2deg! = 180 / pi!
   ang! = 0             'Initial radial plot angle
   dw! = wt% / 300
   k! = pi! * wt% / 1000
   CALL GetLineTint(tint%)

'---Plot points around circumference
   DO
      xr! = r! * COS(ang!)
      yr! = r! * SIN(ang!)
      angle! = ang! * rad2deg!
   
'---Quadrant 1
      quadangle! = angle!
      GOSUB checkangle
      IF plotit% THEN
         xc! = x! + xr! - w!: yc! = y! - yr!
         GOSUB aplotpoint
      END IF

'---Quadrant 2
      quadangle! = 180 - angle!
      GOSUB checkangle
      IF plotit% THEN
         xc! = x! - xr! - w!: yc! = y! - yr!
         GOSUB aplotpoint
      END IF
   
'---Quadrant 3
      quadangle! = 180 + angle!
      GOSUB checkangle
      IF plotit% THEN
         xc! = x! - xr! - w!: yc! = y! + yr!
         GOSUB aplotpoint
      END IF
    
'---Quadrant 4
      quadangle! = 360 - angle!
      GOSUB checkangle
      IF plotit% THEN
         xc! = x! + xr! - w!: yc! = y! + yr!
         GOSUB aplotpoint
      END IF
    
      ang! = ang! + k! / r!
      IF ang! > 1.6 THEN EXIT DO
   LOOP

EXIT SUB

checkangle:
   plotit% = 0
   IF startangle! < endangle! THEN
      IF quadangle! >= startangle! AND quadangle! <= endangle! THEN plotit% = -1
   ELSEIF startangle! > endangle! THEN
      IF NOT (quadangle! <= startangle! AND quadangle! >= endangle!) THEN plotit% = -1
   ELSE
      plotit% = -1
   END IF
RETURN

aplotpoint:
   CALL FillDriver(yc! - woff!, xc! - woff!, dw!, dw!, tint%, 0)
RETURN

END SUB

SUB Bar3D (y!, x!, high!, wide1!, wide2!, angle1!, angle2!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Bar3D:     Creates a line drawing of a three dimensional bar.

'  y!         - y coordinate, in inches
'  x!         - x coordinate, in inches
'  high!      - height, in inches
'  wide1!     - width of right side, in inches
'  wide2!     - width of left side, in inches
'  angle1!    - angle between the right side bottom and horiz., in degrees
'  angle2!    - angle between the left side bottom and horiz., in degrees
'  wt%        - line weight, in dots

'  Calls:     BotBar3D
'             DiagOffset!
'             TopBar3D

'  Externals: FillDriver
'             GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   CALL GetLineTint(tint%)
   woff! = (wt% - 1) / 600

'---Print bottom
   CALL BotBar3D(y!, x!, wide1!, wide2!, angle1!, angle2!, wt%)

'---Print center line
   CALL FillDriver(y! - high! - woff!, x! - woff!, high!, wt% / 300, tint%, 0)

'---Print right side
   dh1! = DiagOffset!(wide1!, angle1!)
   CALL FillDriver(y! - high! - dh1! - woff!, x! + wide1! - woff!, high!, wt% / 300, tint%, 0)

'---Print left side
   dh2! = DiagOffset!(wide2!, angle2!)
   CALL FillDriver(y! - high! - dh2! - woff!, x! - wide2! - woff!, high!, wt% / 300, tint%, 0)

'---Print top
   CALL TopBar3D(y!, x!, high!, wide1!, wide2!, angle1!, angle2!, wt%)
   CALL BotBar3D(y! - high!, x!, wide1!, wide2!, angle1!, angle2!, wt%)

END SUB

SUB BotBar3D (y!, x!, wide1!, wide2!, angle1!, angle2!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  BotBar3D: Draws the bottom of a three dimensional bar.

'  y!        - y coordinate, in inches
'  x!        - x coordinate, in inches
'  wide1!    - width of right side, in inches
'  wide2!    - width of left side, in inches
'  angle1!   - angle between the right side bottom and horiz., in degrees
'  angle2!   - angle between the left side bottom and horiz., in degrees
'  wt%       - line weight, in dots

'  Calls:    DiagLine
'            DiagOffset!
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Print right side
   dh1! = DiagOffset!(wide1!, angle1!)
   CALL DiagLine(y!, x!, y! - dh1!, x! + wide1!, wt%)

'---Print left side
   dh2! = DiagOffset!(wide2!, angle2!)
   CALL DiagLine(y! - dh2!, x! - wide2!, y!, x!, wt%)

END SUB

SUB Circles (y!, x!, r!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Circles:   Creates an open, round circle.

'  y!         - y coordinate, in inches
'  x!         - x coordinate, in inches
'  r!         - radius, in inches
'  wt%        - line weight, in dots

'  Externals: FillDriver
'             GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'---Constants
   ang! = 0             'Initial radial plot angle
   dw! = wt% / 300
   woff! = (wt% - 1) / 600
   k! = 3.14159 * wt% / 1000
   CALL GetLineTint(tint%)

'---Plot points around circumference
   DO
      xr! = r! * COS(ang!)
      yr! = r! * SIN(ang!)
      xc! = x! + xr! - w!: yc! = y! + yr!
      GOSUB plotpoint
      xc! = x! - xr! - w!: yc! = y! + yr!
      GOSUB plotpoint
      xc! = x! + xr! - w!: yc! = y! - yr!
      GOSUB plotpoint
      xc! = x! - xr! - w!: yc! = y! - yr!
      GOSUB plotpoint
      ang! = ang! + k! / r!
      IF ang! > 1.6 THEN EXIT DO
   LOOP

EXIT SUB

plotpoint:
   CALL FillDriver(yc! - woff!, xc! - woff!, dw!, dw!, tint%, 0)
RETURN
 
END SUB

SUB DiagLine (y1!, x1!, y2!, x2!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DiagLine:  Creates a diagonal line.

'  y1!        - y coordinate of start point, in inches
'  x1!        - x coordinate of start point, in inches
'  y2!        - y coordinate of end point, in inches
'  x2!        - x coordinate of end point, in inches
'  wt%        - line weight, in dots

'  Externals: FillDriver
'             GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Compute constants
   IF y1! > y2! THEN ysign% = -1 ELSE ysign% = 1
   IF x1! > x2! THEN xsign% = -1 ELSE xsign% = 1
   dl! = SQR((x2! - x1!) ^ 2 + (y2! - y1!) ^ 2)
   dr! = wt% / 600
   dw! = wt% / 300
   woff! = (wt% - 1) / 600
   ang! = ATN(ABS(y2! - y1!) / ABS(x2! - x1!))

   CALL GetLineTint(tint%)

'---Plot first end point
   CALL FillDriver(y1! - woff!, x1! - woff!, dw!, dw!, tint%, 0)

'---Plot diagonal
   DO
      rad! = rad! + dr!
      IF rad! > dl! THEN EXIT DO
      dx! = x1! + xsign% * rad! * COS(ang!)
      dy! = y1! + ysign% * rad! * SIN(ang!)
      CALL FillDriver(dy! - woff!, dx! - woff!, dw!, dw!, tint%, 0)
   LOOP

'---Plot other end point
   CALL FillDriver(y2! - woff!, x2! - woff!, dw!, dw!, tint%, 0)

END SUB

FUNCTION DiagOffset! (wide!, angle!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DiagOffset!: Computes the elevation of a right triangle with the base and
'               opposite angle known.

'  wide!        - width, in inches
'  angle!       - angle between the bottom and horiz., in degrees
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   DiagOffset! = wide! * SIN(angle! * .017453) / COS(angle! * .017453)

END FUNCTION

SUB Ellipse (y!, x!, r!, aspect!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Ellipse:   Creates an open 360 degree ellipse.

'  y!         - y coordinate, in inches
'  x!         - x coordinate, in inches
'  r!         - radius, in inches
'  aspect!    - ratio of the horizontal radius to the vertical radius
'  wt%        - line weight, in dots

'  Externals: FillDriver
'             GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Constants
   ang! = 0             'Initial radial plot angle
   dw! = wt% / 300
   woff! = (wt% - 1) / 600
   k! = 3.14159 * wt% / 1000
   IF aspect! > 1 THEN k! = k! / aspect!
   CALL GetLineTint(tint%)

'---Plot points around circumference
   DO
      xr! = r! * COS(ang!)
      yr! = r! * SIN(ang!) * aspect!
      xc! = x! + xr! - w!: yc! = y! + yr!
      GOSUB eplotpoint
      xc! = x! - xr! - w!: yc! = y! + yr!
      GOSUB eplotpoint
      xc! = x! + xr! - w!: yc! = y! - yr!
      GOSUB eplotpoint
      xc! = x! - xr! - w!: yc! = y! - yr!
      GOSUB eplotpoint
      ang! = ang! + k! / r!
      IF ang! > 1.6 THEN EXIT DO
   LOOP

EXIT SUB

eplotpoint:
   CALL FillDriver(yc! - woff!, xc! - woff!, dw!, dw!, tint%, 0)
RETURN

END SUB

SUB LeftSideBar (y!, x!, high!, wide!, angle!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  LeftSideBar: Prints the left side of a three dimensional bar.

'  y!           - y coordinate, in inches
'  x!           - x coordinate, in inches
'  high!        - height, in inches
'  wide!        - width, in inches
'  angle!       - angle between the bottom and horiz., in degrees
'  wt%          - line weight, in dots

'  Calls:       DiagLine
'               DiagOffset!
'               Lines

'  Externals:   DoPrint&
'               MoveCursor$
'               PosCursor$
'               VertLine$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   CALL Lines(y! - high!, x!, y!, x!, wt%)

   dh! = DiagOffset!(wide!, angle!)

   CALL Lines(y! - high! - dh!, x! - wide!, y! - dh!, x! - wide!, wt%)

   CALL DiagLine(y! - dh!, x! - wide!, y!, x!, wt%)

   CALL DiagLine(y! - high! - dh!, x! - wide!, y! - high!, x!, wt%)

END SUB

SUB Lines (y1!, x1!, y2!, x2!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Lines:     Prints a straight horizontal, vertical or diagonal line.

'  y1!        - y coordinate of start point, in inches
'  x1!        - x coordinate of start point, in inches
'  y2!        - y coordinate of end point, in inches
'  x2!        - x coordinate of end point, in inches
'  wt%        - line weight, in dots

'  Calls:     FillDriver

'  Externals: DiagLine
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   IF y1! = y2! THEN          'Horizontal line
      woff! = (wt% - 1) / 600
      high! = wt% / 300
      wide! = ABS(x2! - x1!) + high!
      IF x2! < x1! THEN
         CALL FillDriver(y1! - woff!, x2! - woff!, high!, wide!, 0, 0)
      ELSE
         CALL FillDriver(y1! - woff!, x1! - woff!, high!, wide!, 0, 0)
      END IF
   ELSEIF x1! = x2! THEN      'Vertical line
      woff! = (wt% - 1) / 600
      wide! = wt% / 300
      high! = ABS(y2! - y1!) + wide!
      IF y2! < y1! THEN
         CALL FillDriver(y2! - woff!, x1! - woff!, high!, wide!, 0, 0)
      ELSE
         CALL FillDriver(y1! - woff!, x1! - woff!, high!, wide!, 0, 0)
      END IF
   ELSE                       'Diagonal line
      CALL DiagLine(y1!, x1!, y2!, x2!, wt%)
   END IF

END SUB

SUB Pies (y!, x!, r!, startangle!, endangle!, aspect!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Pies:       Creates a pie shaped ellipse.

'  y!          - y coordinate, in inches
'  x!          - x coordinate, in inches
'  r!          - radius, in inches
'  startangle! - beginning angle of an arc, in degrees
'  endangle!   - terminating angle of an arc, in degrees
'  aspect!     - ratio of the horizontal radius to the vertical radius
'  wt%         - line weight, in dots

'  Externals:  FillDriver
'              GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Constants
   pi! = 3.14159
   rad2deg! = 180 / pi!
   ang! = 0             'Initial radial plot angle
   dw! = wt% / 300
   woff! = (wt% - 1) / 600
   k! = pi! * wt% / 1000
   IF aspect! > 1 THEN k! = k! / aspect!
   CALL GetLineTint(tint%)

'---Plot points around circumference
   DO
      xr! = r! * COS(ang!)
      yr! = r! * SIN(ang!) * aspect!
      angle! = ang! * rad2deg!

'---Quadrant 1
      quadangle! = angle!
      GOSUB pcheckangle
      IF plotit% THEN
         xc! = x! + xr! - w!: yc! = y! - yr!
         GOSUB pplotpoint
      END IF

'---Quadrant 2
      quadangle! = 180 - angle!
      GOSUB pcheckangle
      IF plotit% THEN
         xc! = x! - xr! - w!: yc! = y! - yr!
         GOSUB pplotpoint
      END IF
  
'---Quadrant 3
      quadangle! = 180 + angle!
      GOSUB pcheckangle
      IF plotit% THEN
         xc! = x! - xr! - w!: yc! = y! + yr!
         GOSUB pplotpoint
      END IF
   
'---Quadrant 4
      quadangle! = 360 - angle!
      GOSUB pcheckangle
      IF plotit% THEN
         xc! = x! + xr! - w!: yc! = y! + yr!
         GOSUB pplotpoint
      END IF
   
      ang! = ang! + k! / r!
      IF ang! > 1.6 THEN EXIT DO
   LOOP

EXIT SUB

pcheckangle:
   plotit% = 0
   IF startangle! < endangle! THEN
      IF quadangle! >= startangle! AND quadangle! <= endangle! THEN plotit% = -1
   ELSEIF startangle! > endangle! THEN
      IF NOT (quadangle! <= startangle! AND quadangle! >= endangle!) THEN plotit% = -1
   ELSE
      plotit% = -1
   END IF
RETURN

pplotpoint:
   CALL FillDriver(yc! - woff!, xc! - woff!, dw!, dw!, tint%, 0)
RETURN

END SUB

SUB RadialLine (y!, x!, r!, angle!, aspect!, endy!, endx!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  RadialLine: Computes the end points of a radial line of an ellipse.

'  y!          - y coordinate, in inches
'  x!          - x coordinate, in inches
'  r!          - radius, in inches
'  angle!      - angle between the bottom and horiz., in degrees
'  aspect!     - ratio of the horizontal radius to the vertical radius
'  endy!       - y coordinate of end of radial line, in inches
'  endx!       - x coordinate of end of radial line, in inches
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Convert angle to radians
   ang! = angle! * .017453

'---Compute offsets
   xr! = ABS(r! * COS(ang!))
   yr! = ABS(r! * SIN(ang!) * aspect!)

'---Determine quadrant
   IF angle! <= 90 THEN
      endx! = x! + xr!
      endy! = y! - yr!
   ELSEIF angle! > 90 AND angle! <= 180 THEN
      endx! = x! - xr!
      endy! = y! - yr!
   ELSEIF angle! > 180 AND angle! <= 270 THEN
      endx! = x! - xr!
      endy! = y! + yr!
   ELSE
      endx! = x! + xr!
      endy! = y! + yr!
   END IF

END SUB

SUB RightSideBar (y!, x!, high!, wide!, angle!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  RightSideBar: Prints the left side of a three dimensional bar.

'  y!            - y coordinate, in inches
'  x!            - x coordinate, in inches
'  high!         - height, in inches
'  wide!         - width, in inches
'  angle!        - angle between the bottom and horiz., in degrees
'  wt%           - line weight, in dots

'  Calls:        DiagLine
'                DiagOffset!
'                Lines

'  Externals:    DoPrint&
'                MoveCursor$
'                PosCursor$
'                VertLine$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   CALL Lines(y! - high!, x!, y!, x!, wt%)

   dh! = DiagOffset!(wide!, angle!)

   CALL Lines(y! - high! - dh!, x! + wide!, y! - dh!, x! + wide!, wt%)

   CALL DiagLine(y!, x!, y! - dh!, x! + wide!, wt%)

   CALL DiagLine(y! - high!, x!, y! - high! - dh!, x! + wide!, wt%)

END SUB

SUB Slices (y!, x!, r!, startangle!, endangle!, aspect!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Slices:     Creates a pie shaped ellipse, and draws radii.

'  y!          - y coordinate, in inches
'  x!          - x coordinate, in inches
'  r!          - radius, in inches
'  startangle! - beginning angle of an arc, in degrees
'  endangle!   - terminating angle of an arc, in degrees
'  aspect!     - ratio of the horizontal radius to the vertical radius
'  wt%         - line weight, in dots

'  Calls:      DiagLine
'              RadialLine

'  Externals:  FillDriver
'              GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Constants
   pi! = 3.14159
   rad2deg! = 180 / pi!
   ang! = 0             'Initial radial plot angle
   dw! = wt% / 300
   woff! = (wt% - 1) / 600
   k! = pi! * wt% / 1000
   IF aspect! > 1 THEN k! = k! / aspect!
   CALL GetLineTint(tint%)

'---Plot points around circumference
   DO
      xr! = r! * COS(ang!)
      yr! = r! * SIN(ang!) * aspect!
      angle! = ang! * rad2deg!

'---Quadrant 1
      quadangle! = angle!
      GOSUB scheckangle
      IF plotit% THEN
         xc! = x! + xr! - w!: yc! = y! - yr!
         GOSUB splotpoint
      END IF

'---Quadrant 2
      quadangle! = 180 - angle!
      GOSUB scheckangle
      IF plotit% THEN
         xc! = x! - xr! - w!: yc! = y! - yr!
         GOSUB splotpoint
      END IF
 
'---Quadrant 3
      quadangle! = 180 + angle!
      GOSUB scheckangle
      IF plotit% THEN
         xc! = x! - xr! - w!: yc! = y! + yr!
         GOSUB splotpoint
      END IF
  
'---Quadrant 4
      quadangle! = 360 - angle!
      GOSUB scheckangle
      IF plotit% THEN
         xc! = x! + xr! - w!: yc! = y! + yr!
         GOSUB splotpoint
      END IF
  
      ang! = ang! + k! / r!
      IF ang! > 1.6 THEN EXIT DO
   LOOP

   IF startangle! < 0 THEN           'If negative, draw radius
      CALL RadialLine(y!, x!, r!, ABS(startangle!), aspect!, endy!, endx!)
      CALL DiagLine(y!, x!, endy!, endx!, wt%)
   END IF

   IF endangle! < 0 THEN             'If negative, draw radius
      CALL RadialLine(y!, x!, r!, ABS(endangle!), aspect!, endy!, endx!)
      CALL DiagLine(y!, x!, endy!, endx!, wt%)
   END IF
EXIT SUB

scheckangle:
   plotit% = 0
   IF ABS(startangle!) < ABS(endangle!) THEN
      IF quadangle! >= ABS(startangle!) AND quadangle! <= ABS(endangle!) THEN plotit% = -1
   ELSEIF ABS(startangle!) > ABS(endangle!) THEN
      IF NOT (quadangle! <= ABS(startangle!) AND quadangle! >= ABS(endangle!)) THEN plotit% = -1
   ELSE
      plotit% = -1
   END IF
RETURN

splotpoint:
   CALL FillDriver(yc! - woff!, xc! - woff!, dw!, dw!, tint%, 0)
RETURN

END SUB

SUB TopBar3D (y!, x!, high!, wide1!, wide2!, angle1!, angle2!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  TopBar3D: Draws the top of a three dimensional bar.

'  y!        - y coordinate, in inches
'  x!        - x coordinate, in inches
'  high!     - height, in inches
'  wide1!    - width of right side, in inches
'  wide2!    - width of left side, in inches
'  angle1!   - angle between the right side bottom and horiz., in degrees
'  angle2!   - angle between the left side bottom and horiz., in degrees
'  wt%       - line weight, in dots

'  Calls:    DiagLine
'            DiagOffset!
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   dh1! = DiagOffset!(wide1!, angle1!)
   dh2! = DiagOffset!(wide2!, angle2!)
   CALL DiagLine(y! - high! - dh2!, x! - wide2!, y! - high! - dh2! - dh1!, x! - wide2! + wide1!, wt%)
   CALL DiagLine(y! - high! - dh2! - dh1!, x! - wide2! + wide1!, y! - high - dh1!, x! + wide1!, wt%)

END SUB

