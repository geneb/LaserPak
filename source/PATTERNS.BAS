'$INCLUDE: 'laserpak.dec'

'                  Parameter List: PATTERNS.BAS
'-----------------------------------------------------------------------------
''  aspect! - ratio of horizontal to vertical radii
''  bitmap$ - text string containing one line of a bitmapped image
''  crescent% - crescent width, percentage of radius
''  endangle! - ending angle, in degrees (counter clockwise from 3 o'clock)
''  firstdot% - dot position of first dot to be erased, from the left
''  high! - height, in inches
''  lastdot% - dot position of last dot to be erased, from left
''  lean! - amount trapezoid is 'out of vertical' at top, in inches
''  lp! - length of pattern line, in inches
''  pattern$ - string containing a bitmapped user pattern line
''  pattern$() - string array containing a bitmapped user pattern
''  pattern% - Hewlett Packard defined pattern number (1 - 6)
''  patterndots% - width of a user pattern, in dots
''  patternfile$ - file containing a bitmapped user pattern
''  patternlen% - width of a user pattern, in bytes
''  patternlines% - height of a user pattern, in lines
''  r! - radius, in inches
''  reso% - printer graphics resolution, in dots per inch
''  reverse% - reverse flag, set if True
''  shade% - gray scale, in percent
''  startangle! - starting angle, in degrees (counter clockwise from 3 o'clock)
''  stretch! - amount trapezoid is 'out of level', in inches
''  wide! - width, in inches
''  x! - x coordinate of center, in inches
''  x1! - x coordinate of upper left corner, in inches
''  x2! - x coordinate of lower right corner, in inches
''  xp! - x coordinate of left end of pattern line, in inches
''  y! - y coordinate of center, in inches
''  y1! - y coordinate of upper left corner, in inches
''  y2! - y coordinate of lower right corner, in inches
''  yp! - y coordinate of left end of pattern line, in inches

FUNCTION EndGraphics$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  EndGraphics$: Terminates the printer's raster graphics mode.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   EndGraphics$ = CHR$(27) + "*rB"

END FUNCTION

SUB ErasePattern (pattern$, firstdot%, lastdot%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ErasePattern: Erases a portion of a bitmapped pattern line.

'  pattern$      - string containing a bitmapped user pattern line
'  firstdot%     - dot position of first dot to be erased, from the left
'  lastdot%      - dot position of last dot to be erased, from left
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Check parameters
   IF LEN(pattern$) = 0 OR firstdot% > lastdot% THEN EXIT SUB
   IF firstdot% < 1 THEN firstdot% = 1
   IF lastdot% > LEN(pattern$) * 8 THEN lastdot% = LEN(pattern$) * 8

'---Compute dots to be erased
   firstbyte% = (firstdot% + 7) \ 8
   lastbyte% = (lastdot% + 7) \ 8
   keepleft% = firstdot% - (firstbyte% - 1) * 8 - 1
   keepright% = lastbyte% * 8 - lastdot%

'---Erase unwanted dots
   IF lastbyte% > firstbyte% THEN   'First and last dots in different bytes
      IF lastbyte% > firstbyte% + 1 THEN
         MID$(pattern$, firstbyte% + 1) = STRING$(lastbyte% - firstbyte% - 1, 0)
      END IF
      curbyte% = ASC(MID$(pattern$, firstbyte%, 1))
      keep% = 256 - 2 ^ (8 - keepleft%)
      MID$(pattern$, firstbyte%, 1) = CHR$(curbyte% AND keep%)
      curbyte% = ASC(MID$(pattern$, lastbyte%, 1))
      keep% = 2 ^ keepright% - 1
      MID$(pattern$, lastbyte%, 1) = CHR$(curbyte% AND keep%)
   ELSE                             'First and last dots in same byte
      curbyte% = ASC(MID$(pattern$, firstbyte%, 1))
      keep% = (256 - 2 ^ (8 - keepleft%)) OR (2 ^ keepright% - 1)
      MID$(pattern$, firstbyte%, 1) = CHR$(curbyte% AND keep%)
   END IF

END SUB

SUB FillArrow (y!, x!, high!, wide!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillArrow: Fills an arrowhead shaped triangle with a pattern and/or gray
'             scale.

'  y!         - y coordinate of center, in inches
'  x!         - x coordinate of center, in inches
'  high!      - height, in inches
'  wide!      - width, in inches
'  shade%     - gray scale, in percent
'  pattern%   - Hewlett Packard defined pattern number (1 - 6)

'  Externals: FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Arrow geometry
   numlines% = ABS(high!) * 300
   yc! = y! - high! / 2
   IF wide! > 0 THEN xc! = x! - wide! / 2

'---Construct the arrow
   FOR l% = 1 TO numlines%                'For each vertical dot position
      IF l% < numlines% / 2 THEN          'The width of the arrow
         xlen! = 2 * ABS(wide!) * l% / numlines%
      ELSE
         xlen! = ABS(wide!) - 2 * ABS(wide!) * (l% - numlines% / 2) / numlines%
      END IF
      IF wide! < 0 THEN xc! = x! + ABS(wide!) / 2 - xlen!
      CALL FillDriver(yc!, xc!, 1 / 300, xlen!, shade%, pattern%)
      yc! = yc! + 1 / 300
   NEXT l%

END SUB

SUB FillCircle (y!, x!, r!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillCircle: Fills a circle with a pattern and/or gray scale.

'  y!          - y coordinate of center, in inches
'  x!          - x coordinate of center, in inches
'  r!          - radius, in inches
'  shade%      - gray scale, in percent
'  pattern%    - Hewlett Packard defined pattern number (1 - 6)

'  Externals:  FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Plot pattern from top to bottom
   yc! = y! - r!
   DO
      dy! = ABS(y! - yc!)                 'Compute circle geometry
      xr2! = r! ^ 2 - dy! ^ 2
      IF xr2! > 0 THEN xr! = SQR(xr2!) ELSE xr! = 0
      xc! = x! - xr!
      CALL FillDriver(yc!, xc!, 1 / 300, xr! * 2, shade%, pattern%)
      yc! = yc! + 1 / 300                 'Compute next line position
      IF yc! > y! + r! THEN EXIT DO       'Are we done?
   LOOP

END SUB

SUB FillCorner (y!, x!, high!, wide!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillCorner: Fills a triangular corner with a pattern and/or gray scale.

'  y!          - y coordinate of center, in inches
'  x!          - x coordinate of center, in inches
'  high!       - height, in inches
'  wide!       - width, in inches
'  shade%      - gray scale, in percent
'  pattern%    - Hewlett Packard defined pattern number (1 - 6)

'  Externals:  FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Corner geometry
   numlines% = ABS(high!) * 300

'---Set cursor at top vertical end of corner
   yc! = y! - ABS(high!) / 2
   xc! = x! - wide! / 2

'---Construct the corner
   FOR l% = 1 TO numlines%                'For each vertical dot position
      IF high! > 0 THEN
         xlen! = ABS(wide!) * l% / numlines%
      ELSE
         xlen! = ABS(wide!) - ABS(wide!) * l% / numlines%
      END IF
      IF wide! < 0 THEN xp! = xc! - xlen! ELSE xp! = xc!
      CALL FillDriver(yc!, xp!, 1 / 300, xlen!, shade%, pattern%)
      yc! = yc! + 1 / 300
   NEXT l%

END SUB

SUB FillCrescent (y!, x!, r!, aspect!, crescent%, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillCrescent: Fill a crescent shape with a pattern and/or gray scale.

'  y!            - y coordinate of center, in inches
'  x!            - x coordinate of center, in inches
'  r!            - radius, in inches
'  aspect!       - ratio of horizontal to vertical radii
'  crescent%     - crescent width, percentage of radius
'  shade%        - gray scale, in percent
'  pattern%      - Hewlett Packard defined pattern number (1 - 6)

'  Externals:    FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Draw the crescent from top to bottom
   yc! = y! - r! * aspect!
   DO
      dy! = ABS(y! - yc!) / aspect!
      xr2! = r! ^ 2 - dy! ^ 2
      IF xr2! > 0 THEN xr! = SQR(xr2!) ELSE xr! = 0
      xc! = x! - xr!
      CALL FillDriver(yc!, xc!, 1 / 300, xr! * crescent% / 100, shade%, pattern%)
      yc! = yc! + 1 / 300
      IF yc! > y! + r! * aspect! THEN EXIT DO
   LOOP

END SUB

SUB FillDiamond (y!, x!, high!, wide!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillDiamond: Fills a diamond shape with a pattern and/or gray scale.

'  y!           - y coordinate of center, in inches
'  x!           - x coordinate of center, in inches
'  high!        - height, in inches
'  wide!        - width, in inches
'  shade%       - gray scale, in percent
'  pattern%     - Hewlett Packard defined pattern number (1 - 6)

'  Externals:   FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   numlines% = high! * 300
   halfway% = numlines% \ 2
   xinc! = wide! / high! / 150
   xlen! = 1 / 300
   yc! = y! - high! / 2

   FOR l% = 1 TO numlines%                'For each vertical dot position
      xc! = x! - xlen! / 2               'Compute start of strip
      CALL FillDriver(yc!, xc!, 1 / 300, xlen!, shade%, pattern%)
      IF l% > halfway% THEN xlen! = xlen! - xinc! ELSE xlen! = xlen! + xinc!
      yc! = yc! + 1 / 300
   NEXT l%

END SUB

SUB FillEllipse (y!, x!, r!, aspect!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillEllipse: Fills an ellipse with a pattern and/or gray scale.

'  y!           - y coordinate of center, in inches
'  x!           - x coordinate of center, in inches
'  r!           - radius, in inches
'  aspect!      - ratio of horizontal to vertical radii
'  shade%       - gray scale, in percent
'  pattern%     - Hewlett Packard defined pattern number (1 - 6)

'  Externals:   FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Plot the ellipse from top to bottom
   yc! = y! - r! * aspect!
   DO
      dy! = ABS(y! - yc!) / aspect!
      xr2! = r! ^ 2 - dy! ^ 2
      IF xr2! > 0 THEN xr! = SQR(xr2!) ELSE xr! = 0
      xc! = x! - xr!
      CALL FillDriver(yc!, xc!, 1 / 300, xr! * 2, shade%, pattern%)
      yc! = yc! + 1 / 300
      IF yc! > y! + r! * aspect! THEN EXIT DO
   LOOP

END SUB

SUB FillFillet (y!, x!, high!, wide!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillFillet: Fills a convex rounded corner with a pattern and/or gray scale.

'  y!          - y coordinate of center, in inches
'  x!          - x coordinate of center, in inches
'  high!       - height, in inches
'  wide!       - width, in inches
'  shade%      - gray scale, in percent
'  pattern%    - Hewlett Packard defined pattern number (1 - 6)

'  Externals:  FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Corner geometry
   numlines% = ABS(high!) * 300

'---Set cursor at top vertical end of corner
   yc! = y! - ABS(high!) / 2
   xc! = x! - wide! / 2

'---Construct the corner
   aspect! = ABS(high! / wide!)
   r! = ABS(wide!)
   FOR l% = 1 TO numlines%                'For each vertical dot position
      IF high! > 0 THEN
         dy! = (l% - 1) / 300
         IF l% = numlines% THEN dy! = numlines% / 300
      ELSE
         dy! = (numlines% - l% + 1) / 300
      END IF
      dx2! = r! ^ 2 - (dy! / aspect!) ^ 2
      IF dx2! > 0 THEN dx! = SQR(dx2!) ELSE dx! = 0
      xlen! = ABS(wide!) - dx!
      IF xlen! < 1 / 300 THEN xlen! = 1 / 300
      IF wide! < 0 THEN xp! = xc! - xlen! ELSE xp! = xc!
      CALL FillDriver(yc!, xp!, 1 / 300, xlen!, shade%, pattern%)
      yc! = yc! + 1 / 300
   NEXT l%

END SUB

SUB FillPie (y!, x!, r!, startangle!, endangle!, aspect!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillPie:    Fills a pie shape with a pattern and/or gray scale.

'  y!          - y coordinate of center, in inches
'  x!          - x coordinate of center, in inches
'  r!          - radius, in inches
'  startangle! - starting angle, in degrees (counter clockwise from 3 o'clock)
'  endangle!   - ending angle, in degrees (counter clockwise from 3 o'clock)
'  aspect!     - ratio of horizontal to vertical radii
'  shade%      - gray scale, in percent
'  pattern%    - Hewlett Packard defined pattern number (1 - 6)

'  Externals:  FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Adjust angles, if necessary
   IF startangle! < 0 THEN startangle! = 0
   startangle! = startangle! MOD 360
   endangle! = endangle! MOD 360
   IF endangle! = 0 THEN endangle! = 360

'---Set constants
   pf! = 3.141593 / 180
   IF startangle! > endangle! THEN sweep% = -1 ELSE sweep% = 0

   upper% = 0
   lower% = 0
   IF startangle! >= 0 AND startangle! < 180 THEN upper% = -1
   IF endangle! >= 0 AND endangle! < 180 THEN upper% = -1
   IF startangle! > 180 AND startangle! <= 360 THEN lower% = -1
   IF endangle! > 180 AND endangle! <= 360 THEN lower% = -1

   uppers% = 0
   uppere% = 0
   lowers% = 0
   lowere% = 0
   IF startangle! >= 0 AND startangle! < 180 AND sweep% THEN uppers% = -1
   IF endangle! >= 0 AND endangle! < 180 AND sweep% THEN uppere% = -1
   IF startangle! > 180 AND startangle! <= 360 AND sweep% THEN lowers% = -1
   IF endangle! > 180 AND endangle! <= 360 AND sweep% THEN lowere% = -1

'---Dray pie from top to bottom
   yc! = y! - r! * aspect!
   DO
      dy! = ABS(y! - yc!) / aspect!
      xr2! = r! ^ 2 - dy! ^ 2
      IF xr2! >= 0 THEN xr! = SQR(xr2!) ELSE xr! = 0
      GOSUB gppline
      yc! = yc! + 1 / 300
      IF yc! > y! + r! * aspect! THEN EXIT DO
      yc! = yc! + 1 / 300
   LOOP

EXIT SUB

gppline:
'---Find any intercepts with radii
   x1! = x! - xr!
   x2! = x! + xr!

   dys! = r! * SIN(startangle! * pf!) * aspect!
   dxs! = r! * COS(startangle! * pf!)
   dye! = r! * SIN(endangle! * pf!) * aspect!
   dxe! = r! * COS(endangle! * pf!)

   IF dys! = 0 THEN ds! = r! * 2 ELSE ds! = dxs! * ABS(y! - yc!) / dys!
   IF dye! = 0 THEN de! = r! * 2 ELSE de! = dxe! * ABS(y! - yc!) / dye!
   IF endangle! > startangle! THEN GOSUB singleplot ELSE GOSUB doubleplot
RETURN

doubleplot:
   IF yc! <= y! THEN
      xs! = x! + ds!
      xe! = x! + de!
      IF lowere% AND lowers% THEN
         xx! = x1!
         xlen! = xr! * 2
         GOSUB gppprint
      ELSE
         IF uppers% THEN
            IF xs! > x2! THEN xt! = x2! ELSE xt! = xs!
            xx! = x1!
            xlen! = xt! - x1!
            GOSUB gppprint
         END IF
         IF uppere% THEN
            IF xe! > x1! THEN xt! = xe! ELSE xt! = x1!
            xx! = xt!
            xlen! = x2! - xt!
            GOSUB gppprint
         END IF
      END IF
   ELSEIF yc! > y! THEN
      xs! = x! - ds!
      xe! = x! - de!
      IF uppere% AND uppers% THEN
         xx! = x1!
         xlen! = xr! * 2
         GOSUB gppprint
      ELSE
         IF lowere% THEN
            IF x2! < xe! THEN xt! = x2! ELSE xt! = xe!
            xx! = x1!
            xlen! = xt! - x1!
            GOSUB gppprint
         END IF
         IF lowers% THEN
            IF xs! < x1! THEN xt! = x1! ELSE xt! = xs!
            xx! = xt!
            xlen! = x2! - xt!
            GOSUB gppprint
         END IF
      END IF
   END IF
RETURN

singleplot:
   IF upper% AND yc! <= y! THEN
      xs! = x! + ds!
      xe! = x! + de!
      IF endangle! > 0 AND endangle! < 180 AND xe! > x1! THEN xa! = xe! ELSE xa! = x1!
      IF startangle! > 0 AND startangle! < 180 AND xs! < x2! THEN xb! = xs! ELSE xb! = x2!
      xx! = xa!
      xlen! = xb! - xa!
      GOSUB gppprint
   ELSEIF lower% AND yc! > y! THEN
      xs! = x! - ds!
      xe! = x! - de!
      IF endangle! > startangle! THEN
         IF startangle! > 180 AND startangle! < 360 AND xs! > x1! THEN xa! = xs! ELSE xa! = x1!
         IF endangle! > 180 AND endangle! < 360 AND xe! < x2! THEN xb! = xe! ELSE xb! = x2!
      ELSE
         IF startangle! > 180 AND startangle! < 360 AND xs! > x1! THEN xa! = xs! ELSE xa! = x1!
         IF endangle! > 180 AND endangle! < 360 AND xe! < x2! THEN xb! = xe! ELSE xb! = x2!
      END IF
      xx! = xa!
      xlen! = xb! - xa!
      GOSUB gppprint
   END IF
RETURN

gppprint:
   IF INSTR(STR$(xx! * 300) + STR$(xlen! * 300), "-") THEN RETURN 'Value too small, so exit
   CALL FillDriver(yc!, xx!, 1 / 300, xlen!, shade%, pattern%)
RETURN

END SUB

SUB FillRectangle (y!, x!, high!, wide!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillRectangle: Fills a rectangle with a pattern and/or gray scale.

'  y!             - y coordinate of center, in inches
'  x!             - x coordinate of center, in inches
'  high!          - height, in inches
'  wide!          - width, in inches
'  shade%         - gray scale, in percent
'  pattern%       - Hewlett Packard defined pattern number (1 - 6)

'  Externals:     FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Set cursor at upper left
   yc! = y! - high! / 2
   xc! = x! - wide! / 2

'---Print it
   CALL FillDriver(yc!, xc!, high!, wide!, shade%, pattern%)

END SUB

SUB FillTrap (y!, x!, high!, wide!, lean!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillTrap:  Fills a horizontally leaning trapezoid with a pattern and/or
'             gray scale.

'  y!         - y coordinate of center, in inches
'  x!         - x coordinate of center, in inches
'  high!      - height, in inches
'  wide!      - width, in inches
'  lean!      - amount trapezoid is 'out of vertical' at top, in inches
'  shade%     - gray scale, in percent
'  pattern%   - Hewlett Packard defined pattern number (1 - 6)

'  Externals: FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Trapezoid geometry
   numlines% = high! * 300
   xinc! = lean! / high! / 300
   yc! = y! - high!

'---Construct the trapezoid
   xc! = x! + lean!
   FOR l% = 1 TO numlines%                'For each vertical dot position
      CALL FillDriver(yc!, xc!, 1 / 300, wide!, shade%, pattern%)
      yc! = yc! + 1 / 300
      xc! = xc! - xinc!                   'New cursor position
   NEXT l%

END SUB

SUB FillTrap2 (y!, x!, high!, wide!, stretch!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillTrap2: Fills a vertically stretched trapezoid with a pattern and/or
'             gray scale.

'  y!         - y coordinate of center, in inches
'  x!         - x coordinate of center, in inches
'  high!      - height, in inches
'  wide!      - width, in inches
'  stretch!   - amount trapezoid is 'out of level', in inches
'  shade%     - gray scale, in percent
'  pattern%   - Hewlett Packard defined pattern number (1 - 6)

'  Externals: FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Check validity of stretch!
   IF stretch! > high! THEN EXIT SUB

'---Trapezoid geometry
   numlines% = ABS(stretch! * 300)
   IF stretch! < 0 THEN
      xc! = x! + wide!
      yc! = y! + stretch!
   ELSE
      xc! = x!
      yc! = y!
   END IF

'---Construct the top
   FOR l% = 1 TO numlines%
      xlen! = wide! * l% / numlines%      'Compute length of line
      IF stretch! < 0 THEN xc! = x! + wide! - xlen!
      CALL FillDriver(yc!, xc!, 1 / 300, xlen!, shade%, pattern%)
      yc! = yc! + 1 / 300
   NEXT l%

'---Now the middle
   CALL FillDriver(yc!, x!, high! - ABS(stretch!), wide!, shade%, pattern%)
   yc! = yc! + high! - ABS(stretch!)

'---Finally the bottom
   FOR l% = 1 TO numlines%
      xlen! = wide! - wide! * l% / numlines%    'Compute length of line
      IF stretch! > 0 THEN xc! = x! + wide! - xlen!
      CALL FillDriver(yc!, xc!, 1 / 300, xlen!, shade%, pattern%)
      yc! = yc! + 1 / 300
   NEXT l%

END SUB

SUB FillTriangle (y!, x!, high!, wide!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillTriangle: Fills a triangle with a pattern and/or gray scale.

'  y!            - y coordinate of center, in inches
'  x!            - x coordinate of center, in inches
'  high!         - height, in inches
'  wide!         - width, in inches
'  shade%        - gray scale, in percent
'  pattern%      - Hewlett Packard defined pattern number (1 - 6)

'  Externals:    FillDriver
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Triangle geometry
   numlines% = ABS(high!) * 300
   yc! = y! - ABS(high!) / 2

'---Construct the triangle
   FOR l% = 1 TO numlines%                'For each vertical dot position
      IF high! > 0 THEN                   'The width of the triangle
         xlen! = wide! * l% / numlines%
      ELSE
         xlen! = wide! - wide! * l% / numlines%
      END IF
      xc! = x! - xlen! / 2               'Compute horiz. cursor position
      CALL FillDriver(yc!, xc!, 1 / 300, xlen!, shade%, pattern%)
      yc! = yc! + 1 / 300
   NEXT l%

END SUB

SUB GetPattern (patternfile$, pattern$(), patterndots%, patternlines%, patternlen%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  GetPattern:   Retrieves a user defined pattern from a pattern file.

'  patternfile$  - file containing a bitmapped user pattern
'  pattern$()    - string array containing a bitmapped user pattern
'  patterndots%  - width of a user pattern, in dots
'  patternlines% - height of a user pattern, in lines
'  patternlen%   - width of a user pattern, in bytes

'  Externals:    GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   CALL GetBitmapHeader(patternfile$, patterndots%, patternlines%, patternlen%)

   IF patternlen% THEN                    'If a valid pattern file
      REDIM pattern$(patternlines%)       'Dimension array to hold bit pattern
      f% = FREEFILE                       '  of each line in the pattern
      OPEN "r", #f%, patternfile$, patternlen%
      FIELD #f%, patternlen% AS pat$
      FOR i% = 1 TO patternlines%         'For each line in the pattern
         GET #f%, i%                      'Retrieve it
         pattern$(i%) = pat$              'Save it in the array
      NEXT i%
      CLOSE #f%
   ELSE                                   'If not a valid pattern file
      REDIM pattern$(1)                   'Synthesize a black pattern
      pattern$(1) = CHR$(255)             'Make it 1 byte long and 1 line high
      patterndots% = 8
      patternlines% = 1
      patternlen% = 1
   END IF

   pattern$(0) = pattern$(patternlines%)  'Initialize zero array element for
                                          '  when MOD function is used to
END SUB                                   '  select proper pattern line

FUNCTION PatternLine$ (yp!, xp!, lp!, pattern$(), patternlines%, patternlen%, reso%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PatternLine$: Constructs a pattern line from a user defined pattern that
'                will match up horizontally and vertically with other parts
'                of the pattern.

'  yp!           - y coordinate of left end of pattern line, in inches
'  xp!           - x coordinate of left end of pattern line, in inches
'  lp!           - length of pattern line, in inches
'  pattern$()    - string array containing a bitmapped user pattern
'  patternlines% - height of a user pattern, in lines
'  patternlen%   - width of a user pattern, in bytes
'  reso%         - printer graphics resolution, in dots per inch
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Determine geometry
   scanline% = (yp! * reso%) MOD patternlines%
   numdots% = (lp! - xp!) * reso%
   numbytes% = (numdots% + 7) \ 8
   trimdots% = numbytes% * 8 - numdots%

'---Expand the pattern
   pat$ = ""
   DO
      pat$ = pat$ + pattern$(scanline%)
      IF LEN(pat$) >= numbytes% THEN EXIT DO
   LOOP
   pat$ = LEFT$(pat$, numbytes%)

'---Trim the right end of excess dots
   IF trimdots% THEN
      lastbyte% = ASC(MID$(pat$, numbytes%, 1))
      MID$(pat$, numbytes%, 1) = CHR$(lastbyte% AND (256 - 2 ^ trimdots%))
   END IF
   
   PatternLine$ = pat$
  
END FUNCTION

FUNCTION PrintPattern$ (pattern$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PrintPattern$: Prints a user defined pattern line.

'  pattern$       - string containing a bitmapped user pattern line
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

      PrintPattern$ = CHR$(27) + "*b" + LTRIM$(STR$(LEN(pattern$))) + "W" + pattern$

END FUNCTION

SUB ReverseBitmap (bitmap$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ReverseBitmap: Takes one line of a bitmapped image and changes it to
'                 reverse video.

'  bitmap$        - text string containing one line of a bitmapped image
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   FOR i% = 1 TO LEN(bitmap$)
      MID$(bitmap$, i%, 1) = CHR$(ASC(MID$(bitmap$, i%, 1)) XOR 255)
   NEXT i%

END SUB

SUB ReversePattern (pattern$())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ReversePattern: Takes an array containing a user defined pattern, and
'                  changes it to reverse video.

'  pattern$()      - string array containing a bitmapped user pattern
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    
   patlen% = LEN(pattern$(1))
   FOR i% = 0 TO UBOUND(pattern$)
      FOR j% = 1 TO patlen%
         MID$(pattern$(i%), j%, 1) = CHR$(ASC(MID$(pattern$(i%), j%, 1)) XOR 255)
      NEXT j%
   NEXT i%

END SUB

FUNCTION StartGraphics$ (y1!, x1!, reso%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  StartGraphics$: Sets the printer mode for raster graphics.

'  y1!             - y coordinate of upper left corner, in inches
'  x1!             - x coordinate of upper left corner, in inches
'  reso%           - printer graphics resolution, in dots per inch

'  Externals:      PosCursor$
'                  Resolution$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  
   StartGraphics$ = PosCursor$(y1!, x1!) + Resolution$(reso%) + CHR$(27) + "*r1A"

END FUNCTION

SUB UserRectangle (y1!, x1!, y2!, x2!, patternfile$, reso%, reverse%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  UserRectangle: Fills a rectangle with a user defined pattern.

'  y1!            - y coordinate of upper left corner, in inches
'  x1!            - x coordinate of upper left corner, in inches
'  y2!            - y coordinate of lower right corner, in inches
'  x2!            - x coordinate of lower right corner, in inches
'  patternfile$   - file containing a bitmapped user pattern
'  reso%          - printer graphics resolution, in dots per inch
'  reverse%       - reverse flag, set if True

'  Calls:         EndGraphics$
'                 GetPattern
'                 PatternLine$
'                 ReversePattern

'  Externals:     DoPrint&
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Compute geometry
   numlines% = (y2! - y1!) * reso%
   scan! = y1!
   scaninc! = 1 / reso%

'---Get the Pattern
   REDIM pattern$(0)
   CALL GetPattern(patternfile$, pattern$(), patterndots%, patternlines%, patternlen%)
   IF reverse% THEN CALL ReversePattern(pattern$())

'---Set graphics mode
   pstring$ = StartGraphics(y1!, x1!, reso%)
   GOSUB rprint

'---Print the pattern
   FOR l% = 1 TO numlines%
      rect$ = PatternLine$(scan!, x1!, x2!, pattern$(), patternlines%, patternlen%, reso%)
      pstring$ = PrintPattern(rect$)
      GOSUB rprint
      scan! = scan! + scaninc!
   NEXT l%

'---Exit graphics mode
   pstring$ = EndGraphics$
   GOSUB rprint

EXIT SUB

rprint:
   bytes& = DoPrint&(pstring$)
RETURN

END SUB

