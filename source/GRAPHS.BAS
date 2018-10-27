'$INCLUDE: 'laserpak.dec'

'                  Parameter List: GRAPHS.BAS
'-----------------------------------------------------------------------------
''  cpi! - character spacing, in characters per inch
''  gridhigh! - height of grid, in inches
''  gridwide! - width of grid, in inches
''  inc! - distance between major increments, in inches
''  linewt% - weight of connecting line, in dots
''  majorincs% - number of major increments
''  majorwt% - line weight of major division
''  minorincs% - number of minor increments per major increment
''  minorwt% - line weight of minor division
''  pstab%() - array containing proportional spacing table
''  rightjustify% - right justify flag, set if True
''  symbol% - number of symbol to plot
''  symbolwt% - weight of symbol, in dots
''  tick! - length of tick mark, in inches
''  title$() - array containing list of titles
''  wide! - width of symbol, in inches
''  wt% - line weight, in dots
''  x! - x coordinate of plot point, in inches
''  x!() - array containing x coordinates of plot points, in inches
''  y! - y coordinate of plot point, in inches
''  y!() - array containing y coordinates of plot points, in inches

FUNCTION HorzGrid$ (majorincs%, minorincs%, inc!, gridhigh!, majorwt%, minorwt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  HorzGrid$: Draws a horizontal grid from left to right, beginning at the
'             current cursor position.

'  majorincs% - number of major increments
'  minorincs% - number of minor increments per major increment
'  inc!       - distance between major increments, in inches
'  gridhigh!  - height of grid, in inches
'  majorwt%   - line weight of major division
'  minorwt%   - line weight of minor division

'  Externals: MoveCursor$
'             PopCursor$
'             PushCursor$
'             VertLine$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   s$ = PushCursor$ + VertLine$(gridhigh!, majorwt%)

   FOR i% = 1 TO majorincs%
      FOR j% = 1 TO minorincs% - 1
         s$ = s$ + MoveCursor$(0, inc! / minorincs%) + VertLine$(gridhigh!, minorwt%)
      NEXT j%
      s$ = s$ + PopCursor$ + MoveCursor$(0, inc!) + PushCursor$ + VertLine$(gridhigh!, majorwt%)
   NEXT i%

   HorzGrid$ = s$ + PopCursor$

END FUNCTION

FUNCTION HorzScale$ (majorincs%, minorincs%, inc!, tick!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  HorzScale$: Draws a horizontal scale from left to right, beginning at the
'              current cursor position.

'  majorincs%  - number of major increments
'  minorincs%  - number of minor increments per major increment
'  inc!        - distance between major increments, in inches
'  tick!       - length of tick mark, in inches
'  wt%         - line weight, in dots

'  Externals:  HorzLine$
'              MoveCursor$
'              PopCursor$
'              PushCursor$
'              VertLine$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   s$ = HorzLine$(majorincs% * inc!, wt%) + VertLine$(tick!, wt%)

   FOR i% = 1 TO majorincs%
      s$ = s$ + PushCursor$
      FOR j% = 1 TO minorincs% - 1
         s$ = s$ + MoveCursor$(0, inc! / minorincs%) + VertLine$(tick! / 2, wt%)
      NEXT j%
      s$ = s$ + PopCursor$ + MoveCursor$(0, inc!) + VertLine$(tick!, wt%)
   NEXT i%

   HorzScale$ = s$ + VertLine$(tick!, wt%)

END FUNCTION

FUNCTION HorzTitles$ (title$(), majorincs%, inc!, cpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  HorzTitles$: Places a horizontal list of grid titles from left to right,
'               beginning at the current cursor position, using a fixed font.

'  title$()     - array containing list of titles
'  majorincs%   - number of major increments
'  inc!         - distance between major increments, in inches
'  cpi!         - character spacing, in characters per inch

'  Externals:   CenterText$
'               MoveCursor$
'               PopCursor$
'               PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   s$ = PushCursor$ + CenterText$(title$(0), cpi!) + title$(0) + PopCursor$

   FOR i% = 1 TO majorincs%
      s$ = s$ + MoveCursor$(0, inc!) + PushCursor$
      s$ = s$ + CenterText$(title$(i%), cpi!) + title$(i%) + PopCursor$
   NEXT i%

   HorzTitles$ = s$

END FUNCTION

FUNCTION HorzTitlesP$ (title$(), majorincs%, inc!, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  HorzTitlesP$: Places a horizontal list of grid titles from left to right,
'                beginning at the current cursor position, using a
'                proportional font.

'  title$()      - array containing list of titles
'  majorincs%    - number of major increments
'  inc!          - distance between major increments, in inches
'  pstab%()      - array containing proportional spacing table

'  Externals:    CenterTextP$
'                MoveCursor$
'                PopCursor$
'                PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   s$ = PushCursor$ + CenterTextP$(title$(0), pstab%()) + title$(0) + PopCursor$

   FOR i% = 1 TO majorincs%
      s$ = s$ + MoveCursor$(0, inc!) + PushCursor$
      s$ = s$ + CenterTextP$(title$(i%), pstab%()) + title$(i%) + PopCursor$
   NEXT i%

   HorzTitlesP$ = s$

END FUNCTION

SUB PlotLine (y!(), x!(), linewt%, symbol%, wide!, symbolwt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PlotLine:  Plots a series symbols, connected by line segments

'  y!()       - array containing y coordinates of plot points, in inches
'  x!()       - array containing x coordinates of plot points, in inches
'  linewt%    - weight of connecting line, in dots
'  symbol%    - number of symbol to plot
'  wide!      - width of symbol, in inches
'  symbolwt%  - weight of symbol, in dots

'  Calls:     Symbols

'  Externals: DiagLine
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
         'How many points to plot?
   lastpoint% = UBOUND(y!)
         'Make sure each point has 2 coordinates
   IF UBOUND(x!) < lastpoint% THEN lastpoint% = UBOUND(x!)
                                    'Plot the first symbol
      IF symbol% THEN CALL Symbols(y!(1), x!(1), wide!, symbol%, symbolwt%)

   FOR i% = 2 TO lastpoint%         'Plot the next symbol
      IF symbol% THEN CALL Symbols(y!(i%), x!(i%), wide!, symbol%, symbolwt%)
                                    'Connect it with a line
      IF linewt% THEN CALL DiagLine(y!(i% - 1), x!(i% - 1), y!(i%), x!(i%), linewt%)
   NEXT i%                          'Get the next plot point

END SUB

SUB Symbols (y!, x!, wide!, symbol%, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Symbols:   Plots one of 20 symbols

'  y!         - y coordinate of plot point, in inches
'  x!         - x coordinate of plot point, in inches
'  wide!      - width of symbol, in inches
'  symbol%    - number of symbol to plot
'  wt%        - line weight, in dots

'  Externals: Circles
'             CrossHairs$
'             DiagLine
'             DoPrint&
'             FillCircle
'             FillDiamond
'             FillRectangle
'             FillTriangle
'             HorzLine$
'             PosCursor$
'             Rectangle$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   IF symbol% = 1 THEN                       'Cross hairs
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
   ELSEIF symbol% = 2 THEN                   'x
      offset! = wide! / 3
      CALL DiagLine(y! - offset!, x! - offset!, y + offset!, x + offset!, wt%)
      CALL DiagLine(y! + offset!, x! - offset!, y - offset!, x + offset!, wt%)
   ELSEIF symbol% = 3 THEN                   '*
      offset! = wide! / 3
      CALL DiagLine(y! - offset!, x! - offset!, y + offset!, x + offset!, wt%)
      CALL DiagLine(y! + offset!, x! - offset!, y - offset!, x + offset!, wt%)
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
   ELSEIF symbol% = 4 THEN                   'Circle
      CALL Circles(y!, x!, wide! / 2, wt%)
   ELSEIF symbol% = 5 THEN                   'Square
      pstring$ = PosCursor$(y! - wide! / 2, x! - wide! / 2) + Rectangle$(wide!, wide!, wt%)
      GOSUB print.symbol
   ELSEIF symbol% = 6 THEN                   'Triangle
      CALL DiagLine(y! - wide! / 2, x!, y! + wide! / 2, x! - wide! / 2, wt%)
      CALL DiagLine(y! - wide! / 2, x!, y! + wide! / 2, x! + wide! / 2, wt%)
      pstring$ = PosCursor$(y! + wide! / 2, x! - wide! / 2) + HorzLine$(wide!, wt%)
      GOSUB print.symbol
   ELSEIF symbol% = 7 THEN                   'Diamond
      CALL DiagLine(y! - wide! / 2, x!, y!, x! - wide! / 2, wt%)
      CALL DiagLine(y! - wide! / 2, x!, y!, x! + wide! / 2, wt%)
      CALL DiagLine(y!, x! - wide! / 2, y + wide! / 2, x!, wt%)
      CALL DiagLine(y!, x! + wide! / 2, y + wide! / 2, x!, wt%)
   ELSEIF symbol% = 8 THEN                   'Circle w/ "+"
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
      CALL Circles(y!, x!, wide! / 3, wt%)
   ELSEIF symbol% = 9 THEN                   'Square w/ "+"
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
      pstring$ = PosCursor$(y! - wide! / 3, x! - wide! / 3) + Rectangle$(2 * wide! / 3, 2 * wide! / 3, wt%)
      GOSUB print.symbol
   ELSEIF symbol% = 10 THEN                  'Triangle w/ "+"
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
      CALL DiagLine(y! - wide! / 3, x!, y! + wide! / 3, x! - wide! / 3, wt%)
      CALL DiagLine(y! - wide! / 3, x!, y! + wide! / 3, x! + wide! / 3, wt%)
      pstring$ = PosCursor$(y! + wide! / 3, x! - wide! / 3) + HorzLine$(2 * wide! / 3, wt%)
      GOSUB print.symbol
   ELSEIF symbol% = 11 THEN                  'Diamond w/ "+"
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
      CALL DiagLine(y! - wide! / 3, x!, y!, x! - wide! / 3, wt%)
      CALL DiagLine(y! - wide! / 3, x!, y!, x! + wide! / 3, wt%)
      CALL DiagLine(y!, x! - wide! / 3, y + wide! / 3, x!, wt%)
      CALL DiagLine(y!, x! + wide! / 3, y + wide! / 3, x!, wt%)
   ELSEIF symbol% = 12 THEN                  'Filled Circle
      CALL FillCircle(y!, x!, wide! / 2, 100, 0)
   ELSEIF symbol% = 13 THEN                  'Filled Square
      CALL FillRectangle(y!, x!, wide!, wide!, 100, 0)
   ELSEIF symbol% = 14 THEN                  'Filled Triangle
      CALL FillTriangle(y!, x!, wide!, wide!, 100, 0)
   ELSEIF symbol% = 15 THEN                  'Filled Diamond
      CALL FillDiamond(y!, x!, wide!, wide!, 100, 0)
   ELSEIF symbol% = 16 THEN                  'Filled Circle w/ "+"
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
      CALL FillCircle(y!, x!, wide! / 3, 100, 0)
   ELSEIF symbol% = 17 THEN                  'Filled Square w/ "+"
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
      CALL FillRectangle(y!, x!, 2 * wide! / 3, 2 * wide! / 3, 100, 0)
   ELSEIF symbol% = 18 THEN                  'Filled Triangle w/ "+"
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
      CALL FillTriangle(y!, x!, 2 * wide! / 3, 2 * wide! / 3, 100, 0)
   ELSEIF symbol% = 19 THEN                  'Filled Diamond w/ "+"
      pstring$ = PosCursor$(y!, x!) + CrossHairs$(wide!, wt%)
      GOSUB print.symbol
      CALL FillDiamond(y!, x!, 2 * wide! / 3, 2 * wide! / 3, 100, 0)
   ELSEIF symbol% = 20 THEN                  'Bulls eye
      CALL Circles(y!, x!, wide! / 2, wt%)
      CALL FillCircle(y!, x!, wide! / 3, 100, 0)
   END IF

EXIT SUB

print.symbol:
   bytes& = DoPrint&(pstring$)
RETURN

END SUB

FUNCTION VertGrid$ (majorincs%, minorincs%, inc!, gridwide!, majorwt%, minorwt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  VertGrid$: Draws a vertical grid from top to bottom, beginning at the
'             current cursor position.

'  majorincs% - number of major increments
'  minorincs% - number of minor increments per major increment
'  inc!       - distance between major increments, in inches
'  gridwide!  - width of grid, in inches
'  majorwt%   - line weight of major division
'  minorwt%   - line weight of minor division

'  Externals: HorzLine$
'             MoveCursor$
'             PopCursor$
'             PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   s$ = PushCursor$ + HorzLine$(gridwide!, majorwt%)

   FOR i% = 1 TO majorincs%
      FOR j% = 1 TO minorincs% - 1
         s$ = s$ + MoveCursor$(inc! / minorincs%, 0) + HorzLine$(gridwide!, minorwt%)
      NEXT j%
      s$ = s$ + PopCursor$ + MoveCursor$(inc!, 0) + PushCursor$ + HorzLine$(gridwide!, majorwt%)
   NEXT i%

   VertGrid$ = s$ + PopCursor$

END FUNCTION

FUNCTION VertScale$ (majorincs%, minorincs%, inc!, tick!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  VertScale$: Draws a vertical scale from top to bottom, beginning at the
'              current cursor position.

'  majorincs%  - number of major increments
'  minorincs%  - number of minor increments per major increment
'  inc!        - distance between major increments, in inches
'  tick!       - length of tick mark, in inches
'  wt%         - line weight, in dots

'  Externals:  HorzLine$
'              MoveCursor$
'              PopCursor$
'              PushCursor$
'              VertLine$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   s$ = VertLine$(majorincs% * inc!, wt%) + HorzLine$(tick!, wt%)

   FOR i% = 1 TO majorincs%
      s$ = s$ + PushCursor$
      FOR j% = 1 TO minorincs% - 1
         s$ = s$ + MoveCursor$(inc! / minorincs%, 0) + HorzLine$(tick! / 2, wt%)
      NEXT j%
      s$ = s$ + PopCursor$ + MoveCursor$(inc!, 0) + HorzLine$(tick!, wt%)
   NEXT i%

   VertScale$ = s$ + HorzLine$(tick!, wt%)

END FUNCTION

FUNCTION VertTitles$ (title$(), majorincs%, inc!, rightjustify%, cpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  VertTitles$:  Places a vertical list of grid titles from top to bottom,
'                beginning at the current cursor position, using a fixed font.

'  title$()      - array containing list of titles
'  majorincs%    - number of major increments
'  inc!          - distance between major increments, in inches
'  rightjustify% - right justify flag, set if True
'  cpi!          - character spacing, in characters per inch

'  Externals:    FlushRight$
'                MoveCursor$
'                PopCursor$
'                PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   s$ = PushCursor$
   IF rightjustify% THEN s$ = s$ + FlushRight$(title$(0), cpi!)
   s$ = s$ + title$(0) + PopCursor$

   FOR i% = 1 TO majorincs%
      s$ = s$ + MoveCursor$(inc!, 0) + PushCursor$
      IF rightjustify% THEN s$ = s$ + FlushRight$(title$(i%), cpi!)
      s$ = s$ + title$(i%) + PopCursor$
   NEXT i%

   VertTitles$ = s$

END FUNCTION

FUNCTION VertTitlesP$ (title$(), majorincs%, inc!, rightjustify%, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  VertTitlesP$: Places a vertical list of grid titles from top to bottom,
'                beginning at the current cursor position, using a
'                proportional font.

'  title$()      - array containing list of titles
'  majorincs%    - number of major increments
'  inc!          - distance between major increments, in inches
'  rightjustify% - right justify flag, set if True
'  pstab%()      - array containing proportional spacing table

'  Externals:    FlushRightP$
'                MoveCursor$
'                PopCursor$
'                PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   s$ = PushCursor$
   IF rightjustify% THEN s$ = s$ + FlushRightP$(title$(0), pstab%())
   s$ = s$ + title$(0) + PopCursor$

   FOR i% = 1 TO majorincs%
      s$ = s$ + MoveCursor$(inc!, 0) + PushCursor$
      IF rightjustify% THEN s$ = s$ + FlushRightP$(title$(i%), pstab%())
      s$ = s$ + title$(i%) + PopCursor$
   NEXT i%

   VertTitlesP$ = s$

END FUNCTION

