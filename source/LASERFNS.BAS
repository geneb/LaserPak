'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'              Procedure Declarations for Module: LASERFNS.BAS

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'$INCLUDE: 'laserpak.dec'

'                  Parameter List: LASERFNS.BAS
'-----------------------------------------------------------------------------
''  dx! - vertical distance to move, in inches
''  dx% - horizontal distance to move, in dots
''  dy! - horizontal distance to move, in inches
''  dy% - vertical distance to move, in dots
''  filename$ - name of file to receive output
''  high! - height, in inches
''  horizoffset! - horizontal print offset, in inches
''  horzlen! - horizontal length, in inches
''  lpi! - vertical line spacing, in lines per inch
''  pattern% - Hewlett Packard defined pattern number, (1 - 6)
''  pstring$ - text/graphics string to print
''  reso% - graphics resolution of printer, in dots per inch
''  shade% - gray scale, in percent
''  td! - numeric value to be converted to dots
''  tp! - numeric value to be converted to decipoints
''  vertlen! - vertical length, in inches
''  vertoffset! - vertical print offset, in inches
''  wide! - width, in inches
''  wt% - line weight, in dots (1 dot = 1/300 inch)
''  x! - vertical position, in inches
''  y! - horizontal position, in inches

FUNCTION CrossHairs$ (wide!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  CrossHairs$: Draws a cross-hairs at the current cursor position.

'  wide!        - width, in inches
'  wt%          - line weight, in dots (1 dot = 1/300 inch)

'  Calls:       HorzLine$
'               MoveCursor$
'               PopCursor$
'               PushCursor$
'               VertLine$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   cs$ = PushCursor$ + MoveCursor$(0, -wide! / 2) + HorzLine$(wide!, wt%)

   CrossHairs$ = cs$ + PopCursor$ + MoveCursor$(-wide! / 2, 0) + VertLine$(wide!, wt%)

END FUNCTION

FUNCTION DoPrint& (pstring$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DoPrint&:  Sends a string to the printer and returns the number of bytes
'             sent to date.

'  pstring$   - text/graphics string to print

'  Externals: GetCount
'             GetHandle
'             PutCount
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   CALL GetHandle(handle%)                  'Find out what file number we are
                                            '   using
   PRINT #handle%, pstring$;                '   and, add the string

   CALL GetCount(bytecount&)                'Get the current byte count

   bytecount& = bytecount& + LEN(pstring$)  'Update it

   CALL PutCount(bytecount&)                'Save it

   DoPrint& = bytecount&

END FUNCTION

FUNCTION EjectPage$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  EjectPage$: Ejects a page from the printer.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   EjectPage$ = CHR$(12)

END FUNCTION

SUB FillDriver (yfill!, xfill!, high!, wide!, shade%, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FillDriver: FillDriver is a lower level routine that is called by other
'              procedures to draw lines and fill areas.

'  yfill!      - y coordinate of upper left corner of fill area in inches
'  xfill!      - x coordinate of upper left corner of fill area in inches
'  high!       - height, in inches
'  wide!       - width, in inches
'  shade%      - gray scale, in percent
'  pattern%    - Hewlett Packard defined pattern number, (1 - 6)

'  Calls:      TDot$

'  Externals:  GetDefPat
'              GetLineTint
'              GetMode
'              GetOffsets
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Check mode
   CALL GetMode(mode%)
   IF mode% <> 0 THEN
      EXIT SUB
   END IF

'---Retrieve constants
   IF shade% = 0 AND pattern% = 0 THEN     'Go to defaults if none specified
      CALL GetLineTint(shade%)
      CALL GetDefPat(pattern%)
   END IF
   CALL GetOffsets(vo%, ho%)            'Get printer offsets
   xo! = ho% / 720                     'Convert to inches
   yo! = vo% / 720

   IF shade% = 0 AND pattern% = 0 THEN EXIT SUB

'---Set shade value and pattern number
   g$ = CHR$(27) + "*c" + LTRIM$(STR$(shade%)) + "G"
   p$ = CHR$(27) + "*c" + LTRIM$(STR$(pattern%)) + "G"

'---Locate the cursor
   pstring$ = CHR$(27) + "*p" + TDot$(xfill! + xo!) + "x" + TDot$(yfill! + yo!) + "Y"

'---Add the pattern and gray scale
   IF shade% THEN                      'Print gray scale, if necessary
      pstring$ = pstring$ + g$ + CHR$(27) + "*c" + TDot$(wide!) + "a" + TDot$(high!) + "b2P"
   END IF
   IF pattern% THEN                             'Print pattern, if necessary
      IF pattern% <= 6 AND pattern% >= 1 THEN   'HP defined pattern
         pstring$ = pstring$ + p$ + CHR$(27) + "*c" + TDot$(wide!) + "a" + TDot$(high!) + "b3P"
      END IF
   END IF

'---Print it
   bytes& = DoPrint(pstring$)

END SUB

FUNCTION GrayScale$ (high!, wide!, shade%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  GrayScale$: Prints a Hewlett Packard gray scale, with the upper left
'              corner at the current cursor position.

'  high!       - height, in inches
'  wide!       - width, in inches
'  shade%      - gray scale, in percent

'  Calls:      TDot$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   g$ = CHR$(27) + "*c" + TDot$(wide!) + "a" + TDot$(high!) + "B"
   GrayScale$ = g$ + CHR$(27) + "*c" + LTRIM$(STR$(shade%)) + "G" + CHR$(27) + "*c2P"

END FUNCTION

FUNCTION HorzLine$ (horzlen!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  HorzLine$: Draws a horizontal line from left to right, beginning at the
'             current cursor position.

'  horzlen!   - horizontal length, in inches
'  wt%        - line weight, in dots (1 dot = 1/300 inch)

'  Calls:     MoveCursor$
'             PopCursor$
'             PushCursor$
'             TDot$

'  Externals: GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   IF horzlen! <= 0 THEN h! = ABS(horzlen!) ELSE h! = 0

   w! = (wt% - 1) / 600
   x! = ABS(horzlen!) + w! * 2

   CALL GetLineTint(tint%)

   g1$ = CHR$(27) + "*c" + TDot$(x!) + "a" + LTRIM$(STR$(wt%)) + "B"

   g2$ = CHR$(27) + "*c" + LTRIM$(STR$(tint%)) + "G" + CHR$(27) + "*c2P"

   HorzLine$ = PushCursor$ + MoveCursor$(-w!, -(h! + w!)) + g1$ + g2$ + PopCursor$

END FUNCTION

FUNCTION HPPattern$ (high!, wide!, pattern%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  HPPattern$: Draws a Hewlett Packard defined pattern, with the upper left
'              corner at the current cursor position.

'  high!       - height, in inches
'  wide!       - width, in inches
'  pattern%    - Hewlett Packard defined pattern number, (1 - 6)

'  Calls:      TDot$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   p$ = CHR$(27) + "*c" + TDot$(wide!) + "a" + TDot$(high!) + "B"

   HPPattern$ = p$ + CHR$(27) + "*c" + LTRIM$(STR$(pattern%)) + "G" + CHR$(27) + "*c3P"

END FUNCTION

FUNCTION Initial$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Initial$: Clears printer margins and sets line and character spacing.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'---Clear Margins
   i$ = CHR$(27) + "&l0E" + CHR$(27) + "&a0L"

'---Set Horiz./Vert. Spacing
   h$ = CHR$(27) + "&k12H"                   '10 characters per inch
   v$ = CHR$(27) + "&l8C"                    '6 lines per inch

   Initial$ = i$ + h$ + v$
END FUNCTION

FUNCTION LineFeed$ (lpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  LineFeed$: Moves the cursor vertically down the page by one line.

'  lpi!       - vertical line spacing, in lines per inch

'  Calls:     MoveCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   LineFeed$ = MoveCursor$(1 / lpi!, 0)

END FUNCTION

FUNCTION MoveCursor$ (dy!, dx!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  MoveCursor$: Moves the cursor the specified horizontal and vertical
'               distances.

'  dy!          - horizontal distance to move, in inches
'  dx!          - vertical distance to move, in inches

'  Calls:       TPos$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  
   y$ = TPos$(dy!)
   IF dy! >= 0 THEN y$ = "+" + y$
   x$ = TPos$(dx!)
   IF dx! >= 0 THEN x$ = "+" + x$

   MoveCursor$ = CHR$(27) + "&a" + x$ + "h" + y$ + "V"

END FUNCTION

FUNCTION MoveDots$ (dy%, dx%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  MoveDots$: Moves the cursor the specified horizontal and vertical numbers
'             of dots.

'  dy%        - vertical distance to move, in dots
'  dx%        - horizontal distance to move, in dots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   y$ = LTRIM$(STR$(dy%))
   IF dy% >= 0 THEN y$ = "+" + y$
   x$ = LTRIM$(STR$(dx%))
   IF dx% >= 0 THEN x$ = "+" + x$

   MoveDots$ = CHR$(27) + "*p" + x$ + "x" + y$ + "Y"

END FUNCTION

FUNCTION OpenFile% (filename$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  OpenFile%: Opens a DOS file (or the printer) for output, and initializes
'             control parameters.  Returns the file number used.

'  filename$  - name of file to receive output

'  Calls:     DoPrint&
'             Initial$

'  Externals: PutCount
'             PutHandle
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   WIDTH "lpt1:", 255         'Turn off BASIC's line wrap: VERY IMPORTANT!!!

   handle% = FREEFILE                     'Get available file number

   CALL PutHandle(handle%)             'Save it in a DOS buffer - DO NOT
                                       '  send this command with a different
                                       '  file number until you are finished

   OPEN filename$ FOR OUTPUT AS #handle%  'Route printing to file, do not use
                                          'the colon if specifying 'LPT1' as
                                          ' the file (i.e. LPT1: won't work)!
   bytecount& = 0

   CALL PutCount(bytecount&)              'Set the byte count to 0

   bytes& = DoPrint&(Initial$)            'Initialize the margins
                                          
   OpenFile% = handle%

END FUNCTION

FUNCTION PopCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PopCursor$: Resets the cursor position to the last PUSH'ed position.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   PopCursor$ = CHR$(27) + "&f1S"

END FUNCTION

FUNCTION PosCursor$ (y!, x!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PosCursor$: Positions the cursor on the page.

'  y!          - horizontal position, in inches
'  x!          - vertical position, in inches

'  Calls:      TPos$

'  Externals:  GetOffsets
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   CALL GetOffsets(vo%, ho%)            'Get printer offsets

   IF x! > 0 THEN x$ = TPos$(x! + ho% / 720) ELSE x$ = "+0"

   IF y! > 0 THEN y$ = TPos$(y! + vo% / 720) ELSE y$ = "+0"

   PosCursor$ = CHR$(27) + "&a" + x$ + "h" + y$ + "V"

END FUNCTION

SUB PrintOffset (vertoffset!, horizoffset!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PrintOffset: Sets the vertical and horizontal print offsets.

'  vertoffset!  - vertical print offset, in inches
'  horizoffset! - horizontal print offset, in inches

'  Externals:   PutOffsets
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   CALL PutOffsets(vertoffset! * 720, horizoffset! * 720)

END SUB

FUNCTION PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PushCursor$: Saves the current cursor location.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   PushCursor$ = CHR$(27) + "&f0S"

END FUNCTION

FUNCTION Rectangle$ (high!, wide!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Rectangle$: Prints a line rectangle, with the upper left corner at the
'              current cursor location.

'  high!       - height, in inches
'  wide!       - width, in inches
'  wt%         - line weight, in dots (1 dot = 1/300 inch)

'  Calls:      HorzLine$
'              MoveCursor$
'              PopCursor$
'              PushCursor$
'              VertLine$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   r$ = PushCursor$
   r$ = r$ + HorzLine$(wide!, wt%) + VertLine$(high!, wt%)
   r$ = r$ + MoveCursor$(0, wide!)
   r$ = r$ + VertLine$(high!, wt%)
   r$ = r$ + MoveCursor(high!, -wide!)
   r$ = r$ + HorzLine$(wide!, wt%)
   Rectangle$ = r$ + PopCursor$

END FUNCTION

FUNCTION Resolution$ (reso%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Resolution$: Sets the printer graphics resolution.

'  reso%        - graphics resolution of printer, in dots per inch
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   Resolution$ = CHR$(27) + "*t" + LTRIM$(STR$(reso%)) + "R"

END FUNCTION

FUNCTION TDot$ (td!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  TDot$: Takes a numeric value and converts it to a string containing the
'         equivalent number of dots, at 300 dots per inch.

'  td!    - numeric value to be converted to dots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   td% = VAL(STR$(td! * 300))

   TDot$ = LTRIM$(STR$(td%))

END FUNCTION

FUNCTION TPos$ (tp!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  TPos$: Takes a numeric value and converts it to a string containing the
'         equivalent number of decipoints. (1 decipoint = 1/720 inch)

'  tp!    - numeric value to be converted to decipoints
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   t% = VAL(STR$(tp! * 720))

   TPos$ = LTRIM$(STR$(t%))

END FUNCTION

FUNCTION VertLine$ (vertlen!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  VertLine$: Draws a vertical line down from the current cursor position.

'  vertlen!   - vertical length, in inches
'  wt%        - line weight, in dots (1 dot = 1/300 inch)

'  Calls:     MoveCursor$
'             PopCursor$
'             PushCursor$
'             TDot$

'  Externals: GetLineTint
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   IF vertlen! < 0 THEN v! = ABS(vertlen!) ELSE v! = 0

   w! = (wt% - 1) / 600
   y! = ABS(vertlen!) + w! * 2

   CALL GetLineTint(tint%)
   
   g1$ = CHR$(27) + "*c" + TDot$(y!) + "b" + LTRIM$(STR$(wt%)) + "A"

   g2$ = CHR$(27) + "*c" + LTRIM$(STR$(tint%)) + "G" + CHR$(27) + "*c2P"

   VertLine$ = PushCursor$ + MoveCursor$(-(v! + w!), -w!) + g1$ + g2$ + PopCursor$

END FUNCTION

