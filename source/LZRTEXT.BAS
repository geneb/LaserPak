'$INCLUDE: 'laserpak.dec'

'                  Parameter List: LZRTEXT.BAS
'-----------------------------------------------------------------------------
''  cpi! - character spacing, in characters per inch
''  indent! - distance to indent first line, in inches
''  lpi! - line spacing, in lines per inch
''  rightjustify% - right justification flag, set if = True
''  text$ - text to print
''  text$() - array containing text lines to print
''  txtwidth! - width of text printing zone

FUNCTION BoldText$ (text$, cpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  BoldText$: Prints text at the current cursor position, in bold face.

'  text$      - text to print
'  cpi!       - character spacing, in characters per inch

'  Externals: PopCursor$
'             PushCursor$
'             TDot$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  
   IF cpi! > 0 THEN mi$ = CHR$(27) + "&k" + TDot$(.4 / cpi!) + "H" ELSE mi$ = ""
  
   bs1$ = CHR$(27) + "*p-" + TDot$(LEN(text$) / cpi! + 2 / 300) + "X"

   BoldText$ = PushCursor$ + mi$ + text$ + bs1$ + text$ + PopCursor$

END FUNCTION

FUNCTION CenterText$ (text$, cpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  CenterText$: Relocates the cursor, so text will be centered horizontally
'               about the previous cursor position.

'  text$        - text to print
'  cpi!         - character spacing, in characters per inch

'  Externals:   MoveCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   mi$ = ""
   IF cpi! > 0 THEN
      mi$ = mi$ + CHR$(27) + "&k" + LTRIM$(STR$(120 / cpi!)) + "H"
      mi$ = mi$ + MoveCursor$(0, -LEN(text$) / cpi! / 2)
   END IF

   CenterText$ = mi$

END FUNCTION

FUNCTION CharSpacing$ (cpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  CharSpacing$: Sets the text character spacing.

'  cpi!          - character spacing, in characters per inch
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   CharSpacing$ = CHR$(27) + "&k" + LTRIM$(STR$(120 / cpi!)) + "H"

END FUNCTION

FUNCTION CompFont$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  CompFont$: Selects compressed font.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   CompFont$ = CHR$(27) + "&k2S"

END FUNCTION

FUNCTION FixedSpacing$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FixedSpacing$: Selects fixed spacing mode.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
  
   FixedSpacing$ = CHR$(27) + "(s0P"

END FUNCTION

FUNCTION FlushRight$ (text$, cpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FlushRight$: Prints text so that it ends at the current cursor position.

'  text$        - text to print
'  cpi!         - character spacing, in characters per inch

'  Externals:   MoveCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   mi$ = ""
   IF cpi! > 0 THEN
      mi$ = mi$ + CHR$(27) + "&k" + LTRIM$(STR$(120 / cpi!)) + "H"
      mi$ = mi$ + MoveCursor$(0, -LEN(text$) / cpi!)
   END IF

   FlushRight$ = mi$


END FUNCTION

SUB FormatText (text$(), lpi!, txtwidth!, indent!, rightjustify%, cpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FormatText:   Formats an array containing lines of text.  Lines are wrapped
'                if needed.  Includes indenting of the first line and right
'                justification.

'  text$()       - array containing text lines to print
'  lpi!          - line spacing, in lines per inch
'  txtwidth!     - width of text printing zone
'  indent!       - distance to indent first line, in inches
'  rightjustify% - right justification flag, set if = True
'  cpi!          - character spacing, in characters per inch

'  Calls:        JustifyText$

'  Externals:    MoveCursor$
'                MoveDots$
'                PopCursor$
'                PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   IF txtwidth! <= 0 THEN EXIT SUB

   ulstatus% = 0
   uon$ = CHR$(27) + "&dD"
   uoff$ = CHR$(27) + "&d@"

   numlines% = UBOUND(text$)           'How many lines of text?
   ind! = indent!                      'Preserve indent value
   tw! = txtwidth!
   t$ = text$(1): i% = 2               'Initialize
   indent$ = MoveCursor$(0, ind!)      'Move cursor to indent position
   x% = INSTR(t$, CHR$(4))
   IF x% THEN                          'Is a tab set?
      i$ = LEFT$(t$, x% - 1)
      t$ = RIGHT$(t$, LEN(t$) - x%)
      pstring$ = PushCursor$ + i$ + PopCursor$
      IF indent! < 0 THEN
         pstring$ = pstring$ + MoveCursor$(0, -ind!)
         indent$ = "": ind! = 0
         tw! = tw! + indent!
      END IF
      GOSUB printpara
   ELSE
      IF indent! < 0 THEN
         pstring$ = MoveCursor$(0, -ind!)
         GOSUB printpara
         tw! = tw! + indent!
      END IF
   END IF
   DO
      GOSUB linesadd                      'Add additional text lines for wrap
      text$ = JustifyText$(t$, tw! - ind!, cpi!, rightjustify%)
      GOSUB codescheck                    'Any underlining or bold?
      pstring$ = PushCursor$ + indent$ + text$
      GOSUB printpara                     'Print the line
      pstring$ = PopCursor$ + MoveCursor$(1 / lpi!, 0)
      GOSUB printpara                     'Advance to next line
      indent$ = "": ind! = 0              'Cancel indent
      IF t$ = "" THEN EXIT DO             'Exit when nothing left over
   LOOP
   pstring$ = uoff$                       'Make sure underlining if off
   IF indent! < 0 THEN pstring$ = pstring$ + MoveCursor$(0, indent!)
   GOSUB printpara                        '  and, restore left margin
EXIT SUB

linesadd:
   IF i% > numlines% THEN RETURN          'Exit if done
   DO
      IF LEN(t$) > 255 THEN EXIT DO       'Keep at least 255 bytes alive
      IF RIGHT$(t$, 1) = "." THEN         'Insert two spaces after period
         ins% = 2
      ELSEIF RIGHT$(t$, 1) = "-" THEN     '  none after hyphen
         ins% = 0
      ELSE                                '  one after everything else
         ins% = 1
      END IF
      t$ = t$ + SPACE$(ins%) + text$(i%)
      i% = i% + 1
      IF i% > numlines% THEN EXIT DO
   LOOP
RETURN

codescheck:
   DO                                'First, do underlines
      x% = INSTR(text$, CHR$(2))     'Find underline code
      IF x% = 0 THEN EXIT DO         'If none, we're done
      IF ulstatus% THEN addcode$ = uoff$ ELSE addcode$ = uon$  'Status?
      ulstatus% = ulstatus% XOR -1   'Change status
      text$ = LEFT$(text$, x% - 1) + addcode$ + RIGHT$(text$, LEN(text$) - x%)
   LOOP                              'Insert underline command in text string
   DO                                'Now, do bold
      x% = INSTR(text$, CHR$(1))     'Find bold code
      IF x% = 0 THEN EXIT DO         'If none, we're done
      bold$ = RIGHT$(text$, LEN(text$) - x%)   'Start of bold text
      text$ = LEFT$(text$, x% - 1)
      x% = INSTR(bold$, CHR$(1))               'End marker code
      rtext$ = RIGHT$(bold$, LEN(bold$) - x%)  'Remaing text after bold
      bold$ = LEFT$(bold$, x% - 1)             'Text to be bolded
      numdots% = LEN(bold$) * 300 / cpi!
      text$ = text$ + bold$ + PushCursor$ + MoveDots$(0, -numdots% - 2)
      text$ = text$ + bold$ + PopCursor$ + rtext$
   LOOP
RETURN

printpara:
   bytes& = DoPrint(pstring$)
RETURN

END SUB

FUNCTION JustifyText$ (text$, txtwidth!, cpi!, rightjustify%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  JustifyText$: Prints a text string within a specified width, beginning at
'                the current cursor position.

'  text$         - text to print
'  txtwidth!     - width of text printing zone
'  cpi!          - character spacing, in characters per inch
'  rightjustify% - right justification flag, set if = True
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   t$ = LTRIM$(RTRIM$(text$))                   'Preserve text$

   IF cpi! <= 0 THEN cpi! = 10                  'Default char. spacing
   maxchars% = cpi! * txtwidth!
   mi$ = CHR$(27) + "&k" + LTRIM$(STR$(120 / cpi!)) + "H"  'Horiz. motion
                                                           '   index
   IF LEN(t$) = maxchars% THEN         'If already justified
      JustifyText$ = mi$ + t$
      text$ = ""
   ELSE                                         'Need to justify
      IF LEN(t$) > maxchars% THEN GOSUB trimexcess ELSE text$ = ""
      IF rightjustify% THEN GOSUB addpad        'Add extra spaces to justify
      JustifyText$ = mi$ + t$                   'Put it all together
   END IF

EXIT FUNCTION

addpad:
   t$ = RTRIM$(t$)               'Chop excess spaces
   codes% = 0
   numspaces% = 0
   FOR i% = 1 TO LEN(t$)
      IF ASC(MID$(t$, i%, 1)) < 32 THEN codes% = codes% + 1
      IF ASC(MID$(t$, i%, 1)) = 32 THEN numspaces% = numspaces% + 1
   NEXT i%
   pad% = maxchars% - LEN(t$) + codes%
   IF (pad% + numspaces%) / maxchars% > .35 THEN RETURN
   a$ = ""
   sp% = 0
   FOR i% = 1 TO LEN(t$)         'Pad spaces
      a% = ASC(MID$(t$, i%, 1))
      a$ = a$ + CHR$(a%)
      IF a% = 32 THEN
         sp% = sp% + 1
         a$ = a$ + SPACE$(pad% \ numspaces%)
         IF sp% <= pad% MOD numspaces% THEN a$ = a$ + " "
      END IF
   NEXT i%
   t$ = a$
RETURN

trimexcess:
   t$ = LEFT$(t$, maxchars% + 1)
   DO
      a% = ASC(RIGHT$(t$, 1))
      t$ = LEFT$(t$, LEN(t$) - 1)
      IF a% = 32 OR LEN(t$) = 0 THEN EXIT DO
      IF ASC(RIGHT$(t$, 1)) = 45 THEN EXIT DO
   LOOP
   text$ = LTRIM$(RIGHT$(text$, LEN(text$) - LEN(t$)))
RETURN

END FUNCTION

FUNCTION Proportional$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Proportional$: Selects proportional spacing mode.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   Proportional$ = CHR$(27) + "(s1P"

END FUNCTION

FUNCTION SlantText$ (text$, cpi!, lpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  SlantText$: Prints text on a diagonal line, the slope of which is defined

'  text$       - text to print
'  cpi!        - character spacing, in characters per inch
'  lpi!        - line spacing, in lines per inch

'  Externals:  MoveCursor$
'              PopCursor$
'              PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   st$ = ""
  
   FOR i% = 1 TO LEN(text$)
      st$ = st$ + PushCursor$ + MID$(text$, i%, 1) + PopCursor$
      st$ = st$ + MoveCursor$(1 / lpi!, 1 / cpi!)
   NEXT i%

   SlantText$ = st$

END FUNCTION

FUNCTION StdFont$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  StdFont$: Selects a standard font (not compressed).
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   StdFont$ = CHR$(27) + "&k0S"

END FUNCTION

FUNCTION UnderlineOff$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  UnderlineOff$: Turns underline off.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   UnderlineOff$ = CHR$(27) + "&d@"

END FUNCTION

FUNCTION UnderlineOn$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  UnderlineOn$: Turns underline on.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   UnderlineOn$ = CHR$(27) + "&dD"

END FUNCTION

FUNCTION VertText$ (text$, lpi!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  VertText$: Prints text vertically, beginning at the current cursor
'             position.

'  text$      - text to print
'  lpi!       - line spacing, in lines per inch

'  Externals: MoveCursor$
'             PopCursor$
'             PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   vt$ = ""

   FOR i% = 1 TO LEN(text$)
      vt$ = vt$ + PushCursor$ + MID$(text$, i%, 1) + PopCursor$
      vt$ = vt$ + MoveCursor$(1 / lpi!, 0)
   NEXT i%

   VertText$ = vt$

END FUNCTION

