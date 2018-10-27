'$INCLUDE: 'laserpak.dec'

'                  Parameter List: FONTS.BAS
'-----------------------------------------------------------------------------
''  alchar$ - character to be aligned
''  baseline% - baseline position of bitmapped font, in dots
''  bitmapfile$ - file containing a bitmapped image
''  cbytes%() - array containing number of bytes defining font characters
''  center% - center flag, will center text strings if True (i.e. center% = -1)
''  clines% - number of lines in cell defining font character
''  fontfile$ - name of file containing a bitmapped font
''  header$() - array containing the text of the font characteristics
''  id% - I.D. number of a font (must be in the range: 0 to 32767)
''  imagefile$ - name of file containing a bitmapped image
''  indent! - distance to indent, in inches
''  insert$ - portion of text string to be modified
''  landfile$ - name of file containing a landscaped oriented bitmapped image
''  lpi! - line spacing in lines per inch
''  numdots% - total width of a text string excluding spaces, in dots
''  numspaces% - number of space characters in a text string
''  pstab%() - array containing character widths, in dots
''  pstabfile$ - name of file containing a proportional spacing table
''  ptr&() - array containing pointers to font character descriptors
''  rightjustify% - right justification flag, set if = True
''  text$ - text string
''  text$() - array containing text strings
''  txtwidth! - width of the print zone of a text string, in inches
''  ultext$ - text string to be underlined
''  xpos! - horizontal position of alignment character, in inches

FUNCTION AlignChar$ (xpos!, text$, alchar$, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  AlignChar$: Aligns a proportional font text string to specified character.

'  xpos!       - horizontal position of alignment character, in inches
'  text$       - text string
'  alchar$     - character to be aligned
'  pstab%()    - array containing character widths, in dots

'  Calls:      TextDots

'  Externals:  MoveDots$
'              TPos$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   p% = INSTR(text$, alchar$)       'Find alignment character and isolate
                                    'the part of string until it first occurs

                                    'If it doesn't occur, use entire string
   IF p% THEN movetext$ = LEFT$(text$, p%) ELSE movetext$ = text$
                                                    'Count the dots
   CALL TextDots(movetext$, pstab%(), numdots%, numspaces%)
                                                    'Add width of spaces
   numdots% = numdots% + numspaces% * pstab%(32)

                                                    'Back space to start
   AlignChar$ = CHR$(27) + "&a" + TPos$(xpos!) + "H" + MoveDots$(0, -numdots%) + CHR$(27) + "&k" + LTRIM$(STR$(pstab%(32) * .4)) + "H"
  
END FUNCTION

FUNCTION BoldTextP$ (text$, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  BoldTextP$: Prints a proportional font in bold face.

'  text$       - text string
'  pstab%()    - array containing character widths, in dots

'  Calls:      TextDots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   CALL TextDots(text$, pstab%(), numdots%, numspaces%)
  
   cs$ = CHR$(27) + "&k" + LTRIM$(STR$(.4 * pstab%(32))) + "H"

   bs% = -(numdots% + numspaces% * pstab%(32) - 2)
   
   bs$ = CHR$(27) + "*p" + STR$(bs%) + "X"

   BoldTextP$ = cs$ + text$ + bs$ + text$

END FUNCTION

FUNCTION CenterTextP$ (text$, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  CenterTextP$: Centers a proportional font text at the current cursor
'                position.

'  text$         - text string
'  pstab%()      - array containing character widths, in dots

'  Calls:        TextDots

'  Externals:    MoveDots$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   CALL TextDots(text$, pstab%(), numdots%, numspaces%)

   i$ = MoveDots$(0, -(numdots% + numspaces% * pstab%(32)) / 2)
   CenterTextP$ = i$
END FUNCTION

SUB CheckHeader (fontfile$, baseline%, pstab%(), cbytes%(), clines%, ptr&())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  CheckHeader: Retrieves character data from a file containing a
'               downloadable font in Hewlett Packard format.

'  fontfile$    - name of file containing a bitmapped font
'  baseline%    - baseline position of bitmapped font, in dots
'  pstab%()     - array containing character widths, in dots
'  cbytes%()    - array containing number of bytes defining font characters
'  clines%      - number of lines in cell defining font character
'  ptr&()       - array containing pointers to font character descriptors
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   f% = FREEFILE
   OPEN "b", f%, fontfile$

   s1$ = CHR$(27) + ")s"
   s2$ = CHR$(27) + "(s"
   s3$ = CHR$(27) + "*c"
   b$ = " "
   w$ = "  "
   a$ = SPACE$(100)

   GET #f%, 1, a$

'----- Find font header and length
   x1% = INSTR(a$, s1$)
   x2% = INSTR(x1%, a$, "W")

'----- Font characteristics
   GET #f%, x2% + 4, b$: fonttype% = ASC(b$)
   GET #f%, x2% + 7, w$: baseline% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 9, w$: ndots% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 11, w$: clines% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 13, b$: orientation% = ASC(b$)
   GET #f%, x2% + 17, w$: pitch% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256

'----- Compute space character width - same as pitch divided by four
   pstab%(32) = pitch% \ 4

'----- Get character data
   ptr& = x2% + VAL(MID$(a$, x1% + 3, x2% - (x1% + 2))) + 1
   DO

'----- Find character I.D.
      GET #f%, ptr&, a$
      x1% = INSTR(a$, s3$)
      IF x1% = 0 THEN EXIT DO
      x2% = INSTR(a$, "E")
      ch% = VAL(MID$(a$, x1% + 3, x2% - (x1% + 2)))
      ptr& = ptr& + x2%

'----- Get character descriptor and length
      GET #f%, ptr&, a$
      x1% = INSTR(a$, s2$)
      x2% = INSTR(a$, "W")
      bytes% = VAL(MID$(a$, x1% + 3, x2% - (x1% + 2)))
      cbytes%(ch%) = bytes%
      ptr&(ch%) = x2% + ptr&

'----- Get character width from descriptor header
      pstab%(ch%) = INT((ASC(MID$(a$, x2% + 15)) * 256 + ASC(MID$(a$, x2% + 16))) / 4)
      ptr& = ptr& + x2% + bytes%
   LOOP
   CLOSE f%

END SUB

SUB CreatePSTable (fontfile$, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  CreatePSTable: Creates a proportional spacing table.

'  fontfile$      - name of file containing a bitmapped font
'  pstab%()       - array containing character widths, in dots

'  Calls:         CheckHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   REDIM cbytes%(256), ptr&(256)

   CALL CheckHeader(fontfile$, baseline%, pstab%(), cbytes%(), clines%, ptr&())

END SUB

FUNCTION DesignateFont$ (id%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DesignateFont$: Designates a font I.D. number as primary font, effectively
'                  selecting it for use.

'  id%             - I.D. number of a font (must be in the range: 0 to 32767)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   DesignateFont$ = CHR$(27) + "(" + LTRIM$(STR$(id%)) + "X"

END FUNCTION

FUNCTION ExtractID% (fontfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ExtractID%: Extracts the font I.D. from a font file in Hewlett Packard
'              format.

'  fontfile$   - name of file containing a bitmapped font
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   f% = FREEFILE
   OPEN "r", f%, fontfile$, 100
   FIELD #f%, 100 AS font$
   GET #f%, 1
   ptr% = INSTR(font$, CHR$(27) + "*c")
   id% = -1                            'Initialize I.D. value

'  Note:  In this routine, -1 = False, i.e. no I.D. found.  This is because
'         '0', the normal False value, is a valid font I.D.

   IF ptr% THEN
      ptr2% = INSTR(ptr%, font$, "D")
      IF ptr2% THEN
         id# = VAL(MID$(font$, ptr% + 3, ptr2% - ptr% - 3))
         IF id# >= 0 OR id# <= 32756 THEN id% = id#
      END IF
   END IF
   CLOSE f%

   ExtractID% = id%

END FUNCTION

FUNCTION FlushRightP$ (text$, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FlushRightP$: Prints a text of proportional font flush right.

'  text$         - text string
'  pstab%()      - array containing character widths, in dots

'  Calls:        TextDots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   CALL TextDots(text$, pstab%(), numdots%, numspaces%)

   FlushRightP$ = MoveDots(0, -(numdots% + numspaces% * pstab%(32)))

END FUNCTION

SUB FontHeader (fontfile$, header$())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FontHeader: Creates a string containing the characteristics of a soft font.

'  fontfile$   - name of file containing a bitmapped font
'  header$()   - array containing the text of the font characteristics
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   REDIM face$(10), header$(11)        'Standard typefaces
      face$(0) = "Line Printer"
      face$(1) = "Pica"
      face$(2) = "Elite"
      face$(3) = "Courier"
      face$(4) = "Helvetica"
      face$(5) = "Times Roman"
      face$(6) = "Gothic"
      face$(7) = "Script"
      face$(8) = "Prestige"
      face$(9) = "Caslon"
      face$(10) = "Orator"

   f% = FREEFILE
   OPEN "b", f%, fontfile$

   s1$ = CHR$(27) + ")s"
   b$ = " "
   w$ = "  "
   a$ = SPACE$(100)

   GET #f%, 1, a$

'---Find font header and length
   x1% = INSTR(a$, s1$)
   x2% = INSTR(x1%, a$, "W")

'---Font characteristics
   GET #f%, x2% + 4, b$: fonttype% = ASC(b$)
   GET #f%, x2% + 7, w$: baseline% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 9, w$: cellwidth% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 11, w$: cellheight% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 13, b$: orientation% = ASC(b$)
   GET #f%, x2% + 14, b$: spacing% = ASC(b$)
   GET #f%, x2% + 15, w$: symbolset% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 17, w$: pitch% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 19, w$: height% = ASC(RIGHT$(w$, 1)) + ASC(LEFT$(w$, 1)) * 256
   GET #f%, x2% + 24, b$: style% = ASC(b$)
   GET #f%, x2% + 25, b$: stroke% = ASC(b$)
   GET #f%, x2% + 26, b$: face% = ASC(b$)
   CLOSE f%

'---Construct strings
   IF fonttype% = 1 THEN header$(1) = "Font type: 8-bit" ELSE header$(1) = "Font type: 7-bit"
   header$(2) = "Baseline postion:" + STR$(baseline%)
   header$(3) = "Cell width:" + STR$(cellwidth%)
   header$(4) = "Cell height:" + STR$(cellheight%)
   IF orientation% = 0 THEN header$(5) = "Orientation: Portrait" ELSE header$(5) = "Orientation: Landscape"
   IF spacing% = 0 THEN
      header$(6) = "Spacing: Fixed"
      header$(7) = "Character spacing:" + STR$(pitch% / 4) + " dots"
   ELSE
      header$(6) = "Spacing: Proportional"
      header$(7) = "Space character width:" + STR$(pitch% / 4) + " dots"
   END IF
   header$(8) = "Symbol set code:" + STR$(symbolset%)
   header$(9) = "Character height:" + STR$(height% / 4) + " dots"
   IF style% = 0 THEN header$(10) = "Style: Upright" ELSE header$(10) = "Style: Italic"
   IF stroke% < 0 THEN
      header$(10) = "Stroke weight: Light"
   ELSEIF stroke% = 0 THEN
      header$(10) = "Stroke weight: Normal"
   ELSEIF stroke% > 0 THEN
      header$(10) = "Stroke weight: Bold"
   END IF
   IF face% > 0 AND face% < 11 THEN header$(11) = "Type face: " + face$(face%) ELSE header$(11) = "Type face: Custom"

END SUB

SUB FormatTextP (text$(), lpi!, txtwidth!, indent!, rightjustify%, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FormatTextP:  Formats an array containing lines of text.  Includes word
'                wrap and right justification.  Maximizes the print density
'                on each line.

'  text$()       - array containing text strings
'  lpi!          - line spacing in lines per inch
'  txtwidth!     - width of the print zone of a text string, in inches
'  indent!       - distance to indent, in inches
'  rightjustify% - right justification flag, set if = True
'  pstab%()      - array containing character widths, in dots

'  Calls:        JustifyTextP$
'                TextDots

'  Externals:    MoveCursor$
'                MoveDots$
'                PopCursor$
'                PushCursor$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   IF txtwidth! <= 0 THEN EXIT SUB

   ulstatus% = 0
   pitch$ = LTRIM$(STR$(pstab%(32) * .4))
   uon$ = CHR$(27) + "&dD"
   uoff$ = CHR$(27) + "&d@"
   setcpi$ = CHR$(27) + "&k" + pitch$ + "H"

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
      GOSUB paraprint
   ELSE
      IF indent! < 0 THEN
         pstring$ = MoveCursor$(0, -ind!)
         GOSUB paraprint
         tw! = tw! + indent!
      END IF
   END IF
   DO
      GOSUB addlines                      'Add additional text lines for wrap
      text$ = JustifyTextP$(t$, tw! - ind!, pstab%())
      IF NOT rightjustify% THEN           'Strip r.j. spacing escape sequence
         IF LEFT$(text$, 3) = CHR$(27) + "&k" THEN text$ = MID$(text$, INSTR(text$, "H") + 1)
         text$ = setcpi$ + text$          'Add default spacing escape sequence
      END IF
      GOSUB checkforcodes                 'Any underlining or bold?
      pstring$ = PushCursor$ + indent$ + text$
      GOSUB paraprint                     'Print the line
      pstring$ = PopCursor$ + MoveCursor$(1 / lpi!, 0)
      GOSUB paraprint                     'Advance to next line
      indent$ = "": ind! = 0              'Cancel indent
      IF t$ = "" THEN EXIT DO             'Exit when nothing left over
   LOOP
   pstring$ = uoff$                       'Make sure underlining if off
   IF indent! < 0 THEN pstring$ = pstring$ + MoveCursor$(0, indent!)
   GOSUB paraprint                        '  and, restore left margin
EXIT SUB

addlines:
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

checkforcodes:
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
      rtext$ = RIGHT$(bold$, LEN(bold$) - x%)  'Remaining text after bold
      bold$ = LEFT$(bold$, x% - 1)             'Text to be bolded
      CALL TextDots(bold$, pstab%(), numdots%, numspaces%)  'Width of bold
      text$ = text$ + bold$ + PushCursor$ + MoveDots$(0, -numdots% - 2)
      text$ = text$ + bold$ + PopCursor$ + rtext$
   LOOP
RETURN

paraprint:
   bytes& = DoPrint(pstring$)
RETURN

END SUB

FUNCTION InsertBoldCode$ (text$, insert$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  InsertBoldCode$: Inserts underline codes in a text string.

'  text$            - text string
'  insert$          - portion of text string to be modified
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   x% = INSTR(text$, insert$) - 1
   IF x% = -1 THEN
      InsertBoldCode$ = text$
   ELSE
      c$ = CHR$(1)
      i$ = c$
      stat% = -1
      FOR i% = 1 TO LEN(insert$)
         a% = ASC(MID$(insert$, i%, 1))
         IF stat% = 0 AND a% <> 32 THEN i$ = i$ + c$: stat% = stat% XOR -1
         IF stat% = -1 AND a% = 32 THEN i$ = i$ + c$: stat% = stat% XOR -1
         i$ = i$ + CHR$(a%)
      NEXT i%
      InsertBoldCode$ = LEFT$(text$, x%) + i$ + c$ + RIGHT$(text$, LEN(text$) - x% - LEN(insert$))
   END IF

END FUNCTION

FUNCTION InsertTabCode$ (text$, insert$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  InsertTabCode$: Inserts underline codes in a text string.

'  text$           - text string
'  insert$         - portion of text string to be modified
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   x% = INSTR(text$, insert$) - 1
   IF x% = -1 THEN
      InsertTabCode$ = text$
   ELSE
      x$ = CHR$(4) + insert$
      InsertTabCode$ = LEFT$(text$, x%) + x$ + RIGHT$(text$, LEN(text$) - x% - LEN(insert$))
   END IF


END FUNCTION

FUNCTION InsertULCode$ (text$, insert$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  InsertULCode$: Inserts underline codes in a text string.

'  text$          - text string
'  insert$        - portion of text string to be modified
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   x% = INSTR(text$, insert$) - 1
   IF x% = -1 THEN
      InsertULCode$ = text$
   ELSE
      x$ = CHR$(2) + insert$ + CHR$(2)
      InsertULCode$ = LEFT$(text$, x%) + x$ + RIGHT$(text$, LEN(text$) - x% - LEN(insert$))
   END IF
END FUNCTION

FUNCTION JustifyTextP$ (text$, txtwidth!, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  JustifyTextP$: Prints a text string from the current cursor position, so
'                 it is right justified to the specified text width.

'  text$          - text string
'  txtwidth!      - width of the print zone of a text string, in inches
'  pstab%()       - array containing character widths, in dots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Compute text length, in dots, and number of spaces
   numdots% = 0
   numspaces% = 0
   maxdots% = txtwidth! * 300
   minspace% = pstab%(32)
   maxspace% = pstab%(32) * 2
   eot% = LEN(text$)
   GOSUB computedots

'---Is it too short?
   IF eot% = LEN(text$) THEN
      IF numdots% + numspaces% * maxspace% < txtwidth! * 300 THEN
         JustifyTextP$ = text$
         text$ = ""
         EXIT FUNCTION
      END IF
   END IF

'---Adjust for text too long, and compute remainder, if any
   IF eot% < LEN(text$) THEN
      pt$ = LEFT$(text$, eot%)
      text$ = LTRIM$(RIGHT$(text$, LEN(text$) - eot%))
   ELSE
      pt$ = text$
      text$ = ""
   END IF

'---Recompute number of spaces
   numspaces% = 0
   FOR i% = 1 TO LEN(pt$)
      IF ASC(MID$(pt$, i%, 1)) = 32 THEN numspaces% = numspaces% + 1
   NEXT i%

'---Compute proportional spacing
   IF numspaces% = 0 THEN
      JustifyTextP$ = pt$
   ELSE
      spacedots! = (txtwidth! * 300 - numdots%) / numspaces%
      sp$ = CHR$(27) + "&k" + LTRIM$(STR$(spacedots! * .4)) + "H"
      JustifyTextP$ = sp$ + pt$
   END IF
EXIT FUNCTION

computedots:
   FOR i% = 1 TO LEN(text$)
      ch% = ASC(MID$(text$, i%, 1))
      incdots% = pstab%(ch%)
      IF ch% = 32 THEN incdots% = incdots% / 2
      IF numdots% + adddots% + numspaces% * minspace% + incdots% > maxdots% THEN
         RETURN
      ELSE
         IF ch% = 32 THEN
            numspaces% = numspaces% + 1
            IF adddots% THEN
               eot% = i% - 1
               numdots% = numdots% + adddots%
               adddots% = 0
            END IF
         ELSEIF ch% = 45 THEN
            eot% = i%
            numdots% = numdots% + adddots%
            adddots% = 0
         ELSE
            adddots% = adddots% + incdots%
         END IF
      END IF
   NEXT i%
   numdots% = numdots% + adddots%
   eot% = LEN(text$)
RETURN

END FUNCTION

FUNCTION LeftTextP$ (text$, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  LeftTextP$: Prints a text string left justified to the current cursor
'              position with spacing according to the space character.

'  text$       - text string
'  pstab%()    - array containing character widths, in dots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   LeftTextP$ = CHR$(27) + "&k" + LTRIM$(STR$(.4 * pstab%(32))) + "H" + text$

END FUNCTION

SUB LoadPSTable (pstabfile$, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  LoadPSTable: Loads a proportional spacing table that has been saved in
'               a file.

'  pstabfile$   - name of file containing a proportional spacing table
'  pstab%()     - array containing character widths, in dots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'---Load the table string
   f% = FREEFILE
   OPEN "r", f%, pstabfile$, 256
   FIELD #f%, 256 AS pstab$
   GET #f%, 1
   pst$ = pstab$
   CLOSE f%

'---Create PS table array (one byte per character)
   FOR i% = 1 TO UBOUND(pstab%)
      pstab%(i%) = ASC(MID$(pst$, i%, 1))
   NEXT i%

END SUB

SUB SavePSTable (pstabfile$, pstab%())
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  SavePSTable: Saves proportional spacing table data into a file for fast
'               retrieval.

'  pstabfile$   - name of file containing a proportional spacing table
'  pstab%()     - array containing character widths, in dots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Create table (one byte per character)
   FOR i% = 1 TO UBOUND(pstab%)              'Position in table corresponds to
      pst$ = pst$ + CHR$(pstab%(i%))      '  ASCII value
   NEXT i%

'---Save the table
   f% = FREEFILE
   OPEN "r", f%, pstabfile$, 256
   FIELD #f%, 256 AS pstab$
   LSET pstab$ = pst$
   PUT #f%, 1
   CLOSE f%

END SUB

FUNCTION SpecifyFontID$ (id%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  SpecifyFontID$: Specifies a font I.D.

'  id%             - I.D. number of a font (must be in the range: 0 to 32767)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   SpecifyFontID$ = CHR$(27) + "*c" + LTRIM$(STR$(id%)) + "D"

END FUNCTION

SUB TextDots (text$, pstab%(), numdots%, numspaces%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  TextDots:  Takes a text string and computes the total width of non-space
'             characters, in dots; and, the number of spaces in the string.

'  text$      - text string
'  pstab%()   - array containing character widths, in dots
'  numdots%   - total width of a text string excluding spaces, in dots
'  numspaces% - number of space characters in a text string
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Compute text length, in dots, and number of spaces
   numdots% = 0
   numspaces% = 0
   FOR i = 1 TO LEN(text$)
      ch% = ASC(MID$(text$, i, 1))                 'ASCII code of character
      IF ch% > 32 THEN                             'If a character,
         numdots% = numdots% + pstab%(ch%)         '  add it's width
      ELSEIF ch% = 32 THEN                         'If a space,
         numspaces% = numspaces% + 1               '  update the count
      ELSEIF ch% < 32 THEN                         'If a control code
      END IF
   NEXT i

END SUB

FUNCTION UnderlineP$ (text$, ultext$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  UnderlineP$: Underlines specific text in a string of a proportional font,
'               by inserting underline 'on' and 'off' codes in the string.

'  text$        - text string
'  ultext$      - text string to be underlined

'  Externals:   UnderlineOff$
'               UnderlineOn$
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   l% = INSTR(text$, ultext$)
   IF l% THEN
      lt% = l% - 1                           'Find first character to underline
      rt% = LEN(text$) - lt% - LEN(ultext$)  'Find last character to underline
      UnderlineP$ = LEFT$(text$, lt%) + UnderlineOn$ + ultext$ + UnderlineOff$ + RIGHT$(text$, rt%)
   ELSE
      UnderlineP$ = text$                 'If underline string is not found
   END IF

END FUNCTION

FUNCTION ValidFont% (fontfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ValidFont%: Checks a proportional font file for validity.

'  fontfile$   - name of file containing a bitmapped font
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   f% = FREEFILE
   OPEN "b", f%, fontfile$
   IF LOF(f%) < 100 THEN ValidFont% = 0: EXIT FUNCTION

   s1$ = CHR$(27) + ")s"
   s2$ = CHR$(27) + "(s"
   s3$ = CHR$(27) + "*c"
   a$ = SPACE$(100)

   GET #f%, 1, a$

'---Find font header and length
   x1% = INSTR(a$, s1$)
   IF x1% = 0 THEN ValidFont% = 0: EXIT FUNCTION
   x2% = INSTR(x1%, a$, "W")
   IF x2% = 0 THEN ValidFont% = 0: EXIT FUNCTION

'---Clean up and exit
   CLOSE f%

   ValidFont% = -1

END FUNCTION

