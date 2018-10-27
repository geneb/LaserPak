DECLARE SUB IMG2PCX (bitmapfile$, pcxfile$, planes%, hres%, vres%)

'---Enter variables
   COLOR 7, 1: CLS
   LOCATE 5, 10
   LINE INPUT "Enter name of bitmap file: "; bitmapfile$
   LOCATE 7, 10
   LINE INPUT "Enter name of PCX file: "; pcxfile$
   LOCATE 9, 10
   PRINT "Do you want a color PCX file? (y or n)"
   DO
      a$ = INKEY$
      IF a$ <> "" THEN EXIT DO
   LOOP
   IF UCASE$(a$) <> "Y" THEN
      planes% = 1      'Black & white mode is 1 color plane
   ELSE
      planes% = 4      'Default color mode is 4 color planes
   END IF                     '(i.e. 16 colors); use 3 planes for 8 colors

   hres% = 640                     'Horizontal screen resolution
   vres% = 200                     'Vertical screen resolution
                  'Use the resolution that pertains to the monitor you
                  'wish to use, if the image will be displayed on a screen
   CALL IMG2PCX(bitmapfile$, pcxfile$, planes%, hres%, vres%)

SUB IMG2PCX (bitmapfile$, pcxfile$, planes%, hres%, vres%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  IMG2PCX:       Converts a bitmapped image file to a black and white, or
'                 color PCX file.

'  bitmapfile$    - name of file containing bitmapped image
'  pcxfile$       - name of PCX file to create
'  planes%        - number of color planes: 1 - Black & White; 3 - 8 Color, etc.
'  hres%          - horizontal screen resolution, in pixels
'  vres%          - vertical screen resolution, in pixels
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   

'---Get image file header
   f2% = FREEFILE
   OPEN "b", f2%, bitmapfile$
   ptr& = LOF(f2%) - 3                       'Find start of header -
   header$ = SPACE$(4)                       '   last 4 bytes of file
   GET #f2%, ptr&, header$
   numdots% = CVI(LEFT$(header$, 2))         'Width of bitmap, in dots
   numlines% = CVI(RIGHT$(header$, 2))       'Height of bitmap, in lines
   reclen% = (numdots% + 7) \ 8              'Width of bitmap file, in bytes
   pcxlen% = ((reclen% + 1) \ 2) * 2         'Make it end on word boundary

'---Construct .PCX header - 2 color PCX file
   header$ = CHR$(10) + CHR$(3) + CHR$(1) + CHR$(1) + MKI$(0) + MKI$(0)
   header.x2% = numdots% - 1
   header.bplin% = pcxlen%
   header.y2% = numlines% - 1
   header$ = header$ + MKI$(header.x2%) + MKI$(header.y2%)
   header$ = header$ + MKI$(hres%) + MKI$(vres%)
   header$ = header$ + STRING$(48, 0)
   header$ = header$ + CHR$(0) + CHR$(planes%) + MKI$(header.bplin%)
   header$ = header$ + STRING$(60, 0)

'---Save PCX file header
   f% = FREEFILE
   OPEN "o", f%, pcxfile$
   PRINT #f%, "";
   CLOSE f%
   OPEN "b", f%, pcxfile$
   PUT #f%, 1, header$

'---Scan buffer
   img.ptr& = 1                      'record pointer in bitmap file
   pcx.ptr& = 129                    'record pointer in pcx file
   FOR i% = 1 TO numlines%
      buffer$ = STRING$(reclen%, 0)
      GET #f2%, img.ptr&, buffer$
      IF pcxlen% > reclen% THEN buffer$ = buffer$ + CHR$(255)
      num.repeats% = 0
      buff$ = ""
      FOR j% = 1 TO pcxlen%
         byte.val% = ASC(MID$(buffer$, j%, 1)) XOR 255   'PCX considers 0 as black
         IF j% = 1 THEN last.byte% = byte.val%           'Initialize last.byte%
         IF byte.val% = last.byte% THEN                  'Has it changed?
            num.repeats% = num.repeats% + 1              'If not, inc. repeat
            IF num.repeats% = 63 THEN                    'If maxed, save data
               buff$ = buff$ + CHR$(255) + CHR$(last.byte%)
               num.repeats% = 0
            END IF
         ELSE                                            'If changed, save data
            GOSUB update.buff
         END IF
      NEXT j%
      IF num.repeats% > 0 THEN GOSUB update.buff
      FOR p% = 1 TO planes%
         PUT #f%, pcx.ptr&, buff$                  'Save the PCX buffer
         pcx.ptr& = pcx.ptr& + LEN(buff$)          'Update file pointer
      NEXT p%
      num.repeats% = 1                          'Re-initialize repeat counter
      last.byte% = byte.val%                    'Make sure last.byte is correct
      img.ptr& = img.ptr& + reclen%                      'Update bitmap pointer
   NEXT i%                                               'Get next line
   CLOSE f%, f2%

END

update.buff:
   IF num.repeats% = 1 THEN                     'If not repeated
      IF last.byte% > 191 THEN
         buff$ = buff$ + CHR$(193) + CHR$(last.byte%)   'ASCII chars. > 191
      ELSE
         buff$ = buff$ + CHR$(last.byte%)               'ASCII chars. <= 191
      END IF
   ELSEIF num.repeats% > 1 THEN                 'Char. is repeated
      buff$ = buff$ + CHR$(192 + num.repeats%) + CHR$(last.byte%)    'Encode it
   END IF
   last.byte% = byte.val%
   num.repeats% = 1
RETURN

END SUB

