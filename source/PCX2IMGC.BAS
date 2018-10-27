'           Program to convert PCX file to bitmap image
'                 with assignable color conversion

DECLARE SUB PCX2IMGC (pcxfile$, imgfile$, printcolor$)

DEFINT A-Z

'---Enter program variables
   COLOR 7, 1: CLS
   LOCATE 5, 10
   LINE INPUT "   Enter name of PCX file: "; pcxfile$
   LOCATE 7, 10
   LINE INPUT "   Enter name of bitmap file: "; imgfile$
   LOCATE 11, 1: n = 12
   PRINT SPACE$(n) + "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
   PRINT SPACE$(n) + "³              COLOR VALUES TO BE PRINTED              ³"
   PRINT SPACE$(n) + "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
   PRINT SPACE$(n) + "³                Color value                           ³"
   PRINT SPACE$(n) + "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
   PRINT SPACE$(n) + "³    Enter a color value to be printed.  Enter as many ³"
   PRINT SPACE$(n) + "³ as you like.  Color values must be in the range of   ³"
   PRINT SPACE$(n) + "³ 0 to 256.  Enter 'x' to begin processing.            ³"
   PRINT SPACE$(n) + "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"

   printcolor$ = ""
   DO
      LOCATE 14, 42
      PRINT SPACE$(10)
      LOCATE 14, 41
      INPUT a$
      IF UCASE$(a$) = "X" THEN EXIT DO
      colorvalue = VAL(a$)
      IF colorvalue >= 0 AND colorvalue <= 256 THEN
         printcolor$ = printcolor$ + CHR$(colorvalue)
      END IF
   LOOP

   CALL PCX2IMGC(pcxfile$, imgfile$, printcolor$)

SUB PCX2IMGC (pcxfile$, imgfile$, printcolor$)
'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
'  PCX2IMGC:   Converts a PCX file to a bitmapped image, and converts only
'              colors specified.

'  pcxfile$    -  name of PCX source file
'  imgfile$    -  name of bitmapped image file to create
'  printcolor$ -  color conversion string, ASCII value of each character in
'                 the string represent color values to be converted to black
'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
   REDIM bitval(7)            'Array holding powers of 2
      bitval(0) = 1           '  to avoid using the BASIC function which
      bitval(1) = 2           '  is very slow
      bitval(2) = 4           '  example: bitval(4) = 2 ^ 4
      bitval(3) = 8
      bitval(4) = 16
      bitval(5) = 32
      bitval(6) = 64
      bitval(7) = 128

'---PCX header structure
'     manuf        AS STRING * 1         1       Always =10 for Paintbrush
'     hard         AS STRING * 1         2       Version information
'     encod        AS STRING * 1         3       Run-length encoding (=1)
'     bitpx        AS STRING * 1         4       Bits per pixel
'     x1           AS INTEGER            5       Picture dimensions (incl)
'     y1           AS INTEGER            7
'     x2           AS INTEGER            9
'     y2           AS INTEGER           11
'     hres         AS INTEGER           13       Display horiz resolution
'     vres         AS INTEGER           15       Display vert  resolution
'     clrma        AS STRING * 48       17       Palette
'     vmode        AS STRING * 1        65       (ignored)
'     nplanes      AS STRING * 1        66       Number of planes (ver 2.5=0)
'     bplin        AS INTEGER           67       Bytes per line
'     xtra         AS STRING * 60       69       Extra space (filler)

'---Get PCX header
   pcxheader$ = SPACE$(68)             'Reserve PCX header space
   OPEN "b", 1, pcxfile$
   GET #1, 1, pcxheader$               'Get the PCX header
   CLOSE

'---Extract PCX header variables
   num.bits = ASC(MID$(pcxheader$, 4, 1))
   x1 = CVI(MID$(pcxheader$, 5, 2))
   y1 = CVI(MID$(pcxheader$, 7, 2))
   x2 = CVI(MID$(pcxheader$, 9, 2))
   y2 = CVI(MID$(pcxheader$, 11, 2))
   num.planes = ASC(MID$(pcxheader$, 66, 1))
   bytes.per.line = CVI(MID$(pcxheader$, 67, 2))

   num.dots = x2 - x1 + 1                    'Actual width of picture in dots
   num.lines = y2 - y1 + 1                   'Actual height of picture in lines
   num.bytes = bytes.per.line / num.bits     'No. of bytes in each line of file
   img.bytes = (num.dots + 7) \ 8            'No. of bytes in each image line
   trim.bits = img.bytes * 8 - num.dots      'No. of unused bits in last byte
   trim.byte = 256 - 2 ^ trim.bits           'Computed mask byte
   num.colors = 2 ^ num.planes

'---Display Header Data
   CLS
   PRINT "           Converting: "; UCASE$(pcxfile$); " to "; UCASE$(imgfile$)
   PRINT
   PRINT USING "       Width of image: #### dots"; num.dots
   PRINT USING "      Height of image: #### lines"; num.lines
   PRINT USING "       Bytes per line: #### bytes"; num.bytes
   PRINT USING "        No. of planes: #### planes"; num.planes
   PRINT USING "    No. of bytes/line: #### bytes per line"; bytes.per.line
   PRINT USING "       Bits per pixel: ####"; num.bits

'---Set up color conversion array
   REDIM setcolor(num.colors)
   FOR i = 1 TO LEN(printcolor$)
      colorvalue = ASC(MID$(printcolor$, i, 1))
      IF colorvalue <= num.colors AND colorvalue >= 0 THEN setcolor(colorvalue) = -1
   NEXT i

'---Convert *.PCX to *.IMG
   byte.ptr& = 128
   byte$ = " "
   img.ptr& = 1
   LOCATE 10, 10: PRINT "Current Line:"

   OPEN "b", 1, pcxfile$
   OPEN "o", 2, imgfile$        'Delete bitmap file
   PRINT #2, "";                 'if it already exists
   CLOSE 2
   OPEN "b", 2, imgfile$

   buffer$ = STRING$(num.bytes, CHR$(0))     'Initialize bitmap buffer
   plane = 1                                 'Initialize
   line.num = 1                              '  counters
   REDIM plane$(num.planes)                  'Initialize video plane buffers
   DO                                        'Start retrieving values
      GOSUB getvals                          'Retrieve a compressed bitmap value
      IF num.planes = 1 THEN                 'If black & white, convert zero bits to black
         plane$(plane) = plane$(plane) + STRING$(num.repeats, CHR$(byte.val XOR 255))
      ELSE
         plane$(plane) = plane$(plane) + STRING$(num.repeats, CHR$(byte.val))
      END IF
      IF LEN(plane$(plane)) >= bytes.per.line THEN    'If plane has been retrieved
                     'If all planes have been retrieved, update image buffer
         IF plane = num.planes THEN GOSUB buffupdate ELSE plane = plane + 1
      END IF
      IF line.num > num.lines THEN EXIT DO         'Exit if done
   LOOP                                            'Get next set of values
   imgheader$ = MKI$(num.dots) + MKI$(num.lines)   'Create bitmap header
   PUT #2, img.ptr&, imgheader$                    '  & save it
   CLOSE
END

getvals:
      num.repeats = 1                     'Default repeat value
      byte.ptr& = byte.ptr& + 1           'Point to next byte
      GET #1, byte.ptr&, byte$
      byte.val = ASC(byte$)
      IF byte.val >= 192 THEN             'Are there repeating bytes?
         num.repeats = byte.val - 192     'If so, how many?
         byte.ptr& = byte.ptr& + 1        'Point to next byte
         GET #1, byte.ptr&, byte$         'Get the next byte
         byte.val = ASC(byte$)            'This is the value to be repeated
      END IF
RETURN

buffupdate:
   plane = 1
   IF num.planes = 1 THEN          'For only one plane, buffers are identical
      buffer$ = LEFT$(plane$(plane), img.bytes)
      GOSUB leftover               'Save the leftovers
   ELSE
      buffer$ = STRING$(img.bytes, 0)     'Initialize bitmap buffer
      FOR i = 1 TO img.bytes              'Check each byte
         byte = 0                         '  & each plane
         FOR bit = 0 TO 7                 '  & each bit
            colr = 0                      '  to compute the actual color
            FOR plane = 1 TO num.planes
               p = ASC(MID$(plane$(plane), i, 1)) AND bitval(bit)
               IF p THEN colr = colr OR bitval(plane - 1)   'Add the color values
            NEXT plane
            IF setcolor(colr) THEN byte = byte OR bitval(bit)   'Convert it?
         NEXT bit                                               'Check next bit
         MID$(buffer$, i, 1) = CHR$(byte)                       'Update buffer
      NEXT i                                                    'Next byte
      FOR plane = 1 TO num.planes                               'Save the
         GOSUB leftover                                         ' leftovers for
      NEXT plane                                                ' each plane
   END IF
   IF trim.bits THEN
      lastbyte = ASC(MID$(buffer$, img.bytes, 1))
      MID$(buffer$, img.bytes, 1) = CHR$(lastbyte AND trim.byte)
   END IF
   LOCATE 10, 23: PRINT USING "#####"; line.num;
   PUT #2, img.ptr&, buffer$           'Save the bitmap line
   line.num = line.num + 1             'Update line count
   img.ptr& = img.ptr& + img.bytes     'Point to next line in file
   plane = 1                           'Initialize to first plane for next round
            'Trim excess bits in last byte
RETURN

leftover:           'Save repeated bytes left over from previous line
   leftover = LEN(plane$(plane)) - bytes.per.line
   plane$(plane) = RIGHT$(plane$(plane), leftover)
RETURN

END SUB

