'          Program to convert a PCX file to a bitmap image

'                 Must be a 2 or 8 color PCX file

DECLARE SUB PCX2IMG (pcxfile$, imgfile$, anycolor%)

DEFINT A-W
         
'---Enter program variables
   COLOR 7, 1: CLS
   LOCATE 5, 10
   LINE INPUT "Enter name of PCX file: "; pcxfile$
   LOCATE 7, 10
   LINE INPUT "Enter name of bitmap file: "; imgfile$
   LOCATE 9, 10
   PRINT "Do you want all colors considered black? (y or n)"
   DO
      a$ = INKEY$
      IF a$ <> "" THEN EXIT DO
   LOOP
   IF UCASE$(a$) = "Y" THEN anycolor% = -1 ELSE anycolor% = 0

   CALL PCX2IMG(pcxfile$, imgfile$, anycolor%)

DEFINT X-Z
SUB PCX2IMG (pcxfile$, imgfile$, anycolor%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PCX2IMG:  Program to convert a PCX file to a bitmap image

'  pcxfile$  -  name of PCX source file
'  imgfile$  -  name of bitmapped file to create
'  anycolor% -  all colors converted to black if True
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

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

   pcxheader$ = SPACE$(68)       'Reserve space for header string
   OPEN "b", 1, pcxfile$
   GET #1, 1, pcxheader$         'Get the header string
   CLOSE

'---Extra PCX variables from header string
   num.bits = ASC(MID$(pcxheader$, 4, 1))
   x1 = CVI(MID$(pcxheader$, 5, 2))
   y1 = CVI(MID$(pcxheader$, 7, 2))
   x2 = CVI(MID$(pcxheader$, 9, 2))
   y2 = CVI(MID$(pcxheader$, 11, 2))
   num.planes = ASC(MID$(pcxheader$, 66, 1))
   bytes.per.line = CVI(MID$(pcxheader$, 67, 2))

   num.dots = x2 - x1 + 1                     'Actual picture width, dots
   num.lines = y2 - y1 + 1                    'Actual picture raster lines
   num.bytes = bytes.per.line / num.bits      'Picture width in bytes
   img.bytes = (num.dots + 7) \ 8             'Width of bitmap, bytes
   trim.bits = img.bytes * 8 - num.dots       'Excess bits to trim from PCX data
   trim.byte = 256 - 2 ^ trim.bits            'Value to 'AND' with last byte

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

'---Convert *.PCX to *.IMG
   byte.ptr& = 128                           'Point to 1st byte after header
   byte$ = " "
   img.ptr& = 1                              'Start of bitmap
   LOCATE 10, 10: PRINT "Current Line:"

   OPEN "b", 1, pcxfile$
   OPEN "o", 2, imgfile$           'Delete bitmap file if it already exists
   PRINT #2, "";
   CLOSE 2
   OPEN "b", 2, imgfile$

   buffer$ = STRING$(num.bytes, CHR$(0))     'Create bitmap buffer
   plane = 1                                 'Initialize
   line.num = 1                              '  counters
   REDIM plane$(num.planes)
   DO                                        'Loop until all bytes are read
      GOSUB get.vals                         'Get a value and no. of repeats
                                             'Update current plane
      plane$(plane) = plane$(plane) + STRING$(num.repeats, CHR$(byte.val XOR 255))
      IF LEN(plane$(plane)) >= bytes.per.line THEN   'Finished current plane?
                                                     'If last plane, then
                                                     '   update buffer
         IF plane = num.planes THEN GOSUB buff.update ELSE plane = plane + 1
      END IF
      IF line.num > num.lines THEN EXIT DO   'Are we finished?
   LOOP

'---Save the bitmap header - four bytes at end of bitmap section of file
   imgheader$ = MKI$(num.dots) + MKI$(num.lines)
   PUT #2, img.ptr&, imgheader$
   CLOSE
END

get.vals:
      num.repeats = 1
      byte.ptr& = byte.ptr& + 1
      GET #1, byte.ptr&, byte$
      byte.val = ASC(byte$)
      IF byte.val >= 192 THEN       'No. of repeating bytes
         num.repeats = byte.val - 192
         byte.ptr& = byte.ptr& + 1
         GET #1, byte.ptr&, byte$
         byte.val = ASC(byte$)
      END IF
RETURN

buff.update:
   plane = 1
   IF num.planes = 1 THEN
      buffer$ = LEFT$(plane$(plane), img.bytes)
      GOSUB left.over
   ELSE
      IF anycolor% THEN initbyte = 0 ELSE initbyte = 255
      buffer$ = STRING$(img.bytes, initbyte)
      FOR plane = 1 TO num.planes
         FOR i = 1 TO img.bytes
            p = ASC(MID$(plane$(plane), i, 1))
            b = ASC(MID$(buffer$, i, 1))
            IF anycolor% THEN
               MID$(buffer$, i, 1) = CHR$(b OR p)
            ELSE
               MID$(buffer$, i, 1) = CHR$(b AND p)
            END IF
         NEXT i
         GOSUB left.over
      NEXT plane
   END IF
   IF trim.bits THEN
      lastbyte = ASC(MID$(buffer$, img.bytes, 1))
      MID$(buffer$, img.bytes, 1) = CHR$(lastbyte AND trim.byte)
   END IF
   LOCATE 10, 23: PRINT USING "#####"; line.num;
   PUT #2, img.ptr&, buffer$
   line.num = line.num + 1
   img.ptr& = img.ptr& + img.bytes
   plane = 1
RETURN

left.over:
   leftover = LEN(plane$(plane)) - bytes.per.line
   plane$(plane) = RIGHT$(plane$(plane), leftover)
RETURN

END SUB

