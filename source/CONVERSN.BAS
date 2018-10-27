'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             CONVERSN.BAS

'  This module contains routines to convert and trim bitmapped image files

'  (c) 1991, by R. F. Holcombe

'  Requires routines contained in LOWLEVEL.BAS and the supplied library
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'$INCLUDE: 'laserpak.dec'

   COLOR 7, 1: CLS
   LOCATE 10, 10
   LINE INPUT "Enter name of file to be trimmed: "; bitmapfile$
   LOCATE 10, 10
   LINE INPUT "Enter name of file to be created: "; trimfile$
   CALL FineTrimImage(bitmapfile$, trimfile$)

'                  Parameter List: CONVERSN.BAS
'-----------------------------------------------------------------------------
''  bitmapfile$ - file containing a bitmapped image
''  center% - center flag, will center text strings if True (i.e. center% = -1)
''  fontfile$ - name of file containing a bitmapped font
''  imagefile$ - name of file containing a bitmapped image
''  landfile$ - name of file containing a landscaped oriented bitmapped image
''  text$() - array containing text strings
''  textfile$ - name of ASCII text file to be created
''  trimfile$ - file name of the created, trimmed file

SUB FineTrimImage (bitmapfile$, trimfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FineTrimImage: Takes a file containing a bitmapped image and trims the
'                 excess white space all around, including all bits

'  bitmapfile$    - file containing a bitmapped image
'  trimfile$      - file name of the created, trimmed file

'  Externals:     AddBitmapHeader
'                 GetBitmapHeader
'                 InitialFile
'                 Slant
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get file configuration
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   IF bitmaplen% < 1 THEN EXIT SUB        'Exit if not valid bitmap file

'---Determine bulk excess white space
   f% = FREEFILE
   OPEN "r", f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$
   firstnonblank% = bitmaplines%
   lastnonblank% = 0
   leftbyte% = bitmaplen%
   rightbyte% = 0
   FOR i% = 1 TO bitmaplines%
      GET #f%, i%
      IF bitmap$ <> STRING$(bitmaplen%, CHR$(0)) THEN
         IF firstnonblank% > i% THEN firstnonblank% = i%  ' First non-blank line
         lastnonblank% = i%                     ' Last non-blank line so far
         b.left% = 0
         FOR j% = 1 TO bitmaplen%               ' Blank bytes to left
            IF ASC(MID$(bitmap$, j%, 1)) <> 0 THEN EXIT FOR
            b.left% = j%
         NEXT j%
         IF b.left% < leftbyte% THEN leftbyte% = b.left%
         b.right% = bitmaplen% + 1
         FOR j% = bitmaplen% TO 1 STEP -1       ' Blank bytes to right
            IF ASC(MID$(bitmap$, j%, 1)) <> 0 THEN EXIT FOR
            b.right% = j%
         NEXT j%
         IF b.right% > rightbyte% THEN rightbyte% = b.right%
      END IF
   NEXT i%
   trimlen% = rightbyte% - leftbyte% - 1
   trimlines% = lastnonblank% - firstnonblank% + 1

'---Compute fine trim dots
   lbyte% = 0
   lastrbit% = 7
   nextrbit% = 6
   FOR i% = firstnonblank% TO lastnonblank%
      GET #f%, i%
      lbyteval% = ASC(MID$(bitmap$, leftbyte% + 1, 1))
      IF lbyteval% > lbyte% THEN
         lbyte% = lbyteval%
      END IF
      rbyteval% = ASC(MID$(bitmap$, leftbyte% + trimlen%, 1))
      FOR j% = 0 TO nextrbit%
         IF rbyteval% AND 2 ^ j% THEN
            lastrbit% = j%
            nextrbit% = j% - 1
            EXIT FOR
         END IF
      NEXT j%
   NEXT i%
   FOR lefttrim% = 0 TO 7
      IF lbyte% AND 2 ^ (7 - lefttrim%) THEN EXIT FOR
   NEXT lefttrim%
   righttrim% = lastrbit%
   trimdots% = trimlen% * 8 - lefttrim% - righttrim%
   trimlen% = (trimdots% + 7) \ 8

'---Create trimmed file
   CALL InitialFile(trimfile$)
   f2% = FREEFILE
   OPEN "r", f2%, trimfile$, trimlen%
   FIELD #f2%, trimlen% AS trim$
   IF lefttrim% THEN shift% = 8 - lefttrim% ELSE shift% = 0
   FOR i% = firstnonblank% TO lastnonblank%
      GET #f%, i%
      t$ = MID$(bitmap$, leftbyte% + 1, trimlen%)
      IF shift% THEN
         t$ = t$ + CHR$(0)
         CALL Slant(shift%, t$)
         t$ = RIGHT$(t$, trimlen%)
      END IF
      LSET trim$ = t$
      PUT #f2%, i% - firstnonblank% + 1
   NEXT i%
   CLOSE f%, f2%
  
   CALL AddBitmapHeader(trimfile$, trimdots%, trimlines%)

END SUB

SUB Image2Text (bitmapfile$, textfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Image2Text: Takes a file containing a bitmapped image and creates an
'              ASCII text file simulating the dots

'  bitmapfile$ - file containing a bitmapped image
'  textfile$   - name of ASCII text file to be created

'  Externals:  GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get header of bitmap
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   IF bitmaplen% < 1 THEN EXIT SUB     'Exit if not valid bitmap file

'---Set up bitmap and text files
   f1% = FREEFILE
   OPEN "o", #f1%, textfile$
   f2% = FREEFILE
   OPEN "b", #f2%, bitmapfile$
   ptr& = 1
   b$ = STRING$(bitmaplen%, 0)

'---Process each bitmap line
   FOR i% = 1 TO bitmaplines%          'Get a line
      buff$ = ""
      GET #f2%, ptr&, b$
      FOR j% = 1 TO bitmaplen%         'For each byte in the line
         byte% = ASC(MID$(b$, j%, 1))
         FOR k% = 7 TO 0 STEP -1          'Check each bit
            IF byte% AND 2 ^ k% THEN c$ = "*" ELSE c$ = " "    'Is bit set?
            buff$ = buff$ + c$            'Update text file buffer
         NEXT k%
      NEXT j%
      buff$ = LEFT$(buff$, bitmapdots%)   'Ignore excess dots in last byte
      buff$ = RTRIM$(buff$)               'Trim off excess white space
      PRINT #f1%, buff$                   'Save it
      ptr& = ptr& + bitmaplen%
   NEXT i%

'---Tidy up and exit
   CLOSE #f1%, #f2%

END SUB

SUB MakeImage (fontfile$, imagefile$, text$(), center%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  MakeImage: Creates a bitmapped image file of a an array of text strings.

'  fontfile$  - name of file containing a bitmapped font
'  imagefile$ - name of file containing a bitmapped image
'  text$()    - array containing text strings
'  center%    - center flag, will center text strings if True (i.e. center% = -1)

'  Externals: CheckHeader
'             Slant
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   REDIM wid%(256), cbytes%(256), ptr&(256)

   CALL CheckHeader(fontfile$, baseline%, wid%(), cbytes%(), clines%, ptr&())

'---Set up
   REDIM buffer$(clines%)
   textlines% = UBOUND(text$)
   imageptr& = 1
   fi% = FREEFILE
   OPEN "o", fi%, imagefile$
   PRINT #fi%, "";
   CLOSE fi%

'---Calculate length of buffer string
   REDIM linewidth%(textlines%)
   numbytes% = 0
   FOR l% = 1 TO textlines%
      linewidth%(l%) = 0
      FOR i% = 1 TO LEN(text$(l%))
         ch% = ASC(MID$(text$(l%), i%, 1))
         linewidth%(l%) = linewidth%(l%) + wid%(ch%)
      NEXT i%
      bytes% = (linewidth%(l%) + 7) \ 8
      IF bytes% > numbytes% THEN numbytes% = bytes%
   NEXT l%

   FOR textline% = 1 TO textlines%
      GOSUB fill.buffer
      GOSUB save.buffer
   NEXT textline%
   GOSUB save.header

EXIT SUB

fill.buffer:
'---Initialize the buffer
   FOR i% = 1 TO clines%
      buffer$(i%) = STRING$(numbytes%, 0)
   NEXT i%

'---Open the font file
   ff% = FREEFILE
   OPEN "b", ff%, fontfile$
   IF center% THEN
      dotposn% = (numbytes% * 8 - linewidth%(textline%)) \ 2
      IF dotposn% < 1 THEN dotposn% = 1
   ELSE
      dotposn% = 1
   END IF

'---Process the string
   FOR i% = 1 TO LEN(text$(textline%))
      ch% = ASC(MID$(text$(textline%), i%, 1))
      IF ch% > 32 THEN
         c$ = SPACE$(cbytes%(ch%))
         GET #ff%, ptr&(ch%), c$
         leftoffset% = ASC(MID$(c$, 8, 1))
         IF leftoffset% > 127 THEN leftoffset% = leftoffset% - 256
         topoffset% = ASC(MID$(c$, 10, 1))
         cellbytes% = (ASC(MID$(c$, 12, 1)) + 7) \ 8
         deltax% = (ASC(MID$(c$, 15, 1)) * 256 + ASC(MID$(c$, 16, 1))) \ 4
         c$ = RIGHT$(c$, cbytes%(ch%) - 16)
         charlines% = LEN(c$) / cellbytes%
         blank% = baseline% - topoffset%
         dotposn% = dotposn% + leftoffset%
         byteposn% = (dotposn% + 7) \ 8
         moveit% = dotposn% - byteposn% * 8 + 7
         FOR j% = 1 TO charlines%
            d$ = MID$(c$, (j% - 1) * cellbytes% + 1, cellbytes%)
            IF moveit% THEN
               d$ = d$ + CHR$(fillchar%)
               bytes% = LEN(d$)
               CALL Slant(BYVAL moveit%, d$)
               e% = ASC(MID$(buffer$(j% + blank%), byteposn%, 1))
               MID$(buffer$(j% + blank%), byteposn%, 1) = CHR$(ASC(LEFT$(d$, 1)) OR e%)
               MID$(buffer$(j% + blank%), byteposn% + 1, cellbytes%) = RIGHT$(d$, cellbytes%)
            ELSE
               MID$(buffer$(j% + blank%), byteposn%, cellbytes%) = d$
            END IF
         NEXT j%
         dotposn% = dotposn% + deltax% - leftoffset%
      ELSE
         dotposn% = dotposn% + wid%(32)
      END IF
   NEXT i%
   CLOSE ff%
RETURN

save.buffer:
   fi% = FREEFILE
   OPEN "b", fi%, imagefile$
   FOR i% = 1 TO clines%
      PUT #fi%, imageptr&, buffer$(i%)
      imageptr& = imageptr& + numbytes%
   NEXT i%
   CLOSE fi%
RETURN

save.header:
   fi% = FREEFILE
   OPEN "b", fi%, imagefile$
   header$ = MKI$(numbytes% * 8) + MKI$(clines% * textlines%)
   PUT #fi%, imageptr&, header$
   CLOSE fi%
RETURN

END SUB

SUB Port2Land (bitmapfile$, landfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Port2Land:  Takes a bitmapped image file and rotates 90 degrees, from
'              portrait to landscape.

'  bitmapfile$ - file containing a bitmapped image
'  landfile$   - name of file containing a landscaped oriented bitmapped image

'  Externals:  AddBitmapHeader
'              GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Create an array containing powers of '2', to avoid '^' function
   REDIM bitval%(7)
      bitval%(0) = 1
      bitval%(1) = 2
      bitval%(2) = 4
      bitval%(3) = 8
      bitval%(4) = 16
      bitval%(5) = 32
      bitval%(6) = 64
      bitval%(7) = 128

'---Get bitmap header and compute dimensions of rotated image
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   landlines% = bitmapdots%
   landdots% = bitmaplines%
   landlen% = (landdots% + 7) \ 8

'---Initialize buffer
   REDIM buffer$(landlines%)
   FOR i% = 1 TO landlines%
      buffer$(i%) = STRING$(landlen%, 0)
   NEXT i%

'---Rotate the image and save in buffer
   f% = FREEFILE
   OPEN "r", f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$
   FOR l% = 1 TO bitmaplines%
      GET #f%, l%
      landline% = landlines%
      landbyte% = (l% + 7) \ 8
      landbit% = landbyte% * 8 - l%
      FOR byte% = 1 TO bitmaplen%
         curbyte% = ASC(MID$(bitmap$, byte%, 1))
         FOR bit% = 1 TO 8
            IF curbyte% AND bitval%(8 - bit%) THEN
               curland% = ASC(MID$(buffer$(landline%), landbyte%, 1))
               MID$(buffer$(landline%), landbyte%, 1) = CHR$(curland% OR bitval%(landbit%))
            END IF
            landline% = landline% - 1
            IF landline% < 1 THEN EXIT FOR
         NEXT bit%
      NEXT byte%
   NEXT l%
   CLOSE f%

'---Save the buffer to the landscape file
   f% = FREEFILE
   OPEN "o", f%, landfile$
   PRINT #f%, "";
   CLOSE f%
   OPEN "r", f%, landfile$, landlen%
   FIELD #f%, landlen% AS land$
   FOR i% = 1 TO landlines%
      LSET land$ = buffer$(i%)
      PUT #f%, i%
   NEXT i%
   CLOSE f%
 
   CALL AddBitmapHeader(landfile$, landdots%, landlines%)

END SUB

SUB Text2Image (textfile$, bitmapfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  Text2Image: Takes an ASCII file and creates a file containing a
'              bitmapped image

'  textfile$   - name of ASCII text file to be created
'  bitmapfile$ - file containing a bitmapped image

'  Externals:  AddBitmapHeader
'              InitialFile
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Compute bitmap dimensions
   f1% = FREEFILE
   OPEN "i", #f1%, textfile$
   maxlen% = 0                                           'initialize width counter
   linecount% = 0                                        'initialize line counter
   DO
      IF EOF(f1%) THEN EXIT DO               'exit if last line
      LINE INPUT #f1%, text$                 'get next line
      linecount% = linecount% + 1            'update line count
      IF LEN(text$) > maxlen% THEN maxlen% = LEN(text$)  'is it the longest line
   LOOP
   CLOSE #f1%

'---Set up text and bitmap files
   bufflen% = (maxlen% + 7) \ 8              'no. of bytes in buffer
   buffptr% = 1                              'pointer to first line (record)
   f1% = FREEFILE
   OPEN "i", #f1%, textfile$
   CALL InitialFile(bitmapfile$)
   f2% = FREEFILE
   OPEN "r", #f2%, bitmapfile$, bufflen%
   FIELD #f2%, bufflen% AS b$

   DO
      IF EOF(f1%) THEN EXIT DO               'are we done yet?
      LINE INPUT #f1%, text$                 'get line of text
      buff$ = STRING$(bufflen%, 0)           'initialize bitmap image
      FOR i% = 1 TO LEN(text$)               'check each character
         IF MID$(text$, i%, 1) <> " " THEN   'if not a space, bit is set on
            byte% = (i% + 7) \ 8             'compute byte position in bitmap
            bit% = byte% * 8 - i%            'compute bit position
            MID$(buff$, byte%, 1) = CHR$(ASC(MID$(buff$, byte%, 1)) OR 2 ^ bit%)
         END IF                              'update bitmap image
      NEXT i%
      LSET b$ = buff$                        'copy bitmap image to file buffer
      PUT #f2%, buffptr%                     'save it
      buffptr% = buffptr% + 1                'point to next line in file
   LOOP
   CLOSE #f1%, #f2%
  
   CALL AddBitmapHeader(bitmapfile$, maxlen%, linecount%)

END SUB

SUB TrimImage (bitmapfile$, trimfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  TrimImage:  Takes a bitmapped image file and trims excess white space on
'              top and bottom, and on left and right, but only whole bytes

'  bitmapfile$ - file containing a bitmapped image
'  trimfile$   - file name of the created, trimmed file

'  Externals:  AddBitmapHeader
'              GetBitmapHeader
'              InitialFile
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'---Get file configuration
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   IF bitmaplen% < 1 THEN EXIT SUB        'Exit if not valid bitmap file

'---Determine excess white space
   f% = FREEFILE
   OPEN "r", f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$
   firstnonblank% = bitmaplines%
   lastnonblank% = 0
   leftbyte% = bitmaplen%
   rightbyte% = 0
   FOR i% = 1 TO bitmaplines%
      GET #f%, i%
      IF bitmap$ <> STRING$(bitmaplen%, CHR$(0)) THEN
         IF firstnonblank% > i% THEN firstnonblank% = i%  ' First non-blank line
         lastnonblank% = i%                     ' Last non-blank line so far
         b.left% = 0
         FOR j% = 1 TO bitmaplen%               ' Blank bytes to left
            IF ASC(MID$(bitmap$, j%, 1)) <> 0 THEN EXIT FOR
            b.left% = j%
         NEXT j%
         IF b.left% < leftbyte% THEN leftbyte% = b.left%
         b.right% = bitmaplen% + 1
         FOR j% = bitmaplen% TO 1 STEP -1       ' Blank bytes to right
            IF ASC(MID$(bitmap$, j%, 1)) <> 0 THEN EXIT FOR
            b.right% = j%
         NEXT j%
         IF b.right% > rightbyte% THEN rightbyte% = b.right%
      END IF
   NEXT i%
   trimlen% = rightbyte% - leftbyte% - 1
   trimdots% = trimlen% * 8
   trimlines% = lastnonblank% - firstnonblank% + 1

'---Create trimmed file
   CALL InitialFile(trimfile$)
   f2% = FREEFILE
   OPEN "o", f2%, trimfile$
   PRINT #f2%, "";
   CLOSE f2%
   OPEN "r", f2%, trimfile$, trimlen%
   FIELD #f2%, trimlen% AS trim$
   FOR i% = firstnonblank% TO lastnonblank%
      GET #f%, i%
      LSET trim$ = MID$(bitmap$, leftbyte% + 1, trimlen%)
      PUT #f2%, i% - firstnonblank% + 1
   NEXT i%
   CLOSE f%, f2%
 
   CALL AddBitmapHeader(trimfile$, trimdots%, trimlines%)

END SUB

