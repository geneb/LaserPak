'$INCLUDE: 'laserpak.dec'

'                  Parameter List: IMAGES.BAS
'-----------------------------------------------------------------------------
''  anycolor% - any color flag; if True, any color will be considered black
''  bitmapfile$ - name of file containing bitmapped image
''  buffersize% - size of buffer string
''  cropbot% - number of lines of image cropped from the bottom
''  cropfile$ - name of cropped bitmap file
''  cropleft% - number of dots of image cropped from the left side
''  cropright% - number of dots of image cropped from the right side
''  croptop% - number of lines of image cropped from the top
''  file$ - name of file to copy to printer
''  flipfile$ - name of created 'flipped' file
''  image$ - bitmap string to be shifted
''  maskfile$ - file containing the mask
''  mergefile$ - file containing the image to merge
''  padbot% - number of lines at bottom to be added
''  padfile$ - name of padded file to be created
''  padleft% - number of dots to be added to left
''  padright% - number of dots to be added to right
''  padtop% - number of lines to be added to top
''  pcxfile$ - name of PCX file
''  printcolor$ - color conversion string, ASCII values of each character in
''                the string represent color values to be printed
''  LPrinter% - printer number (i.e. LPT1 = 1)
''  reverse% - video print mode; if True, printing will be reverse video
''  screenfile$ - name of file containing the image of the screen
''  shift% - no. of dots to shift right (negative value shifts left)
''  skew! - amount of skew, in percent of height; "+" = skewed to right
''  skewfile$ - name of file to create that will contain skewed image
''  trim% - trim flag; if True, portion of image outside the mask is trimmed
''  xmove% - number of dots to move; '+' moves right, '-' moves left
''  xoffset% - horizontal starting point of process, in dots from the left
''  yoffset% - vertical starting point of process, in lines from the top

SUB CropImage (bitmapfile$, cropfile$, croptop%, cropbot%, cropleft%, cropright%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  CropImage:  Crops a bitmapped image.

'  bitmapfile$ - name of file containing bitmapped image
'  cropfile$   - name of cropped bitmap file
'  croptop%    - number of lines of image cropped from the top
'  cropbot%    - number of lines of image cropped from the bottom
'  cropleft%   - number of dots of image cropped from the left side
'  cropright%  - number of dots of image cropped from the right side

'  Calls:      MergeImage

'  Externals:  AddBitmapHeader
'              GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Retrieve bitmap header information & compute cropped file dimensions
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   cropdots% = bitmapdots% - cropleft% - cropright%
   croplen% = (cropdots% + 7) \ 8
   croplines% = bitmaplines% - croptop% - cropbot%

'---Initialize the cropped file
   f% = FREEFILE
   OPEN "o", #f%, cropfile$
   PRINT #f%, ""
   CLOSE f%
   OPEN "r", #f%, cropfile$, croplen%
   FIELD #f%, croplen% AS crop$
   LSET crop$ = STRING$(croplen%, 0)      'Save a blank image
   FOR i% = 1 TO croplines%
      PUT #f%, i%
   NEXT i%
   CLOSE #f%
                                           'Add the header
   CALL AddBitmapHeader(cropfile$, cropdots%, croplines%)
                                           'Merge with the source file
   CALL MergeImage(cropfile$, bitmapfile$, -croptop%, -cropleft%)

END SUB

SUB FileToPrinter (file$, LPrinter%, buffersize%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FileToPrinter: Copies a DOS file to the printer.

'  file$          - name of file to copy to printer
'  LPrinter%      - printer number (i.e. LPT1 = 1)
'  buffersize%    - size of buffer string

'  Externals:     FontCopy
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   CALL FontCopy(file$, LPrinter%, SPACE$(buffersize%), 0)

END SUB

SUB FlipHoriz (bitmapfile$, flipfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FlipHoriz:  Flips a bitmapped image horizontally (i.e. left and right).

'  bitmapfile$ - name of file containing bitmapped image
'  flipfile$   - name of created 'flipped' file

'  Externals:  AddBitmapHeader
'              GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Initialize new byte array containing 'flipped' byte values
   REDIM newbyte%(255)
   FOR i% = 0 TO 255
      newbyte%(i%) = 0
      bit% = 1
      flipbit% = 128
      FOR j% = 0 TO 7
         IF i% AND bit% THEN newbyte%(i%) = newbyte%(i%) + flipbit%
         bit% = bit% * 2
         flipbit% = flipbit% \ 2
      NEXT j%
   NEXT i%

'---Get file headers and open files
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   flipdots% = bitmapdots%
   fliplines% = bitmaplines%
   fliplen% = bitmaplen%
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$

   f2% = FREEFILE
   OPEN "r", #f2%, flipfile$, fliplen%
   FIELD #f2%, fliplen% AS flip$

'---Flip it
   FOR i% = 1 TO bitmaplines%
      GET #f%, i%
      b$ = ""
      FOR j% = bitmaplen% TO 1 STEP -1
         byte% = ASC(MID$(bitmap$, j%, 1))
         b$ = b$ + CHR$(newbyte%(byte%))
      NEXT j%
      LSET flip$ = b$
      PUT #f2%, i%
   NEXT i%
   CLOSE #f%, #f2%

   CALL AddBitmapHeader(flipfile$, flipdots%, fliplines%)

END SUB

SUB FlipVert (bitmapfile$, flipfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  FlipVert:   Flips a bitmapped image file vertically (i.e. up and down).

'  bitmapfile$ - name of file containing bitmapped image
'  flipfile$   - name of created 'flipped' file

'  Externals:  AddBitmapHeader
'              GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get file headers and open files
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   flipdots% = bitmapdots%
   fliplines% = bitmaplines%
   fliplen% = bitmaplen%
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$

   f2% = FREEFILE
   OPEN "r", #f2%, flipfile$, fliplen%
   FIELD #f2%, fliplen% AS flip$

'---Flip it
   FOR i% = bitmaplines% TO 1 STEP -1
      GET #f%, i%
      LSET flip$ = bitmap$
      PUT #f2%, bitmaplines% - i% + 1
   NEXT i%
   CLOSE #f%, #f2%

   CALL AddBitmapHeader(flipfile$, flipdots%, fliplines%)

END SUB

SUB KnockOut (bitmapfile$, mergefile$, yoffset%, xoffset%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  KnockOut:   Takes a bitmapped image and converts black to white where
'              there is black in the mask.

'  bitmapfile$ - name of file containing bitmapped image
'  mergefile$  - file containing the image to merge
'  yoffset%    - vertical starting point of process, in lines from the top
'  xoffset%    - horizontal starting point of process, in dots from the left

'  Calls:      ShiftImage

'  Externals:  GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get file headers and open files
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$

   CALL GetBitmapHeader(mergefile$, mergedots%, mergelines%, mergelen%)
   f2% = FREEFILE
   OPEN "r", #f2%, mergefile$, mergelen%
      FIELD #f2%, mergelen% AS merge$

'---Compute horizontal offset
   IF xoffset% > 0 THEN
      xbytes% = (xoffset% + 8) \ 8 - 1
      xshift% = xoffset% - xbytes% * 8
   ELSEIF xoffset% < 0 THEN
      xbytes% = (ABS(xoffset%) + 7) \ 8 - 1
      xshift% = -(ABS(xoffset%) - xbytes% * 8)
   ELSEIF xoffset% = 0 THEN
      xbytes% = 0
      xshift% = 0
   END IF

'---Merge each line in merge file with each line in the bitmap file
   FOR i% = 1 TO bitmaplines%
      mergeptr% = i% - yoffset%
      IF mergeptr% >= 1 AND mergeptr% <= mergelines% THEN
         GET #f%, i%
         b$ = bitmap$
         GET #f2%, mergeptr%
         m$ = merge$
         IF xshift% THEN CALL ShiftImage(m$, xshift%)
         FOR j% = xbytes% + 1 TO bitmaplen%                 'Merge the files
            IF j% - xbytes% <= mergelen% AND j% > 0 THEN
               bitmap% = ASC(MID$(b$, j%, 1))
               merge% = ASC(MID$(m$, j% - xbytes%, 1)) XOR 255
               MID$(b$, j%, 1) = CHR$((bitmap% AND merge%))
            END IF
         NEXT j%
         LSET bitmap$ = b$
         PUT #f%, i%
      END IF
   NEXT i%
   CLOSE f%, f2%
END SUB

SUB MaskImage (bitmapfile$, maskfile$, yoffset%, xoffset%, trim%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  MaskImage:  Takes a bitmapped file and converts dots to white where they
'              are white in the mask.

'  bitmapfile$ - name of file containing bitmapped image
'  maskfile$   - file containing the mask
'  yoffset%    - vertical starting point of process, in lines from the top
'  xoffset%    - horizontal starting point of process, in dots from the left
'  trim%       - trim flag; if True, portion of image outside the mask is trimmed

'  Externals:  GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get file headers and open files
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$

   CALL GetBitmapHeader(maskfile$, maskdots%, masklines%, masklen%)
   f2% = FREEFILE
   OPEN "r", #f2%, maskfile$, masklen%
   FIELD #f2%, masklen% AS mask$

'---Compute horizontal offset
   xbytes% = (xoffset% + 8) \ 8 - 1
   xbits% = 8 - (xoffset% - xbytes% * 8)

'---Mask each line in mask file with each line in the bitmap file
   IF trim% THEN
      LSET bitmap$ = STRING$(bitmaplen%, 0)
      FOR i% = 1 TO yoffset%
         PUT #f%, i%
      NEXT i%
   END IF
   FOR i% = yoffset% + 1 TO bitmaplines%
      IF i% <= masklines% THEN
         GET #f%, i%
         b$ = bitmap$
         GET #f2%, i% - yoffset%
         m$ = mask$
         IF xbits% THEN
            m$ = CHR$(0) + m$
            FOR j% = 1 TO masklen%
               lbyte% = ASC(MID$(m$, j%, 1))
               lbyte% = (lbyte% * 2 ^ xbits%) AND 255
               IF j% < masklen% THEN rbyte% = ASC(MID$(m$, j% + 1, 1)) ELSE rbyte% = 0
               rbyte% = rbyte% \ 2 ^ (8 - xbits%)
               MID$(m$, j%, 1) = CHR$(lbyte% OR rbyte%)
            NEXT j%
            m$ = LEFT$(m$, masklen%)
            byte% = ASC(MID$(m$, 1, 1))
            MID$(m$, 1, 1) = CHR$(byte% OR (256 - 2 ^ xbits%))
         END IF
         FOR j% = xbytes% + 1 TO bitmaplen%
            IF j% <= masklen% THEN
               bitmap% = ASC(MID$(b$, j%, 1))
               mask% = ASC(MID$(m$, j% - xbytes%, 1))
               MID$(b$, j%, 1) = CHR$((bitmap% AND mask%))
            END IF
         NEXT j%
      END IF
      IF trim% THEN
         MID$(b$, 1) = STRING$(xbytes%, 0)
         byte% = ASC(MID$(b$, xbytes% + 1, 1))
         MID$(b$, xbytes% + 1, 1) = CHR$(byte% AND (2 ^ xbits% - 1))
      END IF
      LSET bitmap$ = b$
      PUT #f%, i%
   NEXT i%
   CLOSE f%, f2%

END SUB

SUB MergeExclusive (bitmapfile$, mergefile$, yoffset%, xoffset%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  MergeExclusive: Merges two images exclusively.

'  bitmapfile$     - name of file containing bitmapped image
'  mergefile$      - file containing the image to merge
'  yoffset%        - vertical starting point of process, in lines from the top
'  xoffset%        - horizontal starting point of process, in dots from the left

'  Calls:          ShiftImage

'  Externals:      GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get file headers and open files
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$

   CALL GetBitmapHeader(mergefile$, mergedots%, mergelines%, mergelen%)
   f2% = FREEFILE
   OPEN "r", #f2%, mergefile$, mergelen%
      FIELD #f2%, mergelen% AS merge$

'---Compute horizontal offset
   IF xoffset% > 0 THEN
      xbytes% = (xoffset% + 8) \ 8 - 1
      xshift% = xoffset% - xbytes% * 8
   ELSEIF xoffset% < 0 THEN
      xbytes% = (ABS(xoffset%) + 7) \ 8 - 1
      xshift% = -(ABS(xoffset%) - xbytes% * 8)
   ELSEIF xoffset% = 0 THEN
      xbytes% = 0
      xshift% = 0
   END IF

'---Merge each line in merge file with each line in the bitmap file
   FOR i% = 1 TO bitmaplines%
      mergeptr% = i% - yoffset%
      IF mergeptr% >= 1 AND mergeptr% <= mergelines% THEN
         GET #f%, i%
         b$ = bitmap$
         GET #f2%, mergeptr%
         m$ = merge$
         IF xshift% THEN CALL ShiftImage(m$, xshift%)
         FOR j% = xbytes% + 1 TO bitmaplen%                 'Merge the files
            IF j% - xbytes% <= mergelen% AND j% > 0 THEN
               bitmap% = ASC(MID$(b$, j%, 1))
               merge% = ASC(MID$(m$, j% - xbytes%, 1))
               MID$(b$, j%, 1) = CHR$((bitmap% XOR merge%))
            END IF
         NEXT j%
         LSET bitmap$ = b$
         PUT #f%, i%
      END IF
   NEXT i%
   CLOSE f%, f2%
END SUB

SUB MergeImage (bitmapfile$, mergefile$, yoffset%, xoffset%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  MergeImage: Takes a bitmapped file and merges another image.  The resultant
'              image is black where either of the images was black.

'  bitmapfile$ - name of file containing bitmapped image
'  mergefile$  - file containing the image to merge
'  yoffset%    - vertical starting point of process, in lines from the top
'  xoffset%    - horizontal starting point of process, in dots from the left

'  Calls:      ShiftImage

'  Externals:  GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get file headers and open files
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$

   CALL GetBitmapHeader(mergefile$, mergedots%, mergelines%, mergelen%)
   f2% = FREEFILE
   OPEN "r", #f2%, mergefile$, mergelen%
      FIELD #f2%, mergelen% AS merge$

'---Compute horizontal offset
   IF xoffset% > 0 THEN
      xbytes% = (xoffset% + 8) \ 8 - 1
      xshift% = xoffset% - xbytes% * 8
   ELSEIF xoffset% < 0 THEN
      xbytes% = (ABS(xoffset%) + 7) \ 8 - 1
      xshift% = -(ABS(xoffset%) - xbytes% * 8)
   ELSEIF xoffset% = 0 THEN
      xbytes% = 0
      xshift% = 0
   END IF

'---Compute excess bits to trim, if necessary
   trimdots% = bitmaplen% * 8 - bitmapdots%
   trim% = 256 - 2 ^ trimdots%

'---Merge each line in merge file with each line in the bitmap file
   FOR i% = 1 TO bitmaplines%
      mergeptr% = i% - yoffset%
      IF mergeptr% >= 1 AND mergeptr% <= mergelines% THEN
         GET #f%, i%
         b$ = bitmap$
         GET #f2%, mergeptr%
         m$ = merge$
         IF xshift% THEN CALL ShiftImage(m$, xshift%)
         FOR j% = xbytes% + 1 TO bitmaplen%                 'Merge the files
            IF j% - xbytes% <= mergelen% AND j% > 0 THEN
               bitmap% = ASC(MID$(b$, j%, 1))
               merge% = ASC(MID$(m$, j% - xbytes%, 1))
               MID$(b$, j%, 1) = CHR$((bitmap% OR merge%))
            END IF
         NEXT j%
         MID$(b$, bitmaplen%, 1) = CHR$(ASC(MID$(b$, bitmaplen%, 1)) AND trim%)
         LSET bitmap$ = b$
         PUT #f%, i%
      END IF
   NEXT i%
   CLOSE f%, f2%

END SUB

SUB MoveImage (bitmapfile$, xmove%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  MoveImage:  Moves an image from 1 to 7 dots to right or left.

'  bitmapfile$ - name of file containing bitmapped image
'  xmove%      - number of dots to move; '+' moves right, '-' moves left

'  Calls:      ShiftImage

'  Externals:  GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get file headers and open files
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$

'---Move each line in move file with each line in the bitmap file
   FOR i% = 1 TO bitmaplines%
      GET #f%, i%
      b$ = bitmap$
      CALL ShiftImage(b$, xmove%)
      LSET bitmap$ = b$
      PUT #f%, i%
   NEXT i%
   CLOSE f%

END SUB

SUB PadImage (bitmapfile$, padfile$, padtop%, padbot%, padleft%, padright%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PadImage:   Pads a bitmapped image with white space.

'  bitmapfile$ - name of file containing bitmapped image
'  padfile$    - name of padded file to be created
'  padtop%     - number of lines to be added to top
'  padbot%     - number of lines at bottom to be added
'  padleft%    - number of dots to be added to left
'  padright%   - number of dots to be added to right

'  Calls:      MergeImage

'  Externals:  AddBitmapHeader
'              GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Retrieve bitmap header information & compute padded file dimensions
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   paddots% = bitmapdots% + padleft% + padright%
   padlen% = (paddots% + 7) \ 8
   padlines% = bitmaplines% + padtop% + padbot%

'---Initialize the padded file
   f% = FREEFILE
   OPEN "r", #f%, padfile$, padlen%
   FIELD #f%, padlen% AS pad$
   LSET pad$ = STRING$(padlen%, 0)     'Fill it with white space for now
   FOR i% = 1 TO padlines%
      PUT #f%, i%
   NEXT i%
   CLOSE #f%
                                       'Add the header
   CALL AddBitmapHeader(padfile$, paddots%, padlines%)
                                       'Now, add the dots
   CALL MergeImage(padfile$, bitmapfile$, padtop%, padleft%)

END SUB

SUB PrintImage (bitmapfile$, reverse%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PrintImage: Prints a bitmapped file at the current cursor location.

'  bitmapfile$ - name of file containing bitmapped image
'  reverse%    - video print mode; if True, printing will be reverse video

'  Externals:  DoPrint&
'              GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get header information
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   IF bitmaplen% = 0 THEN EXIT SUB              'Not a valid bitmap

'---Construct front end of raster graphics code
   pc$ = CHR$(27) + "*b" + LTRIM$(STR$(bitmaplen%)) + "W"

'---Do we need to trim any excess dots
      exbits% = bitmaplen% * 8 - bitmapdots%    'Extra bits in last byte
      trim% = 256 - 2 ^ exbits%                 'Bits to trim off last byte

'---Set up access to image file
   f% = FREEFILE
   OPEN "r", f%, bitmapfile$, bitmaplen%           ' Open image file
   FIELD #f%, bitmaplen% AS aa$

'---Send it out
   pstring$ = CHR$(27) + "*r1A"              ' Start raster graphics
   GOSUB iprint
   FOR i% = 1 TO bitmaplines%                ' Get lines of data
      GET #f%, i%: a$ = aa$                  ' Get record
      IF reverse% THEN
         FOR j% = 1 TO LEN(a$)
            MID$(a$, j%, 1) = CHR$(ASC(MID$(a$, j%, 1)) XOR 255)
         NEXT j%
      END IF                                 'Trim, if necessary
      IF exbits% THEN MID$(a$, LEN(a$), 1) = CHR$(ASC(MID$(a$, LEN(a$), 1)) AND trim%)
      pstring$ = pc$ + a$                    ' Print
      GOSUB iprint
   NEXT i%
   CLOSE f%

   pstring$ = CHR$(27) + "*rB"               ' End raster graphics
   GOSUB iprint

EXIT SUB

iprint:
   bytes& = DoPrint&(pstring$)
RETURN

END SUB

SUB PrintPCX (pcxfile$, anycolor%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PrintPCX:  Prints a PCX file.

'  pcxfile$   - name of PCX file
'  anycolor%  - any color flag; if True, any color will be considered black

'  Externals: DoPrint&
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Set up for header
   pcxheader$ = SPACE$(68)       'Reserve space for header string
   f% = FREEFILE
   OPEN "b", f%, pcxfile$
   IF LOF(f%) < 128 THEN         'Not a valid PCX file header
      CLOSE f%
      EXIT SUB
   END IF
   GET #f%, 1, pcxheader$         'Get the header string

'---Extra PCX variables from header string
   num.bits% = ASC(MID$(pcxheader$, 4, 1))
   x1% = CVI(MID$(pcxheader$, 5, 2))
   y1% = CVI(MID$(pcxheader$, 7, 2))
   x2% = CVI(MID$(pcxheader$, 9, 2))
   y2% = CVI(MID$(pcxheader$, 11, 2))
   num.planes% = ASC(MID$(pcxheader$, 66, 1))
   bytes.per.line% = CVI(MID$(pcxheader$, 67, 2))

   num.dots% = x2% - x1% + 1                     'Actual picture width, dots
   num.lines% = y2% - y1% + 1                    'Actual picture raster lines
   num.bytes% = bytes.per.line% / num.bits%      'Picture width in bytes
   img.bytes% = (num.dots% + 7) \ 8             'Length of print string, bytes
   trim.bits% = img.bytes% * 8 - num.dots%       'Excess bits to trim from PCX data
   trim.byte% = 256 - 2 ^ trim.bits%            'Value to 'AND' with last byte

'---Set up laser printer
   pstring$ = CHR$(27) + "*r1A"
   GOSUB ppprint                              'Start raster graphics
   pc$ = CHR$(27) + "*b" + LTRIM$(STR$(img.bytes%)) + "W" 'LJ escape code to
                                                          'to print a string
'---Convert *.PCX to bitmap string                        'of raster graphics
   byte.ptr& = 128                           'Point to 1st byte after header
   byte$ = " "
   buffer$ = STRING$(num.bytes%, CHR$(0))     'Create bitmap buffer string
   plane% = 1                                 'Initialize
   line.num% = 1                              '  counters
   REDIM plane$(num.planes%)
   DO                                        'Loop until all bytes are read
      GOSUB get.vals                         'Get a value and no. of repeats
                                             'Update current plane
      plane$(plane%) = plane$(plane%) + STRING$(num.repeats%, CHR$(byte.val% XOR 255))
      IF LEN(plane$(plane%)) >= bytes.per.line% THEN   'Finished current plane?
                                                     'If last plane, then
                                                     '   update buffer
         IF plane% = num.planes% THEN GOSUB buff.update ELSE plane% = plane% + 1
      END IF
      IF line.num% > num.lines% THEN EXIT DO   'Are we finished?
   LOOP
   pstring$ = CHR$(27) + "*rB"                'End raster graphics
   GOSUB ppprint
   CLOSE f%

EXIT SUB

get.vals:
      num.repeats% = 1
      byte.ptr& = byte.ptr& + 1
      GET #f%, byte.ptr&, byte$
      byte.val% = ASC(byte$)
      IF byte.val% >= 192 THEN       'No. of repeating bytes
         num.repeats% = byte.val% - 192
         byte.ptr& = byte.ptr& + 1
         GET #f%, byte.ptr&, byte$
         byte.val% = ASC(byte$)
      END IF
RETURN

buff.update:
   plane% = 1
   IF num.planes% = 1 THEN
      buffer$ = LEFT$(plane$(plane%), img.bytes%)
      GOSUB left.over
   ELSE
      IF anycolor% THEN initbyte% = 0 ELSE initbyte% = 255
      buffer$ = STRING$(img.bytes%, initbyte%)
      FOR plane% = 1 TO num.planes%
         FOR i% = 1 TO img.bytes%
            p% = ASC(MID$(plane$(plane%), i%, 1))
            b% = ASC(MID$(buffer$, i%, 1))
            IF anycolor% THEN
               MID$(buffer$, i%, 1) = CHR$(b% OR p%)
            ELSE
               MID$(buffer$, i%, 1) = CHR$(b% AND p%)
            END IF
         NEXT i%
         GOSUB left.over
      NEXT plane%
   END IF
   IF trim.bits% THEN MID$(buffer$, img.bytes%, 1) = CHR$(ASC(MID$(buffer$, img.bytes%, 1)) AND trim.byte%)
   pstring$ = pc$ + buffer$
   GOSUB ppprint
   line.num% = line.num% + 1
   plane% = 1
RETURN

left.over:
   leftover% = LEN(plane$(plane%)) - bytes.per.line%
   plane$(plane%) = RIGHT$(plane$(plane%), leftover%)
RETURN

ppprint:
   bytes& = DoPrint&(pstring$)
RETURN

END SUB

SUB PrintPCXC (pcxfile$, printcolor$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  PrintPCXC:  Prints a PCX file with user specification of which colors
'              are printed.

'  pcxfile$    - name of PCX file
'  printcolor$ - color conversion string, ASCII values of each character in
'                the string represent color values to be printed

'  Externals:  DoPrint&
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   REDIM bitval%(7)            'Array holding powers of 2
      bitval%(0) = 1           '  to avoid using the BASIC function which
      bitval%(1) = 2           '  is very slow
      bitval%(2) = 4           '  example: bitval(4) = 2 ^ 4
      bitval%(3) = 8
      bitval%(4) = 16
      bitval%(5) = 32
      bitval%(6) = 64
      bitval%(7) = 128

'---Get PCX header
   pcxheader$ = SPACE$(68)             'Reserve PCX header space
   f% = FREEFILE
   OPEN "b", f%, pcxfile$
   IF LOF(f%) < 128 THEN         'Not a valid PCX file header
      CLOSE f%
      EXIT SUB
   END IF
   GET #f%, 1, pcxheader$               'Get the PCX header

'---Extract PCX header variables
   num.bits% = ASC(MID$(pcxheader$, 4, 1))
   x1% = CVI(MID$(pcxheader$, 5, 2))
   y1% = CVI(MID$(pcxheader$, 7, 2))
   x2% = CVI(MID$(pcxheader$, 9, 2))
   y2% = CVI(MID$(pcxheader$, 11, 2))
   num.planes% = ASC(MID$(pcxheader$, 66, 1))
   bytes.per.line% = CVI(MID$(pcxheader$, 67, 2))

   num.dots% = x2% - x1% + 1                    'Actual width of picture in dots
   num.lines% = y2% - y1% + 1                   'Actual height of picture in lines
   num.bytes% = bytes.per.line% / num.bits%     'No. of bytes in each line of file
   img.bytes% = (num.dots% + 7) \ 8            'No. of bytes in each print line
   trim.bits% = img.bytes% * 8 - num.dots%      'No. of unused bits in last byte
   trim.byte% = 256 - 2 ^ trim.bits%           'Computed mask byte
   num.colors% = 2 ^ num.planes%

'---Set up color conversion array
   REDIM setcolor%(num.colors%)          'Array that will hold color conversions
   FOR i% = 1 TO LEN(printcolor$)
      colorvalue% = ASC(MID$(printcolor$, i%, 1))
      IF colorvalue% <= num.colors% AND colorvalue% >= 0 THEN setcolor%(colorvalue%) = -1
   NEXT i%

'---Set up laser printer
   pstring$ = CHR$(27) + "*r1A"
   GOSUB pcprint                              'Start raster graphics
   pc$ = CHR$(27) + "*b" + LTRIM$(STR$(img.bytes%)) + "W" 'LJ escape code to
                                                         'to print a string
                                                         'of raster graphics
'---Convert *.PCX to *.IMG
   byte.ptr& = 128
   byte$ = " "
   buffer$ = STRING$(num.bytes%, CHR$(0))     'Initialize bitmap buffer
   plane% = 1                                 'Initialize
   line.num% = 1                              '  counters
   REDIM plane$(num.planes%)                  'Initialize video plane buffers
   DO                                        'Start retrieving values
      GOSUB pgetvals                          'Retrieve a compressed bitmap value
      IF num.planes% = 1 THEN                 'If black & white, convert zero bits to black
         plane$(plane%) = plane$(plane%) + STRING$(num.repeats%, CHR$(byte.val% XOR 255))
      ELSE
         plane$(plane%) = plane$(plane%) + STRING$(num.repeats%, CHR$(byte.val%))
      END IF
      IF LEN(plane$(plane%)) >= bytes.per.line% THEN    'If plane has been retrieved
                     'If all planes have been retrieved, update image buffer
         IF plane% = num.planes% THEN GOSUB pbuffupdate ELSE plane% = plane% + 1
      END IF
      IF line.num% > num.lines% THEN EXIT DO         'Exit if done
   LOOP                                            'Get next set of values
   CLOSE f%
   pstring$ = CHR$(27) + "*rB"                     'End raster graphics
   GOSUB pcprint
EXIT SUB

pgetvals:
      num.repeats% = 1                     'Default repeat value
      byte.ptr& = byte.ptr& + 1           'Point to next byte
      GET #f%, byte.ptr&, byte$
      byte.val% = ASC(byte$)
      IF byte.val% >= 192 THEN             'Are there repeating bytes?
         num.repeats% = byte.val% - 192     'If so, how many?
         byte.ptr& = byte.ptr& + 1        'Point to next byte
         GET #f%, byte.ptr&, byte$         'Get the next byte
         byte.val% = ASC(byte$)            'This is the value to be repeated
      END IF
RETURN

pbuffupdate:
   plane% = 1
   IF num.planes% = 1 THEN          'For only one plane, buffers are identical
      buffer$ = LEFT$(plane$(plane%), img.bytes%)
      GOSUB pleftover               'Save the leftovers
   ELSE
      buffer$ = STRING$(img.bytes%, 0)     'Initialize bitmap buffer
      FOR i% = 1 TO img.bytes%              'Check each byte
         byte% = 0                         '  & each plane
         FOR bit% = 0 TO 7                 '  & each bit
            colr% = 0                      '  to compute the actual color
            FOR plane% = 1 TO num.planes%
               p% = ASC(MID$(plane$(plane%), i%, 1)) AND bitval%(bit%)
               IF p% THEN colr% = colr% OR bitval%(plane% - 1)   'Add the color values
            NEXT plane%
            IF setcolor%(colr%) THEN byte% = byte% OR bitval%(bit%)   'Convert it?
         NEXT bit%                                               'Check next bit
         MID$(buffer$, i%, 1) = CHR$(byte%)                       'Update buffer
      NEXT i%                                                    'Next byte
      FOR plane% = 1 TO num.planes%                               'Save the
         GOSUB pleftover                                         ' leftovers for
      NEXT plane%                                                ' each plane
   END IF
            'Trim excess bits in last byte
   IF trim.bits% THEN MID$(buffer$, img.bytes%, 1) = CHR$(ASC(MID$(buffer$, img.bytes%, 1)) AND trim.byte%)
   pstring$ = pc$ + buffer$          'Save the bitmap line
   GOSUB pcprint
   line.num% = line.num% + 1             'Update line count
   plane% = 1                           'Initialize to first plane for next round
RETURN

pleftover:           'Save repeated bytes left over from previous line
   leftover% = LEN(plane$(plane%)) - bytes.per.line%
   plane$(plane%) = RIGHT$(plane$(plane%), leftover%)
RETURN

pcprint:
   bytes& = DoPrint&(pstring$)
RETURN
END SUB

SUB ReverseImage (bitmapfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ReverseImage: Takes a bitmapped image and converts it to reverse video.

'  bitmapfile$   - name of file containing bitmapped image

'  Externals:    GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get file header and open file
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$

'---Reverse each line
   FOR i% = 1 TO bitmaplines%
      GET #f%, i%
      b$ = bitmap$
      FOR j% = 1 TO bitmaplen%            'Reverse each byte in the line
         bitmap% = ASC(MID$(b$, j%, 1))
         MID$(b$, j%, 1) = CHR$(bitmap% XOR 255)
      NEXT j%
      LSET bitmap$ = b$
      PUT #f%, i%
   NEXT i%
   CLOSE f%

END SUB

SUB ScreenImage (bitmapfile$, screenfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ScreenImage: Takes a bitmapped image and screens it.  Where there is
'               black in the image and the screen, there will be black.
'               The screen is treated as a pattern, and is repeated
'               horizontally and vertically as necessary.

'  bitmapfile$  - name of file containing bitmapped image
'  screenfile$  - name of file containing the image of the screen

'  Externals:   GetBitmapHeader
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Get Pattern from DOS file & load into buffer array
   CALL GetBitmapHeader(screenfile$, screendots%, screenlines%, screenlen%)
   REDIM buffer$(screenlines%)
   f% = FREEFILE
   OPEN "r", #f%, screenfile$, screenlen%
   FIELD #f%, screenlen% AS buff$
   FOR i% = 1 TO screenlines% - 1
      GET #f%, i%
      buffer$(i%) = RIGHT$(buff$, 1) + LEFT$(buff$, screenlen% - 1)
   NEXT i%
   GET #f%, screenlines%
   buffer$(0) = buff$
   CLOSE #f%

'---Screen each line in the bitmap
   CALL GetBitmapHeader(bitmapfile$, numdots%, numlines%, reclen%)
   OPEN "r", #f%, bitmapfile$, reclen%
   FIELD #f%, reclen% AS b$
   FOR i% = 1 TO numlines%
      GET #f%, i%
      bitmap$ = b$
      curline% = i% MOD screenlines%
      FOR j% = 1 TO reclen%
         curbyte% = ASC(MID$(bitmap$, j%, 1))
         screenbyte% = ASC(MID$(buffer$(curline%), j% MOD screenlen% + 1, 1))
         MID$(bitmap$, j%, 1) = CHR$(curbyte% AND screenbyte%)
      NEXT j%
      LSET b$ = bitmap$
      PUT #f%, i%
   NEXT i%
   CLOSE f%
END SUB

SUB ShiftImage (image$, shift%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ShiftImage: Shifts the bits in a text string.

'  image$      - bitmap string to be shifted
'  shift%      - no. of dots to shift right (negative value shifts left)

'  Externals:  Slant
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Compute shift and set up bitmap string
   origlen% = LEN(image$)
   IF shift% > 0 AND shift% < 8 THEN
      s% = shift%
   ELSEIF shift% < 0 AND shift% > -8 THEN
      s% = 8 - ABS(shift%)
      image$ = image$ + CHR$(0)
   ELSE
      EXIT SUB
   END IF

   CALL Slant(BYVAL s%, image$)
  
   image$ = RIGHT$(image$, origlen%)

END SUB

SUB SkewImage (bitmapfile$, skewfile$, skew!)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  SkewImage:  Takes a bitmapped image and skews it right or left.

'  bitmapfile$ - name of file containing bitmapped image
'  skewfile$   - name of file to create that will contain skewed image
'  skew!       - amount of skew, in percent of height; "+" = skewed to right

'  Calls:      ShiftImage

'  Externals:  AddBitmapHeader
'              GetBitmapHeader
'              InitialFile
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Retrieve bitmap header information & compute skew file dimensions
   CALL GetBitmapHeader(bitmapfile$, bitmapdots%, bitmaplines%, bitmaplen%)
   skewfactor! = ABS(skew! / 100)
   newdots% = bitmaplines% * skewfactor!
   skewdots% = bitmapdots% + newdots%
   skewlines% = bitmaplines%
   skewlen% = (skewdots% + 7) \ 8

'---Open the bitmap and skew files
   f% = FREEFILE
   OPEN "r", #f%, bitmapfile$, bitmaplen%
   FIELD #f%, bitmaplen% AS bitmap$
   CALL InitialFile(skewfile$)
   f2% = FREEFILE
   OPEN "r", #f2%, skewfile$, skewlen%
   FIELD #f2%, skewlen% AS skew$

'---Perform the skew
   FOR i% = 1 TO skewlines%
      IF skew! > 0 THEN
         offset! = INT(i% * skewfactor! + .5)
         offset% = newdots% - offset!
      ELSE
         offset% = i% * skewfactor!
      END IF
      bytes% = offset% \ 8
      shift% = offset% MOD 8
      GET #f%, i%
      map$ = bitmap$
      CALL ShiftImage(map$, shift%)
      s$ = STRING$(bytes%, 0) + map$
      IF LEN(s$) < skewlen% THEN s$ = s$ + STRING$(skewlen% - LEN(s$), 0)
      LSET skew$ = s$
      PUT #f2%, i%
   NEXT i%
   CLOSE #f%, #f2%

'---Create skew file header
   CALL AddBitmapHeader(skewfile$, skewdots%, skewlines%)

END SUB

