'$INCLUDE: 'laserpak.dec'

'                  Parameter List: LOWLEVEL.BAS
'-----------------------------------------------------------------------------
''  bitmapfile$ - name of file containing a bitmapped file image
''  file$ - name of file to initialize
''  numdots% - width of bitmapped image, in dots
''  numlines% - height of bitmapped image, in dots
''  reclen% - width of bitmapped image, in bytes (8 dots per byte)

SUB AddBitmapHeader (bitmapfile$, numdots%, numlines%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  AddBitmapHeader: Adds header information to a bitmapped image file.

'  bitmapfile$      - name of file containing a bitmapped file image
'  numdots%         - width of bitmapped image, in dots
'  numlines%        - height of bitmapped image, in dots
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   f% = FREEFILE
   OPEN "b", #f%, bitmapfile$
   filelen& = LOF(f%)                           'Find the end of the  file
   header$ = MKI$(numdots%) + MKI$(numlines%)   'Add four bytes containing
   PUT #f%, filelen& + 1, header$               '  the width of the image, in
   CLOSE #f%                                    '  dots, and height, in lines

END SUB

SUB GetBitmapHeader (bitmapfile$, numdots%, numlines%, reclen%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  GetBitmapHeader: Retrieves the header information for a bitmapped image
'                   file.

'  bitmapfile$      - name of file containing a bitmapped file image
'  numdots%         - width of bitmapped image, in dots
'  numlines%        - height of bitmapped image, in dots
'  reclen%          - width of bitmapped image, in bytes (8 dots per byte)

'  Calls:           ValidBitmap%
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Check validity
   IF NOT ValidBitmap%(bitmapfile$) THEN
      reclen% = 0                       'If not valid, set byte width of
      EXIT SUB                          '    the bitmap to false
   END IF

'---Get header information of source image file
   f% = FREEFILE
   OPEN "b", f%, bitmapfile$
   filelen& = LOF(f%)
   header$ = SPACE$(4)                  'Set up header string
   GET #f%, filelen& - 3, header$       'Retrieve header string (last 4 bytes)
   numdots% = CVI(LEFT$(header$, 2))    'Compute width and height of
   numlines% = CVI(RIGHT$(header$, 2))  '   bitmap
   reclen% = (numdots% + 7) \ 8         'Byte width of bitmap
   CLOSE f%

END SUB

SUB InitialFile (file$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  InitialFile: Initializes a file to zero length, so it can be written to in
'               binary mode.

'  file$        - name of file to initialize
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'---Create a zero length file
   f% = FREEFILE
   OPEN "o", f%, file$
   PRINT #f%, "";
   CLOSE f%

END SUB

FUNCTION ValidBitmap% (bitmapfile$)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  ValidBitmap%: Checks the validity of a possible bitmapped image file.

'  bitmapfile$   - name of file containing a bitmapped file image
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
   f% = FREEFILE
   OPEN "b", f%, bitmapfile$              'Open file
   filelen& = LOF(f%)                     'How long is it?
   IF filelen& < 4 THEN GOTO no.good      'Must be at least as long as header
   header$ = SPACE$(4)                    'Set up header string
   GET #f%, filelen& - 3, header$         'Retrieve header string
   numdots% = CVI(LEFT$(header$, 2))      'Compute width and height of
   numlines% = CVI(RIGHT$(header$, 2))    'bitmap, and make sure they're
   IF numdots% < 1 OR numlines% < 1 THEN GOTO no.good    'positive values

   CLOSE f%                                  'Close file
   ValidBitmap% = -1                         'Valid bitmap, return true
EXIT FUNCTION

no.good:
   CLOSE f%                                  'Close file
   IF filelen& = 0 THEN KILL bitmapfile$     'If it has no length, then we
   ValidBitmap% = 0                          'created it, so kill it, and
                                             'return false for non-valid
END FUNCTION

