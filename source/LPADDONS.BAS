'$INCLUDE: 'laserpak.dec'

SUB DiagLineLR (y1!, x1!, y2!, x2!, wt%)
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DiagLineLR:  Creates a low resolution diagonal line.  The line is more
'               jagged than when using DiagLine, but uses less memory, and is
'               faster.  The amount of "jaggedness" is more noticeable in
'               lines of greater weight.

'  y1!          - y coordinate of start point, in inches
'  x1!          - x coordinate of start point, in inches
'  y2!          - y coordinate of end point, in inches
'  x2!          - x coordinate of end point, in inches
'  wt%          - line weight, in dots

'  Externals:   DoPrint
'               HorzLine
'               PosCursor
'               VertLine
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

   IF ABS(x2! - x1!) > ABS(y2! - y1!) THEN hsegs% = 1 ELSE vsegs% = 1  'Horizontal or vertical segments
   IF hsegs% THEN IF x2! < x1! THEN SWAP x1!, x2!: SWAP y1!, y2!       'If horizontal
   IF vsegs% THEN IF y2! < y1! THEN SWAP y1!, y2!: SWAP x1!, x2!       'If vertical
   w! = wt% / 300                   'Set up geometry
   dely! = y2! - y1!
   delx! = x2! - x1!
   ws% = ABS(dely! / w!) * hsegs% + ABS(delx! / w!) * vsegs% + 1
   dy! = (ABS(dely!) + w! * hsegs%) / ws% * SGN(dely!)   'Horiz. increment
   dx! = (ABS(delx!) + w! * vsegs%) / ws% * SGN(delx!)   'Vert. increment
   FOR i% = 1 TO ws%                   'Print each of the sements
      IF hsegs% THEN CALL FillDriver(y1! - w! / 2, x1! - w! / 2, w!, dx! + w!, tint%, 0)
      IF vsegs% THEN CALL FillDriver(y1! - w! / 2, x1! - w! / 2, dy! + w!, w!, tint%, 0)
      y1! = y1! + dy!                  'Compute start of next
      x1! = x1! + dx!                  '     segment
   NEXT i%

END SUB

