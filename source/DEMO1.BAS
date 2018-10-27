'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO1.BAS:     Program to demonstrate some of the fundamental graphics
'                 procedures, contained in the module: LASERFNS.BAS; and,
'                 construction of typical code.

'                 (c) 1991, by R. F. Holcombe

'  IMPORTANT:     All the demos in this module use the laser printer,
'                 make sure that it is on-line.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserlib.dec'
'$INCLUDE: 'laserfns.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'Basic procedures with in-line code
'---Initial set up - we'll start with sending output to the printer
'              *** Make sure it's turned on! ***

   WIDTH "lpt1:", 255         'Turn off BASIC's line wrap: VERY IMPORTANT!!!

   handle% = FREEFILE                     'Get available file number

   CALL PutHandle(handle%)             'Save it in a DOS buffer - DO NOT
                                       '  send this command with a different
                                       '  file number until you are finished
   filename$ = "LPT1"                  'Select output device

   OPEN filename$ FOR OUTPUT AS #handle%  'Route printing to file, do not use
                                          'the colon if specifying 'LPT1' as
                                          ' the file (i.e. LPT1: won't work)!
   PRINT #handle%, Initial$;              'Initialize the margins - don't for-
                                          'get the semi-colon at the end

'---Here is where we put our goodies
   PRINT #handle%, PosCursor$(1, 1);      'Position the cursor,
   PRINT #handle%, HorzLine$(3, 3);       '  and, draw a horizontal line.
   PRINT #handle%, VertLine$(3, 3);       'Now, a vertical line, cursor
                                          '  hasn't moved
   PRINT #handle%, PosCursor$(1, 4);      'Draw a couple more lines
   PRINT #handle%, VertLine$(3, 3);
   PRINT #handle%, PosCursor$(4, 1);
   PRINT #handle%, HorzLine$(3, 3);
   PRINT #handle%, PosCursor$(1, 1);
   PRINT #handle%, GrayScale$(3, 3, 10);     'Add some texture
   PRINT #handle%, PosCursor$(2, 2);
   PRINT #handle%, HPPattern$(1.5, 1.5, 6);  'Add a pattern
   PRINT #handle%, PosCursor$(1.1, 4);
   PRINT #handle%, GrayScale$(3, .1, 100);   'Let's try to make it look 3-D
   PRINT #handle%, PosCursor$(4, 1.1);
   PRINT #handle%, GrayScale$(.1, 3, 100);
   PRINT #handle%, PosCursor$(1.5, 1.75); "How does it look?";

   PRINT #handle%, EjectPage$;            'Make sure you use the semi-colon
   CLOSE handle%                          'here or an extra form feed will be
                                          'sent to the printer after the page
                                          'is ejected.
RETURN

demo.2:     'Same demo as demo 1, but with some labor saving calls
'---Initial set up - we'll start with sending output to the printer
'              *** Make sure it's turned on! ***


   filename$ = "LPT1"                     'Select output device
   handle% = OpenFile%(filename$)         'Set up printer, and return handle
   bytes& = DoPrint&(Initial$)            'Initialize the margins - don't for-
'     Same as:  PRINT #handle%, Initial$;

'---Here is where we put our goodies - note: optional ways of sending to file
   bytes& = DoPrint&(PosCursor$(1, 1))                   'Method number 1

   pstring$ = HorzLine$(3, 3) + VertLine$(3, 3)          'Method number 2
   bytes& = DoPrint&(pstring$)

   bytes& = DoPrint&(PosCursor$(1, 4) + VertLine$(3, 3)) 'Method number 3

   PRINT #handle%, PosCursor$(4, 1);                     'Method number 4
                                                   'which we used in demo 1.
   bytes& = DoPrint&(HorzLine$(3, 3))
   bytes& = DoPrint&(PosCursor$(1, 1))
   bytes& = DoPrint&(GrayScale$(3, 3, 10))
   bytes& = DoPrint&(PosCursor$(2, 2))
   bytes& = DoPrint&(HPPattern$(1.5, 1.5, 6))
   bytes& = DoPrint&(PosCursor$(1.1, 4))
   bytes& = DoPrint&(GrayScale$(3, .1, 100))
   bytes& = DoPrint&(PosCursor$(4, 1.1))
   bytes& = DoPrint&(GrayScale$(.1, 3, 100))
   bytes& = DoPrint&(PosCursor$(1.5, 1.75) + "How does it look?")
   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%                    'This is why we returned the file number
                                    'above.
RETURN

demo.3:        'Same as Demo 2, but we'll offset the printing
'---Initial set up - we'll start with sending output to the printer
'              *** Make sure it's turned on! ***

   vo! = 1: ho! = 2
   CALL PutOffsets(vo! * 720, ho! * 720)

   filename$ = "LPT1"                     'Select output device
   handle% = OpenFile%(filename$)         'Set up printer, and return handle
   bytes& = DoPrint&(Initial$)            'Initialize the margins - don't for-
'     Same as:  PRINT #handle%, Initial$;

'---Here is where we put our goodies - note: optional ways of sending to file
   bytes& = DoPrint&(PosCursor$(1, 1))                   'Method number 1

   pstring$ = HorzLine$(3, 3) + VertLine$(3, 3)          'Method number 2
   bytes& = DoPrint&(pstring$)

   bytes& = DoPrint&(PosCursor$(1, 4) + VertLine$(3, 3)) 'Method number 3

   PRINT #handle%, PosCursor$(4, 1);                     'Method number 4
                                                   'which we used in demo 1.
   bytes& = DoPrint&(HorzLine$(3, 3))
   bytes& = DoPrint&(PosCursor$(1, 1))
   bytes& = DoPrint&(GrayScale$(3, 3, 10))
   bytes& = DoPrint&(PosCursor$(2, 2))
   bytes& = DoPrint&(HPPattern$(1.5, 1.5, 6))
   bytes& = DoPrint&(PosCursor$(1.1, 4))
   bytes& = DoPrint&(GrayScale$(3, .1, 100))
   bytes& = DoPrint&(PosCursor$(4, 1.1))
   bytes& = DoPrint&(GrayScale$(.1, 3, 100))
   bytes& = DoPrint&(PosCursor$(1.5, 1.75) + "How does it look?")
   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%                    'This is why we returned the file number
                                    'above.
RETURN

demo.4:        'Same as Demo 3, but we'll send to a file first
'---Initial set up - we'll start with sending output to the printer
'              *** Make sure it's turned on! ***

   vo! = 1: ho! = 2
   CALL PutOffsets(vo! * 720, ho! * 720)

   filename$ = "outfile.hpi"              'Select output device
   handle% = OpenFile%(filename$)         'Set up printer, and return handle
   bytes& = DoPrint&(Initial$)            'Initialize the margins - don't for-
'     Same as:  PRINT #handle%, Initial$;

'---Here is where we put our goodies - note: optional ways of sending to file
   bytes& = DoPrint&(PosCursor$(1, 1))                   'Method number 1

   pstring$ = HorzLine$(3, 3) + VertLine$(3, 3)          'Method number 2
   bytes& = DoPrint&(pstring$)

   bytes& = DoPrint&(PosCursor$(1, 4) + VertLine$(3, 3)) 'Method number 3

   PRINT #handle%, PosCursor$(4, 1);                     'Method number 4
                                                   'which we used in demo 1.
   bytes& = DoPrint&(HorzLine$(3, 3))
   bytes& = DoPrint&(PosCursor$(1, 1))
   bytes& = DoPrint&(GrayScale$(3, 3, 10))
   bytes& = DoPrint&(PosCursor$(2, 2))
   bytes& = DoPrint&(HPPattern$(1.5, 1.5, 6))
   bytes& = DoPrint&(PosCursor$(1.1, 4))
   bytes& = DoPrint&(GrayScale$(3, .1, 100))
   bytes& = DoPrint&(PosCursor$(4, 1.1))
   bytes& = DoPrint&(GrayScale$(.1, 3, 100))
   bytes& = DoPrint&(PosCursor$(1.5, 1.75) + "How does it look?")
   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%                    'This is why we returned the file number
                                    'above.
   s$ = "copy " + filename$ + " prn /b"   'Make sure the '/b' switch is used
   SHELL s$                         'Let DOS copy it to the printer
RETURN                              'See also: 'FileToPrinter'
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(4)
      choice$(1) = "Printer Output: Traditional Techniques"
      choice$(2) = "Printer Output: Improved Techniques"
      choice$(3) = "Use of Offsets"
      choice$(4) = "File Output"
DO
   choice% = DemoChoice%(choice$())    'Select the demo to run

   LOCATE 2, 10
   PRINT TIME$; "  "; choice$(choice%); " demo commencing."
   bytecount& = 0                      'Initialize the number of bytes sent
   CALL PutCount(bytecount&)           'Save it in DOS buffer
                        'Execute the selected demo

   ON choice% GOSUB demo.1, demo.2, demo.3, demo.4

   CALL GetCount(bytecount&)           'Find out how many bytes were sent
   LOCATE CSRLIN, 10                   '  and, display it.
   IF bytecount& > 0 THEN PRINT TIME$; "  Demo over.  Bytes sent to printer/file:"; bytecount&
   LOCATE 20, 15: PRINT "Press any key to continue."
   DO WHILE INKEY$ = "": LOOP
LOOP
RETURN


