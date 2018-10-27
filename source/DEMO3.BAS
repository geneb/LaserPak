'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO3.BAS:     Program to demonstrate the printing of text, contained
'                 in the module: LZRTEXT.BAS

'                 (c) 1991, by R. F. Holcombe

'  IMPORTANT:     All the demos in this module use the laser printer,
'                 make sure that it is on-line.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserlib.dec'
'$INCLUDE: 'laserfns.dec'
'$INCLUDE: 'lzrtext.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'Some new ways to print text
   filename$ = "LPT1"
   handle% = OpenFile(filename$)

'---Method 1: The old way - simple, but not much control
   bytes& = DoPrint(PosCursor$(1, .5))
   PRINT #handle%, "This is the usual way of printing text.  Just print a line,"
   PRINT #handle%, "and, let the printer automatically wrap the lines.  Make sure"
   PRINT #handle%, "you haven't turned the line wrap off, of course.  The advantage"
            'Notice, we can use either 'PRINT #' or 'LPRINT'.
   LPRINT "is that it's very easy.  After the first line, succeeding lines"
   LPRINT "start at the current left margin.  If this is ok, do it this way."

'---Method 2: Now we're creating a crude word processor
   REDIM t$(4)                     'Set up some text strings
   t$(1) = "Here is another method.  We are setting up a little loop"
   t$(2) = "to save on coding.  We are also using the right justification in this"
   t$(3) = "example.  You can see how easy it is to set up routines to print complete"
   t$(4) = "paragraphs, or even complete documents."

   bytes& = DoPrint&(PosCursor$(3, 1))  'Here's where we start
   lpi! = 6                             'Line spacing
   indent! = .5                         'Initial indent value
   txtwidth! = 6                        'Width of print zone @ indent
   cpi! = 12                            'Character spacing
   rightjustify% = -1                   'Right justify the text
   FOR i% = 1 TO UBOUND(t$)             'How many strings to print
      IF i% < UBOUND(t$) THEN           'Justify all but the last line
         IF i% = 1 THEN
            bytes& = DoPrint&(PushCursor$ + MoveCursor$(0, indent!))
            bytes& = DoPrint&(JustifyText$(t$(i%), txtwidth! - indent!, cpi!, rightjustify%))
            bytes& = DoPrint&(PopCursor$)
         ELSE
            bytes& = DoPrint&(PushCursor$)
            bytes& = DoPrint&(JustifyText$(t$(i%), txtwidth!, cpi!, rightjustify%))
            bytes& = DoPrint&(PopCursor$)
         END IF
      ELSE                              'This is the last line
         bytes& = DoPrint&(t$(i%))
      END IF                            'Move to the next line
      bytes& = DoPrint&(LineFeed$(lpi!))
   NEXT i%

'---Now, let's do the job with just one call
   t$(1) = "This was formatted automatically using the FormatText call.  You can see"
   t$(2) = "that the output is more attractive.  Because of the word wrap feature, it is not"
   t$(3) = "necessary to compute the lengths of text lines in advance.  If you wished, you could enter your text as"
   t$(4) = "one very long line, and let the formatter do all the work."
   bytes& = DoPrint&(PosCursor$(5, 1))
   CALL FormatText(t$(), lpi!, txtwidth!, indent!, rightjustify%, cpi!)

   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
RETURN

demo.2:     'Some additional features are demonstrated
   filename$ = "LPT1"
   handle% = OpenFile(filename$)

   REDIM t$(4)                     'Set up some text strings
   t$(1) = "Here is yet another method.  Here, we are setting up a little loop"
   t$(2) = "to save on coding.  We are also using the right justification in this"
   t$(3) = "example.  You can see how easy it is to set up routines to print complete"
   t$(4) = "paragraphs, or even complete documents."

   cpi! = 12                            'Character spacing

'---Let's draw some reference lines
   bytes& = DoPrint(PosCursor$(2, .5))    'Left margin
   bytes& = DoPrint(VertLine$(3, 1))      'Draw a line
   bytes& = DoPrint(PosCursor$(2, 4))     'Location of center of heading
   bytes& = DoPrint(VertLine$(3, 1))
   bytes& = DoPrint(PosCursor$(2, 7.5))   'Right margin
   bytes& = DoPrint(VertLine$(3, 1))

   d$ = DATE$
   bytes& = DoPrint&(PosCursor$(2.2, 7.5))    'Let's put the date at the
   bytes& = DoPrint&(FlushRight$(d$, cpi!))   'right margin using FlushRight
   bytes& = DoPrint&(CharSpacing$(cpi!) + d$)   'And, print it
   bytes& = DoPrint&(PosCursor$(2.7, 4))      'Location of heading
   t$ = "Paragraph Heading"                  'Heading text
   bytes& = DoPrint&(UnderlineOn$)            'Let's underline it
   bytes& = DoPrint&(CenterText$(t$, cpi!))   'Move over for centering
   bytes& = DoPrint&(BoldText$(t$, cpi!))     'Print it in bold
   bytes& = DoPrint&(UnderlineOff$)           'Turn off the underline

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
RETURN

demo.3:     'Printing text in unconventional directions
   filename$ = "LPT1"
   handle% = OpenFile(filename$)

   lpi! = 7
   cpi! = 12
   bytes& = DoPrint(PosCursor$(1, 2))
   bytes& = DoPrint(VertText$("Vertical Text", lpi!))
   bytes& = DoPrint(PosCursor$(4, 4))
   bytes& = DoPrint(VertText$("Inverse Vertical Text", -lpi!))
   bytes& = DoPrint(PosCursor$(4, 2))
   bytes& = DoPrint(SlantText$("Slant Down", cpi!, lpi!))
   bytes& = DoPrint(PosCursor$(3, 5))
   bytes& = DoPrint(SlantText$("Slant Up", cpi!, -lpi! * 4))
   bytes& = DoPrint(PosCursor$(6, 5))
   bytes& = DoPrint(SlantText$("Slant Down and Back", -cpi!, lpi!))


   bytes& = DoPrint(PosCursor$(1, 2))
   bytes& = DoPrint(PosCursor$(1, 2))


   bytes& = DoPrint(EjectPage$)
   CLOSE handle%
RETURN
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(3)
      choice$(1) = "Some new ways to print text"
      choice$(2) = "Bold, Center, and Underline"
      choice$(3) = "Text in other directions"
DO
   choice% = DemoChoice%(choice$())    'Select the demo to run

   LOCATE 2, 10
   PRINT TIME$; "  "; choice$(choice%); " demo commencing."
   bytecount& = 0                      'Initialize the number of bytes sent
   CALL PutCount(bytecount&)           'Save it in DOS buffer
                        'Execute the selected demo

   ON choice% GOSUB demo.1, demo.2, demo.3

   CALL GetCount(bytecount&)           'Find out how many bytes were sent
   LOCATE CSRLIN, 10                   '  and, display it.
   IF bytecount& > 0 THEN PRINT TIME$; "  Demo over.  Bytes sent to printer/file:"; bytecount&
   LOCATE 20, 15: PRINT "Press any key to continue."
   DO WHILE INKEY$ = "": LOOP
LOOP
RETURN


