'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO6.BAS: Demonstration of procedures contained the module: IMAGES.BAS.
             

'             (c) 1991, by R. F. Holcombe

'  IMPORTANT: Some of the demos in this module use the laser printer,
'             make sure that it is on-line when needed.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserlib.dec'
'$INCLUDE: 'laserfns.dec'
'$INCLUDE: 'images.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'Printing an image
   filename$ = "LPT1"
   handle% = OpenFile%(filename$)

   reso% = 300
   bytes& = DoPrint&(Resolution$(reso%) + PosCursor$(1, 1))
   anycolor% = 0
   pcxfile$ = "laser1.pcx"
   CALL PrintPCX(pcxfile$, anycolor%)
   bytes& = DoPrint&(PosCursor$(2, 1) + "Print Black only")

   bytes& = DoPrint&(PosCursor$(1, 4))
   anycolor% = -1
   CALL PrintPCX(pcxfile$, anycolor%)
   bytes& = DoPrint&(PosCursor$(2, 4) + "Print any color")

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
RETURN

demo.2:     'Color Separations
   filename$ = "LPT1"
   pcxfile$ = "laser1.pcx"
   handle% = OpenFile%(filename$)
   reso% = 300    'Set the printing resolution
   bytes& = DoPrint&(Resolution$(reso%) + PosCursor$(1, 1))
                  'Separate the black
   bytes& = DoPrint&(PosCursor$(1, 1))
   printcolor$ = CHR$(0)      ' 0 = black
   CALL PrintPCXC(pcxfile$, printcolor$)
   bytes& = DoPrint&(PosCursor$(2, 1) + "Black Separation")
                  'Separate the cyan
   bytes& = DoPrint&(PosCursor$(1, 3))
   printcolor$ = CHR$(3)      ' 3 = cyan
   reso% = 300
   CALL PrintPCXC(pcxfile$, printcolor$)
   bytes& = DoPrint&(PosCursor$(2, 3) + "Cyan Separation")
                  'Separate the red
   bytes& = DoPrint&(PosCursor$(1, 5))
   printcolor$ = CHR$(4)      ' 4 = red
   reso% = 300
   CALL PrintPCXC(pcxfile$, printcolor$)
   bytes& = DoPrint&(PosCursor$(2, 5) + "Red Separation")

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
RETURN

demo.3:     'Image modification
   filename$ = "LPT1"
   handle% = OpenFile%(filename$)
               'Start off with printing the image normally
   reso% = 300    'Set the printing resolution
   bytes& = DoPrint&(Resolution$(reso%) + PosCursor$(1, 1))
   bitmapfile$ = "laser2.img"
   reverse% = 0
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(2, 1) + "Normal Printing")
               'To reverse it, just set reverse% = True
   bytes& = DoPrint&(PosCursor$(1, 4))
   reverse% = -1
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(2, 4) + "Reverse Video")
               'Now, let's flip the image around
   flipfile$ = "lsr2flip.img"
   CALL FlipHoriz(bitmapfile$, flipfile$)
               'Add, print it
   bytes& = DoPrint&(PosCursor$(4, 1))
   reverse% = 0
   CALL PrintImage(flipfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(5, 1) + "Flipped Horizontally")
               'Now, let's flip it vertically
   CALL FlipVert(bitmapfile$, flipfile$)
               'Add, print it
   bytes& = DoPrint&(PosCursor$(4, 4))
   CALL PrintImage(flipfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(5, 4) + "Flipped Horizontally")
               'And, lastly, flip it both ways
   flp2file$ = "lsr2flp2.img"
   CALL FlipHoriz(flipfile$, flp2file$)
               'Add, print it
   bytes& = DoPrint&(PosCursor$(7, 1))
   CALL PrintImage(flp2file$, reverse%)
   bytes& = DoPrint&(PosCursor$(8, 1) + "Flipped Both Ways")
               'For good measure, let's skew it
   skewfile$ = "lsr2skew.img"
   skew! = 25                       'Skew 25%
   CALL SkewImage(bitmapfile$, skewfile$, skew!)
               'Add, print it
   bytes& = DoPrint&(PosCursor$(7, 4))
   CALL PrintImage(skewfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(8, 4) + "Skewed 25 percent")

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
RETURN

demo.4:     'Merge and Knock Out
   filename$ = "demofile.prn"
   handle% = OpenFile%(filename$)
               'Start off with printing the image normally
   reso% = 300    'Set the printing resolution
   bitmapfile$ = "laser3.img"
   backup$ = "laser3.bak"
   mergefile$ = "moose.img"
   s$ = "copy " + bitmapfile$ + " " + backup$
   SHELL s$                'Backup original file
   yoffset% = 450          'Merge offsets, in dots (i.e. inches * reso%)
   xoffset% = 100          'Merge the two images
   CALL MergeImage(bitmapfile$, mergefile$, yoffset%, xoffset%)
   bytes& = DoPrint&(Resolution$(reso%) + PosCursor$(1, 1))
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(3.3, 1.75) + "Images were merged")
                           'Merge exclusively
   s$ = "copy " + backup$ + " " + bitmapfile$
   SHELL s$                'Restore original file
   CALL MergeExclusive(bitmapfile$, mergefile$, yoffset%, xoffset%)
   bytes& = DoPrint&(PosCursor$(4, 1))
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(6.3, 1.75) + "Image was merged, exclusively")
                           'Knock out the merged image
   s$ = "copy " + backup$ + " " + bitmapfile$
   SHELL s$                'Restore original file
   CALL KnockOut(bitmapfile$, mergefile$, yoffset%, xoffset%)
   bytes& = DoPrint&(PosCursor$(7, 1))
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(9.3, 1.75) + "Image was knocked out")
   s$ = "copy " + backup$ + " " + bitmapfile$
   SHELL s$                'Restore original file


   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)
RETURN

demo.5:     'Some more effects
   filename$ = "demofile.prn"
   bitmapfile$ = "laser2.img"
   backup$ = "laser2.bak"
   s$ = "copy laser2.img laser2.bak"
   SHELL s$
   screenfile$ = "screen25"
   handle% = OpenFile%(filename$)
               'Start off with printing the image normally
   reso% = 300    'Set the printing resolution
   bytes& = DoPrint&(Resolution$(reso%) + PosCursor$(3, 4))
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(4, 4) + "Voila: a shadow font")
               'Screen the image & print it
   CALL ScreenImage(bitmapfile$, screenfile$)
   bytes& = DoPrint&(PosCursor$(3, 1))
   CALL PrintImage(bitmapfile$, reverse%)       'Print by itself
   bytes& = DoPrint&(PosCursor$(4, 1) + "Image was screened")
   bytes& = DoPrint&(PosCursor$(3.05, 4.05))
   CALL PrintImage(bitmapfile$, reverse%)       'Now with the original
               'Let's add another layer
   s$ = "copy " + backup$ + " " + bitmapfile$
   SHELL s$                'Restore original file
   screenfile$ = "screen12"
   CALL ScreenImage(bitmapfile$, screenfile$)
   bytes& = DoPrint&(PosCursor$(3.1, 4.1))
   CALL PrintImage(bitmapfile$, reverse%)       'Print with the other two

   s$ = "copy " + backup$ + " " + bitmapfile$
   SHELL s$                'Restore original file

                        'Let's put a few processes together
   skewfile$ = "lsr2skew.img"
   flipfile$ = "lsr2flip.img"
   screenfile$ = "screen12"
   skew! = -200
   CALL FlipVert(bitmapfile$, flipfile$)
   CALL ScreenImage(flipfile$, screenfile$)
   CALL SkewImage(flipfile$, skewfile$, skew!)
   bytes& = DoPrint&(PosCursor$(6.32 + 2 / 300, 2.87 + 1 / 300))
   CALL PrintImage(skewfile$, reverse%)

   s$ = "copy " + backup$ + " " + bitmapfile$
   SHELL s$                'Restore original file
   bytes& = DoPrint&(PosCursor$(6, 3))
   CALL PrintImage(bitmapfile$, reverse%)



   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)
RETURN
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(5)
      choice$(1) = "Printing PCX files: Black & White"
      choice$(2) = "Color Separations of PCX Files"
      choice$(3) = "Printing a Bitmapped Image"
      choice$(4) = "Several Ways to Merge Images"
      choice$(5) = "The Screening Process"
DO
   choice% = DemoChoice%(choice$())    'Select the demo to run

   LOCATE 2, 10
   PRINT TIME$; "  "; choice$(choice%); " demo commencing."
   bytecount& = 0                      'Initialize the number of bytes sent
   CALL PutCount(bytecount&)           'Save it in DOS buffer
                        'Execute the selected demo

   ON choice% GOSUB demo.1, demo.2, demo.3, demo.4, demo.5

   CALL GetCount(bytecount&)           'Find out how many bytes were sent
   LOCATE CSRLIN, 10                   '  and, display it.
   IF bytecount& > 0 THEN PRINT TIME$; "  Demo over.  Bytes sent to printer/file:"; bytecount&
   LOCATE 20, 15: PRINT "Press any key to continue."
   DO WHILE INKEY$ = "": LOOP
LOOP
RETURN


