'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'  DEMO7.BAS: Demonstration of procedures contained the module: CONVERSN.BAS
             

'             (c) 1991, by R. F. Holcombe

'  IMPORTANT: Some of the demos in this module use the laser printer,
'             make sure that it is on-line when needed.
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

DECLARE FUNCTION DemoChoice% (choices$())

'$INCLUDE: 'laserlib.dec'
'$INCLUDE: 'laserfns.dec'
'$INCLUDE: 'images.dec'
'$INCLUDE: 'fonts.dec'
'$INCLUDE: 'conversn.dec'

   GOSUB run.demo

END
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'                             Demo Programs
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
demo.1:     'Convert an image from portrait to landscape
   bitmapfile$ = "laser2.img"
   landfile$ = "lsr2land.img"
   landfile2$ = "lsr2lan2.img"

   filename$ = "demofile.prn"
   reverse% = 0
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Resolution$(300) + PosCursor$(3.25, 1))
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(4, 1) + "Normal view: Portrait")


   CALL Port2Land(bitmapfile$, landfile$)    'First, let's rotate it
   bytes& = DoPrint&(PosCursor$(2, 4.5))
   reverse% = -1                             'Reverse video on this one
   CALL PrintImage(landfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(4, 4.5) + "Rotated view: Landscape")

   CALL Port2Land(landfile$, landfile2$)   'Let's rotate it again
   bytes& = DoPrint&(PosCursor$(5.5, 3.5))
   reverse% = 0
   CALL PrintImage(landfile2$, reverse%)
   bytes& = DoPrint&(PosCursor$(6.25, 3.5) + "Rotated view: Portrait?")

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)    'Send it to the printer
RETURN

demo.2:     'Creating a bitmapped image from a soft font
   fontfile$ = "demofont"
   bitmapfile$ = "demofont.img"
   REDIM text$(2)
   text$(1) = "Just testing."
   text$(2) = "How are we doing so far?"
   center% = -1               'Create an image of the font
   CALL MakeImage(fontfile$, bitmapfile$, text$(), center%)
                  'Print the image of the text using the demo soft font
   filename$ = "demofile.prn"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Resolution$(300) + PosCursor$(3, 1))
   reverse% = 0
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(4, 1) + "Font to Image Conversion")
                  'Print it in reverse video, to prove it was not downloaded
   bytes& = DoPrint&(PosCursor$(3, 4))
   reverse% = -1
   CALL PrintImage(bitmapfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(4, 4) + "Could a downloaded font do this?")

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)
RETURN

demo.3:     'Converting an image to an ASCII file
   bitmapfile$ = "demofont.img"
   filename$ = "demofile.prn"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Resolution$(300) + PosCursor$(3, 1))
                  'Let's crop the image to leave just the 'H'
   cropfile$ = "lsr2crop.img"
   croptop% = 10: cropright% = 436: cropleft% = 0: cropbot% = 0
   croptop% = 10: cropright% = 360: cropleft% = 0: cropbot% = 0
   CALL CropImage(bitmapfile$, cropfile$, croptop%, cropbot%, cropleft%, cropright%)
   bytes& = DoPrint&(PosCursor$(6, 1))
   reverse% = 0
   CALL PrintImage(cropfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(6, 2))
   reverse% = -1
   CALL PrintImage(cropfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(6.5, 1) + "This is the cropped image.")
                  'Trim excess white space off the cropped image
   trimfile$ = "lsr2trim.img"
   CALL TrimImage(cropfile$, trimfile$)
                  'Print the trimmed file
   bytes& = DoPrint&(PosCursor$(6, 4))
   reverse% = 0
   CALL PrintImage(trimfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(6.5, 4) + "Here it is all trimmed.")
   bytes& = DoPrint&(PosCursor$(6, 5))
   reverse% = -1
   CALL PrintImage(trimfile$, reverse%)
                  'Convert the trimmed image to a text file
   textfile$ = "lsr2text.txt"
   CALL Image2Text(trimfile$, textfile$)
                  'Now, it can be edited on any word processor, just make
                  'sure the file is loaded as ASCII format

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)
RETURN

demo.4:     'Converting the ASCII file back again
   textfile$ = "lsr2text.txt"
   filename$ = "demofile.prn"
   handle% = OpenFile%(filename$)
   bytes& = DoPrint&(Resolution$(300) + PosCursor$(3, 1))
   newfile$ = "lsr2new.img"
   CALL Text2Image(textfile$, newfile$)  'Convert from ASCII to bitmap
                  'Now, to make sure, let's print it
   bytes& = DoPrint&(PosCursor$(8, 1))
   reverse% = 0
   CALL PrintImage(newfile$, reverse%)
   bytes& = DoPrint&(PosCursor$(8.5, 1) + "Here is the final product.")
   bytes& = DoPrint&(PosCursor$(8, 2))
   reverse% = -1
   CALL PrintImage(newfile$, reverse%)

   bytes& = DoPrint&(EjectPage$)
   CLOSE handle%
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)
RETURN

demo.5:
   bitmapfile$ = "laser2.img"
   outlinefile$ = "outline.img"
   borderfile$ = "border.img"
   outline2file$ = "outline2.img"
   outline3file$ = "outline3.img"
   knockoutfile$ = "knockout.img"
   s$ = "copy " + bitmapfile$ + " " + outlinefile$
   SHELL s$
   s$ = "copy " + bitmapfile$ + " " + knockoutfile$
   SHELL s$

'---Bolden image once
   bold% = 4
   CALL MergeImage(outlinefile$, bitmapfile$, 0, bold%)
   CALL MergeImage(outlinefile$, bitmapfile$, 0, -bold%)
   s$ = "copy " + outlinefile$ + " " + outline2file$
   SHELL s$
   CALL MergeImage(outlinefile$, outline2file$, bold%, 0)
   CALL MergeImage(outlinefile$, outline2file$, -bold%, 0)
   CALL KnockOut(outlinefile$, bitmapfile$, 0, 0)

'---Bolden image again
   bold% = 2
   s$ = "copy " + bitmapfile$ + " " + outline2file$
   SHELL s$
   CALL MergeImage(outline2file$, bitmapfile$, 0, bold%)
   CALL MergeImage(outline2file$, bitmapfile$, 0, -bold%)
   s$ = "copy " + outline2file$ + " " + outline3file$
   SHELL s$
   CALL MergeImage(outline2file$, outline3file$, bold%, 0)
   CALL MergeImage(outline2file$, outline3file$, -bold%, 0)
   s$ = "copy " + outlinefile$ + " " + borderfile$
   SHELL s$
   CALL KnockOut(borderfile$, outline2file$, 0, 0)
   CALL MergeImage(borderfile$, bitmapfile$, 0, 0)

'---Create output file
   filename$ = "demofile"
   handle% = OpenFile(filename$)
   bytes& = DoPrint(PosCursor(2, 2) + Resolution(300))
   CALL PrintImage(outlinefile$, 0)
   bytes& = DoPrint(PosCursor(4, 2) + Resolution(300))
   CALL PrintImage(borderfile$, 0)
   bytes& = DoPrint(EjectPage)
   CLOSE handle%

'---Send to printer
   CALL FontCopy(filename$, 1, SPACE$(4096), 0)

RETURN
'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
run.demo:
'---Set up demo menu selection
   REDIM choice$(5)
      choice$(1) = "Portrait to Landscape         ณ IMPORTANT: Run these"
      choice$(2) = "Text Image From a Soft Font   ณ Demos in order.  They"
      choice$(3) = "Bitmapped Image to ASCII File ณ need certain files that"
      choice$(4) = "ASCII File Back to Bitmap     ณ will be created."
      choice$(5) = "Font Outline Effect"
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


