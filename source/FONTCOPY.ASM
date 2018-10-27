;----- FONTCOPY.ASM - copies a font file to the specified printer

;Copyright (c) 1990 Crescent Software
;written by Jay Munro
;DOS Error portion by J. R. Conrad
;BASIC 7.0 version
;Syntax - CALL FontCopy(FontFile$, Printer%, Buffer$, FontIDNum%)
;
;  where FontFile$ is the name of the file to copy to the printer,
;  and Printer% is the printer number to use (1,2,etc...),
;  and Buffer$ is a buffer FontCopy uses when transferring the file,
;  and IDNum% tells what number the font will be designated.
;  Errors may be detected with the DOSError and WhichError functions.


.Model Medium, Basic
.Code
    Extrn Setup_CEH:Far, Restore_CEH:Far, Load_DOSErr:Far
    Extrn Load_File:Far, DOS:Far
    Extrn StringLength:Proc
    Extrn StringAddress:Proc
    
FontCopy Proc Uses ES DI SI, File:Ptr, Printer:Word, Buffer:Ptr,IDNum:Ptr

    Mov  CS:SaveDS,DS
    Mov  DX,CS              ;install CEH
    Lea  BX,CS:CEH_Ret
    Call Setup_CEH

    Mov  SI,File            ;SI holds address of FileName$ descriptor
    Call Load_File          ;create an ASCIIZ string
    Jnc  OpenFile           ;quit if an error was detected

ByeBye:
    Jmp  ErrExit

OpenFile:
    Mov  AX,3D00h           ;specify Open File service, read-only operation
    Call DOS                ;call DOS Int 21h
    Jc   ByeBye             ;carry set means an error, otherwise handle in AX

    Mov  CS:Fonthandle,AX   ;put the handle aside for subsequent reads
    Mov  SI,Printer         ;get printer to copy to
    Mov  CX,[SI]            ;move it to CX to use
    And  CX,3               ;only accept ports 1 - 4
    Jcxz OpenPort
    Add  CX,30h             ;make CL = ASCII value of 1 - 4
    Mov  CS:Port,CL         ;this fixes the port definition

OpenPort:
    Mov  DX,offset PrinterPort
    Mov  AX,3D02h           ;open printer port for read/write
    Push CS
    Pop  DS
    Call DOS                ;do it

    Mov  DS,CS:SaveDS
    Jc   ByeBye
    Mov  CS:PrintHandle,AX
    Mov  BX,AX              ;get mode of print handle
    Mov  AX,4400h           ;service 44h, subfunction 0
    Call DOS
    Jc   ByeBye

    Or   DL,100000b         ;force "raw" data mode (don't expand Tabs, etc.)
    Xor  DH,DH              ;DOS requires that we clear DH
    Mov  AX,4401h           ;service 44h, subfunction 1
    Call DOS
    Jc   ByeBye

    Mov  SI,IDNum
    Mov  CX,[SI]
    Or   CX,CX
    Js   ByeBye             ;sorry no numbers greater than 32K
    Push BX                 ;BX = number to convert
    Mov  BX,CX              ;
    Push DS                 ;point DS at CS
    Push CS                 ;
    Pop  DS                 ;
    Mov  DI,Offset FontID   ;point SI at FontID area
    Add  DI,3               ;offset 3
    Call LJStr              ;make string from number in BX
    Pop  DS                 ;retrieve DS and BX
    Pop  BX

LoadBuffer:
    Mov  SI,Buffer          ;get descriptor for buffer
    Push SI                 ;push value twice for both calls to BC7
    Push SI
    Call StringLength
    Mov  CX,AX 
    Call StringAddress     
    Mov  DS,DX
    Mov  DX,AX
    Mov  AX,27
    Jcxz ErrExit
    Mov  DI,CX              ;save CX for later
    Mov  SI,DX

    Mov  CS:SaveDS2,DS
    Mov  BX,CS:PrintHandle  ;Ok so far
    Mov  CX,9
    Mov  DX,Offset FontId
    Push CS
    Pop  DS
    Mov  AH,40h             ;DOS write
    Call DOS
    Mov  DS,CS:SaveDS2
    Jc   ErrExit
    Mov  CX,DI
    Mov  DX,SI

DoRead:
    Mov  BX,CS:FontHandle
    Mov  AH,3Fh             ;specify Read service
    Call DOS                ;call DOS
    Jc   ErrExit            ;oops, found an error

    Cmp  AX,CX              ;check for read past end
    Jz   KeepGoin
    Mov  CX,AX

KeepGoin:
    Mov  BX,CS:PrintHandle  ;now write the buffer
    Mov  AH,40h             ;with DOS's write
    Call DOS
    Jc   ErrExit
    Cmp  DI,AX              ;did we write at least original buffer?
    Jne  SetMode            ;no, so we're done
    Jmp  Short DoRead

SetMode:
    Mov  BX,CS:PrintHandle
    Push CS
    Pop  DS
    Mov  CX,14              ;5 we only use make temp/perm
    Mov  DX, Offset CS:FontID   ;FontControl
    Mov  AH,40h                 ;with DOS's write
    Call DOS
    Jmp  short Exit

ErrExit:
    Mov  DS,CS:SaveDS
    Call Load_DOSErr

CEH_Ret:
    Mov  AX,-1              ;clear the file handle variable

Exit:
    Mov  DS,CS:SaveDS       ;get DS back again
    Mov  BX,CS:FontHandle
    Mov  AH,3Eh             ;close file service
    Call DOS                ;call DOS

    Mov  BX,CS:PrintHandle
    Mov  AH,3Eh             ;close file service
    Call DOS                ;call DOS

    Call Restore_CEH        ;restore BASIC's critical error handler
    Ret                     ;return to BASIC

FontCopy Endp

LJStr Proc Near
    Push ES
    Cld                     ;move in an upward direction
    Push DS                 ;point ES at DS for StoSb
    Pop  ES
    Mov  AL,'0'
    Mov  CX,5
    Rep  StoSb              ;DI will be pointing at end after this
    Or   BX,BX              ;is incoming number a 0 ?
    Jz   Exit               ;yes? then bye bye
    Mov  AX,BX
    Mov  BX,CX              ;CX = 0
    Mov  CX,10              ;set divisor to 10

GetDigit:
    Dec  DI                 ;point DI at correct character
    Inc  BX                 ;register that we have a character
    Xor  DX,DX              ;clear DX to take remainder
    Div  CX                 ;divide AX (Low word)
    Add  DL,30h             ;make it an ASCII digit from remainder
    Mov  [DI],DL            ;put it in our string
    Or   AX,AX              ;is high word zero?
    Jnz  GetDigit           ;no keep going

Exit:
    Pop  ES
    Ret

LJStr Endp


    SaveDS         DW ?
    SaveDS2        DW ?
    FontHandle     DW ?
    PrintHandle    DW ?

    PrinterPort DB 'LPT'            ;first part of 'LPTx',0
    Port   Label byte               ;define next address of data as a label
                DB '1',0            ;the '1' is what changes. . .

    FontID         DB 27,'*c00000D' ;set for font 1 default

    FontControl    DB 27,'*c5F'
                   DB 27,'(00000X'  ;set for temporary default

End
