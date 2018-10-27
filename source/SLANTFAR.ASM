               title     slant
               .model    medium,BASIC
               .data  
filehandle     dw        0
linetint       dw        100
bytecount      dw        2 dup (0)
voffset        dw        0                   ;vertical offset, decipoints
hoffset        dw        0                   ;horizontal offset, decipoints
               .code  
               extrn     StringInfo:proc

               public    putoffsets
putoffsets     proc      far
               push      bp                  ;save base pointer
               mov       bp,sp               ;point to stack frame
               mov       ax,[bp+8]           ;get vertical offset from stack
               mov       [voffset],ax        ;save it
               mov       ax,[bp+6]           ;get horiz. offset from stack
               mov       [hoffset],ax        ;save it
               pop       bp
               ret       4
putoffsets     endp

               public    getoffsets
getoffsets     proc      far
               push      bp
               mov       bp,sp
               mov       si,[bp+8]           ;point to BASIC vert. offset
               mov       di,[bp+6]           ;point to BASIC horiz. offset
               pop       bp
               mov       ax,[voffset]        ;retrieve vertical offset
               mov       [si],ax             ;insert into BASIC
               mov       ax,[hoffset]        ;retrieve horizontal offset
               mov       [di],ax             ;insert into BASIC
               ret       4
getoffsets     endp

               public    putlinetint
putlinetint    proc      far
               push      bp
               mov       bp,sp
               mov       ax,[bp+6]           ;passed line tint value
               pop       bp
               mov       [linetint],ax       ;save it
               ret       2
putlinetint    endp

               public    getlinetint
getlinetint    proc      far
               push      bp
               mov       bp,sp
               mov       si,[bp+6]
               pop       bp
               mov       ax,[linetint]
               mov       [si],ax
               ret       2
getlinetint    endp

               public    putcount 
putcount       proc      far
               push      bp
               mov       bp,sp
               mov       si,[bp+6]           ;pointer to passed byte count
               pop       bp
               mov       ax,[si]             ;get high word
               mov       [bytecount],ax      ;save it
               mov       ax,[si+2]           ;get low word
               mov       [bytecount+2],ax    ;save it
               ret       2
putcount       endp

               public    getcount 
getcount       proc      far
               push      bp
               mov       bp,sp
               mov       si,[bp+6]           ;point to BASIC variable
               pop       bp
               mov       ax,[bytecount]
               mov       [si],ax             ;insert high word
               mov       ax,[bytecount+2]
               mov       [si+2],ax           ;insert high word
               ret       2
getcount       endp

               public    puthandle
puthandle      proc      far
               push      bp
               mov       bp,sp
               mov       ax,[bp+6]
               mov       [filehandle],ax
               pop       bp
               ret       2
puthandle      endp

               public    gethandle
gethandle      proc      far
               push      bp
               mov       bp,sp
               mov       si,[bp+6]
               pop       bp
               mov       ax,[filehandle]
               mov       [si],ax
               ret       2
gethandle      endp

               public    slant
slant          proc      far  
               push      bp  
               mov       bp,sp  
               mov       dx,[bp+8]           ;extra dots into dx
               mov       si,[bp+6]           ;buffer string  
               pop       bp

               push      dx                  ;save shift value
               call      StringInfo
               mov       si,ax
               mov       ds,dx
               pop       dx                  ;retrieve shift value
  
               mov       bx,0                ;initialize bx  
sloop:         push      cx                  ;save byte counter  
               mov       ah,0  
               mov       al,[si]             ;load current byte  
               push      ax                  ;save original value  
               mov       cx,dx               ;set up cx w/extra dots  
               shr       ax,cl               ;move bits  
               add       ax,bx               ;add previous remainder  
               mov       [si],al             ;put back in string  
               pop       ax                  ;get original value  
               mov       cx,8                ;set up cx  
               sub       cx,dx               ;left shift value (8 - extra) 

               shl       ax,cl               ;compute remainder in al  
               mov       bl,al               ;transfer to bl  
               mov       bh,0                ;zero msb  
               pop       cx                  ;restore byte counter  
               inc       si                  ;point to next byte  
               loop      sloop               ;loop until done  
               ret       4                   ;reset stack
slant          endp
               end