IDEAL
MODEL small
STACK 100

;TO-DO:
;1. fix the computation of the distance vector

;Variables
DATASEG

   posMat dw 10 dup(0) ;position matrix of all the planets. contatined as (x1, y1, x2, y2, x3, y3 ... )
                      ;max number of planets is 10/2 = 5
   velMat dw 10 dup(0) ;velocity matrix of all the planets. contained the same way as the position matrix
   massMat dw 5 dup(0)  ;Mass matrix for all the olanets conttained as a vector
   accMat dw 10 dup(0) ;acceleration matrix of all the planets. contained the same way as the posMat
   numPlanets db 0     ;current number of planets, ds:0041
   G dw 1h ;the gravitational constant
   lastPosMat dw 10 dup(0)
   scaleVelocityDown dw 10
   scaleForceUp dw 500
   scaleForceDown dw 6000
   mindistance dw 7
   BoomTxt db 'Boom!', '$'

   ;placeholders:
   address dw ?
   retAddress dw ?
   a_x dw ?
   a_y dw ?
   v_x dw ?
   v_y dw ?
   aAbs dw ?
   cPlanet dw 0
   otherPlanet dw 0
   sr dd ? ;a temp var
   distanceVector dd ?
   rx dw 0
   ry dw 0
   tempNum dw 0
   m dw 0
   n dd ?
   nd dw ? ;n divided by 2
   num dw ?
   prevNum dw ?
   distance dw ?
   miliseconds db ?
   ticks dw 115   ;used for debugging
   mG dd ?

;Code
CODESEG

   x equ [bp + 8]
   y equ [bp + 6]
   color equ [bp + 4]

;signed division of ax by a positive number
;push num
proc signedDiv
   push bp
   mov bp, sp

   mov bx, [bp + 4]

   cmp ax, 0
      jge numPos
      jl  numNeg
   numPos:
      mov dx, 0
      div bx
      jmp afterDIV
   
   numNeg:
      neg ax
      mov dx, 0
      div bx
      neg ax

   afterDIV:
   pop bp
   ret 2
endp signedDiv

;takes the square root of a number and inserts it into ax floored
;push dword
proc squareRoot
   pop [retAddress]
   pop [n]

   mov bx, offset n

   mov ax, [bx]
   mov dx, [bx + 2]
   mov bx, 2
   div bx

   mov [nd], ax
   mov cx, [nd]
   mov ax, [bx]
   mov [num], ax
   newtons_method:
      ;x = (x + n/x)/2 = (x/2 + n/2x) = x/2 + nd/x
      mov ax, [nd]
      xor dx, dx
      div [num]
      push ax ;nd/x

      mov ax,[num]
      xor dx, dx
      mov bx, 2
      div bx ;ax = x/2
      pop bx ;bx = nd/x

      add ax, bx
      mov [num], ax
   loop newtons_method
   mov ax,[num]
   push [retAddress]
   ret
endp squareRoot

;print at x, y, in , color
;push x
;push y
;push color
;320x200
proc print
   push bp
   mov bp, sp

   ; Print red dot
   xor bx, bx
   mov cx,  x
   mov dx,  y
   mov al,  color
   mov ah,  0ch
   int 10h


   pop bp
   ret 6
endp print

;print cross inorder to represent the place of the planet (temporary)
;push x
;push y
proc printRedCross
   push bp
   mov bp, sp

   mov ax, [bp + 6]
   mov bx, [bp + 4]

   mov [rx], ax
   mov [ry], bx
   push [rx]    ;x
   push [ry]    ;y
   push 4      ;color
   call print
   add [rx], 1
   push [rx]
   push [ry]
   push 4
   call print
   sub [rx], 2
   push [rx]
   push [ry]
   push 4
   call print
   add [rx], 1
   add [ry], 1
   push [rx]
   push [ry]
   push 4
   call print
   sub [ry], 2
   push [rx]
   push [ry]
   push 4
   call print

   pop bp
   ret 6
endp printRedCross

;print cross inorder to clear the screen
;push x
;push y
proc printBlackCross
   push bp
   mov bp, sp

   mov ax, [bp + 6]
   mov bx, [bp + 4]

   mov [rx], ax
   mov [ry], bx
   push [rx]    ;x
   push [ry]    ;y
   push 0      ;color
   call print
   add [rx], 1
   push [rx]
   push [ry]
   push 0
   call print
   sub [rx], 2
   push [rx]
   push [ry]
   push 0
   call print
   add [rx], 1
   add [ry], 1
   push [rx]
   push [ry]
   push 0
   call print
   sub [ry], 2
   push [rx]
   push [ry]
   push 0
   call print

   pop bp
   ret 6
endp printBlackCross


;adds a planet to posMat
;push starting x
;push starting y
;;push offset posMat
proc addPlanetPos
   push bp
   mov bp, sp

   ;compute the position of which the new planet would be added to the matrix
   mov ax, 4
   mul [numPlanets]
   mov cx, [bp + 6] ;x
   mov dx, [bp + 4] ;y
   mov bx, offset posMat
   add bx, ax
   mov [bx], cx
   mov [bx + 2], dx

   ;inc [numPlanets]
   ;draw
   ;push [bp + 8]
   ;push [bp + 6]
   ;call printRedCross

   pop bp
   ret 6
endp addPlanetPos

;adds a planet to VelMat
;push starting vx
;push starting vy
;push offset velMat
proc addPlanetVel
   push bp
   mov bp, sp

   mov ax, 4
   mul [numPlanets]
   mov cx, [bp + 6] ;vx
   mov dx, [bp + 4] ;vy
   mov bx, offset velMat
   add bx, ax
   mov [bx], cx
   mov [bx + 2], dx

   pop bp
   ret 6
endp addPlanetVel

mass equ [bp + 4]

;adds the planet mass to the massMat
;push mass
proc addPlanetMass
   push bp
   mov bp, sp

   mov bx, offset massMat
   mov al, 2
   mul [numPlanets]
   add bx, ax
   mov ax, mass
   mov [bx], ax

   pop bp
   ret 6
endp addPlanetMass

;adds a planet to all the matrices
;push x_pos
;push y_pos
;push x_vel
;push y_vel
;push mass
proc addPlanet
   pop [retAddress]
   pop [m]
   pop [v_y]
   pop [v_x]
   pop [ry]
   pop [rx]

   push [m]
   call addPlanetMass

   push [v_x]
   push [v_y]
   call addPlanetVel

   push [rx]
   push [ry]
   call addPlanetPos

   inc [numPlanets]

   push [retAddress]
   ret
endp addPlanet

;draw the currnet position of each planet
proc drawPlanets
   pop [retAddress]
   mov ch, 0
   mov cl, [numPlanets]
drawplanet:
   mov bh, 0
   mov bl, [numPlanets]
   sub bx, cx
   mov al, 4
   mul bl
   mov bx, ax
   add bx, offset posMat
   mov [tempNum], cx
   push [bx]
   push [bx + 2]
   call printRedCross
   mov cx, [tempNum]
   loop drawplanet
   push [retAddress]
   ret
endp drawPlanets

;clear the screen
proc clearScreen
   pop [retAddress]
   mov ch, 0
   mov cl, [numPlanets]
clearplanet:
   mov bh, 0
   mov bl, [numPlanets]
   sub bx, cx
   mov al, 4
   mul bl
   mov bx, ax
   add bx, offset lastPosMat
   mov [tempNum], cx
   push [bx]
   push [bx + 2]
   call printBlackCross
   mov cx, [tempNum]
loop clearplanet
   push [retAddress]
   ret
endp clearScreen

;add to [a] the acceleration created by planet number [otherPlanet]
proc sumAcceleration
      pop [address]
      mov bl, [numPlanets]
      mov bh, 0
      sub bx, cx
      mov [otherPlanet], bx ;number of the other planet

      cmp bx, dx
      je samePlanetmid ;if the planets are the same, skip the computation

   ;compute the distance between the two current planets
      mov di, offset posMat
      mov ax, 4
      mul [cPlanet]
      mov bx, ax
      mov ax, [di + bx] ;x position of the planet which the force is computed on
      push ax

      mov ax, 4
      mul [otherPlanet]
      mov bx, ax
      mov bx, [di + bx] ;x position of the planet the applies the force
      pop ax

      jmp afterSamePlanetmid
samePlanetmid:
      jmp samePlanet          ;jumping shortcut for the loop
afterSamePlanetmid:

      sub bx, ax
      mov [rx], bx ;x component of the distance vector
      mov ax, [rx]
      imul ax
      mov bx, offset sr
      mov [bx], ax
      mov [bx + 2], dx

      mov di, offset posMat
      mov ax, 4
      mul [cPlanet]
      mov bx, ax
      mov ax, [di + bx + 2] ;y position of the planet which the force is computed on
      push ax

      mov ax, 4
      mul [otherPlanet]
      mov bx, ax
      mov bx, [di + bx + 2] ;y position of the planet the applies the force
      pop ax

      sub bx, ax
      mov [ry], bx ;y component of the distance vector
      mov ax, [ry]
      imul ax
      mov bx, offset sr
      add [bx], ax
      ;mov ax, 100h
      ;mul dx
      add [bx + 2], dx ;with the pythagarian theorom we can see that this is the squared distance between the
      ;two planets

   ;compute the acceleration created by the planet "other planet"
      mov di, offset massMat
      mov bx, [otherPlanet]
      mov ax, 2
      mul bl
      mov bx, ax
      mov ax, [di + bx]
      mov [m], ax

      push cx
      push [sr]
      call squareRoot
      mov [distance], ax
      pop cx

      mov ax, [mindistance] 
      cmp [distance], ax
      jg disIsOk

      mov dx, offset BoomTxt
      mov ah, 9
      int 21h

      ;wait for press
      mov ax, 0
      int 16h

      ;exit graphics mode
      mov ax, 3
      int 10h
      
      jmp exit

      

   disIsOk:
      mov ax, [m]
      mov dx, 0
      mul [G]     ;dx:ax = mG

      ;mov ax, [distance]
      ;imul [distance]
      ;imul [distance]
      ;mov bx, ax

      div [distance]
      mov dx, 0
      div [distance]
      mov dx, 0
      mul [scaleForceUp]
      div [distance]
      mov [aAbs], ax ;aAbs = mG/r^2

      ;a_x
      mov ax, [rx]
      ; mov dx, 0
      ; push [distance]
      ; call signedDiv

      mov bx, [aAbs]
      imul bx
      mov dx, 0
      push [scaleForceDown]
      call signedDiv
      add [a_x], ax

      ;a_y
      mov ax, [ry]
      ; mov dx, 0
      ; push [distance]
      ; call signedDiv

      mov bx, [aAbs]
      imul bx
      mov dx, 0
      push [scaleForceDown]
      call signedDiv
      add [a_y], ax

      mov di, offset accMat
      mov ax, [cPlanet]
      mov bx, 4
      mul bx

      mov bx, ax
      mov ax, [a_x]
      mov [di + bx], ax
      mov ax, [a_y]
      mov [di + bx + 2], ax


      ;a = mG/r^3*(rx, ry)

      ;x_i+1 = x_i + v
   samePlanet:
   push [address]
   ret
endp sumAcceleration

;a general procedure created for the long jump at the ComputeNexttick loop
proc computeNextTickLoopCode
      mov [a_x], 0
      mov [a_y], 0
      mov dl, [numPlanets]
      mov dh, 0
      sub dx, cx ;the number of the current planet
      mov [cPlanet], dx

      push cx ;save cx inorder to do a nested loop
      mov cl, [numPlanets]
      mov ch, 0
      sumAccelerations: ;loop over all the other planets
         push dx
         push cx
         call sumAcceleration
         pop cx
         pop dx
      loop sumAccelerations
      pop cx

   ;update the position according to the velocity
      mov di, offset posMat
      mov si, offset velMat

      mov ax, [cPlanet]
      mov bx, 4
      mul bx
      mov bx, ax

      mov dx, [di + bx] ;x component
      mov ax, [si + bx] ;v_x component
      push bx
      push dx
      push [scaleVelocityDown]
      call signedDiv
      pop dx
      pop bx
      add ax, dx
      cmp ax, 320
      jge xToBig
      jmp xNotBig
   xToBig:
      sub ax, 320
   xNotBig:
      cmp ax, 0
      jl xToSmall
      jmp xIsNormal
   xToSmall:
      add ax, 320
   
   xIsNormal:
      mov [di + bx], ax

      mov dx, [di + bx + 2] ;y component
      mov ax, [si + bx + 2] ;v_y component
      push dx
      push bx
      push [scaleVelocityDown]
      call signedDiv
      pop bx
      pop dx
      add ax, dx
      cmp ax, 200
      jge yToBig
      jmp yNotBig
   yToBig:
      sub ax, 200
   yNotBig:
      cmp ax, 0
      jl  yToSmall
      jmp yIsNormal
   yToSmall:
      add ax, 200

   yIsNormal:
      mov [di + bx + 2], ax

   ;update the velocity according to the acceleration
      mov di, offset velMat
      mov si, offset accMat

      mov ax, [cPlanet]
      mov bx, 4
      mul bx
      mov bx, ax

      mov dx, [di + bx] ;v_x component
      mov ax, [si + bx] ;a_x component
      ; mul [scaleVelocityDown]
      ; push [scaleVelocityDown]
      ; call signedDiv
      add ax, dx
      mov [di + bx], ax

      mov dx, [di + bx + 2] ;v_y component
      mov ax, [si + bx + 2] ;a_y component
      ; mul [scaleVelocityDown]
      ; push [scaleVelocityDown]
      ; call signedDiv
      add ax, dx
      mov [di + bx + 2], ax
      ret
endp computeNextTickLoopCode

;compute the next position of each planet according to the Newton's laws of Gravity
proc computeNextTick
   mov cl, [numPlanets]
   mov ch, 0
computeNextTickLoop: ;loop over current planet (cplanet)
   push cx
   call computeNextTickLoopCode
   pop cx
loop computeNextTickLoop
      ret
endp computeNextTick

;waits one tick
proc waitOneTick
   mov ah, 2ch
   mov al, 0
   int 21h
   mov cx, 1
   mov [miliseconds], dl
checkTickProggression:
   push cx
   mov ah, 2ch
   mov al, 0
   int 21h
   pop cx

   cmp [miliseconds], dl
   jne TickProgress
   inc cx
TickProgress:
   loop checkTickProggression
   ret
endp waitOneTick

;stops the simulation
proc stop
keepChecking:
   ;check if a key was pressed
   in al, 64h
   cmp al, 10b
   je keepChecking

   ;check if the space key was pressed
   in al, 60h
   cmp al, 1Ch
   jne keepChecking
stopStopping:
   ret
endp stop

start:
   mov ax, @data
   mov ds, ax

   ;enter graphics mode
   mov ax, 13h
   int 10h

   push 160 ;x
   push 100 ;y
   push 0   ;vx
   push 0  ;vy
   push 44000   ;mass
   call addplanet

   push 110
   push 100
   push 0
   push 10
   push 1
   call addPlanet

   push 50
   push 100
   push 1
   push 0
   push 34000
   call addplanet

   call drawPlanets

   mov cx, [ticks]

Update:
      mov [ticks], cx

      ;check if a key was pressed
      in al, 64h
      cmp al, 10b
      je keyNotPressed

      ;check if the space key was pressed
      in al, 60h
      cmp al, 0B9h
      jne notSpace

      call stop

   notSpace:
      ;check if the esc key was pressed
      cmp al, 1h
      je exit
      

   keyNotPressed:

      call waitOneTick

   ;mov [lastPosMat], [posMat]
      push cx
      mov cx, 10
      mov di, offset posMat
      mov si, offset lastPosMat

   movEachComponent:
      mov bx, 10
      sub bx, cx
      mov ax, 2
      mul bl
      mov bx, ax
      mov ax, [di + bx]
      mov [si + bx], ax
   loop movEachComponent
      pop cx

      call computeNextTick

      call clearScreen

      call drawPlanets

      mov cx, [ticks] ;used for debugging
      inc cx
loop Update

   mov dl, 'd'
   mov ah, 2h
   int 21h

   ;wait for press
   mov ax, 0
   int 16h

exit:

   ;exit graphics mode
   mov ax, 3
   int 10h

   mov ax, 4c00h
   int 21h
END start
