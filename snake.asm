[org 0x0100]
jmp start
printstr:
		push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di
		push ds
		pop es ; load ds in es
		mov di, [bp+4] ; point di to string
		mov cx, 0xffff ; load maximum number in cx
		xor al, al ; load a zero in al
		repne scasb ; find zero in the string
		mov ax, 0xffff ; load maximum number in ax
		sub ax, cx ; find change in cx
		dec ax ; exclude null from length
		jz e ; no printing if string is empty
		mov cx, ax ; load string length in cx
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov al, 80 ; load al with columns per row
		mul byte [bp+8] ; multiply with y position
		add ax, [bp+10] ; add x position
		shl ax, 1 ; turn into byte offset
		mov di,ax ; point di to required location
		mov si, [bp+4] ; point si to string
		mov ah, [bp+6] ; load attribute in ah
		cld ; auto increment mode
		nchar: lodsb ; load next char in al
		stosw ; print char/attribute pair
		loop nchar ; repeat for the whole string
		e: pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 8



clrscr:    
    push es
    push ax
    push di

    mov  ax, 0xb800
    mov  es, ax
    mov  di, 0

    nextloc:
        mov  word [es:di], 0x0720
        add  di, 2
        cmp  di, 4000
        jne  nextloc

    pop  di
    pop  ax
    pop  es
    ret

snakeMaking:
    push bp
    mov bp, sp
    push ax
    push bx
    push si
    push cx
    push dx

    mov si, [bp + 6]        ;snake
    mov cx, [bp + 8]        ;length of snake
    mov di, 1680 ; starting location of snake
    mov ax, 0xb800    
    mov es, ax

    mov bx, [bp + 4]
    mov ah, 0x07    ; at start the color of the snake will be grey 
    snake_next_part:
        mov al, [si]02
        mov [es:di], ax
        mov [bx], di
        inc si
        add bx, 2

        add di, 2
        loop snake_next_part

    pop dx
    pop cx
    pop si
    pop bx
    pop ax
    pop bp
    ret 6

; subroutine for moving snake left
moveSnakeLeft:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si
    ;snake_parts colision detection
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]				; move snake location on scren

    mov cx, [bp + 8]  ; snake length 
    sub dx, 2		             ;decrement left screen location while movement of snake
    check_left_colision:
        cmp dx, [bx]             ; conditon to check snake head with screen border
        je noLeftMovement 
        add bx, 2                ;if no then increment
        loop check_left_colision 
    left_movement:
    mov si, [bp + 6]            ;snake
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]
    sub dx, 2 
    mov di, dx ;after left movemnt screen location 

    mov ax, 0xb800
    mov es, ax
    mov ah, 0x04                 ; color after left movement
    mov al, [si]
    mov [es:di],ax             ;snake head placed

    mov cx, [bp + 8]  ; snake length
    mov di, [bx] ; 
    inc si  ; move to next location
    mov ah, 0x04
    mov al, [si] ;moving snake next part in al 
    mov [es:di],ax
    left_location_sort:
        mov ax, [bx]
        mov [bx], dx
        mov dx, ax
        add bx, 2
       
        loop left_location_sort
    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax

    noLeftMovement:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
;SubRoutine for Up movement
moveSnakeUp:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si
     ;snake_parts colision detection
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]

    mov cx, [bp + 8] ; snake lenght;
    sub dx, 160   ;change the position of the snake by one line 
    check_up_colision:
        cmp dx, [bx] ;check snake head
        je no_up_movement
        add bx, 2 ; if no then increment
        loop check_up_colision
    upward_movement:
    mov si, [bp + 6]            ;snake
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]
    sub dx, 160  ; column location
    mov di, dx ; after upward movement screen location

    mov ax, 0xb800
    mov es, ax
    mov ah, 0x04 ; color of the snake after that movemnt
    mov al, [si] ; move snake next part
    mov [es:di],ax             ;snake head placed

    mov cx, [bp + 8] ; snake lenght
    mov di, [bx]
    inc si ; move to next location
    mov ah, 0x04 ;color of the snake after that movemnt
    mov al, [si]
    mov [es:di],ax
    up_location_sort:
        mov ax, [bx]
        mov [bx], dx
        mov dx, ax
        add bx, 2
       
        loop up_location_sort
    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax
    no_up_movement:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
;SubRoutine for Down Movement
moveSnakeDown:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si
     ;snake_parts colision detection
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]

    mov cx, [bp + 8]
    add dx, 160
    check_down_colision:
        cmp dx, [bx]
        je no_down_movement
        add bx, 2
        loop check_down_colision

    downward_movement:
    mov si, [bp + 6]            ;snake
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx] 
    add dx, 160 ; screen column location
    mov di, dx

    mov ax, 0xb800
    mov es, ax
    mov ah, 0x04  ; red color 
    mov al, [si]
    mov [es:di], ax             ;snake head placed

    mov cx, [bp + 8]            ;snake length
    mov di, [bx]
    inc si
    mov ah, 0x04 ;red color 
    mov al, [si] ; move snake next part
    mov [es:di],ax
    down_location_sort: ; sorting snake parts after down movement
        mov ax, [bx]
        mov [bx], dx
        mov dx, ax
        add bx, 2
        loop down_location_sort
    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax

    no_down_movement:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
moveSnakeRight:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si
    ;snake_parts colision detection
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]

    mov cx, [bp + 8] ; snake length
    add dx, 2 ; screen row location
    check_right_colision:
        cmp dx, [bx] ; check snake head with border
        je no_right_movement
        add bx, 2
        loop check_right_colision

    right_movement:
    mov si, [bp + 6]            ;snake
    mov bx, [bp + 4]            ;snake location
    mov dx, [bx]
    add dx, 2   ; screen row location
    mov di, dx

    mov ax, 0xb800
    mov es, ax
    mov ah, 0x04  ; red color
    mov al, [si] ; move snake next part
    mov [es:di], ax             ;snake head placed

    mov cx, [bp + 8]            ;snake length
    mov di, [bx]
    inc si
    mov ah, 0x04
    mov al, [si]
    mov [es:di],ax
    right_location_sort:
        mov ax, [bx]
        mov [bx], dx
        mov dx, ax
        add bx, 2
       
        loop right_location_sort
    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax

    no_right_movement:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
;Moving Snake
controlWithKeys:
   
   
   
    push ax
    push bx

    repeat:
    mov ah,0
    int 0x16
    cmp ah,0x48
    je up
    cmp ah,0x4B
    je left
    cmp ah,0x4D
    je right
    cmp ah,0x50
    je down
	 cmp ah,0x11
    je up
    cmp ah,0x1e
    je left
    cmp ah,0x20
    je right
    cmp ah,0x1f
    je down
	
	
    cmp ah,1
    jne repeat      ; loop until Esc is pressed
    ;Escape check
    mov ah,0x4c
    je exit
    ;UpWard Movement
    up:
        push word [intial_snake_length] 
        mov bx, snake ; moving snake to bx
        push bx  
        mov bx, snake_locations
        push bx  ; snake location
        call moveSnakeUp 
        jmp snakeeatFruit

    down:
        push word [intial_snake_length]
        mov bx, snake ; moving snake to bx
        push bx
        mov bx, snake_locations
        push bx ; pusing snake location
        call moveSnakeDown
        jmp snakeeatFruit

    left:
        push word [intial_snake_length]
        mov bx, snake ; moving snake to bx
        push bx 
        mov bx, snake_locations
        push bx  ; pusing snake location
        call moveSnakeLeft
        jmp snakeeatFruit

    right:
        push word [intial_snake_length]
        mov bx, snake ; moving snake to bx
        push bx 
        mov bx, snake_locations
        push bx  ; pusing snake location
        call moveSnakeRight
        jmp snakeeatFruit
    snakeeatFruit:
        call OutCheck
      
        push word [food_location]  ; location of food
        push word [intial_snake_length]  
        mov bx, snake
        push bx
        mov bx, snake_locations
        push bx
        call updateSizeOnEating  ;
        jmp repeat
    exit:
        pop bx
        pop ax
        ret

printName:
    push bp
    mov  bp, sp
    push es
    push ax
    push cx
    push si
    push di

    mov ax, 0xb800
    mov es, ax
           

    mov di, [bp + 8]
    mov si, [bp + 6]
    mov cx, [bp + 4]
    mov ah, 0x07 

    nextchar1:
        mov al, [si]
        mov [es:di], ax
        add di, 2
        add si, 1
       

        loop nextchar1


    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 6

printNum:
    push bp
    mov  bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

   

    mov ax, [bp+4]  
    mov bx, 10      
    mov cx, 0        

    nextdigit:
        mov dx, 0    
        div bx      
        add dl, 0x30
        push dx      
        inc cx      
        cmp ax, 0    
        jnz nextdigit

   

    mov ax, 0xb800
    mov es, ax

    mov di, [bp + 6]
    nextpos:
        pop dx          
        mov dh, 0x07    
        mov [es:di], dx
        add di, 2
        loop nextpos    

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 4

;display food
display_food:
    push bp
    mov bp, sp
    push ax
    push di
    push es

    mov ax, 0xb800
    mov es, ax
    mov di, [bp + 4]        ;food location
    mov ax, 0x060A ;;food 06 color 0A food
    mov [es:di], ax

    pop es
    pop di
    pop ax
    pop bp
    ret 2
updateSizeOnEating:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push di
    push si

    mov bx, [bp + 4] ; here we have loaction of food 
    mov dx, [bp + 10] ; here we have location of  head of snake  

    cmp [bx], dx
    jne notIncreaseInSize
    ;otherwise
    mov cx, [bp + 8]        ;snake length
    shl cx, 1
    sub cx, 2
    add bx, cx      
    mov dx, [bx]
    sub dx, [bx - 2]     

    mov ax, [bx]
    add ax, dx          
    mov dx, ax
   
    add cx, 2
    shr cx,1
    inc cx
    mov [intial_snake_length], cx

    add bx, 2
    mov [bx], dx
    mov si, [bp + 6]
    inc si

    mov ax, 0xb800
    mov es, ax
    mov di, dx
    mov ah, 0x07
    mov al, [si]

    mov [es:di], ax
   ;increse in size
    add word [score], 2


    push 14  ;location of score
    push word [score]
    call printNum
   
    call RandGen
   
    push word [food_location]
    call display_food

   notIncreaseInSize:
    pop si
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 8
;Random Numebr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RandGen:        
push cx
push dx
push ax
push bx
push si
push di

mov word [temp], 0


RandStart:
   push cx
   mov ah, 00h  ; interrupts to get system time        
   int 1ah      ; cx:dx now hold number of clock ticks since midnight      
               
   pop cx
   mov [temp], dh
   xor dh, dh
   push dx
   xor dx, dx
   mov dl, [temp]
   pop si
   add dx, si
   mov bx, 999   ; set limit to 999
   mov ax, dx
   cmp ax, bx      
   ja RandStart

   mov bx, 0   ; set limit to 0
   mov ax, dx  
   cmp ax, bx
   jb RandStart


   mov cx, [count]
   add dx, cx
   add cx, 1000
   add word [count], 1000
   cmp word [count], 4000
   jne skip1
        mov word [count], 0
        xor cx,cx

   skip1:
   cmp dx, 162
   jb RandStart

   cmp dx, 3680
   ja RandStart

   mov [temp], cx
   mov cx, dx
   shr cx, 1
   jnc move

        add dx, 1

    move:
    mov di, 160              
    mov cx, 23

    check:
        cmp di, dx
        je RandStart

        add di, 158
        cmp di, dx
        je RandStart

        add di, 2
        cmp di, dx
        je RandStart
        loop check


   mov cx, [temp]

   mov word [food_location ], dx

    pop di
    pop si
    pop bx
    pop ax
    pop dx
    pop cx
    ret
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver:
push cx
push dx
push ax
push bx
push si
push di

    call clrscr
    mov dx, 1994
    push dx
    mov dx, Over
    push dx
    push word [OverLength]
    call printName


pop di
pop si
pop bx
pop ax
pop dx
pop cx
mov ax, 0x4c00
int 0x21
ret

OutCheck:
push ax
push di
push cx


    mov ax, [snake_locations]
    cmp ax, 160
    jb END
    mov di, 160              
    mov cx, 22


    check1:
        cmp ax, di
        je END
        add di, 158
        cmp ax, di
        je END
        add di, 2

        loop check1
   
    mov di, 3680
    cmp ax, di
    ja END
    jmp else
     
	 
    END:
    call GameOver
else:
pop cx
pop di
pop ax
ret

;;;;;;;;;;;;;;;;;;;;;;border for start;;;;;;;;
boarder:



	push ax
	mov ax,0xb800
	mov es,ax

	mov di,0
	mov cx, 160
	mov ah,0x4
	mov al,'0'
	upperboarder:
	mov word[es:di],ax
	add di,2
	dec cx
	loop upperboarder
	mov di,3840
	mov cx,160

	lowerboarder:
	mov word[es:di],ax
	add di,2
	dec cx
	loop lowerboarder
	mov di,480
	mov cx,25

	leftboarder:
	mov word[es:di],ax
	add di,160
	loop leftboarder
	mov di,638
	mov cx,25

	rightboarder:
	mov word[es:di],ax
	add di,160
	loop rightboarder
	pop ax
	
	ret


nameOfMemebrs:
    mov ax, 0xb800
    mov es, ax
    call boarder
	mov ah, 0x07
    mov al, 'P'
    mov word [es:2302], ax
    mov al, 'R'
    mov word [es:2304], ax
    mov al, 'E'
    mov word [es:2306], ax
    mov al, 'S'
    mov word [es:2308], ax
    mov al, 'S'
    mov word [es:2310], ax
    mov al, 'A'
    mov word [es:2314], ax
    mov al, 'N'
    mov word [es:2316], ax
    mov al, 'Y'
    mov word [es:2318], ax
    mov al, 'K'
    mov word [es:2322], ax
    mov al, 'E'
    mov word [es:2324], ax
	  mov al, 'Y'
    mov word [es:2326], ax

	
	

mov ah, 0x04
    mov al, '2'
    mov word [es:1826], ax
    mov al, '1'
    mov word [es:1828], ax
    mov al, 'F'
    mov word [es:1830], ax
    mov al, '-'
    mov word [es:1832], ax
    mov al, '9'
    mov word [es:1834], ax
mov al, '2'
    mov word [es:1836], ax
    mov al, '1'
    mov word [es:1838], ax
    mov al, '2'
    mov word [es:1840], ax


mov ah, 0x04
    mov al, '2'
    mov word [es:1986], ax
    mov al, '1'
    mov word [es:1988], ax
    mov al, 'F'
    mov word [es:1990], ax
    mov al, '-'
    mov word [es:1992], ax
    mov al, '9'
    mov word [es:1994], ax
	mov al, '1'
    mov word [es:1996], ax
    mov al, '7'
    mov word [es:1998], ax
    mov al, '9'
    mov word [es:2000], ax
ret


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;start;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;
;;;;;;;
start:
    call clrscr

    mov dx, 1504  ;snake game position
    push dx
    mov dx, welcome_message
    push dx
    push word [welcome_message_length]
    call printName
	
		
	call nameOfMemebrs


    mov ah, 01
    int 0x21
	;;;;;;;;;;;;;;;instructions
    call clrscr

		mov ax, 160
		push ax ; push x position
		mov ax, 0
		push ax ; push y position
		mov ax, 7 ; grey on black attribute
		push ax ; push attribute
		mov ax, rule1
		push ax ; push address of message
		call printstr
		mov ax, 320
		push ax ; push x position
		mov ax, 0
		push ax ; push y position
		mov ax, 7 ; grey on black attribute
		push ax ; push attribute
		mov ax, rule2
		push ax ; push address of message
		call printstr
		mov ax, 480
		push ax ; push x position
		mov ax, 0
		push ax ; push y position
		mov ax, 7 ; grey on black attribute
		push ax ; push attribute
		mov ax, rule3
		push ax ; push address of message
		call printstr
		
	 mov ax, 0xb800
    mov es, ax
  ;;;;wishing good luck
	mov ah, 0x07
    mov al, 'G'
    mov word [es:2302], ax
    mov al, 'O'
    mov word [es:2304], ax
    mov al, 'O'
    mov word [es:2306], ax
    mov al, 'D'
    mov word [es:2308], ax
    mov al, 'L'
    mov word [es:2312], ax
    mov al, 'U'
    mov word [es:2314], ax
    mov al, 'C'
    mov word [es:2316], ax
    mov al, 'K'
    mov word [es:2318], ax

	mov ah, 01
    int 0x21
	
	;;;;;;;;;;;;;;;;;end of instruection;;;;;;;;;;;;;;;;;;
	
	call clrscr

	call boarder
			mov dx, 0  ;display scorecard
			push dx
			mov dx, scoreTitle
			push dx
			push word [scoreLength]
			call printName

			mov dx, 14
			push dx
			mov dx, [score]
			push dx
			call printNum
			
    push word [intial_snake_length]
    mov bx, snake
    push bx
    mov bx, snake_locations
    push bx
    call snakeMaking

    ;call RandGen
 
    push word [food_location]
    call display_food

    call controlWithKeys

    mov ax, 0x4c00
    int 0x21

	snake_locations: dw 0
	welcome_message: db 'Snake Game'
    Over: db 'Game Over'
    OverLength: dw 9
    welcome_message_length: dw 10
    intial_snake_length: dw 4
    scoreTitle: db   'Score = '
    scoreLength:dw   8
    score:      dw   0
    food_location:dw 1950
	rule1: db "1)Control the movement of the snake in the game using keys w,s,d,e or arrow keys",0
	rule2: db "2)Eat the food to increase the size of the snake and to increase score",0
	rule3: db "3)Avoid touching the boundry otherwise snake will die and as a result game will be over",0
	gluck: db "Good Luck",0
    count: dw 0
    temp: dw 0
    snake: db '~','%','%','%'

