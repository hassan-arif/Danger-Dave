;Danger Dave

;created by..
;Hassan Mahmood
;& Hafiz Uzair Warsi

;INDEX is given at the end of this code

[org 100h]
jmp main

;------------------------------------------------------------------------
;subroutine which adds delay
sleep:	push bx
		push cx
		
		mov bx, 3
	turns:
		mov cx, 0xffff
		delay: loop delay
		sub bx, 1
		jnz turns	
		
		pop cx
		pop bx
		ret
;------------------------------------------------------------------------
;subroutine that clears screen
clrscr:	push es
		push di
		push ax
		push cx
		
	mov ax, 0xb800
	mov es, ax
	xor di, di
	mov ax, 0x0720
	mov cx, 2000
	cld					;DF=0	(forward)
	rep stosw			;mov [es:di], ax for cx times. increment di by 2 on each loop
		
		pop cx
		pop ax
		pop di
		pop es
		ret
;------------------------------------------------------------------------		
;subroutine that calculates string's size				stack order:	di [bp+4], es [bp+6]
strlen:	push bp
		mov bp, sp
		push es
		push di
		push cx

	les di, [bp+4]		;mov es, [bp+6]   and   mov di, [bp+4]
	mov cx, 0xffff
	xor al, al
	repne scasb			;compares al with es:di and decrements cx until it finds 0 in string.	note: repne repeats instruction till ZF remains 0
	mov ax, 0xffff
	sub ax, cx
	dec ax
		
		pop cx
		pop di
		pop es
		pop bp
		ret 4	; di, es
;------------------------------------------------------------------------
;subroutine that prints string on desired location with desired attributes			stack order:	msg [bp+4], attribute [bp+6], left [bp+8], top [bp+10]	
printstr:	push bp
			mov bp, sp
			push es
			push ax
			push cx
			push di
			push si
	
	mov ax, 0xb800
	mov es, ax
	mov al, 80
	mul byte [bp+10]
	add ax, [bp+8]
	shl ax, 1
	mov di, ax
	mov si, [bp+4]
	
	push ds
	push word [bp+4]
	call strlen			;returns with string length in ax
	cmp ax, 0
	je exitprintstr
	
	mov cx, ax
	mov ah, [bp+6]
	cld
	printnextchar:	lodsb					;mov al, [ds:si]		si+=1
					stosw					;mov [es:di], ax		di+=2
					loop printnextchar								;--cx
	
	exitprintstr:	pop si
					pop di
					pop cx
					pop ax
					pop es
					pop bp
					ret 8	;msg, attribute, left, top
;------------------------------------------------------------------------
;subroutine to print number passed as parameter from main on desired location
printnum:	push bp
		mov bp, sp
		push es
		push ax
		push bx
		push cx
		push dx
		push di

		mov ax, 0xb800
		mov es, ax
		mov ax, [bp+4]
		mov bx, 10	;use base 10 for division
		mov cx, 0	;initialize count for digits

nextdigit:	mov dx, 0
		div bx
		add dl, 0x30	;+48 to convert into ascii
		push dx
		inc cx
		cmp ax, 0
		jnz nextdigit

		mov al, 80
		mul byte [bp+8]
		add ax, [bp+6]
		shl ax, 1
		mov di, ax
nextpos:	pop dx
		mov dh, 0x02
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
		ret 6
;------------------------------------------------------------------------
;subroutine to draw horizontal line				stack-order:	right [bp+4], left [bp+6], top [bp+8], character with attribute [bp+10]
drawhorizontalline:		push bp
						mov bp, sp
						push es
						push ax
						push cx
						push di
			
			mov ax, 0xb800
			mov es, ax
			
			mov al, 80
			mul byte [bp+8]
			add ax, [bp+6]
			shl ax, 1
			mov di, ax
			
			mov ax, [bp+10]
			mov cx, [bp+4]
			sub cx, [bp+6]
			
			;rep stosw
			nexthorizontalpoint:	mov [es:di], ax
									add di, 2
									loop nexthorizontalpoint
						
						pop di
						pop cx
						pop ax
						pop es
						pop bp
						ret 8	;right, left, top, character with attribute
;------------------------------------------------------------------------
;subroutine to draw filled rectangle			stack-order:	right [bp+4], left [bp+6], bottom [bp+8], top [bp+10], character with attribute [bp+12]
drawrect:				push bp
						mov bp, sp
						push es
						push ax
						push cx
						push di
	
	mov cx, [bp+8]
	sub cx, [bp+10]
	
	mov di, [bp+10]
	sub di, 1
	
	nextrectline:	push word [bp+12]
					add di, 1
					push di
					push word [bp+6]
					push word [bp+4]
					call drawhorizontalline
					loop nextrectline
						
						pop di
						pop cx
						pop ax
						pop es
						pop bp
						ret 10	;right, left, bottom, top, character with attribute
;------------------------------------------------------------------------
;INTRO SCREEN SUBROUTINES...
;------------------------------------------------------------------------
;subroutine to draw crown on screen
drawcrown:	push bp
			mov bp, sp
			push es
			push di
			push ax
			push cx
	mov ax, 0xb800
	mov es, ax
	
	mov al, 80
	mul byte [bp+6]
	add ax, [bp+4]
	shl ax, 1
	mov di, ax
	
	mov ax, 0x6020
	mov cx, 3
	drawcrown_dottedline:	mov [es:di], ax
							add di, 4
							loop drawcrown_dottedline
	mov cx, 5
	sub di, 12
	add di, 160
	drawcrown_bottomline:	mov [es:di], ax
							add di, 2
							loop drawcrown_bottomline
			pop cx
			pop ax
			pop di
			pop es
			pop bp
			ret 4
;------------------------------------------------------------------------
;subroutine to draw diamond on screen
drawstylishdiamond:
				push bp
				mov bp, sp
				push es
				push di
				push ax
				push cx			
	mov ax, 0xb800
	mov es, ax
	
	mov al, 80
	mul byte [bp+6]
	add ax, [bp+4]
	shl ax, 1
	mov di, ax
	add di, 10
	
	mov cx, 5
	push cx
	mov ax, 0x3020
	
	stylishdiamond_nextline:
		pop cx
		cmp cx, 0xFFFF
		je exitstylishdiamond
		sub di, cx
		sub di, cx
		sub cx, 2
		push cx
		add cx, 2
		
	stylishdiamondlines:	
					mov [es:di], ax
					add di, 2
					loop stylishdiamondlines
					sub di, 2
					add di, 160
					jmp stylishdiamond_nextline
				
exitstylishdiamond:	
				pop cx
				pop ax
				pop di
				pop es
				pop bp
				ret 4
;------------------------------------------------------------------------
printstage1:
	push ax
		;-----------------TITLE-----------------
		
		mov ax, 0x6020	;attribute	[bp+12]			;orange rectangle
		push ax
		mov ax, 1		;top		[bp+10]
		push ax
		mov ax, 3		;bottom		[bp+8]
		push ax
		mov ax, 33		;left		[bp+6]
		push ax
		mov ax, 47		;right		[bp+4]
		push ax
		call drawrect
		
		mov ax, 1		;y coordinate				;DANGER
		push ax
		mov ax, 37		;x coordinate
		push ax
		mov ax, 0xE0
		push ax
		mov ax, title1
		push ax
		call printstr
		
		mov ax, 2		;y coordinate				;DAVE
		push ax
		mov ax, 38		;x coordinate
		push ax
		mov ax, 0xE0
		push ax
		mov ax, title2
		push ax
		call printstr
		
		;-----------------CREDITS-----------------
		
		mov ax, 4		;y coordinate				;first group member
		push ax
		mov ax, 26		;x coordinate
		push ax
		mov ax, 0x07
		push ax
		mov ax, credit1
		push ax
		call printstr
		
		mov ax, 5		;y coordinate				;second group member
		push ax
		mov ax, 24		;x coordinate
		push ax
		mov ax, 0x07
		push ax
		mov ax, credit2
		push ax
		call printstr
		
		;-----------------FIGURE-----------------
		
		mov ax, 0x4020	;attribute	[bp+12]		;left vertical long bar
		push ax
		mov ax, 7		;top		[bp+10]
		push ax
		mov ax, 19		;bottom		[bp+8]
		push ax
		mov ax, 10		;left		[bp+6]
		push ax
		mov ax, 13		;right		[bp+4]
		push ax
		call drawrect
		
		mov ax, 0x4020	;attribute	[bp+12]		;left vertical small bar
		push ax
		mov ax, 7		;top		[bp+10]
		push ax
		mov ax, 14		;bottom		[bp+8]
		push ax
		mov ax, 18		;left		[bp+6]
		push ax
		mov ax, 21		;right		[bp+4]
		push ax
		call drawrect
		
		mov ax, 0x4020	;attribute	[bp+12]		;top horizontal long bar
		push ax
		mov ax, 7		;top		[bp+10]
		push ax
		mov ax, 9		;bottom		[bp+8]
		push ax
		mov ax, 18		;left		[bp+6]
		push ax
		mov ax, 45		;right		[bp+4]
		push ax
		call drawrect
		
		mov ax, 0x4020	;attribute	[bp+12]		;upper right small horzontal bar
		push ax
		mov ax, 7		;top		[bp+10]
		push ax
		mov ax, 9		;bottom		[bp+8]
		push ax
		mov ax, 52		;left		[bp+6]
		push ax
		mov ax, 58		;right		[bp+4]
		push ax
		call drawrect
		
		mov ax, 0x4020	;attribute	[bp+12]		;middle horizontal bar
		push ax
		mov ax, 12		;top		[bp+10]
		push ax
		mov ax, 14		;bottom		[bp+8]
		push ax
		mov ax, 30		;left		[bp+6]
		push ax
		mov ax, 58		;right		[bp+4]
		push ax
		call drawrect
		
		mov ax, 0x4020	;attribute	[bp+12]		;bottom horizontal bar
		push ax
		mov ax, 17		;top		[bp+10]
		push ax
		mov ax, 19		;bottom		[bp+8]
		push ax
		mov ax, 10		;left		[bp+6]
		push ax
		mov ax, 58		;right		[bp+4]
		push ax
		call drawrect
		
		mov ax, 0x4020	;attribute	[bp+12]		;right vertical bar
		push ax
		mov ax, 7		;top		[bp+10]
		push ax
		mov ax, 19		;bottom		[bp+8]
		push ax
		mov ax, 65		;left		[bp+6]
		push ax
		mov ax, 68		;right		[bp+4]
		push ax
		call drawrect
		
		;-----------------ELEMENTS-----------------
		
		mov ax, 12		;y coordinate			;middle crown
		push ax
		mov ax, 23		;x coordinate
		push ax
		call drawcrown
		
		mov ax, 7		;y coordinate			;top diamond
		push ax
		mov ax, 46		;x coordinate
		push ax
		call drawstylishdiamond
		
		mov ax, 11		;y coordinate			;right diamond
		push ax
		mov ax, 59		;x coordinate
		push ax
		call drawstylishdiamond
		
		;-----------------RULES-----------------
		
		mov ax, 20		;y coordinate			;rules heading
		push ax
		mov ax, 10		;x coordinate
		push ax
		mov ax, 0x02
		push ax
		mov ax, heading
		push ax
		call printstr
		
		mov ax, 21		;y coordinate			;rest are all points
		push ax
		mov ax, 10		;x coordinate
		push ax
		mov ax, 0x07
		push ax
		mov ax, point1
		push ax
		call printstr
		
		mov ax, 22		;y coordinate
		push ax
		mov ax, 15		;x coordinate
		push ax
		mov ax, 0x07
		push ax
		mov ax, point2
		push ax
		call printstr
		
		mov ax, 23		;y coordinate
		push ax
		mov ax, 15		;x coordinate
		push ax
		mov ax, 0x07
		push ax
		mov ax, point3
		push ax
		call printstr
		
		mov ax, 24		;y coordinate
		push ax
		mov ax, 10		;x coordinate
		push ax
		mov ax, 0x07
		push ax
		mov ax, point4
		push ax
		call printstr
		
	pop ax
	ret
;------------------------------------------------------------------------
;IN GAME SUBROUTINES...
;------------------------------------------------------------------------
;subroutine to draw cup on screen			stack order:	left [bp+4], top [bp+6]
drawcup:	push bp
			mov bp, sp
			push es
			push di
			push ax
			push cx	
	mov ax, 0xb800
	mov es, ax
	
	mov al, 80
	mul byte [bp+6]
	add ax, [bp+4]
	shl ax, 1
	mov di, ax
	mov ah, 0x6F	;white on orange
	
	mov al, 0x5C	;'\'
	mov [es:di], ax
	add di, 2
	
	mov al, 0x76	;'v'
	mov [es:di], ax
	add di, 2
	
	mov al, 0x2F	;'/'
	mov [es:di], ax
	add di, 158
	
	mov al, 0x7C	;'|'
	mov [es:di], ax
			pop cx
			pop ax
			pop di
			pop es
			pop bp
			ret 4	;left, top
;------------------------------------------------------------------------
;subroutine to draw diamond on screen
drawdiamond:	push bp		;stack order: x [bp+4], y [bp+6]
				mov bp, sp
				push es
				push di
				push ax
				push cx
	mov ax, 0xb800
	mov es, ax
	mov al, 80
	mul byte [bp+6]
	add ax, [bp+4]
	shl ax, 1
	mov di, ax
	
	mov ax, 0x3F2A	;white asterisk on aqua background
	mov cx, 3
	rep stosw		;stosw-> mov [es:di], ax		rep-> repeats cx times
	
	add di, 156
	mov [es:di], ax
				
				pop cx
				pop ax
				pop di
				pop es
				pop bp
				ret 4	;x, y
;------------------------------------------------------------------------
;subroutine to draw dave's entry door on screen
drawopening:	push bp		;stack order:	x [bp+4], y[bp+6]
				mov bp, sp
				push es
				push di
				push ax
				push cx
	mov ax, 0xb800
	mov es, ax
	mov al, 80
	mul byte [bp+6]
	add ax, [bp+4]
	shl ax, 1
	mov di, ax
	
	mov ax, 0x70B0	;black on white
	;1st line
	mov cx, 4
	rep stosw		;stosw-> mov [es:di], ax		rep-> repeats cx times
	;2nd line
	add di, 152
	mov cx, 3
	rep stosw		;stosw-> mov [es:di], ax		rep-> repeats cx times
	;3rd line
	add di, 154
	mov cx, 3
	rep stosw		;stosw-> mov [es:di], ax		rep-> repeats cx times	
				pop cx
				pop ax
				pop di
				pop es
				pop bp
				ret 4	; x, y
;------------------------------------------------------------------------
;subroutine to draw exit door on screen
drawdoor:	push bp		;stack order:	x [bp+4], y [bp+6]
			mov bp, sp
			push es
			push ax
			push di
			push cx
	mov ax, 0xb800
	mov es, ax
	mov al, 80
	mul byte [bp+6]
	add ax, [bp+4]
	shl ax, 1
	mov di, ax
	
	mov ax, 0x6020	;black on orange with nothing to print
	mov cx, 2
	rep stosw		;stosw-> mov [es:di], ax		rep-> repeats cx times
	mov ax, 0x606F	;black on orange with 'o' to print
	mov [es:di], ax
	add di, 156
	mov ax, 0x6020	;black on orange with nothing to print
	mov cx, 3
	rep stosw		;stosw-> mov [es:di], ax		rep-> repeats cx times
			pop cx
			pop di
			pop ax
			pop es
			pop bp
			ret 4	;x, y
;------------------------------------------------------------------------
;subroutine that prints dave on screen
printdave:	push bp										;stack order:	dave [bp+4], left [bp+6], top [bp+8]
			mov bp, sp
			push es
			push di
			push ax
			push bx
			push cx
			push si
	mov ax, 0xb800
	mov es, ax
	mov al, 80
	mul byte [bp+8]
	add ax, [bp+6]
	shl ax, 1
	mov di, ax
	
	mov bx, [bp+4]
	mov si, 0
	
	;left arm
	mov ah, 0x0F		;white on black
	mov al, [bx+si]
	mov [es:di], ax
	add si, 1
	add di, 2
	
	;head
	mov ah, 0x0C		;red on black
	mov al, [bx+si]
	mov [es:di], ax
	add si, 1
	add di, 2
	
	;right arm
	mov ah, 0x0F		;white on black
	mov al, [bx+si]
	mov [es:di], ax
	add si, 1
		
	;body and legs
	add di, 156			;aqua on black
	mov ah, 0x09
	mov cx, 3
	bodyline:	mov al, [bx+si]
				mov [es:di], ax
				add si, 1
				add di, 2
				loop bodyline	
			pop si
			pop cx
			pop bx
			pop ax
			pop di
			pop es
			pop bp
			ret 6	;dave, x, y
;------------------------------------------------------------------------
;subroutines that assists in movement of dave by removing old body parts on screen
;remove older left half of dave
cutleft:		push es
				push di
				push ax
				push bx
	push 0xb800
	pop es
	mov ax, 80
	mov bx, [davePosition]
	mul bl
	add ax, [davePosition+2]
	shl ax, 1
	mov di, ax
	mov ax, 0x0720
	
	mov [es:di], ax
	add di, 160
	mov [es:di], ax
				pop bx
				pop ax
				pop di
				pop es
	ret
;remove older right half of dave
cutright:		push es
				push di
				push ax
				push bx
	push 0xb800
	pop es
	mov ax, 80
	mov bx, [davePosition]
	mul bl
	add ax, [davePosition+2]
	shl ax, 1
	add ax, 4
	mov di, ax
	mov ax, 0x0720
	
	mov [es:di], ax
	add di, 160
	mov [es:di], ax
				pop bx
				pop ax
				pop di
				pop es
	ret
;remove older upper half of dave
cuttop:			push es
				push di
				push ax
				push bx
				push cx
	push 0xb800
	pop es
	mov ax, 80
	mov bx, [davePosition]
	mul bl
	add ax, [davePosition+2]
	shl ax, 1
	mov di, ax
	
	mov ax, 0x0720
	mov cx, 3
	cld
	rep stosw
				pop cx
				pop bx
				pop ax
				pop di
				pop es
	ret
;remove older lower half of dave
cutbottom:		push es
				push di
				push ax
				push bx
				push cx
	push 0xb800
	pop es
	mov ax, 80
	mov bx, [davePosition]
	mul bl
	add ax, [davePosition+2]
	shl ax, 1
	add ax, 160
	mov di, ax
	
	mov ax, 0x0720
	mov cx, 3
	cld
	rep stosw
				pop cx
				pop bx
				pop ax
				pop di
				pop es
	ret
;------------------------------------------------------------------------
;subroutines that control dave's movement (ctrl + c, ctrl + v) and uses cut-subroutines to remove older position (delete)
;for right key press
scrollright:
	push ax
	push dx
	
	mov dx, di
	add dx, 6
	push dx
	call burstitem
	add dx, 160
	push dx
	call burstitem
	
	mov ax, [es:di+6]
	cmp ah, 0x07
	jne skipright1
	
	mov ax, [es:di+166]
	cmp ah, 0x07
	jne skipright1
	
	mov bx, 1
	call cutleft
	add di, 2
	add word [davePosition+2], 1
	
	skipright1:
	pop dx
	pop ax
	ret
;for left key press
scrollleft:
	push ax
	push dx	
	
	mov dx, di
	sub dx, 2
	push dx
	call burstitem
	add dx, 160
	push dx
	call burstitem
	
	;check if dave reached door after collecting cup and sets doorPassed byte equal to 1
	mov ax, [es:di-2]
	cmp ah, 0x60
	jne another
	cmp byte [cupCollected], 1
	jne skipleft1
		mov byte [doorPassed], 1
	jmp skipleft1
	another:
	cmp ah, 0x07
	
	jne skipleft1
	
	mov ax, [es:di+158]
	cmp ah, 0x07
	jne skipleft1
	
	mov bx, 1
	call cutright
	sub di, 2
	sub word [davePosition+2], 1
	
	skipleft1:
	pop dx
	pop ax
	ret
;for up key press
scrollup:
	push ax
	push dx
	
	mov dx, di
	sub dx, 160
	push dx
	call burstitem
	add dx, 2
	push dx
	call burstitem
	add dx, 2
	push dx
	call burstitem
	
	mov ax, [es:di-160]
	cmp ah, 0x07
	jne skipup1
	
	mov ax, [es:di-158]
	cmp ah, 0x07
	jne skipup1
	
	mov ax, [es:di-156]
	cmp ah, 0x07
	jne skipup1

	mov bx, 1
	call cutbottom
	sub di, 160
	sub word [davePosition], 1
	
	skipup1:
	pop dx
	pop ax
	ret
;for falling scenarios
scrolldown:
	push ax
	push dx
	
	mov dx, di
	add dx, 320
	push dx
	call burstitem
	add dx, 2
	push dx
	call burstitem
	add dx, 2
	push dx
	call burstitem
	
	mov ax, [es:di+320]
	cmp ah, 0x07
	jne skipdown1
	
	mov ax, [es:di+322]
	cmp ah, 0x07
	jne skipdown1
	
	mov ax, [es:di+324]
	cmp ah, 0x07
	jne skipdown1

	mov bx, 1
	call cuttop
	add di, 160
	add word [davePosition], 1
	
	skipdown1:
	pop dx
	pop ax
	ret
;------------------------------------------------------------------------
;subroutine that control's dave's diagonal movement
curvemovement:
		cmp byte [fuel], 0
		je skipclimb
		
	;rising curve
		cmp word [es:di-160], 0x0720
		jne skiplift
		cmp word [es:di-158], 0x0720
		jne skiplift
		cmp word [es:di-156], 0x0720
		jne skiplift
		
		call scrollup
		sub byte [fuel], 1
		jnz skipcurve
	
	;up fuel emptied, prepare for down curve
	skiplift:	
		mov byte [fuel], 0
		mov byte [fuel+1], 6
		jmp skipcurve
	
	;falling curve
	skipclimb:
			
		cmp word [es:di+320], 0x0720
		jne skipcurve
		cmp word [es:di+322], 0x0720
		jne skipcurve
		cmp word [es:di+324], 0x0720
		jne skipcurve
		call scrolldown
		
	skipcurve:
	ret
;------------------------------------------------------------------------
;subroutine that breaks diamonds or cups while dave's movement
burstitem:	push bp
			mov bp, sp
			push ax
			push di
	mov di, [bp+4]

	mov ax, [es:di]
	cmp ax, 0x402D	;compare if object is wall
	je nofighting
	cmp ah, 0x60	;compare if object is exit door
	je nofighting
	cmp ax, 0x70b0	;compare if object is entry door
	je nofighting
	cmp ax, 0x0720	;compare if object is empty space
	je nofighting
	
	jmp fight
nofighting:			;don't break anything, just exit
		pop di
		pop ax
		pop bp
		ret 2
fight:
	;tracking top left of diamond/cup
	loop101:
	sub di, 2
	mov ax, [es:di]
	cmp ax, 0x3f2a
	je loop101
	cmp ah, 0x6f
	je loop101
	
	add di, 2
	sub di, 160
	mov ax, [es:di]
	cmp ax, 0x3f2a
	je loop102
	cmp ah, 0x6f
	je loop102
	
	add di, 160
	loop102:
		sub di, 2
		mov ax, [es:di]
		cmp ax, 0x3f2a
		je loop102
		cmp ah, 0x6f
		je loop102
	add di, 2
	;top left of diamond/cup tracked
	
	mov ax, [es:di]
	cmp ax, 0x3f2a
	jne notdiamond
	add word [points], 3					;add 3 points because the object is a diamond
	jmp remove
	notdiamond:	add word [points], 5		;add 5 points because the object is a cup
				mov byte [cupCollected], 1	;set cupCollected byte, 1
	
	;break diamond/cup traced
	remove:
		mov word [es:di], 0x0720
		mov word [es:di+2], 0x0720
		mov word [es:di+4], 0x0720
		mov word [es:di+162], 0x0720

		pop di
		pop ax
		pop bp
		ret 2
;------------------------------------------------------------------------	
;subroutine that prints fixed contents of stage-2 (starting/ending lines, maze, walls, opening/exit door, diamonds, cup)
printstage2fixed:
	push si
	;-----------------STARTING LINE-----------------
		
		push 1			;y							"SCORE"
		push 2			;x
		push 2			;green on black
		push score
		call printstr
		
		push 1			;y
		push 22			;x
		push 2			;green on black				"LEVEL 01"
		push level
		call printstr
		
		push 1			;y							"DANGER DAVE"
		push 37			;x
		push 2			;green on black
		push title
		call printstr
		
	;-----------------ENDING LINE-----------------
		
		push 22			;y							cup on left side of text
		push 18			;x
		call drawcup
		
		push 23			;y							"COLLECT CUP N GO THRU THE DOOR!"
		push 22			;x
		push 2			;green on black
		push tip
		call printstr
		
		push 22			;y							cup on right side of text
		push 54			;x
		call drawcup
		
	;-----------------MAZE LAYOUT-----------------
		
		push 0x402D		;black on red				huge red rectangle with black dashes (back most layer)
		push 3			;y-top
		push 21			;y-bottom
		push 0			;x-left
		push 80			;x-right
		call drawrect
		
		push 0x0720		;black on white				inner black frame to make red rectangle hollow
		push 5			;y-top
		push 20			;y-bottom
		push 4			;x-left
		push 76			;x-right
		call drawrect
		
	;-----------------TOP ROW-----------------
		
		push 0x402D									;block 1
		push 8
		push 10
		push 11
		push 15
		call drawrect
		
		push 0x402D									;block 2
		push 8
		push 10
		push 28
		push 32
		call drawrect
		
		push 0x402D									;block 3
		push 8
		push 10
		push 46
		push 50
		call drawrect
		
		push 0x402D									;block 4
		push 8
		push 10
		push 64
		push 68
		call drawrect
		
	;-----------------MID ROW-----------------
		
		push 0x402D									;block 1
		push 12
		push 14
		push 4
		push 8
		call drawrect
		
		push 0x402D									;block 2
		push 12
		push 14
		push 21
		push 25
		call drawrect
		
		push 0x402D									;block 3
		push 12
		push 14
		push 38
		push 42
		call drawrect
		
		push 0x402D									;block 4
		push 12
		push 14
		push 55
		push 59
		call drawrect
		
		push 0x402D									;block 5
		push 12
		push 14
		push 72
		push 76
		call drawrect
		
	;-----------------DOWN ROW-----------------
	
		push 0x402D									;left long row
		push 16
		push 18
		push 15
		push 32
		call drawrect
		
		push 0x402D									;right long row
		push 16
		push 18
		push 46
		push 72
		call drawrect
		
		push 0x402D									;block with exit door
		push 18
		push 20
		push 46
		push 50
		call drawrect
		
		push 17				;y						;opening from where DAVE appears
		push 4				;x
		call drawopening
		
	;--------------------DIAMONDS-------------------
	
	mov si, 0
	printDiamonds:	push word [diamondsY+si]
					push word [diamondsX+si]
					call drawdiamond
					add si, 2
					cmp si, 20
					jne printDiamonds
	
	;----------------MAIN COMPONENTS----------------
	
		push word [cupPosition]
		push word [cupPosition+2]
		call drawcup
			
		push 18				;y						;EXIT DOOR
		push 50				;x
		call drawdoor
	
	pop si
	ret
;------------------------------------------------------------------------
;subroutine that prints unfixed contents of stage-2 (score point, dave, last msg)
printstage2unfixed:
	push si
		
	;-----------------SCORE POINT-----------------
		
		push 1			;y							integer value of score
		push 9			;x
		push word [points]
		call printnum
		
	;---------------------DAVE--------------------
		
		push word [davePosition]		;y						;DAVE
		push word [davePosition+2]		;x
		push dave
		call printdave
		
		cmp byte [cupCollected], 1
		jne skiplastmsg
				push 24			;y							"Cup Collected! Please press SPACE near door to exit!"
				push 12			;x
				push 3			;aqua
				push last
				call printstr
	skiplastmsg:
	pop si
	ret
;------------------------------------------------------------------------
;new keyboard interrupt service routine
gamekbisr:	push es
		push ax
		push bx		;bx determines obstacle free movement and writes back in exitkbisr
		push di
		push dx		;dx assists in burstitem subroutine
	;dave's position on screen
	push 0xb800
	pop es
	mov ax, 80
	mov bx, [davePosition]
	mul bl
	add ax, [davePosition+2]
	shl ax, 1
	mov di, ax
	
	mov bx, 0
	in al, 0x60
	
	cmp al, 0x4d	;right arrow pressed
	jne nextcomparison1
	call scrollright
	cmp bx, 0
	je checkpoint
	mov dx, di
	add dx, 6
	push dx
	call burstitem
	cmp word [es:di+6], 0x0720
	jne checkpoint
	call curvemovement
	
	nextcomparison1: cmp al, 0x4b	;left arrow pressed
					jne nextcomparison2
					call scrollleft
					cmp bx, 0
					je checkpoint
					mov dx, di
					sub dx, 2
					push dx
					call burstitem
					cmp word [es:di-2], 0x0720
					jne checkpoint
					call curvemovement
					
	nextcomparison2: cmp al, 0x48	;up arrow pressed
					jne nextcomparison3
					cmp byte [fuel+1], 0
					jne nextcomparison3
					cmp byte [fuel+2], 1
					je nextcomparison3
					
					cmp byte [fuel], 0
					jne riseup
					
					mov byte [fuel], 7
					riseup:	call scrollup
							cmp bx, 1
							jne resetupfuel
							sub byte [fuel], 1
							jnz exitkbisr
						resetupfuel:
						mov byte [fuel], 0
						mov byte [fuel+1], 10
						mov byte [fuel+2], 1
						
checkpoint:	jmp falldown
							
	nextcomparison3: cmp al, 0xcd	;right arrow key released		
					jne nextcomparison4
					jmp falldown
	nextcomparison4: cmp al, 0xcb	;left arrow key released		
					jne nextcomparison5
					jmp falldown
	nextcomparison5: cmp al, 0xc8	;up arrow key released		
					jne nomatch
					mov byte [fuel+2], 0
					
					falldown:	mov bx, 0
								call scrolldown
								call printstage2unfixed
								call sleep
								cmp bx, 1
								je falldown
								
						mov byte [fuel], 0
						mov byte [fuel+1], 0
						jmp exitkbisr
nomatch:	cmp bx, 1
			je exitkbisr
			
			pop dx
			pop di
			pop bx
			pop ax
			pop es
		jmp far [cs:oldisr]
exitkbisr:	mov al, 0x20
			out 0x20, al
			call printstage2unfixed
			pop dx
			pop di
			pop bx
			pop ax
			pop es
			iret
;------------------------------------------------------------------------
	
main:	;save oldkeyboardisr
		xor ax, ax
		mov es, ax
		mov ax, [es:9*4]
		mov bx, [es:9*4+2]
		mov [oldisr], ax
		mov [oldisr+2], bx

		call clrscr
		call printstage1
		
		interruptloop1:	mov ah, 0
						int 0x16
						cmp al, 27
						je leavegame
						cmp al, 13			;enter key pressed
						jne interruptloop1
		
		call clrscr
		call printstage2fixed
		call printstage2unfixed
		
		;hook keyboardisr
		cli
		mov word [es:9*4], gamekbisr
		mov [es:9*4+2], cs
		sti
		
		mov cx, 0
		interruptloop2:	cmp byte [doorPassed], 1
						jne continuegame
						inc cx
						
						cmp word [davePosition], 18
						jne continuegame
						inc cx
						
						cmp word [davePosition+2], 53
						jne continuegame
						inc cx
						
						cmp cx, 3
						je endgame
						mov cx, 0
			continuegame:		mov ah, 0
								int 0x16
								cmp al, 27
								jne interruptloop2
	;scenario where user pressed escape key
	leavegame:
		call clrscr
		push 11
		push 23
		push 7
		push escapemsg
		call printstr
		
		push 13
		push 36
		push 2
		push score
		call printstr
		
		push 13
		push 43
		push word [points]
		call printnum
		jmp return
	;scenario where user played and finished the game by passing through the door
	endgame:
		call clrscr
		push 11
		push 33
		push 7
		push finalmsg
		call printstr
		
		push 13
		push 36
		push 2
		push score
		call printstr
		
		push 13
		push 43
		push word [points]
		call printnum
				
	return:	
		;restore oldkeyboardisr
		mov ax, [oldisr]
		mov bx, [oldisr+2]
		cli
		mov [es:9*4], ax
		mov [es:9*4+2], bx
		sti
		
mov ax, 4c00h
int 21h

;--------------phase 1--------------
title1: db 'DANGER', 0
title2: db 'DAVE', 0
credit1: db 'by Hassan Mahmood (20L-1052)', 0
credit2: db 'and Hafiz Uzair Warsi (20L-2113)', 0
heading: db 'RULES:-', 0
point1: db '1) press ENTER to start', 0
point2: db ' i. use arrow keys to move Dave', 0
point3: db 'ii. collect the items and enter the door for victory', 0
point4: db '2) press ESCAPE to exit', 0

;--------------phase 2--------------
score: db 'SCORE:', 0
level: db 'LEVEL 01', 0
title: db 'DANGER DAVE', 0
tip: db 'COLLECT CUP N GO THRU THE DOOR!', 0
last: db 'Cup Collected! Please press SPACE near door to exit!', 0

dave: db '_o_/"\'
davePosition: dw 18, 7	;y, x
diamondsY: dw 6, 6, 6, 10, 10, 10, 10, 10, 15, 14
diamondsX: dw 11, 28, 64, 4, 21, 38, 55, 72, 4, 29
cupPosition: dw 6, 46	;y, x

;--------------phase 3--------------
oldisr: dd 0
points: dw 0		;x: 9
cupCollected: db 0	;when 1, if dave reaches door, game ends
doorPassed: db 0	;when 1, interrupt routine ignores and exits interrupt routine for each keypress
fuel: db 0, 0, 0	;for upward's and downward's movement, also, it controls one jump at a time

;--------------phase 4--------------
escapemsg: db 'You exited the game successfully!', 0	;displayed after pressing Escape
finalmsg: db 'Game Finished!', 0						;displayed after passing via door

;---------------INDEX---------------
;l-14: 		sleep				(delay)
;l-29: 		clrscr
;l-49: 		strlen
;l-70: 		printstr
;l-109: 	printnum
;l-153:		drawhorizontalline	(for drawrect)
;l-186:		drawrect

;phase 1
;l-217:		drawcrown
;l-251:		drawstylishdiamond
;l-298:		printstage1

;phase 2,3
;l-520:		drawcup
;l-558:		drawdiamond
;l-587:		drawopening
;l-621:		drawdoor
;l-652:		printdave
;l-711:		cutleft
;l-734:		cutright
;l-758:		cuttop
;l-783:		cutbottom
;l-811:		scrollright
;l-841:		scrollleft
;l-880:		scrollup
;l-917:		scrolldown
;l-955:		curvemovement
;l-992:		burstitem
;l-1064:	printstage2fixed
;l-1236:	printstage2unfixed
;l-1265:	gamekbisr

;l-1372:	main