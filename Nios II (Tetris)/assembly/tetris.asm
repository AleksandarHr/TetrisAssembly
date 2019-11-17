; BEGIN:main
main:
	call   get_input
	ldw t7, DRAW_Ax(zero)
	addi t6, zero, DRAW_Ax
	addi sp, zero, 0x2000
	call  generate_tetromino
	call  draw_gsa
	call  end
; END:main

; BEGIN:increment_score
increment_score:
	ldw  t0, SCORE(zero)	# load current game score
	addi t0, t0, 1			# increment current game score by 1
	stw  t0, SCORE(zero)	# store the updated game score
; END:increment_score

; BEGIN:get_input
get_input:
	ldw  t0, BUTTONS+4(zero)	# load edgecapture word
	add  t1, zero, zero			# counter for bits 0, 1, 2, 3, 4
	addi t2, zero, 0x1			# mask for the LSBit
	
	and  t3, t0, t2				# take the LSBit (now, bit 0 - moveL)
	beq  t3, t2, mL

	srli t0, t0, 1			
	and  t3, t0, t2				# take the LSBit (now, bit 1 -> rotL)
	beq  t3, t2, rL
	
	srli t0, t0, 1			
	and  t3, t0, t2				# take the LSBit (now, bit 2 -> reset)
	beq  t3, t2, res
		
	srli t0, t0, 1			
	and  t3, t0, t2				# take the LSBit (now, bit 3 -> rotR)
	beq  t3, t2, rR

	srli t0, t0, 1			
	and  t3, t0, t2				# take the LSBit (now, bit 4 -> moveR)
	beq  t3, t2, mR

	add  v0, zero, zero
	jmpi return_input

# Save corresponding value to the return register v0
mL:
	addi v0, zero, moveL
	jmpi return_input
rL:
	addi v0, zero, rotL
	jmpi return_input
res:
	addi v0, zero, reset
	jmpi return_input
rR:
	addi v0, zero, rotR
	jmpi return_input
mR:
	addi v0, zero, moveR

return_input:
	stw  zero, BUTTONS+4(zero)	# clear the edgecapture
	ret
; END:get_input

; BEGIN:clear_leds
clear_leds:
	add t0, zero, zero
	stw zero,  0x2000(t0)
	addi t0, t0, 4
	stw zero,  0x2000(t0)
	addi t0, t0, 4
	stw zero,  0x2000(t0)
	ret
; END: clear_leds

; BEGIN:set_pixel
set_pixel:
	srli  t0,  a0, 2  		# get the LED[x] word
	andi  t1,  a0, 3		# get remainder modulo 4
	slli   t1,  t1, 3		# get 0, 8, 16, or 24
	add   t1,  t1,  a1		# get the actual position of LED within the word; add y
	addi  t2,  zero, 1		
 	sll   t2,  t2,  t1		# LED mask; shift 1 by position in the word
	slli   t0,  t0, 2
	ldw    t3,  0x2000(t0)		# get memory address of LED word
	or    t3,  t3,  t2			# set pixel
	stw    t3,  0x2000(t0)		# store back in memory
	ret
; END:set_pixel

; BEGIN:wait
wait:
	addi  t0,  zero, 2
	slli  t0, t0, 20
	loop:
		addi  t0,  t0, -1
		bne   t0,  zero, loop
		ret
; END:wait

; BEGIN:in_gsa
in_gsa:	
	add t0, zero, a0	# x-coord
	add t1, zero, a1	# y-coord
	add v0, zero, zero	# default return is 0 (in_gsa)
	addi t2, zero, 7
	addi t3, zero, 11
	blt  t0, zero, out
	blt  t3, t0, out
	blt  t1, zero, out
	blt  t2, t1, out
	ret
; END:in_gsa

; BEGIN:out
out:
	addi v0, zero, 1
	ret
; END:out

; BEGIN:get_gsa
get_gsa:
	add t0, zero, a0	# x-coord
	add t1, zero, a1	# y-coord
	slli t0, t0, 3		# get 8*x
	add t4, t0, t1		# get 8*x + y - index in GSA
	slli t4, t4, 2
	ldw v0, GSA(t4)
	ret
; END:get_gsa

; BEGIN:set_gsa
set_gsa:	
	add t0, zero, a0	# x-coord
	add t1, zero, a1	# y-coord
	slli t0, t0, 3		# get 8*x
	add t4, t0, t1		# get 8*x + y - index in GSA
	slli t4, t4, 2
	stw a2, GSA(t4)
	ret
; END:get_gsa


; BEGIN:draw_gsa
draw_gsa:
#	call clear_leds

	addi sp, sp, -12
	stw   s0, 0(sp)
	stw   s1, 4(sp)
	stw	  ra, 8(sp)

	addi s0, zero, 12	# x-counter 0,1,2,...,11
	addi s1, zero, 8	# y-counter 0,1,2,...,7


outer:
	beq  s0, zero, finish
	addi s0, s0, -1
	addi s1, zero, 8	
inner:
	beq s1, zero, outer
	addi s1, s1, -1
	
	add a0, zero, s0
	add a1, zero, s1
	call get_gsa

	beq v0, zero, inner
	
	add a0, zero, s0
	add a1, zero, s1
	call set_pixel
	
	jmpi inner

finish:
	ldw   s0, 0(sp)
	ldw   s1, 4(sp)
	ldw   ra, 8(sp)
	addi sp, sp, 12
	ret
; END:draw_gsa

; BEGIN:draw_tetromino
draw_tetromino:
	ldw s0, T_X(zero)
	ldw s1, T_Y(zero)
	ldw s2, T_type(zero)
	ldw s3, T_orientation(zero)
	add s4, a0, zero #p-value
	add s5, zero, zero #offset
	
	
	addi sp, sp, -4
	stw ra, 0(sp)


	slli s2, s2, 4 #s2 = 16*s2; offset by type * 48
	add s5, zero, s2

	slli s3, s3, 2
	add s5, s5, s3


	#call set_gsa on Anchor
	add a2, s4, zero #p-value
	add a0, s0, zero
	add a1, s1, zero
	call set_gsa

	
	#call set_gsa on offset 0
	ldw t0, DRAW_Ax(s5) #address of Array of X-offset
	ldw t1, DRAW_Ay(s5) #address of Array of Y-offset
	ldw t0, 0(t0) #x first offset
	ldw t1, 0(t1)
	
	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	call set_gsa

	#call set_gsa on offset 1
	ldw t0, DRAW_Ax(s5) #address of Array of X-offset
	ldw t1, DRAW_Ay(s5) #address of Array of Y-offset
	ldw t0, 4(t0) #x offset
	ldw t1, 4(t1)
	
	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	call set_gsa


	#call set_gsa on offset 2

	ldw t0, DRAW_Ax(s5) #address of Array of X-offset
	ldw t1, DRAW_Ay(s5) #address of Array of Y-offset
	ldw t0, 8(t0) #x  offset
	ldw t1, 8(t1)

	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	call set_gsa


	ldw ra, 0(sp)
	addi sp, sp, 4

	ret

;END:draw_tetromino


;BEGIN:generate_tetromino

generate_tetromino:
add t0, zero, zero
addi t1, zero, 4
 
;BEGIN:regenerate
regenerate:
addi t0, zero, 1
slli t0, t0, 28
srli t0, t0, 28
blt  t1, t0, regenerate
;END:regenerate

;; initialize (6,1) for x,y coords, T_orientation and T_type
stw  t0, T_type(zero)
addi t0, zero, 6
addi t1, zero, 1
stw  t0, T_X(zero)
stw  t1, T_Y(zero)
stw  zero, T_orientation(zero)
addi a0, zero, 0x02 ;; FALLING
 
addi sp, sp, -4
stw  ra, 0(sp)

call draw_tetromino
 
ldw  ra, 0(sp)
addi sp, sp, 4
ret

;END:generate_tetromino

; BEGIN:end
end:
	break
; END:end



;BEGIN:collision_detection
collision_detection:
	addi t0, zero, 0

	addi sp, sp, -32
	stw  s0, 0(sp)
	stw  s1, 4(sp)
	stw  s2, 8(sp)
	stw  s3, 12(sp)
	stw  s4, 16(sp)
	stw  s5, 20(sp)	
	stw  s6, 24(sp)
	stw ra, 28(sp)


	ldw s0, T_X(zero)
	ldw s1, T_Y(zero)
	ldw s2, T_type(zero)
	ldw s3, T_orientation(zero)
	add s4, a0, zero 	# Collision type
	add s5, zero, zero 	# offset
	addi s6, zero, PLACED


	beq a0, t0, west_collision
	addi t0, zero, 1
	beq a0, t0, east_collision
	addi t0, zero, 2
	beq a0, t0, south_collision
	jmpi overlapping

	slli s2, s2, 4 #s2 = 16*s2; offset by type * 48
	add s5, zero, s2

	slli s3, s3, 2
	add s5, s5, s3

east_collision:
	addi s0, s0, 1			# update x-coordinate for movement EAST
	addi t0, zero, 12		# check if it is within bounds of the screen
	beq  s0, t0, COLLISION
	jmpi OVERLAP
west_collision:
	addi s0, s0, -1			# update x-coordinate for movement WEST
	addi t0, zero, -1		# check if it is within bounds of the screen
	beq  s0, t0, COLLISION
	jmpi OVERLAP
south_collision:
	addi s1, s1, 1			# update y-coordinate for movement SOUTH
	addi t0, zero, 8		# check if it is within bounds of the screen
	beq  s0, t0, COLLISION
overlapping:

	#call set_gsa on Anchor
	add a2, s4, zero #p-value
	add a0, s0, zero
	add a1, s1, zero
	call get_gsa
	beq v0, s6, COLLISION	# check if anchor point has been moved so that it collides

	#call set_gsa on offset 0
	ldw t0, DRAW_Ax(s5) #address of Array of X-offset
	ldw t1, DRAW_Ay(s5) #address of Array of Y-offset
	ldw t0, 0(t0) #x first offset
	ldw t1, 0(t1)
	
	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	call get_gsa
	beq v0, s6, COLLISION	# check another pixel from the tetromino for collision

	#call set_gsa on offset 1
	ldw t0, DRAW_Ax(s5) #address of Array of X-offset
	ldw t1, DRAW_Ay(s5) #address of Array of Y-offset
	ldw t0, 4(t0) #x offset
	ldw t1, 4(t1)
	
	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	call set_gsa
	beq v0, s6, COLLISION	# check another pixel from the tetromino for collision

	#call set_gsa on offset 2
	ldw t0, DRAW_Ax(s5) #address of Array of X-offset
	ldw t1, DRAW_Ay(s5) #address of Array of Y-offset
	ldw t0, 8(t0) #x  offset
	ldw t1, 8(t1)

	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	call get_gsa
	beq v0, s6, COLLISION	# check another pixel from the tetromino for collision

NO_COLLISION:
	# if no collision or overlap was detected, return NONE
	ldw  s0, 0(sp)
	ldw  s1, 4(sp)
	ldw  s2, 8(sp)
	ldw  s3, 12(sp)
	ldw  s4, 16(sp)
	ldw  s5, 20(sp)	
	ldw  s6, 24(sp)
	ldw  ra, 28(sp)
	addi sp, sp, 32

	addi v0, zero, NONE
	ret

COLLISION:
	# if collision or an overlap was detected, return type of collision/overlap
	add v0, zero, s4	# store collision type in return register

	ldw  s0, 0(sp)
	ldw  s1, 4(sp)
	ldw  s2, 8(sp)
	ldw  s3, 12(sp)
	ldw  s4, 16(sp)
	ldw  s5, 20(sp)	
	ldw  s6, 24(sp)
	ldw  ra, 28(sp)
	addi sp, sp, 32

	ret

;END:collision_detection






  ;; game state memory location
  .equ T_X, 0x1000                  ; falling tetrominoe position on x
  .equ T_Y, 0x1004                  ; falling tetrominoe position on y
  .equ T_type, 0x1008               ; falling tetrominoe type
  .equ T_orientation, 0x100C        ; falling tetrominoe orientation
  .equ SCORE,  0x1010               ; score
  .equ GSA, 0x1014                  ; Game State Array starting address
  .equ SEVEN_SEGS, 0x1198           ; 7-segment display addresses
  .equ LEDS, 0x2000                 ; LED address
  .equ RANDOM_NUM, 0x2010           ; Random number generator address
  .equ BUTTONS, 0x2030              ; Buttons addresses

  ;; type enumeration
  .equ C, 0x00
  .equ B, 0x01
  .equ T, 0x02
  .equ S, 0x03
  .equ L, 0x04

  ;; GSA type
  .equ NOTHING, 0x0
  .equ PLACED, 0x1
  .equ FALLING, 0x2

  ;; orientation enumeration
  .equ N, 0
  .equ E, 1
  .equ So, 2
  .equ W, 3
  .equ ORIENTATION_END, 4

  ;; collision boundaries
  .equ COL_X, 4
  .equ COL_Y, 3

  ;; Rotation enumeration
  .equ CLOCKWISE, 0
  .equ COUNTERCLOCKWISE, 1

  ;; Button enumeration
  .equ moveL, 0x01
  .equ rotL, 0x02
  .equ reset, 0x04
  .equ rotR, 0x08
  .equ moveR, 0x10
  .equ moveD, 0x20

  ;; Collision return ENUM
  .equ W_COL, 0
  .equ E_COL, 1
  .equ So_COL, 2
  .equ OVERLAP, 3
  .equ NONE, 4

  ;; start location
  .equ START_X, 6
  .equ START_Y, 1

  ;; game rate of tetrominoe falling down (in terms of game loop iteration)
  .equ RATE, 5

  ;; standard limits
  .equ X_LIMIT, 12
  .equ Y_LIMIT, 8


C_N_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_N_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_E_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_E_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_So_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

C_W_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_W_Y:
  .word 0x00
  .word 0x01
  .word 0x01

B_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_N_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_So_X:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

B_So_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_Y:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

T_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_E_X:
  .word 0x00
  .word 0x01
  .word 0x00

T_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_Y:
  .word 0x00
  .word 0x01
  .word 0x00

T_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_W_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_E_X:
  .word 0x00
  .word 0x01
  .word 0x01

S_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_So_X:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

S_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

S_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_W_Y:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

L_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_N_Y:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_E_X:
  .word 0x00
  .word 0x00
  .word 0x01

L_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_So_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0xFFFFFFFF

L_So_Y:
  .word 0x00
  .word 0x00
  .word 0x01

L_W_X:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_W_Y:
  .word 0x01
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

DRAW_Ax:                        ; address of shape arrays, x axis
    .word C_N_X
    .word C_E_X
    .word C_So_X
    .word C_W_X
    .word B_N_X
    .word B_E_X
    .word B_So_X
    .word B_W_X
    .word T_N_X
    .word T_E_X
    .word T_So_X
    .word T_W_X
    .word S_N_X
    .word S_E_X
    .word S_So_X
    .word S_W_X
    .word L_N_X
    .word L_E_X
    .word L_So_X
    .word L_W_X

DRAW_Ay:                        ; address of shape arrays, y_axis
    .word C_N_Y
    .word C_E_Y
    .word C_So_Y
    .word C_W_Y
    .word B_N_Y
    .word B_E_Y
    .word B_So_Y
    .word B_W_Y
    .word T_N_Y
    .word T_E_Y
    .word T_So_Y
    .word T_W_Y
    .word S_N_Y
    .word S_E_Y
    .word S_So_Y
    .word S_W_Y
    .word L_N_Y
    .word L_E_Y
    .word L_So_Y
    .word L_W_Y
