; BEGIN:main
main:
	addi sp, zero, 0x2000
	addi sp, sp, -4
	stw  ra, 0(sp)
	call reset_game

move_down_loop:
add  s0, zero, zero
rate_loop:
	call clear_leds
	call draw_gsa					# draw gsa
												# TODO: display score
	add a0, zero, zero
	call draw_tetromino		# Remove falling tetromino from the screen

	call wait							# wait approx 0.2s

	call get_input				# get button input

	add  a0, zero, v0			# get corresponding action
	#addi  a0, zero, moveL
	call act							# try to execute user input action

	addi  a0, zero, FALLING
	call draw_tetromino		# draw falling tetromino

	addi s0, s0, 1
	addi t0, zero, RATE
	bne  s0, t0, rate_loop

	add  a0, zero, zero
	call draw_tetromino		# Remove falling tetromino from the screen

	addi a0, zero, moveD
	call act						# move tetromino downwards
	add  s1, v0, zero		# store the act result

	addi a0, zero, FALLING
	call draw_tetromino		# draw falling tetromino

	beq  s1, zero, move_down_loop	# if we havent reached the bottom, loop

	addi a0, zero, PLACED
	call draw_tetromino		# draw placed tetromino

full_line_removal_loop:
	call detect_full_line 	# check for a full ine
	addi t0, zero, 8
	beq  v0, t0, generate_new_tetromino		# generate new tetromino

	add  a0, zero, v0
	call remove_full_line					# remove full ine

	# increment and display score
	call increment_score
	call display_score

	call clear_leds
	call draw_gsa
	jmpi full_line_removal_loop		# loop, until no more full ines

generate_new_tetromino:

	addi a0, zero, 6
	addi a1, zero, 1
	call get_gsa
	addi t0, zero, PLACED
	beq  v0, t0, end

	addi a0, zero, 5
	addi a1, zero, 1
	call get_gsa
	addi t0, zero, PLACED
	beq  v0, t0, end

	call generate_tetromino

	addi a0, zero, 2
	call draw_tetromino

	jmpi move_down_loop
; END:main

; BEGIN:end
end:
	call reset_game
	jmpi move_down_loop

	ldw  ra, 0(sp)
	addi sp, sp, 4		
	break
; END:end

; BEGIN:reset_game
reset_game:
	addi sp, sp, -12
	stw  ra, 0(sp)
	stw s0, 4(sp)
	stw s1, 8(sp)

	add s0, a0, zero
	add s1, a1, zero

	# clear score
	stw zero, SCORE(zero)

	add t1, zero, zero

#clear_gsa_loop:
#	addi t0, zero, 96
#	slli t0, t0, 2

	# clear the GSA
#	stw zero, GSA(t1)
#	addi t1, t1, 4
#	bne	t1, t0, clear_gsa_loop
	addi a2, zero, NOTHING

	addi a1, zero, 7
clearing_loop_y:
	addi a0, zero, 11
	blt  a1, zero, done_clearing_gsa
	addi s1, s1, -1
clearing_loop_x:
	blt  a0, zero, clearing_loop_y
	add a0, s0, zero
	add a1, s1, zero
	addi a2, zero, NOTHING
	call set_gsa
	addi s0, s0, -1
	jmpi clearing_loop_x

done_clearing_gsa:
	call draw_gsa
	# clear LEDS
	call clear_leds

	# clear SEVEN_SEGS
	ldw t0, font_data(zero)
	stw t0, SEVEN_SEGS(zero)
	stw t0, SEVEN_SEGS+4(zero)
	stw t0, SEVEN_SEGS+8(zero)
	stw t0, SEVEN_SEGS+12(zero)
	
	call display_score
	# generate and draw a random tetromino
	call generate_tetromino


	stw  ra, 0(sp)
	stw s0, 4(sp)
	stw s1, 8(sp)
	addi sp, sp, 12
	ret
; END:reset_game


; BEGIN:detect_full_line
detect_full_line:
	addi sp, sp, -16
	stw  s6, 0(sp)
	stw  s7, 4(sp)
	stw  s1, 8(sp)
	stw  ra, 12(sp)
	addi s6, zero, -1	# x-iterator
	addi s7, zero, -1	# y-iterator

y_loop:
	add  s1, zero, zero						# keep track of a single line
	addi s7, s7, 1
	addi s6, zero, -1
	addi v0, zero, 8	# default return value for v0 - y=8, smallest y-coordinate that is too high fit on the screen
	beq  s7, v0, return_detect_full_line

x_loop:
	addi s6, s6, 1
	add  a0, zero, s6
	add  a1, zero, s7
	call get_gsa

	addi t0, zero, 1
	bne  v0, t0, y_loop

	addi s1, s1, 1
	addi t1, zero, 12
	beq  s1, t1, full_line_detected

	addi  t0, zero, 11
	blt  s6, t0, x_loop
	jmpi y_loop

full_line_detected:
	add v0, zero, s7

return_detect_full_line:
	ldw  s6, 0(sp)
	ldw  s7, 4(sp)
	ldw  s1, 8(sp)
	ldw  ra, 12(sp)
	addi sp, sp, 16
	ret
; END:detect_full_line

; BEGIN:remove_full_line
remove_full_line:
	addi sp, sp, -12
	stw  s0, 0(sp)
	stw  s1, 4(sp)
	stw  ra, 8(sp)
	add  s1, zero, a0		# y-coordinate of line to be removed
	addi s0, zero, 12

# MAKE LINE BLINK - OFF_1
disable_x_loop:
	addi s0, s0, -1
	add  a0, zero, s0
	add  a1, zero, s1
	add  a2, zero, zero

	call set_gsa
	bne s0, zero, disable_x_loop

	call clear_leds
	call draw_gsa
	call wait
	addi s0, zero, 12

# MAKE LINE BLINK - ON_1
enable_x_loop:
	addi s0, s0, -1
	add  a0, zero, s0
	add  a1, zero, s1
	addi a2, zero, 1

	call set_gsa
	bne s0, zero, enable_x_loop

	call clear_leds
	call draw_gsa
	call wait

# BLINK AGAIN
	addi s0, zero, 12

# MAKE LINE BLINK - OFF_2
disable_x_loop_two:
	addi s0, s0, -1
	add  a0, zero, s0
	add  a1, zero, s1
	add  a2, zero, zero

	call set_gsa
	bne s0, zero, disable_x_loop_two

	call clear_leds
	call draw_gsa
	call wait
	addi s0, zero, 12

# MAKE LINE BLINK - ON_2
enable_x_loop_two:
	addi s0, s0, -1
	add  a0, zero, s0
	add  a1, zero, s1
	addi a2, zero, 1

	call set_gsa
	bne s0, zero, enable_x_loop_two

	call clear_leds
	call draw_gsa
	call wait

# REMOVE line and shift lines
remove_y_loop:
	addi s1, s1, -1
	blt  s1, zero, remove_line_finished
	addi s0, zero, 12
remove_x_loop:
	beq  s0, zero, remove_y_loop
	addi s0, s0, -1

	add  a0, s0, zero
	add  a1, s1, zero
	call get_gsa

	add  a0, s0, zero
	addi a1, s1, 1
	add a2, v0, zero
	call set_gsa

	jmpi remove_x_loop

remove_line_finished:
	#remove top-most line
	addi s0, zero, 11
	addi s5, zero, 0
loopi:
	add a0, s0, zero
	add a1, s5, zero
	addi a2, zero, 0
	call set_gsa
	addi s0, s0, -1
	addi t0, zero, -1
	blt t0, s0, loopi

	ldw  s0, 0(sp)
	ldw  s1, 4(sp)
	ldw  ra, 8(sp)
	addi sp, sp, 12
	ret
; END:remove_full_line


; BEGIN:increment_score
increment_score:
	ldw  t0, SCORE(zero)	# load current game score
	addi t0, t0, 1			# increment current game score by 1
	stw  t0, SCORE(zero)	# store the updated game score
	ret
; END:increment_score

; BEGIN:display_score
display_score:
	addi sp, sp, -24
	stw  s0, 0(sp)
	stw  s1, 4(sp)
	stw  s2, 8(sp)
	stw  s3, 12(sp)
	stw  s4, 16(sp)
	stw  s5, 20(sp)

	ldw  t0, SCORE(zero)	# load current game score
	addi s5, zero, 1000
	addi s0, zero, 100
	addi s1, zero, 10
	add  t0, t0, s0
	addi s2, zero, -1 	# counter for hundreds digit
	addi s3, zero, -1		# counter for tens digit
	addi s4, zero, -1	# counter for thousands digit

	add  t0, t0, s5
thousands_loop:
	addi s4, s4, 1
	addi t0, t0, -1000
	bge  t0, s5, thousands_loop

	# here we have the thousands digit in s4
	slli s4, s4, 2
	ldw  s4, font_data(s4)

	add  t0, t0, s0
hundreds_loop:
	addi s2, s2, 1
	addi t0, t0, -100
	bge  t0, s0, hundreds_loop

	# here we have the hundreds digit in s2
	slli s2, s2, 2
	addi s2, s2, -1
	ldw  s2, font_data(s2)

	add t0, t0, s1
tens_loop:
	addi s3, s3, 1
	addi t0, t0, -10
	bge  t0, s1, tens_loop

	# here we have the tens digit in s3
	slli s3, s3, 2
	ldw  s3, font_data(s3)

	# here we have the singles digit in t0
	slli t0, t0, 2
	ldw  t0, font_data(t0)

	stw  s4, SEVEN_SEGS(zero)
	stw  s2, SEVEN_SEGS+4(zero)
	stw  s3, SEVEN_SEGS+8(zero)
	stw  t0, SEVEN_SEGS+12(zero)

end_display_score:
	ldw  s0, 0(sp)
	ldw  s1, 4(sp)
	ldw  s2, 8(sp)
	ldw  s3, 12(sp)
	ldw  s4, 16(sp)
	ldw  s5, 20(sp)
	addi sp, sp, 24

	ret
; END:display_score


; BEGIN:get_input
get_input:
	ldw  t0, BUTTONS+4(zero)	# load edgecapture word
	add  t1, zero, zero			# counter for bits 0, 1, 2, 3, 4
	addi t2, zero, 0x1			# mask for the two LSBits

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
; END:clear_leds

; BEGIN:set_pixel
set_pixel:
	srli  t0,  a0, 2  		# get the LED[x] word
	andi  t1,  a0, 3		# get remainder modulo 4
	slli   t1,  t1, 3		# get 0, 8, 16, or 24
	add   t1,  t1,  a1		# get the actual position of LED within the word; add y
	addi  t2,  zero, 1
 	sll   t2,  t2,  t1		# LED mask; shift 1 by position in the word
	slli   t0,  t0, 2
	ldw    t3,  LEDS(t0)		# get memory address of LED word
	or    t3,  t3,  t2			# set pixel
	stw    t3,  LEDS(t0)		# store back in memory
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

; BEGIN:helper
out:
	addi v0, zero, 1
	ret
; END:helper

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
; END:set_gsa


; BEGIN:draw_gsa
draw_gsa:
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
	addi sp, sp, -28
	stw s0, 0(sp)
	stw s1, 4(sp)
	stw s2, 8(sp)
	stw s3, 12(sp)
	stw s4, 16(sp)
	stw s5, 20(sp)
	stw ra, 24(sp)

	ldw s0, T_X(zero)
	ldw s1, T_Y(zero)
	ldw s2, T_type(zero)
	ldw s3, T_orientation(zero)
	add s4, a0, zero #p-value
	add s5, zero, zero #offset


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

	ldw s0, 0(sp)
	ldw s1, 4(sp)
	ldw s2, 8(sp)
	ldw s3, 12(sp)
	ldw s4, 16(sp)
	ldw s5, 20(sp)
	ldw ra, 24(sp)
	addi sp, sp, 28

	ret

;END:draw_tetromino


;BEGIN:generate_tetromino
generate_tetromino:
	addi t1, zero, 4

regenerate:
	add  t0, zero, zero
	stw  t0, RANDOM_NUM(zero)
	ldw  t0, RANDOM_NUM(zero)
	slli t0, t0, 28
	srli t0, t0, 28
	blt  t1, t0, regenerate

;; initialize (6,1) for x,y coords, T_orientation and T_type
	stw  t0, T_type(zero)
	addi t0, zero, 6
	addi t1, zero, 1
	stw  t0, T_X(zero)
	stw  t1, T_Y(zero)
	stw  zero, T_orientation(zero)
	addi  a0, zero, FALLING ;; FALLING

	addi sp, sp, -4
	stw  ra, 0(sp)

	call draw_tetromino

	ldw  ra, 0(sp)
	addi sp, sp, 4
	ret
;END:generate_tetromino

;BEGIN:detect_collision
detect_collision:
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

east_collision:
	addi s0, s0, 1			# update x-coordinate for movement EAST
	addi t0, zero, 12		# check if it is within bounds of the screen
	beq  s0, t0, COLLISION
	jmpi overlapping
west_collision:
	addi s0, s0, -1			# update x-coordinate for movement WEST
	addi t0, zero, -1		# check if it is within bounds of the screen
	beq  s0, t0, COLLISION
	jmpi overlapping
south_collision:
	addi s1, s1, 1			# update y-coordinate for movement SOUTH
	addi t0, zero, 8		# check if it is within bounds of the screen
	beq  s1, t0, COLLISION
overlapping:

	slli s2, s2, 4 			#s2 = 16*s2; offset by type * 16
	add s5, zero, s2

	slli s3, s3, 2			#s2 = 16*s2; offset by type * 4
	add s5, s5, s3

	#call get_gsa on Anchor
	add a2, s4, zero 		#p-value
	add a0, s0, zero
	add a1, s1, zero

	addi t2, zero, 7		# to check for x coordinate out of bounds
	addi t3, zero, 11		# to check for y coordinate out of bounds
	blt  a0, zero, COLLISION
	blt  a1, zero, COLLISION
	blt  t2, a1, COLLISION
	blt  t3, a0, COLLISION

	call get_gsa
	beq v0, s6, COLLISION	# check if anchor point has been moved so that it collides

	#call get_gsa on offset 0
	ldw t0, DRAW_Ax(s5) 	#address of Array of X-offset
	ldw t1, DRAW_Ay(s5) 	#address of Array of Y-offset
	ldw t0, 0(t0) 			#x first offset
	ldw t1, 0(t1)

	addi t2, zero, 7		# to check for x coordinate out of bounds
	addi t3, zero, 11		# to check for y coordinate out of bounds
	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	blt  a0, zero, COLLISION
	blt  t2, a1, COLLISION
	blt  t3, a0, COLLISION

	call get_gsa
	beq v0, s6, COLLISION	# check another pixel from the tetromino for collision

	#call get_gsa on offset 1
	ldw t0, DRAW_Ax(s5) 	#address of Array of X-offset
	ldw t1, DRAW_Ay(s5) 	#address of Array of Y-offset
	ldw t0, 4(t0) #x offset
	ldw t1, 4(t1)

	addi t2, zero, 7		# to check for x coordinate out of bounds
	addi t3, zero, 11		# to check for y coordinate out of bounds

	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	blt  a0, zero, COLLISION
	blt  t2, a1, COLLISION
	blt  t3, a0, COLLISION

	call get_gsa
	beq v0, s6, COLLISION	# check another pixel from the tetromino for collision

	addi t2, zero, 7		# to check for x coordinate out of bounds
	addi t3, zero, 11		# to check for y coordinate out of bounds

	#call get_gsa on offset 2
	ldw t0, DRAW_Ax(s5)		#address of Array of X-offset
	ldw t1, DRAW_Ay(s5) 	#address of Array of Y-offset
	ldw t0, 8(t0) #x  offset
	ldw t1, 8(t1)

	addi t2, zero, 7		# to check for x coordinate out of bounds
	addi t3, zero, 11		# to check for y coordinate out of bounds

	add a2, s4, zero #p-value
	add a0, s0, t0
	add a1, s1, t1
	blt  a0, zero, COLLISION
	blt  t2, a1, COLLISION
	blt  t3, a0, COLLISION

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
	jmpi return_collision

COLLISION:
	# if collision or an overlap was detected, return type of collision/overlap
	add v0, zero, s4		# store collision type in return register

	ldw  s0, 0(sp)
	ldw  s1, 4(sp)
	ldw  s2, 8(sp)
	ldw  s3, 12(sp)
	ldw  s4, 16(sp)
	ldw  s5, 20(sp)
	ldw  s6, 24(sp)
	ldw  ra, 28(sp)
	addi sp, sp, 32

return_collision:
	ret
;END:detect_collision


;BEGIN:act
act:
	addi sp, sp, -12
	stw s6, 0(sp)
	stw s7, 4(sp)
	stw s0, 8(sp)

	add s0, a0, zero		#action type
	addi t0, zero, moveL
	beq t0, a0, moveLProcedure

	addi t0, zero, rotL
	beq t0, a0, rotLProcedure

	addi t0, zero, reset
	beq t0, a0, resetProcedure

	addi t0, zero, rotR
	beq t0, a0, rotRProcedure

	addi t0, zero, moveR
	beq t0, a0, moveRProcedure

	addi t0, zero, moveD
	beq t0, a0, moveDProcedure

movingSuccess:
	add s6, zero, zero
	jmpi endOfAction					 #v0 already taken care of

resetProcedure:
	addi sp, sp, -4
	stw  ra, 0(sp)

	call reset_game

	ldw  ra, 0(sp)
	addi sp, sp, 4

	jmpi endOfAction

moveLProcedure:
	addi sp, sp, -4
	stw  ra, 0(sp)
										# check if collision left
	add a0, zero, zero
	call detect_collision

	ldw ra, 0(sp)
	addi sp, sp, 4

	addi t0, zero, NONE
	beq t0, v0, moveLeft 				# if no collision do the effective move
	addi s6, zero, 1 #failed			# if collision return
	jmpi endOfAction

moveRProcedure:
	addi sp, sp, -4
	stw  ra, 0(sp)

	# check if collision right
	addi a0, zero, 1
	call detect_collision

	ldw ra, 0(sp)
	addi sp, sp, 4

	# if yes return
	addi t0, zero, NONE
	beq t0, v0, moveRight				# if yes return
	addi s6, zero, 1 					#fail
	jmpi endOfAction

moveDProcedure:
	# check if collision down
	addi sp, sp, -4
	stw  ra, 0(sp)

	addi a0, zero, 2
	call detect_collision

	ldw ra, 0(sp)
	addi sp, sp, 4

	# if yes return
	addi t0, zero, NONE
	beq t0, v0, moveDown 			# if yes return
	addi s6, zero, 1 				#fail
	jmpi endOfAction

moveLeft:
	# if no collision update x position in memory
	addi sp, sp, -4
	stw  ra, 0(sp)

	ldw t0, T_X(zero)
	addi t0, t0, -1
	stw t0, T_X(zero)
	call draw_tetromino

	ldw  ra, 0(sp)
	addi sp, sp, 4
	jmpi movingSuccess

moveRight:
	# if no collision update x position in memory
	addi sp, sp, -4
	stw  ra, 0(sp)

	ldw t0, T_X(zero)
	addi t0, t0, 1
	stw t0, T_X(zero)
	call draw_tetromino

	ldw  ra, 0(sp)
	addi sp, sp, 4
	jmpi movingSuccess

moveDown:
	# if no collision update y position in memory
	addi sp, sp, -4
	stw  ra, 0(sp)

	ldw t0, T_Y(zero)
	addi t0, t0, 1
	stw t0, T_Y(zero)
	call draw_tetromino

	ldw  ra, 0(sp)
	addi sp, sp, 4

	jmpi movingSuccess

rotLProcedure:
	addi sp, sp, -4
	stw  ra, 0(sp)

	add s7, a0, zero 				# store rotation variable
	call rotate_tetromino 			# argument (rotL; rotR)
	addi a0, zero, 3 				# overlap
	call detect_collision

	ldw ra, 0(sp)
	addi sp, sp, 4

	addi t0, zero, NONE
	beq t0, v0, rotateLeftOk 		#if success, return
									#if T_X is larger than 6, call tryMovingleft
	ldw t0, T_X(zero)				#if fail decide on trying to move left or right depending on location
	addi t1, zero, 6
	blt t0, t1, tryMovingRight
	jmpi tryMovingLeft

rotRProcedure:
	addi sp, sp, -4
	stw  ra, 0(sp)

	add s7, a0, zero 				# store rotation variable
	call rotate_tetromino 			# argument (rotL; rotR)
	addi a0, zero, 3 				# overlap
	call detect_collision

	ldw ra, 0(sp)
	addi sp, sp, 4

	addi t0, zero, NONE
	beq t0, v0, rotateRightOk 		#if success, return
									#if T_X is larger than 6, call tryMovingleft
	ldw t0, T_X(zero)				#if fail decide on trying to move left or right depending on location
	addi t1, zero, 6
	blt t0, t1, tryMovingRight
	jmpi tryMovingLeft


rotateLeftOk:
	addi sp, sp, -4
	stw  ra, 0(sp)

	addi v0, zero, 0				#success
	add a0, s7, zero
	call draw_tetromino

	ldw  ra, 0(sp)
	addi sp, sp, 4
	jmpi endOfAction

rotateRightOk:
	addi sp, sp, -4
	stw  ra, 0(sp)

	addi s6, zero, 0				#success
	add a0, s7, zero
	call draw_tetromino

	ldw  ra, 0(sp)
	addi sp, sp, 4
	jmpi endOfAction

tryMovingLeft:
	addi sp, sp, -4
	stw  ra, 0(sp)

	addi a0, zero, 1
	call act						#moveL
	ldw  ra, 0(sp)
	addi sp, sp, 4

	beq v0, zero, movingSuccess		#succeeded

	ldw t0, T_X(zero)
	addi t0, t0, -1
	stw t0, T_X(zero)

	addi sp, sp, -4
	stw  ra, 0(sp)

	addi a0, zero, 1
	call act						#moveL
	ldw  ra, 0(sp)
	addi sp, sp, 4

	beq v0, zero, movingSuccess
	#double fail
	ldw t0, T_X(zero)
	addi t0, t0, 1
	stw t0, T_X(zero)

	addi t7, zero, 2 #rotL
	beq t7, s7, rotateBackRight
	jmpi rotateBackLeft

tryMovingRight:
	addi sp, sp, -4
	stw  ra, 0(sp)

	addi a0, zero, 0x10
	call act						#moveR

	ldw  ra, 0(sp)
	addi sp, sp, 4

	beq v0, zero, movingSuccess

	ldw t0, T_X(zero)
	addi t0, t0, 1
	stw t0, T_X(zero)

	addi sp, sp, -4
	stw  ra, 0(sp)

	addi a0, zero, 0x10
	call act						#moveR
	ldw  ra, 0(sp)
	addi sp, sp, 4

	beq v0, zero, movingSuccess

	#double fail
	ldw t0, T_X(zero)
	addi t0, t0, -1
	stw t0, T_X(zero)

	addi t7, zero, 2 #rotL
	beq t7, s7, rotateBackRight
	jmpi rotateBackLeft


rotateBackRight:
	addi sp, sp, -4
	stw  ra, 0(sp)
	addi a0, zero, 8
	call rotate_tetromino

	ldw  ra, 0(sp)
	addi sp, sp, 4
	addi s6, zero, 1 				#fail
	jmpi endOfAction


rotateBackLeft:
	addi sp, sp, -4
	stw  ra, 0(sp)
	addi a0, zero, 2
	call rotate_tetromino
	ldw  ra, 0(sp)
	addi sp, sp, 4
	addi s6, zero, 1 				#fail
	jmpi endOfAction

endOfAction:
	add v0, s6, zero		#transfer s6 to return value
							#ra isnt popped, because endOfAction must return to wherever the base procedure was called
	ldw s6, 0(sp)
	ldw s7, 4(sp)
	ldw s0, 8(sp)
	addi sp, sp, 12
	ret
; END:act

; BEGIN:rotate_tetromino
rotate_tetromino:
	addi t0, zero, rotL
	beq a0, t0, rotate_tetromino_left

	ldw t0, T_orientation(zero) 	#rotate to the right
	addi t0, t0, 1
	andi t0, t0, 3
	stw t0, T_orientation(zero)
	jmpi end_rotation

rotate_tetromino_left:
	ldw t0, T_orientation(zero) 	#rotate to the left
	addi t0, t0, -1
	andi t0, t0, 3
	stw t0, T_orientation(zero)

end_rotation:
	ret
; END:rotate_tetromino

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


font_data:
    .word 0xFC  ; 0
    .word 0x60  ; 1
    .word 0xDA  ; 2
    .word 0xF2  ; 3
    .word 0x66  ; 4
    .word 0xB6  ; 5
    .word 0xBE  ; 6
    .word 0xE0  ; 7
    .word 0xFE  ; 8
    .word 0xF6  ; 9

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
  .word 0x01	#call moveleft
	#if moveLeft returns 0, return
	#else, update T_X to the left and call moveLeft again
	#if moveLeft returns 0, return, else moveLeft failed again, call rotateback

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
