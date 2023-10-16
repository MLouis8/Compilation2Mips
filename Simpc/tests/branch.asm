.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
init_end:
	subi $sp, $sp, 4
	sw $v0, 0($sp)
	jal main
	li $v0, 10
	syscall
main:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
	li $t0, 2
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 1
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	slt $t0, $t0, $t1
	bnez $t0, __main_0
	li $t0, 99
	move $a0, $t0
	li $v0, 11
	syscall
	b __main_1
__main_0:
	li $t0, 3
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 4
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	slt $t0, $t0, $t1
	bnez $t0, __main_2
	li $t0, 98
	move $a0, $t0
	li $v0, 11
	syscall
	b __main_3
__main_2:
	li $t0, 97
	move $a0, $t0
	li $v0, 11
	syscall
__main_3:
__main_1:
	li $t0, 100
	move $a0, $t0
	li $v0, 11
	syscall
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
#built-in atoi
atoi:
	li $v0, 0
atoi_loop:
	lbu $t0, 0($a0)
	beqz $t0, atoi_end
	addi $t0, $t0, -48
	bltz $t0, atoi_error
	bge $t0, 10, atoi_error
	mul $v0, $v0, 10
	add $v0, $v0, $t0
	addi $a0, $a0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	jr $ra
.data
