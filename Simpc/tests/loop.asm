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
	addi $sp, $sp, -4
	li $t0, 10
	la $t1, n
	sw $t0, 0($t1)
	li $t0, 0
	sw $t0, -8($fp)
	b __main_0
__main_1:
	lw $t0, -8($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 48
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	move $a0, $t0
	li $v0, 11
	syscall
	li $t0, 1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, -8($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	sw $t0, -8($fp)
__main_0:
	la $t0, n
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, -8($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	slt $t0, $t0, $t1
	bnez $t0, __main_1
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
n:
	.word 0