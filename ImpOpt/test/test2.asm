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
	subi $sp, $sp, 4
	sw $s0, 0($sp)
	subi $sp, $sp, 4
	sw $s1, 0($sp)
	subi $sp, $sp, 4
	sw $s2, 0($sp)
	subi $sp, $sp, 4
	sw $s3, 0($sp)
	subi $sp, $sp, 4
	sw $s4, 0($sp)
	addi $sp, $sp, -28
	li $t0, 1
	move $s1, $t0
	li $t0, 2
	sw $t0, -12($fp)
	li $t0, 4
	move $s0, $t0
	li $t0, 6
	sw $t0, -8($fp)
	li $t0, 8
	sw $t0, -16($fp)
	li $t0, -2
	sw $t0, -20($fp)
	li $t0, 48
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, -12($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, -20($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, -16($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	move $a0, $t0
	li $v0, 11
	syscall
	addi $sp, $sp, 28
	lw $s4, 0($sp)
	addi $sp, $sp, 4
	lw $s3, 0($sp)
	addi $sp, $sp, 4
	lw $s2, 0($sp)
	addi $sp, $sp, 4
	lw $s1, 0($sp)
	addi $sp, $sp, 4
	lw $s0, 0($sp)
	addi $sp, $sp, 4
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	li $t0, 0
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
