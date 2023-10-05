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
	li $t0, 1
	li $t1, 1
	li $t2, 1
	li $t3, 1
	li $t4, 1
	li $t5, 1
	li $t6, 1
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	li $t6, 1
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	li $t6, 1
	li $t7, 1
	add $t7, $t7, $t6
	lw $t6, 0($sp)
	addi $sp, $sp, 4
	add $t7, $t7, $t6
	lw $t6, 0($sp)
	addi $sp, $sp, 4
	add $t6, $t7, $t6
	add $t5, $t6, $t5
	add $t4, $t5, $t4
	add $t3, $t4, $t3
	add $t2, $t3, $t2
	add $t1, $t2, $t1
	add $t0, $t1, $t0
	sw $t0, -8($fp)
	subi $sp, $sp, 4
	sw $t7, 0($sp)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	subi $sp, $sp, 4
	sw $t5, 0($sp)
	subi $sp, $sp, 4
	sw $t4, 0($sp)
	subi $sp, $sp, 4
	sw $t3, 0($sp)
	subi $sp, $sp, 4
	sw $t2, 0($sp)
	subi $sp, $sp, 4
	sw $t1, 0($sp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal bis
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	lw $t2, 0($sp)
	addi $sp, $sp, 4
	lw $t3, 0($sp)
	addi $sp, $sp, 4
	lw $t4, 0($sp)
	addi $sp, $sp, 4
	lw $t5, 0($sp)
	addi $sp, $sp, 4
	lw $t6, 0($sp)
	addi $sp, $sp, 4
	lw $t7, 0($sp)
	addi $sp, $sp, 4
	addi $sp, $sp, 32
	b __main_0
__main_1:
	li $t0, 35
	move $a0, $t0
	li $v0, 11
	syscall
	li $t0, -1
	lw $t1, -8($fp)
	add $t0, $t1, $t0
	sw $t0, -8($fp)
__main_0:
	lw $t0, -8($fp)
	li $t1, 0
	slt $t0, $t1, $t0
	bnez $t0, __main_1
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
bis:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, -4
	li $t0, 4
	li $t1, 3
	li $t2, 2
	li $t3, 4
	li $t4, 3
	li $t5, 2
	add $t4, $t5, $t4
	add $t3, $t4, $t3
	add $t2, $t3, $t2
	add $t1, $t2, $t1
	add $t0, $t1, $t0
	sw $t0, -8($fp)
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
