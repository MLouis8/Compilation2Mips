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
	li $t0, 1
	la $t1, x
	sw $t0, 0($t1)
	li $t0, 2
	la $t1, y
	sw $t0, 0($t1)
	la $t0, x
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	la $t0, y
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 3
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	mul $t0, $t0, $t1
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	la $t1, x
	sw $t0, 0($t1)
	la $t0, y
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	la $t0, x
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 48
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
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
x:
	.word 0
y:
	.word 0
