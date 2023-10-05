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
	li $t0, 0
	sw $t0, -8($fp)
	li $t0, 10
	la $t1, retour
	sw $t0, 0($t1)
	li $t0, 32
	la $t1, espace
	sw $t0, 0($t1)
	b __main_0
__main_1:
	lw $t0, 4($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, -8($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal affiche_ligne
	addi $sp, $sp, 8
	addi $sp, $sp, 0
	la $t0, retour
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 11
	syscall
	li $t0, 1
	lw $t1, -8($fp)
	add $t0, $t1, $t0
	sw $t0, -8($fp)
__main_0:
	li $t0, 1
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	lw $t1, -8($fp)
	slt $t0, $t1, $t0
	bnez $t0, __main_1
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
affiche_ligne:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, -4
	li $t0, 0
	sw $t0, -8($fp)
	b __affiche_ligne_0
__affiche_ligne_1:
	lw $t0, 8($fp)
	lw $t1, 8($fp)
	mul $t0, $t1, $t0
	lw $t1, -8($fp)
	lw $t2, -8($fp)
	mul $t1, $t2, $t1
	lw $t2, 4($fp)
	lw $t3, 4($fp)
	mul $t2, $t3, $t2
	add $t1, $t2, $t1
	slt $t0, $t1, $t0
	bnez $t0, __affiche_ligne_2
	li $t0, 35
	move $a0, $t0
	li $v0, 11
	syscall
	b __affiche_ligne_3
__affiche_ligne_2:
	li $t0, 46
	move $a0, $t0
	li $v0, 11
	syscall
__affiche_ligne_3:
	la $t0, espace
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 11
	syscall
	li $t0, 1
	lw $t1, -8($fp)
	add $t0, $t1, $t0
	sw $t0, -8($fp)
__affiche_ligne_0:
	li $t0, 1
	lw $t1, 8($fp)
	add $t0, $t1, $t0
	lw $t1, -8($fp)
	slt $t0, $t1, $t0
	bnez $t0, __affiche_ligne_1
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
retour:
	.word 0
espace:
	.word 0
