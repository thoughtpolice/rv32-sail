	.section .text
	.global start

reset_vec:
	nop
	nop
	j start
	nop

/* Main program
 **********************************/

start:
	/* zero-initialize all registers */
	addi x1,  zero, 0
	addi x2,  zero, 0
	addi x3,  zero, 0
	addi x4,  zero, 0
	addi x5,  zero, 0
	addi x6,  zero, 0
	addi x7,  zero, 0
	addi x8,  zero, 0
	addi x9,  zero, 0
	addi x10, zero, 0
	addi x11, zero, 0
	addi x12, zero, 0
	addi x13, zero, 0
	addi x14, zero, 0
	addi x15, zero, 0
	addi x16, zero, 0
	addi x17, zero, 0
	addi x18, zero, 0
	addi x19, zero, 0
	addi x20, zero, 0
	addi x21, zero, 0
	addi x22, zero, 0
	addi x23, zero, 0
	addi x24, zero, 0
	addi x25, zero, 0
	addi x26, zero, 0
	addi x27, zero, 0
	addi x28, zero, 0
	addi x29, zero, 0
	addi x30, zero, 0
	addi x31, zero, 0

	/* set stack pointer */
	lui sp,(64*1024)>>12

	/* set gp and tp */
	lui gp, %hi(0xdeadbeef)
	addi gp, gp, %lo(0xdeadbeef)
	addi tp, gp, 0

	/* call main */
	jal ra, main
	mv t1, a0 /* save return */

	/* print "DONE\n" */
	addi a0,zero,0
	addi a1,zero,'\n'
	ecall
	addi a1,zero,'D'
	ecall
	addi a1,zero,'O'
	ecall
	addi a1,zero,'N'
	ecall
	addi a1,zero,'E'
	ecall
	addi a1,zero,'\n'
	ecall

	/* trap */
	mv a0, t1
	ebreak
