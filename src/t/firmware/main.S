	.section text
	.global main

.intro_text:
	.ascii "\nRUNNING RISC-V TESTS\n\n"
	.byte 0x00
	.balign 4, 0
.finished_text:
	.ascii "\nFINISHED RISC-V TESTS\n"
	.byte 0x00
	.balign 4, 0

main:
	addi sp, sp, -16
	sw ra, 12(sp) /* ra = caller save */

	/* nb: tests don't go by the normal ABI, so x1-x6 get used,
	** even though e.g. sp is callee save. unbreak that. */
	mv s4, gp
	mv s5, tp
	mv s6, sp

	/* body, part 1: risc-v tests */

	addi a0,zero,0
	lui  a2,%hi(.intro_text)
	addi a2,a2,%lo(.intro_text)
.1:
	lb    a1,0(a2)
	beq   a1,zero,.2
	ecall
	addi  a2,a2,1
	jal   zero,.1
.2:

#define TEST(n)				\
	.global n;			\
	jal zero,n; 			\
	.global n ## _ret; 		\
	n ## _ret:

#define NO_TEST(n)			\
	.global n ## _ret; 		\
	n ## _ret:

	TEST(lui)
	TEST(auipc)
	TEST(j)
	TEST(jal)
	TEST(jalr)

	TEST(beq)
	TEST(bne)
	TEST(blt)
	TEST(bge)
	TEST(bltu)
	TEST(bgeu)

	TEST(lb)
	TEST(lh)
	TEST(lw)
	TEST(lbu)
	TEST(lhu)

	TEST(sb)
	TEST(sh)
	TEST(sw)

	TEST(addi)
	TEST(slti) // also tests sltiu
	TEST(xori)
	TEST(ori)
	TEST(andi)
	TEST(slli)
	TEST(srli)
	TEST(srai)

	TEST(add)
	TEST(sub)
	TEST(sll)
	TEST(slt) // what is with sltu ?
	TEST(xor)
	TEST(srl)
	TEST(sra)
	TEST(or)
	TEST(and)

	TEST(mulh)
	TEST(mulhsu)
	TEST(mulhu)
	TEST(mul)

	TEST(div)
	TEST(divu)
	TEST(rem)
	TEST(remu)

	TEST(simple)

	addi a0,zero,0
	lui  a2,%hi(.finished_text)
	addi a2,a2,%lo(.finished_text)
.3:
	lb    a1,0(a2)
	beq   a1,zero,.4
	ecall
	addi  a2,a2,1
	jal   zero,.3
.4:

	/* NB: unclobber caller stack */
	mv gp, s4
	mv tp, s5
	mv sp, s6

	/* body, part 2: sieve and stat output */
	jal ra, sieve
	jal ra, stats

	/* body, part 3: no part 3 */
	li a0, 42

	/* fin */
	lw ra, 12(sp)
	addi sp, sp, 16
	ret
