	.section text
	.global main

main:
	addi sp, sp, -4
	sw ra, 4(sp) /* ra = caller save */

	/* body, part 1: risc-v tests */

#define TEST(n)				\
	.global n;			\
	jal zero,n; 			\
	.global n ## _ret; 		\
	n ## _ret:

#define NO_TEST(n)			\
	.global n ## _ret; 		\
	n ## _ret:

	NO_TEST(lui)
	NO_TEST(auipc)
	NO_TEST(j)
	NO_TEST(jal)
	NO_TEST(jalr)

	NO_TEST(beq)
	NO_TEST(bne)
	NO_TEST(blt)
	NO_TEST(bge)
	NO_TEST(bltu)
	NO_TEST(bgeu)

	NO_TEST(lb)
	NO_TEST(lh)
	NO_TEST(lw)
	NO_TEST(lbu)
	NO_TEST(lhu)

	NO_TEST(sb)
	NO_TEST(sh)
	NO_TEST(sw)

	NO_TEST(addi)
	NO_TEST(slti) // also tests sltiu
	NO_TEST(xori)
	NO_TEST(ori)
	NO_TEST(andi)
	NO_TEST(slli)
	NO_TEST(srli)
	NO_TEST(srai)

	NO_TEST(add)
	NO_TEST(sub)
	NO_TEST(sll)
	NO_TEST(slt) // what is with sltu ?
	NO_TEST(xor)
	NO_TEST(srl)
	NO_TEST(sra)
	NO_TEST(or)
	NO_TEST(and)

	NO_TEST(mulh)
	NO_TEST(mulhsu)
	NO_TEST(mulhu)
	NO_TEST(mul)

	NO_TEST(div)
	NO_TEST(divu)
	NO_TEST(rem)
	NO_TEST(remu)

	NO_TEST(simple)

	/* body, part 2: sieve and stat output */
	jal ra, sieve
	jal ra, stats

	/* body, part 3: no part 3 */
	li a0, 42

	/* fin */
	lw ra, 4(sp)
	addi sp, sp, 4
	ret
