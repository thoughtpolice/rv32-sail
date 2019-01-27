#ifndef _ENV_PICORV32_TEST_H
#define _ENV_PICORV32_TEST_H

#ifndef TEST_FUNC_NAME
#  define TEST_FUNC_NAME mytest
#  define TEST_FUNC_TXT "mytest"
#  define TEST_FUNC_RET mytest_ret
#endif

#define RVTEST_RV32U
#define TESTNUM x28

#define RVTEST_CODE_BEGIN		\
	.text;				\
	.global TEST_FUNC_NAME;		\
	.global TEST_FUNC_RET;		\
TEST_FUNC_NAME:				\
	addi	a0,zero,0;		\
	lui	a2,%hi(.test_name);	\
	addi	a2,a2,%lo(.test_name);	\
.prname_next:				\
	lb	a1,0(a2);		\
	beq	a1,zero,.prname_done;	\
	ecall;							\
	addi	a2,a2,1;		\
	jal	zero,.prname_next;	\
.test_name:				\
	.ascii TEST_FUNC_TXT;	\
	.byte 0x00;			\
	.balign 4, 0;			\
.prname_done:				\
	addi	a1,zero,':';	\
	ecall;					\
	addi	a1,zero,' ';	\
	ecall;

#define RVTEST_PASS		\
	addi	a0,zero,0;		\
	addi	a1,zero,'O';	\
	ecall;					\
	addi	a1,zero,'K';	\
	ecall;					\
	addi	a1,zero,'!';	\
	ecall;					\
	addi	a1,zero,'\n';	\
	ecall;					\
	jal	zero,TEST_FUNC_RET;

#define RVTEST_FAIL		\
	addi	a0,zero,0;		\
	addi	a1,zero,'E';	\
	ecall;					\
	addi	a1,zero,'R';	\
	ecall;					\
	addi	a1,zero,'R';	\
	ecall;					\
	addi	a1,zero,'O';	\
	ecall;					\
	addi	a1,zero,'R';	\
	ecall;					\
	addi	a1,zero,'\n';	\
	ecall;					\
	ebreak;

#define RVTEST_CODE_END
#define RVTEST_DATA_BEGIN .balign 4;
#define RVTEST_DATA_END

#endif
