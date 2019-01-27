// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.

#include "firmware.h"

void print_chr(char ch)
{
	__asm__ __volatile__(
		"addi a0,zero,0;"
		"add a1,zero,%0;"
		"ecall;\n\t"
	:	/* No outputs */
	:	"g" (ch)
	:	"a0", "a1"
	);
}

void print_str(const char *p)
{
	while (*p != 0) {
		print_chr(*p);
		p++;
	}
}

void print_dec(unsigned int val)
{
	char buffer[10];
	char *p = buffer;
	while (val || p == buffer) {
		*(p++) = val % 10;
		val = val / 10;
	}
	while (p != buffer) {
		char c = *(--p);
		print_chr('0' + c);
	}
}

void print_hex(unsigned int val, int digits)
{
	for (int i = (4*digits)-4; i >= 0; i -= 4) {
		char c = "0123456789ABCDEF"[(val >> i) % 16];
		print_chr(c);
	}
}

