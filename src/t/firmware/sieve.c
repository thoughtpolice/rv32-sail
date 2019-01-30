// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.

// A simple Sieve of Eratosthenes

#include "firmware.h"

#define BITMAP_SIZE 64

static uint32_t bitmap[BITMAP_SIZE/32];
static uint32_t hash;

static uint32_t mkhash(uint32_t a, uint32_t b)
{
	// The XOR version of DJB2
	return ((a << 5) + a) ^ b;
}

static void bitmap_set(int idx)
{
	bitmap[idx/32] |= 1 << (idx % 32);
}

static bool bitmap_get(int idx)
{
	return (bitmap[idx/32] & (1 << (idx % 32))) != 0;
}

static void print_prime(int idx, int val)
{
	if (idx < 10)
		printf(" ");
	print_dec(idx);
	if (idx / 10 == 1)
		goto force_th;
	switch (idx % 10) {
		case 1: printf("st"); break;
		case 2: printf("nd"); break;
		case 3: printf("rd"); break;
	force_th:
		default: printf("th"); break;
	}
	printf(" prime is ");
	print_dec(val);
	puts(".");

	hash = mkhash(hash, idx);
	hash = mkhash(hash, val);
}

void sieve(void)
{
	puts("\nSieve test:");
	int idx = 1;
	hash = 5381;
	print_prime(idx++, 2);
	for (int i = 0; i < BITMAP_SIZE; i++) {
		if (bitmap_get(i))
			continue;
		print_prime(idx++, 3+2*i);
		for (int j = 2*(3+2*i);; j += 3+2*i) {
			if (j%2 == 0)
				continue;
			int k = (j-3)/2;
			if (k >= BITMAP_SIZE)
				break;
			bitmap_set(k);
		}
	}

	printf("checksum: ");
	print_hex(hash, 8);

	if (hash == 0x1772A48F) {
		puts(" OK");
	} else {
		puts(" ERROR");
		__asm__ volatile ("ebreak");
	}
}

