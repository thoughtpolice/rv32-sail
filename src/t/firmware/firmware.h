// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.

#ifndef FIRMWARE_H
#define FIRMWARE_H

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

// print.c
void print_dec(unsigned int val);
void print_hex(unsigned int val, int digits);

// sieve.c
void sieve(void);

// stats.c
void stats(void);

#endif
