// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.

#include "firmware.h"

int main(int ac, char** av)
{
	(void)ac, (void)av;
	//sieve();
	stats();
	
	return 42;
}
