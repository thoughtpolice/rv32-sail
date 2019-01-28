#include <stdlib.h>

void abort(void) {
  exit(EXIT_FAILURE);
}

void exit(int status) {
  (void)status; //  TODO FIXME: propagate status
  __asm__ __volatile__("ebreak; 1: j 1b");
  __builtin_unreachable();
}
