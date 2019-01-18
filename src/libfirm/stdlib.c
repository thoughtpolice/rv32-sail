#include <stdlib.h>

void abort(void) {
  exit(EXIT_FAILURE);
}

void exit(int status) {
  //  TODO FIXME
  (void)status;
  __asm__ __volatile__("1: j 1b");
  __builtin_unreachable();
}
