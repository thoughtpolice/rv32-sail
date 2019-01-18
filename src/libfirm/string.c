#include <string.h>

void *memcpy(void *dest, const void* src, size_t n) {
  char *p = (char*)dest;
  const char *q = (const char*)src;

  while (n-- > 0)
    *p++ = *q++;

  return p;
}
