#include <stddef.h>
#include <string.h>

int strcmp(const char *s1, const char *s2) {
  while (*s1 && *s2 && *s1 == *s2) { s1++; s2++; }
  return *s1 - *s2;
}

char *strcpy(char *dest, const char *src) {
  const unsigned char *s = (const unsigned char*)src;
  unsigned char *d = (unsigned char*)dest;
  while ((*d++ = *s++));
  return dest;
}

char *strncpy(char *dst, const char *src, size_t n) {
  size_t i;

  for (i = 0; i < n && src[i]; i++)
    dst[i] = src[i];

  for (; i < n; i++)
    dst[i] = 0;

  return dst;
}

void *memcpy(void *dest, const void* src, size_t n) {
  char *p = (char*)dest;
  const char *q = (const char*)src;

  while (n-- > 0)
    *p++ = *q++;

  return p;
}
