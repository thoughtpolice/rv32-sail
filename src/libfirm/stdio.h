#ifndef __LIBFIRM_STDIO_H__
#define __LIBFIRM_STDIO_H__

#include <stdarg.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

int putchar(int c);
int puts(const char *s);
int printf(const char *s, ...);
int snprintf(char *out, size_t n, const char *s, ...);
int vprintf(const char *s, va_list vl);
int vsnprintf(char *out, size_t n, const char *s, va_list vl);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __LIBFIRM_STDIO_H__ */
