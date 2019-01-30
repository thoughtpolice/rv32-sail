#ifndef __LIBFIRM_STRING_H__
#define __LIBFIRM_STRING_H__

#ifdef __cplusplus
extern "C" {
#endif

char *strcpy(char *dest, const char *src);
char *strncpy(char *dst, const char *src, size_t n);
int strcmp(const char *s1, const char *s2);
void *memcpy(void *dest, const void* src, size_t n);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __LIBFIRM_STRING_H__ */
