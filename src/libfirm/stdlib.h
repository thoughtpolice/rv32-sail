#ifndef __LIBFIRM_STDLIB_H__
#define __LIBFIRM_STDLIB_H__

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

__attribute__((noreturn)) void abort(void);
__attribute__((noreturn)) void exit(int status);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __LIBFIRM_STDLIB_H__ */
