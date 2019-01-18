#ifndef __LIBFIRM_STDLIB_H__
#define __LIBFIRM_STDLIB_H__

#include <stddef.h>

#ifndef __cplusplus
extern "C" {
#endif

__attribute__((noreturn)) void abort(void);
__attribute__((noreturn)) void exit(int status);

#ifndef __cplusplus
} /* extern "C" */
#endif

#endif /* __LIBFIRM_STDLIB_H__ */
