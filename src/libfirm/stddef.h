#ifndef __LIBFIRM_STDDEF_H__
#define __LIBFIRM_STDDEF_H__

#define NULL (0L)

#define offsetof(type, member) __builtin_offsetof(type, member)

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;
typedef __WCHAR_TYPE__   wchar_t;

#endif /* __LIBFIRM_STDDEF_H__ */
