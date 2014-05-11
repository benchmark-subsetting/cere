AC_INIT([llvm-loop-extractor], [0.0.1], [llvm-loop-extractor@prism.uvsq.fr])
AM_SILENT_RULES([yes])
AC_CONFIG_AUX_DIR(autoconf)
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([subdir-objects silent-rules foreign])
AC_PROG_LIBTOOL  

LT_INIT
AC_PROG_CC
AC_PROG_CXX
AX_LLVM([3.3],[all])


# Checks for header files.
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([inttypes.h libintl.h limits.h malloc.h stddef.h stdint.h stdlib.h string.h sys/ioctl.h sys/param.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([atexit isascii memmove memset strcasecmp strchr strcspn strdup strndup strrchr strspn strstr strtol])

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 src/LoopExtractorAll/Makefile
                 src/LoopInstrumentation/Makefile
                 src/LoopManager/Makefile
                 src/ccan/Makefile
                 src/rdtsc/Makefile
                 src/memory_dump/Makefile])
AC_OUTPUT

