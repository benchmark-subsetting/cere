AC_PREREQ([2.69])
AC_INIT([llvm-loop-extractor], [0.0.1], [llvm-loop-extractor@prism.uvsq.fr])
AM_SILENT_RULES([yes])
AC_CONFIG_AUX_DIR(autoconf)
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])
AC_REQUIRE_AUX_FILE([tap-driver.sh])
AM_INIT_AUTOMAKE([subdir-objects silent-rules foreign])
AC_PROG_LIBTOOL  

LT_INIT
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AX_LLVM([3.3],[all])

AC_CHECK_LIB([c], [exit])
AC_CHECK_LIB([gfortran], [exit])
AC_CHECK_LIB([m], [sin])

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h inttypes.h limits.h malloc.h stdint.h stdlib.h string.h sys/time.h unistd.h utime.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_PID_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_CHOWN
AC_FUNC_ERROR_AT_LINE
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_REALLOC


AC_CHECK_FUNCS([atexit floor gettimeofday memset mkdir pow sqrt strchr strdup strerror strrchr strstr strtoul tzset utime])

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 src/LoopExtractorAll/Makefile
                 src/LoopInstrumentation/Makefile
                 src/LoopManager/Makefile
                 src/rdtsc/Makefile
                 src/memory_dump/Makefile
                 tests/Makefile])

AC_CONFIG_FILES([src/lec/lec], [chmod +x src/lec/lec])
AC_CONFIG_FILES([src/lel/lel], [chmod +x src/lel/lel])

AC_OUTPUT
