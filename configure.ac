AC_PREREQ([2.69])
AC_INIT([cere],[0.3.1],[cere-dev@googlegroups.com])
AM_SILENT_RULES([yes])
AC_CONFIG_AUX_DIR(autoconf)
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])
AC_REQUIRE_AUX_FILE([tap-driver.sh])
AM_INIT_AUTOMAKE([subdir-objects silent-rules foreign serial-tests])
LT_INIT

LT_INIT
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_SUBST(CERELIBS, $libdir)

# Check for specific compilers
AC_PROG_CC([gcc])
AC_DEFINE_UNQUOTED([GCC_PATH], ["$CC"], [GCC path for dragonegg])
AC_SUBST(GCC_PATH, $CC)

AX_LLVM([7.0],[16.0.6],[all])
AC_SUBST(LLVM_CPPFLAGS, $LLVM_CPPFLAGS)

# Check if Fortran/flang has been enabled (disabled by default)
AC_ARG_WITH(flang,
[  --with-flang                allows CERE to call flang from LLVM_BINDIR when passed Fortran source files])
if test x"${with_flang}" != x"yes"; then
    AC_MSG_NOTICE([Fortran support enabled.])
else
    AC_MSG_NOTICE([Fortran support left disabled.])
fi
AC_DEFINE_UNQUOTED([FORTRAN_SUPPORT], ["$with_flang"], [enables or disables Fortran/flang support])
AC_SUBST(FORTRAN_SUPPORT, $with_flang)

# Define Automake var to optionally distribute ceref
AM_CONDITIONAL([WITH_FLANG],  [test "x$with_flang"  = "xyes"])

# The profiler and gfortran libraries added to LIBS by AC_CHECK_LIB are needed only at
# cere run time, not when building cere. Therefore we have to reset LIBS.
LIBS=""

AC_CHECK_LIB([c], [exit], , AC_MSG_ERROR([Could not find c library]))
AC_CHECK_LIB([m], [sin], , AC_MSG_ERROR([Could not find math library]))

# Check for common programs and tools
AC_PATH_PROG(SED,  [sed], [])
if test x"$SED" == x"" ; then
    AC_MSG_ERROR([could not find sed])
fi

AC_PATH_PROG(DOT,  [dot], [])
if test x"$DOT" == x"" ; then
    AC_MSG_ERROR([could not find dot])
fi

AC_CHECK_TOOL([OBJCOPY], [objcopy], [])
if test x"$OBJCOPY" == x"" ; then
    AC_MSG_ERROR([could not find objcopy])
fi

AC_CHECK_TOOL([READELF], [readelf], [])
if test x"$READELF" == x"" ; then
    AC_MSG_ERROR([could not find readelf])
fi

# Check for ronn, used to generate man pages
AC_PATH_PROGS(RONN, [ronn], [])
if test x"$RONN" == x"" ; then
    AC_MSG_ERROR([could not find ronn])
fi

# Check for google-pprof
AC_PATH_PROGS(PPROF, [pprof google-pprof], [])
if test x"$PPROF" == x"" ; then
    AC_MSG_ERROR([could not find google-pprof])
fi

# Check for header files.
AC_CHECK_HEADERS([fcntl.h inttypes.h limits.h malloc.h stdint.h stdlib.h string.h sys/time.h unistd.h utime.h], , AC_MSG_ERROR([Missing required headers]))

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

# Required for ccan
AC_C_BIGENDIAN(
               [AC_DEFINE(HAVE_BIG_ENDIAN, 1, [Define to 1 if machine is big endian])],
               [AC_DEFINE(HAVE_LITTLE_ENDIAN, 1, [Define to 1 if machine is little endian])],
               [AC_MSG_ERROR([endianess of this machine is unknown])],
               [AC_MSG_ERROR([universal endianess not supported])]
               )


# Check for Python >= 3

AM_PATH_PYTHON([3])

# Check for Python modules

AX_PYTHON_MODULE(matplotlib.pyplot, fatal)
AX_PYTHON_MODULE(jinja2, fatal)
AX_PYTHON_MODULE(numpy, fatal)
AX_PYTHON_MODULE(pulp, fatal)
AX_PYTHON_MODULE(pydot, fatal)
AX_PYTHON_MODULE(pygraphviz, fatal)
AX_PYTHON_MODULE(pydotplus, fatal)

AX_PYTHON_MODULE(scikit-learn, [])
AX_PYTHON_MODULE(sklearn, [])

AS_IF([test "x${HAVE_PYMOD_SCIKIT_LEARN}" = "xno"],
    AS_IF([test "x${HAVE_PYMOD_SKLEARN}" = "xno"],
        [AC_MSG_ERROR([Neither of scikit-learn or deprecated sklearn were found])]))

# Generate config files

AC_CONFIG_FILES([Makefile
                 doc/Makefile
                 src/Makefile
                 src/ccan/Makefile
                 src/RegionOutliner/Makefile
                 src/RegionInstrumentation/Makefile
                 src/RegionDump/Makefile
                 src/RegionReplay/Makefile
                 src/GlobalRename/Makefile
                 src/rdtsc/Makefile
                 src/memory_dump/Makefile
                 tests/Makefile])

AC_CONFIG_FILES([src/cere/vars.py.in], [])


AC_OUTPUT
