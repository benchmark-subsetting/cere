/* This configuration file has been edited manually for the
 * SPECbench submission. All hardware/compiler-specific
 * optimizations have been disabled, and our software
 * versions of 1/sqrt is not used.
 * 
 * This default setup should work without alterations at 
 * least on: 
 * Linux (both with gcc and intel compilers)
 * SGI
 * Solaris
 * Tru64 / Digital unix
 *
 * ... and probably most other unix systems too.  
 *
 * On AIX you have to change the Fortran name mangling macro and not
 * underscore names, i.e. "#define F77_FUNC(name,NAME) name", or use
 * the -qextname flag to tell the compiler to underscore them.
 *
 * Erik Lindahl  2002-03-30
 */

/* Use the fortran77 versions of the inner loops and
 * constraint routines.
 */
#define USE_FORTRAN


/* Define to a macro mangling the given C identifier (in lower and upper
 * case), which must not contain underscores, for linking with Fortran.
 * An example; The fortran function is called "myfunc". In the C code
 * we call it as F77_FUNC(myfunc,MYFUNC). 
 * Thus, if your f77 compiler exports symbols as upper case with an 
 * underscore suffix, define it as: #define F77_FUNC(name,NAME) NAME ## _
 */
#if defined(SPEC_CPU_WINDOWS) && !defined(SPEC_CPU_APPEND_UNDERSCORE)
# define F77_FUNC(name,NAME) NAME
#else
# define F77_FUNC(name,NAME) name ## _
#endif


/* The computationally intensive part of gromacs doesn't use any 
 * library calls at all, but it is nice to be able to use the standard
 * input/output formats for the benchmark too. This will NOT affect
 * performance.
 */

/* Define if you have the `strdup' function. */
#define HAVE_STRDUP 1


/* Define if you have the `strcasecmp' function. Undef on windows! */

#ifndef SPEC_CPU_WINDOWS
#define HAVE_STRCASECMP 1
#endif

/* Define if you have the <unistd.h> header file. Undef on windows! */

#ifndef SPEC_CPU_WINDOWS
#define HAVE_UNISTD_H 1
#endif


/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */


/* Define to `int' if <sys/types.h> doesn't define (do it on windows) */


#ifdef SPEC_CPU_WINDOWS
#define gid_t int
#endif


/* Define to `unsigned' if <sys/types.h> does not define. */
/* #undef size_t */


/* Define to `int' if <sys/types.h> doesn't define. (do it on windows) */

#ifdef SPEC_CPU_WINDOWS
#define uid_t int
#endif

/* Define this if you don't have the popen/pclose system calls 
 * Windows actually has _popen/_pclose calls, but it is not necessary
 * for the benchmark, so define this on windows 
 */

#ifdef SPEC_CPU_WINDOWS
#define NO_PIPE 
#endif


/* We want all platforms to use the same code as far as possible,
 * so by default we use the Gromacs internal version of the XDR
 * libraries. If this doesnt work for some reason (say, the compiler
 * might link automatically with the system libs so you get a 
 * nameclash), you can undefine this to use the system libraries.
 */
#if !defined(SPEC_CPU_USE_SYSTEM_XDR) && !defined(USE_GMX_XDR)
#define USE_GMX_XDR
#endif


/* The defines below are not of interest to the SPEC benchmark,
 * but they make it possible for us to use the same codebase
 * as for our hardware-optimized production version of Gromacs.
 */
/* Gromacs uses single precision by default. */
/* #undef DOUBLE */

/* The SPECbench version does not use FFTW */
#define WITHOUT_FFTW 

/* We don't need X for the benchmark either */
#define X_DISPLAY_MISSING 

/* Inline not supported everywhere - define it empty */
#define inline 

/* Uid calls are only used to print the user id in some files */
#define NO_PWUID

/* We never ever use the error function for the benchmark, but 
 * to avoid compiler errors when it is not defined on windows 
 * we replace it with our own versions. If you have the error
 * function you can just remove this.
 */
#if ((defined _WIN32 || defined _WIN64) && !defined __CYGWIN__) && !defined(SPEC_CPU_HAVE_ERF)
#define erf(x) gmx_erf(x)
#define erfc(x) gmx_erfc(x)
#endif
