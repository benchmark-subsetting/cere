/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

/*
   common definitions for namd.
*/

#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <limits.h>

#if ( INT_MAX == 2147483647 )
typedef	int	int32;
#else
typedef	short	int32;
#endif

typedef unsigned short Index;

#if defined(PLACEMENT_NEW)
void * ::operator new (size_t, void *p) { return p; }
#elif defined(PLACEMENT_NEW_GLOBAL)
void * operator new (size_t, void *p) { return p; }
#endif

#define COLOUMB 332.0636
#define BOLTZMAN 0.001987191
#define TIMEFACTOR 48.88821
#define PRESSUREFACTOR 6.95E4
#define PDBVELFACTOR 20.45482706
#define PDBVELINVFACTOR (1.0/PDBVELFACTOR)
#define PNPERKCALMOL 69.479

#ifndef PI
#define PI	3.141592653589793
#endif

#ifndef TWOPI
#define TWOPI	2.0 * PI
#endif

#ifndef ONE
#define ONE	1.000000000000000
#endif

#ifndef ZERO
#define ZERO	0.000000000000000
#endif

#ifndef SMALLRAD
#define SMALLRAD      0.0005
#endif

#ifndef SMALLRAD2
#define SMALLRAD2     SMALLRAD*SMALLRAD
#endif

/* Define the size for Real and BigReal.  Real is usually mapped to float */
/* and BigReal to double.  To get BigReal mapped to float, use the 	  */
/* -DSHORTREALS compile time option					  */
typedef float	Real;

typedef double  BigReal;

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#ifndef NO
#define NO 0
#define YES 1
#endif

#ifndef STRINGNULL
#define STRINGNULL '\0'
#endif

#define MAX_NEIGHBORS 27

typedef int Bool;

class Communicate;

// global functions
void NAMD_check_messages(void);
void NAMD_quit(Bool die_hard=FALSE);
void NAMD_die(const char *);
void NAMD_err(const char *);  // also prints strerror(errno)
void NAMD_bug(const char *);
void NAMD_backup_file(const char *filename, const char *extension = 0);
void NAMD_write(int fd, const void *buf, size_t count); // NAMD_die on error
char *NAMD_stringdup(const char *);
FILE *Fopen(const char *filename, const char *mode);
int  Fclose(FILE *fout);

// message tags
#define SIMPARAMSTAG	100	//  Tag for SimParameters class
#define STATICPARAMSTAG 101	//  Tag for Parameters class
#define MOLECULETAG	102	//  Tag for Molecule class
#define FULLTAG	104
#define FULLFORCETAG 105
#define DPMTATAG 106

// for proxy spanning tree
#define PROXY_SPAN_DIM 4

#define CYCLE_BARRIER   0
#define PME_BARRIER     0

#define USE_BARRIER   (CYCLE_BARRIER || PME_BARRIER)

#endif

