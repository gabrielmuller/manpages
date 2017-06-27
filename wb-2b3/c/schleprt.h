/* scleprt.h: RunTime support for program translating SCM code to C */
/* Copyright (C) 1991-2006 Aubrey Jaffer and Radey Shouman */
/* Copyright (C) 2008, 2009 Aubrey Jaffer */

/* Permission to copy this software, to modify it, to redistribute it, */
/* to distribute modified versions, and to use it for any purpose is */
/* granted, subject to the following restrictions and understandings. */

/* 1.  Any copy made of this software must include this copyright notice */
/* in full. */

/* 2.  I have made no warranty or representation that the operation of */
/* this software will be error-free, and I am under no obligation to */
/* provide any services, by way of maintenance, update, or otherwise. */

/* 3.  In conjunction with products arising from the use of this */
/* material, there shall be no use of my name in any advertising, */
/* promotional, or sales literature without prior written consent in */
/* each case. */

/* 	  http://people.csail.mit.edu/jaffer/Schlep/scm2c */

#include <stdio.h>
#include <time.h>

extern FILE *diagout;
#define dprintf(DaRgS) {if (diagout) {fprintf DaRgS; fflush(diagout);}}

/* SCM_EXPORT and P() are macros (from SCM) for portable declaration
   of function prototypes */
#ifndef SCM_EXPORT
# define SCM_EXPORT extern
#endif

#ifndef P
# ifdef __STDC__
#  ifndef __HIGHC__
#   define USE_ANSI_PROTOTYPES
#  endif
# endif
# ifdef __sgi__
#  define USE_ANSI_PROTOTYPES
# endif

# ifdef USE_ANSI_PROTOTYPES
#  define P(s) s
# else
#  define P(s) ()
# endif
#endif

#ifdef __STDC__
# define STDC_INCLUDES
#endif
#ifdef __TURBOC__
# define MSDOS
#endif
#ifdef __MINGW32__
# define MSDOS
# ifndef max
#  define max(a,b)		((a)<(b)?(b):(a))
#  define min(a,b)		((a)>(b)?(b):(a))
# endif
#endif
#ifdef _MSC_VER
# define MSDOS
#endif
#ifdef MSDOS
# define STDC_INCLUDES
#endif
#ifdef VMS
# define STDC_INCLUDES
#endif
#ifndef MSDOS
# define max(a,b)		((a)<(b)?(b):(a))
# define min(a,b)		((a)>(b)?(b):(a))
#endif

#ifdef STDC_INCLUDES
# include <stdlib.h>
# include <string.h>
#else
	unsigned char *malloc(size_t size);
	unsigned char *realloc(void *ptr, size_t size);
#endif

/* Analogize byte support from R2RS */
#define subbytes_move_left(src,start,end,dst,dstart) (memmove(&dst[dstart],&src[start],(end)-(start)))
#define subbytes_move_right subbytes_move_left
#define subbytes_move(src,start,end,dst,dstart) (memcpy(&dst[dstart],&src[start],(end)-(start)))
