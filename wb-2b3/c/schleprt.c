/* scleprt.c: RunTime support for program translating SCM code to C */

#include <stdio.h>
/* this is where all diagnostic and error messages will appear */
FILE *diagout;

#include "schleprt.h"

#ifndef STDC_INCLUDES
unsigned char *memcpy(unsigned char *dest, unsigned char *src, int len)
{
  while (len--) dest[len]=src[len];
  return src;
}
# define LACK_MEMMOVE
#endif
#ifdef GNUDOS
# define LACK_MEMMOVE
#endif
#ifdef LACK_MEMMOVE
unsigned char *memmove(unsigned char *dest, unsigned char *src, int len)
{
  if (dest >= src)
    while (len--) dest[len]=src[len];
  else {
    int i=0;
    while (i<len) dest[i]=src[i++];
  }
  return src;
}
#endif
