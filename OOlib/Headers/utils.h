#ifndef _UTILS_H_
#define _UTILS_H_


#include "config.h"
#include <unicode.h>

extern void syserr(const char* msg);

#define tryRet(Exp) do{ retCode ret=(Exp); if(ret!=Ok)return ret; }while(False)

#ifndef Null
#define Null ((void*)0)
#endif

#ifndef ALIGNED
#define ALIGNED(ptr,size) (((((integer)ptr)+size-1)/size)*(size)==(integer)ptr)
#endif

#ifndef NumberOf
#define NumberOf(a) (sizeof(a)/sizeof(a[0]))
#endif

#ifndef GROW
#define GROW(var,type) do{					\
    if(var##Pos>=var##Size){					\
      long nsize = (var##Size)*3/2;				\
      type *nbuffer = (type*)malloc(nsize*sizeof(type));	\
      for(long cx=0;cx<var##Pos;cx++)				\
	nbuffer[cx] = var[cx];					\
      free(var);						\
      var = nbuffer;						\
      var##Size = nsize;					\
    }								\
  } while(0)
#endif

static long inline min(long a,long b)
{
  if(a<b)
    return a;
  else
    return b;
}

#endif
