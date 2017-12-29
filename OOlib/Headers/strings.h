//
// Created by Francis McCabe on 9/8/17.
//

#ifndef LANDO_STRINGS_H
#define LANDO_STRINGS_H

#include "utils.h"

typedef struct _string_base_ *strBasePo;

typedef struct _string_ {
  strBasePo base;
  long off;
  long len;
} StringRec, *stringPo;

typedef struct _string_base_ {
  long len;
  char txt[];
} StringBase;

#define _b(t) { NumberOf(t), t}


#endif //LANDO_STRINGS_H
