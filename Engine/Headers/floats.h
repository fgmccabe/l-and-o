/* 
  Number related definitions for the L&O engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/


#ifndef _ENGINE_FLOAT_H_
#define _ENGINE_FLOAT_H_

#include "word.h"
#include <string.h>			/* access template for memcpy */

#ifndef PI
// Define PI to 50 decimal places ....
#define  PI  3.14159265358979323846264338327950288419716939937510
#endif

typedef struct _integer_record_ {
  ptrI class;      /* == integerClass */
  integer i;
} integerRec, *integerPo;

#define IntegerCellCount CellCount(sizeof(integerRec))


extern ptrI integerClass;    /* integerClass is a specialClass */

static inline ptrI allocateInteger(heapPo P, integer i) {
  integerPo new = (integerPo) allocateSpecial(P, integerClass);

  memcpy(&new->i, &i, sizeof(integer));
  return objP(new);
}

static inline logical IsInt(ptrI p) {
  return HasClass(p, integerClass);
}

static inline logical isInteger(objPo p) {
  return hasClass(p, integerClass);
}

static inline integerPo intV(ptrI x) {
  assert(IsInt(x));
  return (integerPo) objV(x);
}

static inline integer integerVal(integerPo p) {
#ifdef DOUBLE_ALIGNMENT
  integer i;

  memcpy(&i,&p->i,sizeof(integer));
  return i;
#else
  return p->i;
#endif
}

static inline integer IntVal(ptrI p) {
  assert(IsInt(p));
  return integerVal(intV(p));
}

extern ptrI permInteger(integer i);

typedef struct _float_record_ {
  ptrI class;        /* == floatClass */
  double f;
} floatRec, *floatPo;

#define FloatCellCount CellCount(sizeof(floatRec))


extern ptrI floatClass;

static inline logical isFloat(objPo p) {
  return hasClass(p, floatClass);
}

static inline floatPo floatV(ptrI x) {
  assert(HasClass(x, floatClass));
  return (floatPo) objV(x);
}

static inline double floatVal(floatPo p) {
  assert(isFloat((objPo) p));

#ifdef DOUBLE_ALIGNMENT
  number f;
  memcpy(&f,&p->f,sizeof(number));
  return f;
#else
  return p->f;
#endif
}

static inline logical IsFloat(objPo p) {
  return (logical) (isFloat(p) || isInteger(p));
}

static inline double FloatVal(objPo p) {
  return floatVal((floatPo) p);
}

static inline double roundNumber(register double f) {
  if (f >= 0.0)
    return floor(f);
  else
    return ceil(f);
}

static inline ptrI allocateFloat(heapPo P, double f) {
  floatPo new;

  new = (floatPo) allocateSpecial(P, floatClass);

  memcpy(&new->f, &f, sizeof(double));
  return objP(new);
}

extern ptrI permFloat(double f);

static inline ptrI allocateNumber(heapPo P, double f) {
  integer n = (integer) f;

  if (((double) n) == f)
    return allocateInteger(P, n);
  else
    return allocateFloat(P, f);
}

void initIntegerClass(void);
void initFloatClass(void);
#endif
