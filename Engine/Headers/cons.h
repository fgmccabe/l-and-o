/* 
  Constructor related definitions for the L&O engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/


#ifndef _ENGINE_CONS_H_
#define _ENGINE_CONS_H_

#include "word.h"

/*
 * Constructor term
 */

typedef struct _cons_record_ {
  ptrI sign;			/* == class specific Mark */
  ptrI data[ZEROARRAYSIZE];
} consRec, *consPo;

static inline consPo consV(ptrI x)
{
  return (consPo)(x&PTR_MASK);
}

static inline ptrPo ConsEl(consPo p,long n)
{
  assert(objectArity((objPo)p)>n);

  return &p->data[n];
}

static inline void updateCons(consPo cns,int el,ptrI val)
{
  assert(objectArity((objPo)cns)>el);

  cns->data[el]=val;
}

static inline ptrPo FirstConsEl(consPo cons)
{
  return cons->data;
}

extern ptrI commaClass;

#endif /* _ENGINE_CONS_H_ */

