/* 
  Header for evaluation engine for L&O engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _ENGINE_EVAL_H_
#define _ENGINE_EVAL_H_

#include "logical.h"		/* import a definition of true and false */
#include "integer.h"
#include "retcode.h"
#include <math.h>
#include <assert.h>
#include "opcodes.h"
#include "process.h"
#include "word.h"

#ifdef EXECTRACE

#include "stats.h"

#endif

retCode equal(processPo P, ptrPo T1, ptrPo T2);
retCode unifyType(processPo P, ptrPo T1, ptrPo T2);
logical identical(ptrI T1, ptrI T2);
retCode match(processPo P, ptrPo T1, ptrPo T2);

typedef enum {
  readMode, writeMode, dummyMode
} rwmode;

extern void chainSuspension(processPo P, ptrPo var);

static inline void bindVar(processPo P, ptrPo ptr, ptrI val) {
  assert((ptr >= (ptrPo) P->proc.heap.base && ptr < (ptrPo) P->proc.heap.create) ||
         (ptr >= (ptrPo) P->proc.sBase && ptr < (ptrPo) P->proc.sTop));

  if (((void *) P->proc.B < (void *) P->proc.T ?
       (ptr < (ptrPo) P->proc.B->H) :
       ptr < (ptrPo)P->proc.T->H)
      || ptr > (ptrPo) P->proc.B) {
    P->proc.trail->var = ptr;
    P->proc.trail->val = *ptr;
    P->proc.trail++;
  }
  if (isSuspVar(ptr))
    chainSuspension(P, ptr);

  *ptr = val;
}

static inline void bndVar(processPo P, ptrPo ptr, ptrI val) {
  assert((ptr >= (ptrPo) P->proc.heap.base && ptr < (ptrPo) P->proc.heap.create) ||
         (ptr >= (ptrPo) P->proc.sBase && ptr < (ptrPo) P->proc.sTop));

  if (((void *) P->proc.B < (void *) P->proc.T ?
       (ptr < (ptrPo) P->proc.B->H) :
       ptr < (ptrPo)P->proc.T->H)
      || ptr > (ptrPo) P->proc.B) {
    P->proc.trail->var = ptr;
    P->proc.trail->val = *ptr;
    P->proc.trail++;
  }

  *ptr = val;
}

static inline ptrI unBind(ptrPo x) {
  return *x = (ptrI) x;
}

static inline int16 envSize(insPo pc) {
  assert(op_code(*pc) == gcmap);
  return op_o_val(*pc);
}

static inline int16 carefulEnv(insPo pc) {
  switch (op_code(*pc)) {
    case escape:
    case kawl:
    case kawlO:
      return envSize(pc + 1);
    default:
      return envSize(pc);
  }
}

static inline int16 argArity(insPo pc) {
  assert(op_code(*pc) == gcmap || op_code(*pc) == escape || op_code(*pc) == kawl || op_code(*pc) == kawlO ||
         op_code(*pc) == trycl || op_code(*pc) == tryme || op_code(*pc) == gc || op_code(*pc) == alloc);
  return op_h_val(*pc);
}

retCode raiseError(processPo P, string name, ptrI code);
retCode raiseException(processPo P, ptrI exc);
void recoverFromException(processPo P);

void init_args(char **argv, int argc, int start);

static inline retCode funResult(processPo P, ptrPo args, int which, ptrI value) {
  rootPo root = gcAddRoot(&P->proc.heap, &value);
  retCode ret = equal(P, &value, &args[which]);
  gcRemoveRoot(&P->proc.heap, root);
  return ret;
}

// Define the event flags
#ifndef SUSP_ACTIVE
#define SUSP_ACTIVE (1)                 /* activated suspension */
#define INT_ACTIVE (2)                  /* interrupt request */
#define GC_ACTIVE (4)                   /* gc request */
#endif

#endif


