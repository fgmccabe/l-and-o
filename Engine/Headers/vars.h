/* 
  Variable related definitions for the L&O engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _ENGINE_VARS_H_
#define _ENGINE_VARS_H_

#include "word.h"

/* Dynamic variable, not often used because most variables are in the ptr */
typedef struct _variable_record_ {
  ptrI class;			/* == varClass */
  ptrI val;			/* value to which variable is bound */
} variableRec, *variablePo;

extern ptrI varClass;

static inline ptrPo deRef(register ptrPo p)
{
  do{
    register ptrI q = *p;

    if(!isvar(q) || (ptrPo)q==p)
      return p;
    p = (ptrPo)q;
  } while(True);
}

static inline ptrI deRefI(register ptrPo p)
{
  do{
    register ptrI q = *p;

    if(!isvar(q))
      return q;
    else if((ptrPo)q==p)
      return q;
    p = (ptrPo)q;
  } while(True);
}

//#define deRefI(p) (!isvar(*p) ? *p : dRfI(p))

static inline ptrI mkvar(ptrPo p)
{
  return (ptrI)p;
}

static inline logical IsFrozenVar(ptrI x)
{
  return isobj(x) && HasClass(x,varClass);
}

extern logical notRecentVar(ptrPo vx);

#define VariableCellCount CellCount(sizeof(variableRec))

static inline ptrI allocateVar(heapPo H)
{
  variablePo new = (variablePo)allocate(H,VariableCellCount);
  ptrPo var = &new->val;

  new->class = varClass;
  new->val = (ptrI)var;		/* make new variable unbound */

  return (ptrI)var;
}

// Suspension record

#define SuspensionCellCount CellCount(sizeof(suspensionRec))
#define SuspensionMark objectMark(suspensionKey,SuspensionCellCount)

typedef struct _susp_record_ {
  ptrI class;                            /* = suspClass */
  ptrI var;                             /* variable that is suspended */
  ptrI key;				/* A copy of the sentinel for a suspension record */
  ptrI goal;                            /* goal structure to activate on suspension */
} suspensionRec, *suspensionPo;

extern ptrI suspClass;

static inline logical IsSusp(ptrI x)
{
  return HasClass(x,suspClass);
}

static inline logical isSuspVar(ptrPo x)
{
  return x[1]==suspClass && x[-1]==suspClass;
}


static inline ptrI allocateSusp(heapPo H,ptrI goal)
{
  rootPo root = gcAddRoot(H,&goal);
  suspensionPo new = (suspensionPo)allocate(H,SuspensionCellCount);
  ptrPo var = &new->var;
  ptrI susp = objP(new);

  new->class = new->key = suspClass;
  new->var = (ptrI)var;		/* make new suspension's variable unbound */
  new->goal = emptyList;

  gcAddRoot(H,&susp);

  new->goal = consLsPair(H,goal,emptyList);

  gcRemoveRoot(H,root);
  return (ptrI)var;			/* looks like a variable ... until you try to bind it */
}

#endif
