/*
  Global memory management of L&O engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _GLOBAL_H_
#define _GLOBAL_H_

#include "heap.h"

extern HeapRec globalHeap;

typedef struct _global_gc_support_ {
  logical left;				/* Are we moving up or down? */
  heapPo fH;
  heapPo tH;
} globalGcRec;

static inline logical oldGeneration(objPo p)
{
  extern objPo globalSpace;
  extern objPo leftBase;
  return (logical)(globalSpace<=p && p<leftBase);
}

static inline logical inGlobalHeap(const objPo p)
{
  return (logical)(inHeap(&globalHeap,p) || oldGeneration(p));
}

extern void initGlobal(long size);
extern void globalGC(long request);

extern retCode reserveGlobalSpace(long size);
extern void markGrey(objPo p);
extern objPo permAllocate(long size);
extern objPo permObject(heapPo H,ptrI cls);
extern ptrI realPtr(ptrI X);

extern volatile logical globalGcRequest;
extern ptrI scanPtr(globalGcPo G,ptrI orig);

extern void lockGlobal(void);
extern void unlockGlobal(void);

extern rootPo globalRoot(ptrPo p,rootPo r);
extern void resetGlobalRoot(rootPo r);

#endif
