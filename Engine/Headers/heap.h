/*
 * Header file for the heap management of L&O engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _HEAP_H_
#define _HEAP_H_

#ifndef MAXROOT
#define MAXROOT 128
#endif

typedef long rootPo;

typedef struct _heap_rec_ {
  objPo base;
  /* base of the heap */
  objPo end;
  /* end of the heap */
  objPo create;
  /* Where to create the next heap structure */
  processPo owner;
  /* This is NULL for the global heap */
  ptrPo rts[MAXROOT];
  ptrPo *roots;
  long topRoot;
  long maxRoot;
} HeapRec;

extern HeapRec globalHeap;

typedef struct _gc_support_ *gcSupportPo;
typedef struct _global_gc_support_ *globalGcPo;

#ifndef CARDSHIFT
#define CARDSHIFT 6    /* 64 bits in an integer */
#define CARDWIDTH  (1<<CARDSHIFT)
#define CARDMASK  (CARDWIDTH-1)
#endif

typedef uint64 cardMap;

extern cardMap masks[CARDWIDTH];
extern void initMasks(void);

extern long initStackHeapSize;

extern void setupHeap(heapPo H, processPo owner, long size);

void gcCollect(heapPo P, long amount);

void pushPtr(gcSupportPo G, ptrPo x, long count);
ptrI adjustPtr(gcSupportPo G, ptrI cell);

static inline logical inHeap(heapPo P, const objPo x) {
  return (logical) (x >= P->base && x < P->create);
}

/* Root management */
extern void growRoots(heapPo H);

static inline rootPo gcAddRoot(heapPo H, ptrPo ptr) {
  assert(H != &globalHeap);
  if (H->topRoot == H->maxRoot)
    growRoots(H);

  long R = H->topRoot++;

  H->roots[R] = ptr;

  return R;
}

static inline rootPo gcCurrRoot(heapPo H) {
  return H->topRoot;
}

static inline void gcRemoveRoot(heapPo H, rootPo mk) {
  H->topRoot = mk;
}

static inline objPo allocSpace(heapPo P, size_t size) {
  objPo new;

  if (P->create + size > P->end)
    return NULL;    /* allow caller to invoke GC */

  new = (objPo) P->create;
  P->create += size;

  return new;
}

extern retCode reserveSpace(heapPo P, size_t size);

static inline objPo allocate(heapPo H, size_t size) {

#ifdef MEMTRACE
  if (stressMemory)
    gcCollect(H, size);    /* gc on every allocation */
#endif

  if (H->create + size > H->end)
    gcCollect(H, size);    /* this aborts if there is no memory */

  {
    register objPo new = H->create;
    H->create += size;

    return new;
  }
}

static inline objPo allocateObject(heapPo H, ptrI class) {
  assert(isClass(class));

  objPo o = allocate(H, (size_t)(((clssPo) objV(class))->arity + 1));
  o->class = class;
  return o;
}

static inline objPo allocateSpecial(heapPo H, ptrI class) {
  assert(IsSpecialClass(class));

  specialClassPo sClass = (specialClassPo) objV(class);

  objPo o = allocate(H, (size_t)(sClass->sizeFun(sClass, NULL)));
  o->class = class;
  return o;
}

static inline long spaceLeft(heapPo H) {
  return H->end - H->create;
}

static inline long totalHeapSize(heapPo H) {
  return H->end - H->base;
}

retCode localCopy(ptrPo dst, heapPo H, ptrPo src);
retCode freezeTerm(heapPo H, ptrPo dst, ptrI src, byte *eMsg, long len);
logical isGroundTerm(ptrPo p);
void markStandardClasses(globalGcPo G);

#ifdef MEMTRACE
void verifyAllProcesses(void);
void verifyHeap(heapPo P);
void verifyProc(processPo p);
#endif

#ifdef EXECTRACE
void verifyVar(ptrPo ptr, processPo P);
#endif

#endif
