//
// Created by Francis McCabe on 2/5/17.
//
// The thread class - a live thread has a pointer to its process structure

#include "lo.h"
#include "thred.h"

ptrI threadClass;

static long thrSizeFun(specialClassPo class, objPo o);
static comparison thrCompFun(specialClassPo class, objPo o1, objPo o2);
static retCode thrOutFun(specialClassPo class, ioPo out, objPo o);
static retCode thrScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o);
static objPo thrCopyFun(specialClassPo class, objPo dst, objPo src);
static uinteger thrHashFun(specialClassPo class, objPo o);

void initThreadClass(void) {
  threadClass = newSpecialClass("lo.core#thread", thrSizeFun, thrCompFun,
                                thrOutFun, thrCopyFun, thrScanFun, thrHashFun);
}

static long thrSizeFun(specialClassPo class, objPo o) {
  assert(o->class == threadClass);

  return CellCount(sizeof(threadRec));
}

static comparison thrCompFun(specialClassPo class, objPo o1, objPo o2) {
  if (o1 == o2)
    return same;
  else
    return incomparible;
}

static retCode thrOutFun(specialClassPo class, ioPo out, objPo o) {
  threadPo s = (threadPo) o;

  return outMsg(out, "thread[0x%x]", s);
}

static retCode thrScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o) {
  return Ok;
}

static objPo thrCopyFun(specialClassPo class, objPo dst, objPo src) {
  long size = thrSizeFun(class, src);
  memmove((void *) dst, (void *) src, size * sizeof(ptrI));

  return (objPo) (((ptrPo) dst) + size);
}

static uinteger thrHashFun(specialClassPo class, objPo o) {
  assert(o->class == threadClass);

  return (uinteger) o;
}

ptrI newThread() {
  threadPo new = (threadPo) permAllocate(ThreadCellCount);

  new->class = threadClass;
  new->process = NULL;
  return objP(new);
}

void setProcess(ptrI T, processPo P) {
  objPo t = objV(T);
  assert(hasClass(t, threadClass));

  ((threadPo) t)->process = P;
}

processPo getProcessVal(ptrI P) {
  objPo t = objV(P);
  assert(hasClass(t, threadClass));
  threadPo po = (threadPo) t;
  return po->process;
}

void clearProcess(ptrI P) {
  objPo p = objV(P);
  assert(hasClass(p, threadClass));

  ((threadPo) p)->process = NULL;
}


