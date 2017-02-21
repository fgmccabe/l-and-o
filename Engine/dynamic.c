/*
  Property management
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <string.h>		/* Access string defs */

#include "lo.h"

ptrI dynamicClass;

static long dySizeFun(specialClassPo class, objPo o);
static comparison dyCompFun(specialClassPo class, objPo o1, objPo o2);
static retCode dyOutFun(specialClassPo class, ioPo out, objPo o);
static retCode dyScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o);
static objPo dyCopyFun(specialClassPo class, objPo dst, objPo src);
static uinteger dyHashFun(specialClassPo class, objPo o);

void initDynamicClass(void) {
  dynamicClass = newSpecialClass("#dynamic", dySizeFun, dyCompFun,
                                 dyOutFun, dyCopyFun, dyScanFun, dyHashFun);
}

static long dySizeFun(specialClassPo class, objPo o) {
  return CellCount(sizeof(dynRec));
}

static comparison dyCompFun(specialClassPo class, objPo o1, objPo o2) {
  if (o1 == o2)
    return same;
  else
    return incomparible;
}

static retCode dyOutFun(specialClassPo class, ioPo out, objPo o) {
  dynPo dObj = (dynPo) o;

  return outMsg(out, "obj#%ld", dObj->hash);
}

static retCode dyScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o) {
  assert(o->class == dynamicClass);

  dynPo d = (dynPo) o;

  return helper(&d->code, c);
}

static objPo dyCopyFun(specialClassPo class, objPo dst, objPo src) {
  long size = dySizeFun(class, src);
  memmove((void *) dst, (void *) src, size * sizeof(ptrI));

  return (objPo) (((ptrPo) dst) + size);
}

static uinteger dyHashFun(specialClassPo class, objPo o) {
  assert(o->class == dynamicClass);

  dynPo d = (dynPo) o;

  return d->hash;
}

static uinteger objNumber = 0;

ptrI dynamicObject(heapPo H) {
  dynPo perm = (dynPo) permAllocate(CellCount(sizeof(dynRec)));

  perm->class = dynamicClass;
  perm->code = kvoid;
  perm->lock = newLock();
  perm->hash = objNumber++;

  return objP(perm);
}

void setDynamicCode(ptrI O, ptrI code) {
  assert(hasClass(objV(O), dynamicClass));
  dynPo o = (dynPo) objV(O);
  o->code = code;
}
