/*
  Code handling functions for L&O
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
#include "code.h"

ptrI codeClass;

static long cdeSizeFun(specialClassPo class, objPo o);
static comparison cdeCompFun(specialClassPo class, objPo o1, objPo o2);
static retCode cdeOutFun(specialClassPo class, ioPo out, objPo o);
static retCode cdeScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o);
static objPo cdeCopyFun(specialClassPo class, objPo dst, objPo src);
static uinteger cdeHashFun(specialClassPo class, objPo o);

void initCodeClass(void) {
  codeClass = newSpecialClass("#code", cdeSizeFun, cdeCompFun,
                              cdeOutFun, cdeCopyFun,
                              cdeScanFun, cdeHashFun);
}

static long cdeSizeFun(specialClassPo class, objPo o) {
  codePo c = (codePo) o;

  assert(c->class == codeClass);

  return CellCount(sizeof(codeRec) + (c->size + c->litCnt) * sizeof(ptrI));
}

static retCode cdeScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o) {
  long ix = 0;
  retCode ret = Ok;
  codePo cde = (codePo) o;
  ptrPo lits = codeLits(cde);
  long count = codeLitCount(cde);

  for (ix = 0; ret == Ok && ix < count; ix++, lits++)
    ret = helper(lits, c);
  return ret;
}

static objPo cdeCopyFun(specialClassPo class, objPo dst, objPo src) {
  long size = cdeSizeFun(class, src);
  memmove((void *) dst, (void *) src, size * sizeof(ptrI));

  return (objPo) (((ptrPo) dst) + size);
}

static comparison cdeCompFun(specialClassPo class, objPo o1, objPo o2) {
  if (o1 == o2)
    return same;
  else
    return incomparible;
}

static retCode cdeOutFun(specialClassPo class, ioPo out, objPo o) {
  codePo cde = (codePo) o;
  return outMsg(out, "code[%d/%d]", cde->arity, cde->litCnt);
}

static uinteger cdeHashFun(specialClassPo class, objPo o) {
  uinteger hash = 0;
  codePo cde = (codePo) o;

  long ix = 0;
  long count = codeInsCount(cde);
  insPo pc = codeIns(cde);

  for (ix = 0; ix < count; ix++)
    hash += *pc++;

  return hash;
}

ptrI permCode(uinteger size, uinteger litCnt, packagePo owner, uinteger srcMapCount) {
  codePo block = (codePo) permAllocate(CodeCellCount(size, litCnt, srcMapCount));

  block->class = codeClass;
  block->size = size;
  block->litCnt = litCnt;
  block->srcMapCount = srcMapCount;
  block->owner = owner;

  int i;
  ptrPo lits = codeLits(block);

  for (i = 0; i < litCnt; i++)
    lits[i] = kvoid;

  return objP(block);
}

srcMapPo locateSourceFragment(codePo cde, insPo pc) {
  srcMapPo srcMap = sourceMap(cde);
  uinteger mapSize = sourceMapCount(cde);
  uinteger pcOffset = pc - codeIns(cde);

  uinteger entrySize = (uinteger) MAX_INT;
  srcMapPo soFar = NULL;

  // We return the smallest entry that encloses the program counter
  for (uinteger ix = 0; ix < mapSize; ix++) {
    srcMapPo s = &srcMap[ix];

    if (s->startOff <= pcOffset && s->endOff > pcOffset) {
      if ((s->endOff - s->startOff) < entrySize) {
        soFar = s;
      }
    }
  }

  return soFar;
}

