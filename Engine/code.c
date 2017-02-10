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

ptrI permCode(unsigned long size, unsigned long litCnt) {
  codePo block = (codePo) permAllocate(CodeCellCount(size, litCnt));

  block->class = codeClass;
  block->size = size;
  block->litCnt = litCnt;

  int i;
  ptrPo lits = codeLits(block);

  for (i = 0; i < litCnt; i++)
    lits[i] = kvoid;

  return objP(block);
}

retCode g__ensure_loaded(processPo P, ptrPo a) {
  ptrI pname = deRefI(&a[1]);

  if (isvar(pname))
    return liberror(P, "__ensure_loaded", eINSUFARG);
  else if (!IsSymb(pname))
    return liberror(P, "__ensure_loaded", eINVAL);
  else {
    if (isLoaded(pname))
      return Ok;
    else {
      heapPo H = &P->proc.heap;

      switchProcessState(P, wait_io); /* Potentially nec. to wait */

      retCode ret = loadPkg(stringVal(stringV(pname)), stringVal(stringV(deRefI(&a[2]))), P->proc.errorMsg,
                            NumberOf(P->proc.errorMsg));
      setProcessRunnable(P);

      switch (ret) {
        case Error: {
          byte msg[MAX_MSG_LEN];

          strMsg(msg, NumberOf(msg), "__ensure_loaded: %#w in %#w", &a[2], &a[1]);
          return raiseError(P, msg, eNOTFND);
        }
        case Eof: {
          byte msg[MAX_MSG_LEN];

          strMsg(msg, NumberOf(msg), "__ensure_loaded: %#w in %#w", &a[2], &a[1]);
          return raiseError(P, msg, eNOFILE);
        }
        case Ok:
          return Ok;
        case Fail:
          return Fail;
        case Space:
          outMsg(logFile, "Out of heap space, increase and try again\n%_");
          return liberror(P, "__ensure_loaded", eSPACE);
        default:
          return liberror(P, "__ensure_loaded", eINVAL);
      }
    }
  }
}
