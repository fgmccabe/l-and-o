/* 
  Program management module
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <string.h>

#include "lo.h"
#include "hashTable.h"

/* Dictionary of known programs ... */
static hashPo programs;

static long pSizeFun(specialClassPo class, objPo o);
static comparison pCompFun(specialClassPo class, objPo o1, objPo o2);
static retCode pOutFun(specialClassPo class, ioPo out, objPo o);
static retCode pScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o);
static objPo pCopyFun(specialClassPo class, objPo dst, objPo src);
static uinteger pHashFun(specialClassPo class, objPo o);

void initPrograms(void) {
  programs = NewHash(256, (hashFun) hashPrgLabel, (compFun) compPrgLabel, NULL);

  programClass = newSpecialClass("#program", pSizeFun, pCompFun,
                                 pOutFun, pCopyFun, pScanFun, pHashFun);
}

static long pSizeFun(specialClassPo class, objPo o) {
  assert(o->class == programClass);

  programPo cl = (programPo) o;

  return CellCount(sizeof(programRec) + sizeof(byte) * (uniStrLen(cl->lbl.name) + 1));
}

static comparison pCompFun(specialClassPo class, objPo o1, objPo o2) {
  if (o1 == o2)
    return same;
  else
    return incomparible;
}

static retCode pOutFun(specialClassPo class, ioPo out, objPo o) {
  programPo cl = (programPo) o;

  return outMsg(out, "%U/%d", cl->lbl.name, cl->lbl.arity);
}

static retCode pScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o) {
  assert(o->class == programClass);

  programPo d = (programPo) o;

  return helper(&d->code, c);
}

static objPo pCopyFun(specialClassPo class, objPo dst, objPo src) {
  long size = pSizeFun(class, src);
  memmove((void *) dst, (void *) src, size * sizeof(ptrI));

  return (objPo) (((ptrPo) dst) + size);
}

static uinteger pHashFun(specialClassPo class, objPo o) {
  programPo cl = (programPo) o;

  return cl->hash;
}

static uinteger prgHash(const char * name, int16 arity) {
  char fullLbl[MAX_SYMB_LEN];
  strMsg(fullLbl, NumberOf(fullLbl), "%s/%d", name, arity);
  return uniHash((char *) fullLbl);
}

static ptrI allocateProgramLabel(heapPo H, const char * name, int16 arity, ptrI code) {
  rootPo root = gcAddRoot(H, &code);

  long symlen = uniStrLen(name);
  long len = CellCount(sizeof(programRec) + sizeof(char) * (symlen + 1));
  programPo new = (programPo) permAllocate(len);

  new->class = programClass;
  new->lbl.arity = arity;
  new->code = code;
  new->hash = prgHash(name, arity);

  memcpy(new->lbl.name, name, (symlen + 1) * sizeof(char));
  gcRemoveRoot(H, root);

  return objP(new);
}

ptrI newProgramLbl(char * name, int16 arity) {
  struct {
    long arity;
    char nm[MAX_SYMB_LEN];
  } lbl;

  lbl.arity = arity;
  uniCpy((char *) &lbl.nm, NumberOf(lbl.nm), name);

  ptrI def = (ptrI) hashGet(programs, (void *) &lbl);

  if (objV(def) == NULL) {
    def = allocateProgramLabel(&globalHeap, name, arity, kvoid);

    hashPut(programs, programName(objV(def)), (void *) def);

    return def;
  } else {
    assert(programArity(objV(def)) == arity);
    return def;
  }
}

ptrI programLbl(char * name, int16 arity) {
  struct {
    long arity;
    char nm[MAX_SYMB_LEN];
  } lbl;

  lbl.arity = arity;
  uniCpy((char *) &lbl.nm, NumberOf(lbl.nm), name);

  return (ptrI) hashGet(programs, (void *) &lbl);
}

ptrI newProgLbl(const char *name, int16 arity) {
  return newProgramLbl((char *) name, arity);
}

comparison compPrgLabel(PrgLabel *p1, PrgLabel *p2) {
  if (uniCmp(p1->name, p2->name) == same && p1->arity == p2->arity)
    return same;
  else
    return incomparible;
}

uinteger hashPrgLabel(PrgLabel *p) {
  return uniHash(p->name) * 37 + p->arity;
}

ptrI programOfClass(objPo o) {
  assert(isTermClass(o));

  clssPo cl = (clssPo) o;
  ptrI pr = (ptrI) hashGet(programs, className(cl));

  if (pr == 0) {
    char buff[MAX_SYMB_LEN];
    uniCpy(buff, NumberOf(buff), className(cl));
    return newProgramLbl(buff, OBJECT_ARITY);
  } else
    return pr;
}

void defineProg(ptrI defn, ptrI code) {
  assert(IsProgLbl(defn));
  assert(identical(code, kvoid) || IsCode(code));

  setCode((programPo) objV(defn), code);
}

// Handle GC of program labels

typedef struct {
  globalGcPo G;
  hashPo newDict;
} DInfoRec;

static retCode markProgram(void *n, void *r, void *c) {
  DInfoRec *I = (DInfoRec *) c;
  ptrI S = (ptrI) r;

  objPo o = objV(S);

  /* This fragment allows code to be garbage collected - except for code loaded as part of a package */
  if (oldGeneration(o))
    hashPut(I->newDict, programName(objV(S)), (void *) S); /* put symbol directly into the new dictionary */
  else
    scanPtr(I->G, S); /* We keep defined programs FIXME */

  return Ok;
}

void markPrograms(globalGcPo G) {
  DInfoRec help = {G, NewHash(256, (hashFun) uniHash, (compFun) uniCmp, NULL)};
  hashPo currTable = programs;

  programs = help.newDict;
  ProcessTable(markProgram, currTable, &help);

  thingProg = scanPtr(G, thingProg);
}

void installProgram(objPo p) {
  PrgLabel *name = programName(p);
  ptrI sym = (ptrI) hashGet(programs, name);

  if (objV(sym) == NULL)    /* A new entry in the table */
    hashPut(programs, name, (void *) objP(p)); /* Install in program dictionary */
}

