/* 
  Code related definitions for the L&O engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/


#ifndef _ENGINE_CODE_H_
#define _ENGINE_CODE_H_

#include "word.h"
#include "opcodes.h"
#include "lock.h"
#include "load.h"

typedef struct _program_record_ {
  ptrI class;                   // == programClass
  ptrI code;                    // Code to execute
  uinteger hash;                // Hash code of the program name
  PrgLabel lbl;
} programRec;

/*
 * A dynamically created object
 */
typedef struct _object_record_ {
  ptrI class;                   // == dynamicClass
  ptrI code;                    // Program associated with dynamic object
  lockPo lock;                  // The mutex lock on the symbol
  uinteger hash;                // A hash for uniqueness
} dynRec, *dynPo;

extern ptrI dynamicClass;       // A dynamically created entity */

#define OBJECT_ARITY 3          // A method call has 3 args */

/*
 * A Code fragment
 */

typedef struct _code_record_ {
  ptrI class;                   // == codeClass
  uinteger size;                // Number of instruction words
  uinteger brks;                // Size of the break table */
  uint16 arity;                 // The program arity -- used for verification purposes
  unsigned long litCnt;         // The number of literals in the code */
  packagePo owner;              // Owning package
  ptrI srcMap;                  // Source map for this code segment
  insWord data[ZEROARRAYSIZE];  // Instruction words
} codeRec, *codePo;

extern void initPrograms(void);
extern ptrI newProgLbl(const char *name, int16 arity);
extern ptrI newProgramLbl(string name, int16 arity);
extern ptrI programLbl(string name, int16 arity);
extern ptrI defineSpecialProg(const char *name);
extern ptrI programOfClass(objPo o);
extern ptrI programOfSymbol(objPo o);
extern ptrI programOfTerm(ptrI x);
extern void defineProg(ptrI sym, ptrI code);

/*
 Each entry in the break table consists of two program counters
 the first is the last pc address an onerror may apply to this break
 the second is the PC of the onerror handler for this code
*/

extern ptrI codeClass;

static inline logical isProgLbl(objPo o) {
  return hasClass(o, programClass);
}

static inline logical IsProgLbl(ptrI X) {
  return isProgLbl(objV(X));
}

static inline logical IsCode(ptrI X) {
  return HasClass(X, codeClass);
}

static inline logical isDefined(programPo p) {
  return IsCode(p->code);
}

static inline logical IsDefined(ptrI X) {
  return (logical) (IsProgLbl(X) && isDefined((programPo) objV(X)));
}

static inline codePo codeV(ptrI x) {
  assert(IsCode(x));

  return (codePo) objV(x);
}

#define CodeCellCount(code, litcnt) CellCount(sizeof(codeRec)+(code)*sizeof(insWord)+(litcnt)*sizeof(ptrI))

static inline insPo codeIns(codePo pc) {
  assert(pc->class == codeClass);

  return pc->data;
}

static inline uint16 codeArity(codePo pc) {
  assert(pc->class == codeClass);

  return pc->arity;
}

static inline ptrPo codeLits(codePo pc) {
  assert(pc->class == codeClass);
  return (ptrPo) &pc->data[pc->size];
}

static inline ptrI sourceMap(codePo pc) {
  assert(pc->class == codeClass);
  return pc->srcMap;
}

static inline packagePo codeOwner(codePo pc){
  assert(pc->class == codeClass);
  return pc->owner;
}

/* static inline long codeBreakCount(codePo pc) */
/* { */
/*   assert(pc->class==codeClass); */
/*   return pc->brkCount; */
/* } */

static inline ptrPo CodeLits(ptrI x) {
  return codeLits((codePo) objV(x));
}

static inline long codeLitCount(codePo pc) {
  return pc->litCnt;
}

static inline void updateCodeLit(codePo pc, long ix, ptrI lit) {
  extern void markGrey(objPo o);
  markGrey((objPo) pc);

  codeLits(pc)[ix] = lit;
}

static inline unsigned long codeInsCount(codePo pc) {
  assert(pc->class == codeClass);
  return pc->size;
}

static inline logical IsProgram(ptrI x) {
  assert(IsProgLbl(x));

  programPo prog = (programPo) objV(x);

  return IsCode(prog->code);
}

static inline long programArity(objPo p) {
  assert(isProgLbl(p));

  return ((programPo) p)->lbl.arity;
}

static inline prgLabelPo programName(objPo p) {
  assert(isProgLbl(p));

  return &((programPo) p)->lbl;
}

static inline codePo programCode(objPo p) {
  assert(isProgLbl(p));
  programPo pr = (programPo) p;

  return codeV(pr->code);
}

static inline logical isGoObject(objPo p) {
  return hasClass(p, dynamicClass);
}

static inline logical IsGoObject(ptrI x) {
  return isGoObject(objV(x));
}

static inline dynPo loObjV(ptrI x) {
  assert(IsGoObject(x));

  return (dynPo) objV(x);
}

static inline ptrI objectCode(dynPo ob) {
  return ob->code;
}

static inline ptrI ProgramOf(ptrI x) {
  objPo p = objV(x);

  if (isProgLbl(p))
    return ((programPo) p)->code;
  else if (isGoObject(p))
    return objectCode((dynPo) p);
  else {
    assert(isSpecialObject(p));
    p = objV(((specialClassPo) objV(p->class))->program);

    assert(isProgLbl(p));
    programPo pr = (programPo) p;

    return pr->code;
  }
}

static inline insPo FirstInstruction(ptrI cl) {
  register codePo code = codeV(cl);

  return code->data;
}

static inline void setCode(programPo lb, ptrI code) {
  extern void markGrey(objPo p);
  lb->code = code;
  markGrey((objPo) lb);
}

extern ptrI loObject(heapPo H, ptrI T);
/* create a L&O object */

extern void initPrograms(void);
extern ptrI newProgLbl(const char *name, int16 arity);
extern ptrI newProgramLbl(string name, int16 arity);
extern ptrI defineSpecialProg(const char *name);
extern ptrI programOfClass(objPo o);
extern ptrI programOfSymbol(objPo o);
extern ptrI programOfTerm(ptrI x);
extern void defineProg(ptrI sym, ptrI code);
extern ptrI permCode(uinteger size, uinteger litCnt, packagePo owner);
extern retCode verifyCode(ptrI prog);

extern retCode loadPackage(string pkg, string version, string errorMsg, long msgSize, void *cl);

extern void initCodeClass(void);
logical isLoaded(ptrI package);

retCode locateSourceFragment(codePo cde, insPo pc, packagePo *pkg, integer *start, integer *size);

void initDynamicClass();
ptrI dynamicObject(heapPo H);
void setDynamicCode(ptrI O, ptrI code);
extern void markPrograms(globalGcPo G);
extern void installProgram(objPo p);
#endif

