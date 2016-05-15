/* 
  Program management module
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <string.h>

#include "go.h"
#include "hashTable.h"

/* Dictionary of known programs ... */
static hashPo programs;

static long pSizeFun(specialClassPo class,objPo o);
static comparison pCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode pOutFun(specialClassPo class,ioPo out,objPo o);
static retCode pScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo pCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger pHashFun(specialClassPo class,objPo o);

void initPrograms(void)
{
  programs = NewHash(256,(hashFun)uniHash,(compFun)uniCmp,NULL);

  programClass = newSpecialClass("#program",pSizeFun,pCompFun,
				 pOutFun,pCopyFun,pScanFun,pHashFun);
}

static long pSizeFun(specialClassPo class,objPo o)
{
  assert(o->class==programClass);

  programPo cl = (programPo)o;

  return CellCount(sizeof(programRec)+sizeof(byte)*(uniStrLen(cl->name)+1));
}

static comparison pCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else
    return incomparible;
}

static retCode pOutFun(specialClassPo class,ioPo out,objPo o)
{
  programPo cl = (programPo)o;

  return outMsg(out,"%U%%%d",cl->name,cl->arity);
}

static retCode pScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  assert(o->class==programClass);

  programPo d = (programPo)o;

  return helper(&d->code,c);
}

static objPo pCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = pSizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger pHashFun(specialClassPo class,objPo o)
{
  programPo cl = (programPo)o;

  return uniHash(cl->name);
}

static ptrI allocateProgramLabel(heapPo H,const string name,long arity,ptrI code)
{
  rootPo root = gcAddRoot(H,&code);

  long symlen = uniStrLen(name);
  long len = CellCount(sizeof(programRec)+sizeof(byte)*(symlen+1));
  programPo new = (programPo)permAllocate(len);

  new->class = programClass;
  new->arity = arity;
  new->code = code;

  memcpy(new->name,name,(symlen+1)*sizeof(byte));
  gcRemoveRoot(H,root);

  return objP(new);
}

ptrI newProgramLbl(string name,long arity)
{
  ptrI def = (ptrI)hashGet(programs,(void*)name);

  if(objV(def)==NULL){
    def = allocateProgramLabel(&globalHeap,name,arity,kvoid);

    hashPut(programs,programName(objV(def)),(void*)def);

    return def;
  }
  else{
    assert(programArity(objV(def))==arity);
    return def;
  }
}

ptrI newProgLbl(const char *name,long arity)
{
  return newProgramLbl((string)name,arity);
}

ptrI programByName(string name)
{
  return (ptrI)hashGet(programs,name);
}

ptrI programOfClass(objPo o)
{
  assert(isTermClass(o));

  clssPo cl = (clssPo)o;
  ptrI pr = (ptrI)hashGet(programs,className(cl));

  if(pr==0){
    byte buff[MAX_SYMB_LEN];
    uniCpy(buff,NumberOf(buff),className(cl));
    return newProgramLbl(buff,OBJECT_ARITY);
  }
  else
    return pr;
}

ptrI programOfTerm(ptrI x)
{
  if(isvar(x))
    return ProgramOf(thingProg);
  else
    return programOfClass(objV(objV(x)->class));
}

void defineProg(ptrI defn,ptrI code)
{
  assert(IsProgLbl(defn));
  assert(identical(code,kvoid)||IsCode(code));

  setCode((programPo)objV(defn),code);
}

// Handle GC of program labels

typedef struct {
  globalGcPo G;
  hashPo newDict;
} DInfoRec;

static retCode markProgram(void *n,void *r,void *c)
{
  DInfoRec *I = (DInfoRec *)c;
  ptrI S = (ptrI)r;

  objPo o = objV(S);

  /* This fragment allows code to be garbage collected - except for code loaded as part of a package */
  if(oldGeneration(o))
    hashPut(I->newDict,programName(objV(S)),(void*)S); /* put symbol directly into the new dictionary */
  else
    scanPtr(I->G,S); /* We keep defined programs FIXME */

  return Ok;
}

void markPrograms(globalGcPo G)
{
  DInfoRec help = {G,NewHash(256,(hashFun)uniHash,(compFun)uniCmp, NULL)};
  hashPo currTable = programs;

  programs = help.newDict;
  ProcessTable(markProgram,currTable,&help);

  thingProg = scanPtr(G,thingProg);
}

void installProgram(objPo p)
{
  string name = programName(p);
  ptrI sym = (ptrI)hashGet(programs,name);

  if(objV(sym)==NULL)		/* A new entry in the table */
    hashPut(programs,name,(void*)objP(p)); /* Install in program dictionary */
}

