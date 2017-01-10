/*
  Dictionary and symbol handling functions for L&O
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"
#include <string.h>		/* Access string defs */

#include "lo.h"
#include "hashTable.h"

/* standard symbols */
ptrI kvoid;			    		/* void value marker */

ptrI emptySymbol;				/* the null symbol */
ptrI zero;              		/* the 0 number */

ptrI kmain;				/* Main entry point */
ptrI kmainThread;			/* The main thread name */

ptrI bootProg;                             /* The bootstrap entry point */
ptrI dieProg;				/* program just dies */
ptrI exitProg;			      /* program that succeeds out of process */
ptrI trapProg;				/* default trap handler */

ptrI kprocessFlag;                      /* The property that a thread is stored under */

ptrI varClass;				/* Standard globalized variable */
ptrI suspClass;				/* Standard suspension class */

ptrI kfifo,kdir,kcharfile,kblock,kplain,ksymlink,ksock,kunknown;

ptrI kloadflag;                         /* the loaded flag */
ptrI kversion;                          /* which package version is loaded */
ptrI kdefined;                          /* the imported package description */
ptrI universal;                         /* universal package version */
ptrI klabel;                            /* The special $label property */
ptrI kstart;                            /* entry point for new threads */
ptrI doResume[LO_REGS];                 /* array of special exit points for  */
ptrI kdelay;				/* standard delay handler */

/* Standard dictionaries ... */
static hashPo dictionary;	/* The main symbol table */

static long sySizeFun(specialClassPo class,objPo o);
static comparison syCompFun(specialClassPo class,objPo o1,objPo o2);
static retCode syOutFun(specialClassPo class,ioPo out,objPo o);
static retCode syScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o);
static objPo syCopyFun(specialClassPo class,objPo dst,objPo src);
static uinteger syHashFun(specialClassPo class,objPo o);

void initSymbolClass(void)
{
  symbolClass=newSpecialClass("lo.stdlib#symbol",sySizeFun,syCompFun,
			      syOutFun,syCopyFun,syScanFun,syHashFun);
}

static long sySizeFun(specialClassPo class,objPo o)
{
  assert(o->class==symbolClass);
  symbPo s = (symbPo)o;

  return CellCount(sizeof(symbolRec)+(uniStrLen(s->data)+1)*sizeof(byte));
}

static comparison syCompFun(specialClassPo class,objPo o1,objPo o2)
{
  if(o1==o2)
    return same;
  else if(o1->class==symbolClass && o2->class==symbolClass){
    symbPo s1 = (symbPo)o1;
    symbPo s2 = (symbPo)o2;

    int comp = uniCmp(SymVal(s1),SymVal(s2));

    if(comp==0)
      return same;
    else if(comp<0)
      return smaller;
    else
      return bigger;
  }
  else
    return incomparible;
}

static retCode syOutFun(specialClassPo class,ioPo out,objPo o)
{
  symbPo s = (symbPo)o;
  string sym = SymVal(s);
  retCode r = outChar(out,'\'');
	
  while(r==Ok && *sym!=0)
    r = wStringChr(out,*sym++);
  if(r==Ok)
    r = outChar(out,'\'');
  return r;
}

static retCode syScanFun(specialClassPo class,specialHelperFun helper,void *c,objPo o)
{
  return Ok;
}

static objPo syCopyFun(specialClassPo class,objPo dst,objPo src)
{
  long size = sySizeFun(class,src);
  memmove((void*)dst,(void*)src,size*sizeof(ptrI));

  return (objPo)(((ptrPo)dst)+size);
}

static uinteger syHashFun(specialClassPo class,objPo o)
{
  assert(o->class==symbolClass);

  symbPo s = (symbPo)o;
  return uniHash(SymVal(s));
}

ptrI permUniSymbol(const string txt)
{
  long symlen = uniStrLen(txt);
  long len = CellCount(sizeof(symbolRec)+(symlen+1)*sizeof(byte));
  symbPo new = (symbPo)permAllocate(len);

  new->class = symbolClass;
  memcpy(new->data,txt,(symlen+1)*sizeof(byte)); /* copy symbol's text */
  return objP(new);
}

/* Locate a symbol in the dictionary */
ptrI newSymbol(const char *name)
{
  size_t slen = strlen(name);
  byte buff[slen+1];

  memcpy(buff,name,slen+1);

  return newUniSymbol(buff);
}

ptrI newUniSymbol(const string name)
{
  ptrI sym = (ptrI)hashGet(dictionary,(void*)name);

  if(objV(sym)==NULL){		/* A new entry in the table */
    sym = permUniSymbol((byte*)name);

    hashPut(dictionary,SymVal(symbV(sym)),(void*)sym);
  }
  return sym;			/* return the symbol */
}

void installSymbol(symbPo s)
{
  string name = SymVal(s);
  ptrI sym = (ptrI)hashGet(dictionary,name);

  if(objV(sym)==NULL)		/* A new entry in the table */
    hashPut(dictionary,name,(void*)objP(s)); /* Install in dictionary */
}

void initDict()				/* Initialize the dictionary */
{
  dictionary = NewHash(256,(hashFun)uniHash,(compFun)uniCmp, NULL);

  standardClasses();			/* fill in the initial set of classes */

  /* Declare run-time symbols */

  initErrorSymbols();

  emptySymbol=newSymbol("");
  zero = permInteger(0);

  kmain = newProgLbl("main",1);
  kmainThread = newEnumSym("lo.stdlib#rootThread");
  bootProg = newSymbol("lo.boot");

  kprocessFlag = newSymbol("$process");
  
  /* special value symbols */

  kfifo = newEnumSym("lo.io#fifoSpecial");
  kdir = newEnumSym("lo.io#directory");
  kcharfile = newEnumSym("lo.io#charSpecial");
  kblock = newEnumSym("lo.io#blockSpecial");
  kplain = newEnumSym("lo.io#plainFile");
  ksymlink = newEnumSym("lo.io#symlink");
  ksock = newEnumSym("lo.io#socket");
  kunknown = newEnumSym("lo.io#unknownFileType");

  kloadflag = newSymbol("$loaded");     /* This property is set on a package as it is loaded */

  kversion = newSymbol("$version");     /* This property is set to the loaded version */
  universal = newSymbol("*");
  kdefined = newSymbol("$defined");     /* definition of imported package */

  klabel = newSymbol("$label");

  kstart = newEnumSym("start_thread%0"); /* first call to a thread */
  kdelay = newProgLbl("lo.stdlib@delayHandler",1);
}

/* remove all entries from the dictionary */
typedef struct {
  globalGcPo G;
  hashPo newDict;
} DInfoRec;

static retCode remSym(void *n,void *r,void *c)
{
  DInfoRec *I = (DInfoRec *)c;
  ptrI S = (ptrI)r;

  hashRemove(dictionary,n);

  objPo o = objV(S);

  /* This fragment allows code to be garbage collected - except for code loaded as part of a package */
  if(oldGeneration(o))
    hashPut(I->newDict,SymVal(symbV(S)),(void*)S); /* put symbol directly into the new dictionary */

  return Ok;
}

void restartDictionary(globalGcPo G)
{
  DInfoRec help = {G,NewHash(256,(hashFun)uniHash,(compFun)uniCmp, NULL)};
  hashPo currDict = dictionary;

  dictionary = help.newDict;
  ProcessTable(remSym,currDict,&help);

  DelHash(currDict);			/* clear down the existing dictionary */

  markStandardClasses(G);
  markPrograms(G);
  
  kmain = scanPtr(G,kmain);
  kmainThread = scanPtr(G,kmainThread);
  bootProg = scanPtr(G,bootProg);
  dieProg = scanPtr(G,dieProg);
  exitProg = scanPtr(G,exitProg);
  trapProg = scanPtr(G,trapProg);
  kprocessFlag = scanPtr(G,kprocessFlag);

  kvoid = scanPtr(G,kvoid);		/* void value marker */
  emptySymbol = scanPtr(G,emptySymbol);
  zero = scanPtr(G,zero);

  kfifo = scanPtr(G,kfifo);
  kdir = scanPtr(G,kdir);
  kcharfile = scanPtr(G,kcharfile);
  kblock = scanPtr(G,kblock);
  kplain = scanPtr(G,kplain);
  ksymlink = scanPtr(G,ksymlink);
  ksock = scanPtr(G,ksock);
  kunknown = scanPtr(G,kunknown);

  kloadflag = scanPtr(G,kloadflag);
  kversion = scanPtr(G,kversion);
  universal = scanPtr(G,universal);
  kdefined = scanPtr(G,kdefined);
  klabel = scanPtr(G,klabel);

  kstart = scanPtr(G,kstart);

  {
    int ii;
    for(ii=0;ii<NumberOf(doResume);ii++)
      doResume[ii] = scanPtr(G,doResume[ii]);
  }

  kdelay = scanPtr(G,kdelay);

  eINSUFARG = scanPtr(G,eINSUFARG);
  eVARNEEDD = scanPtr(G,eVARNEEDD);
  eINTNEEDD = scanPtr(G,eINTNEEDD);
  eNUMNEEDD = scanPtr(G,eNUMNEEDD);
  eSPACE = scanPtr(G,eSPACE);
  eUNIFY = scanPtr(G,eUNIFY);
  eOCCUR = scanPtr(G,eOCCUR);
  eCODE = scanPtr(G,eCODE);
  eDIVZERO = scanPtr(G,eDIVZERO);
  eLSTNEEDD = scanPtr(G,eLSTNEEDD);
  eTPLNEEDD = scanPtr(G,eTPLNEEDD);
  eSYMNEEDD = scanPtr(G,eSYMNEEDD);
  eCHRNEEDD = scanPtr(G,eCHRNEEDD);
  eSTRNEEDD = scanPtr(G,eSTRNEEDD);
  eHANDLE = scanPtr(G,eHANDLE);
  eINVAL = scanPtr(G,eINVAL);
  eRANGE = scanPtr(G,eRANGE);
  eNOPERM = scanPtr(G,eNOPERM);
  eNOFILE = scanPtr(G,eNOFILE);
  eNOTDIR = scanPtr(G,eNOTDIR);
  eCFGERR = scanPtr(G,eCFGERR);
  eEOF = scanPtr(G,eEOF);
  eIOERROR = scanPtr(G,eIOERROR);
  eABORT = scanPtr(G,eABORT);
  eNOTFND = scanPtr(G,eNOTFND);
  eCONNECT = scanPtr(G,eCONNECT);
  eFAIL = scanPtr(G,eFAIL);
  eINVCODE = scanPtr(G,eINVCODE);
  eASSIGN = scanPtr(G,eASSIGN);
  eSYSTEM = scanPtr(G,eSYSTEM);
  eDEAD = scanPtr(G,eDEAD);
  eTIME = scanPtr(G,eTIME);
  eDUPLICATE = scanPtr(G,eDUPLICATE);
  eNOIMPL = scanPtr(G,eNOIMPL);
  eNOTENUF = scanPtr(G,eNOTENUF);
  eINTRUPT = scanPtr(G,eINTRUPT);
}

#ifdef ALLTRACE
static retCode dSym(void *n,void *r,void *c)
{
  ioPo f = (ioPo)c;
  symbPo sym = symbV((ptrI)r);

  outMsg(f,"%U\n",SymVal(sym));

  return Ok;
}

void dumpDict(ioPo out)
{
  ProcessTable(dSym,dictionary,out);
  flushOut();
}

void dDict(void)
{
  dumpDict(logFile);
}

void showSymbol(char *s)
{
  ptrI sym = (ptrI)hashGet(dictionary,s);

  if(objV(sym)!=NULL){
    symbPo sm = symbV(sym);

    outMsg(logFile,"%U\n",SymVal(sm));
  }
  else
    outMsg(logFile,"%s not found in dictionary\n",s);
  flushOut();
}
  

#endif

