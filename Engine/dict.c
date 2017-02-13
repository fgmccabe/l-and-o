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
ptrI kvoid;              /* void value marker */

ptrI kmain;        /* Main entry point */
ptrI kmainThread;      /* The main thread name */

ptrI bootProg;                             /* The bootstrap entry point */
ptrI dieProg;        /* program just dies */
ptrI exitProg;            /* program that succeeds out of process */
ptrI trapProg;        /* default trap handler */

ptrI kprocessFlag;                      /* The property that a thread is stored under */

ptrI varClass;        /* Standard globalized variable */
ptrI suspClass;        /* Standard suspension class */

ptrI kfifo, kdir, kcharfile, kblock, kplain, ksymlink, ksock, kunknown;

ptrI kloadflag;                         /* the loaded flag */
ptrI kversion;                          /* which package version is loaded */
ptrI kdefined;                          /* the imported package description */
ptrI universal;                         /* universal package version */
ptrI klabel;                            /* The special $label property */
ptrI kstart;                            /* entry point for new threads */
ptrI doResume[LO_REGS];                 /* array of special exit points for  */
ptrI kdelay;        /* standard delay handler */

void initDict()        /* Initialize the dictionary */
{
  standardClasses();      /* fill in the initial set of classes */

  /* Declare run-time symbols */

  initErrorSymbols();

  kmain = newProgLbl("main", 1);
  kmainThread = newEnumSym("lo.core#rootThread");
  bootProg = newEnumSym("lo.boot");

  kprocessFlag = newEnumSym("$process");

  /* special value symbols */

  kfifo = newEnumSym("lo.io#fifoSpecial");
  kdir = newEnumSym("lo.io#directory");
  kcharfile = newEnumSym("lo.io#charSpecial");
  kblock = newEnumSym("lo.io#blockSpecial");
  kplain = newEnumSym("lo.io#plainFile");
  ksymlink = newEnumSym("lo.io#symlink");
  ksock = newEnumSym("lo.io#socket");
  kunknown = newEnumSym("lo.io#unknownFileType");

  kloadflag = newEnumSym("$loaded");     /* This property is set on a package as it is loaded */

  kversion = newEnumSym("$version");     /* This property is set to the loaded version */
  universal = newEnumSym("*");
  kdefined = newEnumSym("$defined");     /* definition of imported package */

  klabel = newEnumSym("$label");

  kstart = newEnumSym("start_thread%0"); /* first call to a thread */
  kdelay = newProgLbl("lo.stdlib@delayHandler", 1);
}

void restartDictionary(globalGcPo G) {
  markStandardClasses(G);
  markPrograms(G);

  kmain = scanPtr(G, kmain);
  kmainThread = scanPtr(G, kmainThread);
  bootProg = scanPtr(G, bootProg);
  dieProg = scanPtr(G, dieProg);
  exitProg = scanPtr(G, exitProg);
  trapProg = scanPtr(G, trapProg);
  kprocessFlag = scanPtr(G, kprocessFlag);

  kvoid = scanPtr(G, kvoid);    /* void value marker */

  kfifo = scanPtr(G, kfifo);
  kdir = scanPtr(G, kdir);
  kcharfile = scanPtr(G, kcharfile);
  kblock = scanPtr(G, kblock);
  kplain = scanPtr(G, kplain);
  ksymlink = scanPtr(G, ksymlink);
  ksock = scanPtr(G, ksock);
  kunknown = scanPtr(G, kunknown);

  kloadflag = scanPtr(G, kloadflag);
  kversion = scanPtr(G, kversion);
  universal = scanPtr(G, universal);
  kdefined = scanPtr(G, kdefined);
  klabel = scanPtr(G, klabel);

  kstart = scanPtr(G, kstart);

  {
    int ii;
    for (ii = 0; ii < NumberOf(doResume); ii++)
      doResume[ii] = scanPtr(G, doResume[ii]);
  }

  kdelay = scanPtr(G, kdelay);

  eINSUFARG = scanPtr(G, eINSUFARG);
  eVARNEEDD = scanPtr(G, eVARNEEDD);
  eINTNEEDD = scanPtr(G, eINTNEEDD);
  eNUMNEEDD = scanPtr(G, eNUMNEEDD);
  eSPACE = scanPtr(G, eSPACE);
  eUNIFY = scanPtr(G, eUNIFY);
  eOCCUR = scanPtr(G, eOCCUR);
  eCODE = scanPtr(G, eCODE);
  eDIVZERO = scanPtr(G, eDIVZERO);
  eLSTNEEDD = scanPtr(G, eLSTNEEDD);
  eTPLNEEDD = scanPtr(G, eTPLNEEDD);
  eSYMNEEDD = scanPtr(G, eSYMNEEDD);
  eCHRNEEDD = scanPtr(G, eCHRNEEDD);
  eSTRNEEDD = scanPtr(G, eSTRNEEDD);
  eHANDLE = scanPtr(G, eHANDLE);
  eINVAL = scanPtr(G, eINVAL);
  eRANGE = scanPtr(G, eRANGE);
  eNOPERM = scanPtr(G, eNOPERM);
  eNOFILE = scanPtr(G, eNOFILE);
  eNOTDIR = scanPtr(G, eNOTDIR);
  eCFGERR = scanPtr(G, eCFGERR);
  eEOF = scanPtr(G, eEOF);
  eIOERROR = scanPtr(G, eIOERROR);
  eABORT = scanPtr(G, eABORT);
  eNOTFND = scanPtr(G, eNOTFND);
  eCONNECT = scanPtr(G, eCONNECT);
  eFAIL = scanPtr(G, eFAIL);
  eINVCODE = scanPtr(G, eINVCODE);
  eASSIGN = scanPtr(G, eASSIGN);
  eSYSTEM = scanPtr(G, eSYSTEM);
  eDEAD = scanPtr(G, eDEAD);
  eTIME = scanPtr(G, eTIME);
  eDUPLICATE = scanPtr(G, eDUPLICATE);
  eNOIMPL = scanPtr(G, eNOIMPL);
  eNOTENUF = scanPtr(G, eNOTENUF);
  eINTRUPT = scanPtr(G, eINTRUPT);
}



