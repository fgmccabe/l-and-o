/*
  Bootstrap the L&O run-time
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "lo.h"			/* Main L&O header file */
#include "thred.h"
#include "escodes.h"

/* An initial bootstrap sequence */

static ptrI buildCode(insPo cd, uint16 arity, unsigned long cdlen, unsigned long litCnt) {
  ptrI code = permCode(cdlen, litCnt, NULL);
  codePo pc = codeV(code);

  memcpy(pc->data, cd, cdlen * sizeof(insWord));
  pc->arity = arity;
  return code;
}

/* This code is associated with all symbols which have no definition */
static void defineDieProg(void) {
  static insWord proc_exit_seq[] = {
    instrhb(gcmap, 0, 0),
    instr(die)                  /* Then we expire */
  };

  dieProg = buildCode(proc_exit_seq, 0, NumberOf(proc_exit_seq), 0);
}

/* This code is executed when a process terminates */

static void defineExitProg(void) {
  static insWord proc_exit_seq[] = {
    instrhb(gcmap, 0, 0),
    instr(die)                  /* Then we expire */
  };

  exitProg = buildCode(proc_exit_seq, 0, NumberOf(proc_exit_seq), 0);
}

//
//static void defineTrapProg(void)
//{
//  insWord proc_seq[] = {
//    instrhb(alloc,3,2),
//    instrhb(gcmap,3,0),
//    instrhb(clAY,1,1),                  /* pick up the error trap value */
//    instrhb(escape,1,Esc_errorcode),
//    instrhb(gcmap,1,1),
//    instrhb(mAlit,2,0),         /* 0 */
//    instrhb(mAlit,3,0),         /* 0 */
//    instrhb(clAY,4,2),          /* $s */
//    instrhb(escape,4,Esc_stringOf),      /* __stringOf(xx,0,0,$s) */
//    instrhb(gcmap,4,2),
//    instrhb(mAY,1,2),           /* $s */
//    instrhb(escape,1,Esc_logmsg),      /* __logMsg($s) */
//    instrhb(gcmap,1,0),
//    instr(die)                  /* Finally we expire */
//  };
//  ptrI trap = buildCode(proc_seq,0,NumberOf(proc_seq),1);
//
//  updateCodeLit(codeV(trap),0,zero);
//
//  trapProg=trap;
//}


//static void defineObjectProg(ptrI sym)
//{
//  insWord proc_seq[] = {                /* (gVar,_,tVar) :- _getProp(tVar,"$label",T),
//                                           ocall(gVar,T,tVar)*/
//    instrhb(alloc,3,3),
//    instrhb(gcmap,3,0),
//    instrhb(mYA,1,1),                   /* gVar */
//    instrhb(mYA,3,2),                   /* tVar */
//    instrhm(mAA,1,3),                   /* tVar */
//    instrhb(mAlit,2,0),                 /* $label */
//    instrhb(clAY,3,3),
//    instrhb(escape,3,Esc_getProp),
//    instrhb(gcmap,3,3),
//    instrhb(mAY,1,1),                   /* gVar */
//    instrhb(mAY,2,3),                   /* T */
//    instrhb(mAY,3,2),                   /* tVar */
//    instrhm(dlkawlO,3,2),
//    instrhb(gcmap,3,0)
//  };
//  ptrI obj = buildCode(proc_seq,3,NumberOf(proc_seq),1);
//
//  updateCodeLit(codeV(obj),0,klabel);
//
//  defineProg(sym,obj);
//}

/*
 * This program is used to map special classes like integer into
 * L&O programs
 * This program is only safe before the first GC
 */
ptrI defineSpecialProg(const char *name) {
#ifdef MEMTRACE
  extern long gcCount;
  long gCount = gcCount;
#endif

  insWord proc_seq[] = {       // (gVar,_,tVar) :- name%3(gVar,name,tVar)
    instrhb(mAlit, 2, 0),      /* name */
    instrhb(lkawl, 3, 1),      /* name%3 */
  };
  ptrI obj = buildCode(proc_seq, 3, NumberOf(proc_seq), 2);
  ptrI nameEnum = newEnumSym(name);

  updateCodeLit(codeV(obj), 0, nameEnum);  /* name */

  ptrI nameProg = newProgLbl(name, 3);  /* name%3 */

  defineProg(nameProg, obj);
  updateCodeLit(codeV(obj), 1, nameProg);

#ifdef MEMTRACE
  assert(gcCount == gCount);
#endif
  return obj;
}

static void defineResumeProgs(void) {
  int ar;

  for (ar = 0; ar < NumberOf(doResume); ar++) {
    insWord seq[] = {         /* Y[arity] = interrupted code, Y[i] arguments */
      instrhb(gcmap, 0, ar),
      instrh(resume, ar),    /* this resumes the execution */
      instr(succ)
    };
    ptrI cde = buildCode(seq, 1, NumberOf(seq), 0);

    doResume[ar] = cde;
  }
}

/* Top-level bootstrap sequence, load the initial boot program and enter with standard entry point */

void bootstrap(char * entry, char * bootPkg, char * version) {
  char errorMsg[MAX_MSG_LEN];

  defineExitProg();
  defineDieProg();
//  defineTrapProg();
//  defineObjectProg(kmain);
  defineResumeProgs();                  /* define the resume entry points */

  ptrI mainThread = newThread();

  processPo root = rootProcess(mainThread, bootPkg); // We create the root with a early death code - updated later

  switch (loadPackage(bootPkg, version, errorMsg, NumberOf(errorMsg), NULL)) {
    default:
      logMsg(logFile, "corrupt or no boot package found in %s:%s", bootPkg, errorMsg);
      lo_exit(EXIT_FAIL);
      return;
    case Eof:
      logMsg(logFile, "short boot package %s", bootPkg);
      lo_exit(EXIT_FAIL);
      return;
    case Ok: {
      ptrI prog = newProgramLbl(entry, 0);

      if (!IsProgram(prog)) {
        logMsg(logFile, "%U entry point not found", entry);
        lo_exit(EXIT_FAIL);
      } else {
        root->proc.PROG = ProgramOf(prog); /* this is where we establish the program */
        root->proc.PC = FirstInstruction(root->proc.PROG);
        root->proc.thread = kmainThread;

        runGo(root);
      }
    }
  }
}
