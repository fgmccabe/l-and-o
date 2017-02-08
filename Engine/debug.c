/* 
 Instruction-level debugging of L&O code
 Copyright (c) 2016, 2017. Francis G. McCabe

 Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the
 License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied. See the License for the specific language governing
 permissions and limitations under the License.
 */
#include "lo.h"
#include "debug.h"
#include "disass.h"
#include "esc.h"

logical tracing = True;

/* do we show each step */

retCode g__ins_debug(processPo P, ptrPo a) {
  debugging = interactive = True;
  return Ok;
}

/* Parse a command line to display a register value */
void showReg(ptrPo a, char *name, integer reg) {
  ptrPo ptr = &a[reg];

  outMsg(logFile, "%s[%d] = ", name, (int) reg);

  if (*ptr == 0)
    outMsg(logFile, "<<NULL>>\n");
  else {
    while (isvar(*ptr)) {
      if ((ptrPo) *ptr == ptr) {
        if (isSuspVar(ptr))
          outMsg(logFile, "_*%x", ptr);
        else
          outMsg(logFile, "_%x", ptr);
        return;
      } else {
        outMsg(logFile, "_%x->", ptr);
        ptr = (ptrPo) *ptr;
      }
    }

    outMsg(logFile, "%.40w\n", ptr);
  }
}

DebugWaitFor waitingFor = nextIns;
/* waiting for next instruction */
long cmdCounter = 0;

#ifdef EXECTRACE

static long cmdCount(string cmdLine) {
  long count = (long) parseInteger(cmdLine, strlen((char*) cmdLine));
  if (count == 0)
    return 1; /* never return 0 */
  else
    return count;
}

static processPo focus = NULL;

static pthread_mutex_t debugMutex = PTHREAD_MUTEX_INITIALIZER;

retCode debug_stop(processPo p, ptrI prog, insPo pc, ptrI cprog, insPo cpc, ptrPo a, ptrPo y, ptrPo S,
    long Svalid, rwmode mode, choicePo B, choicePo SB, choicePo T, ptrPo hBase, ptrPo H, trailPo trail,
    ptrI prefix) {
  byte ch;
  static byte cmdLine[256] = "n";
  codePo code = codeV(prog);
  codePo ccode = codeV(cprog);
  extern HeapRec globalHeap;

  pthread_mutex_lock(&debugMutex);

  if (focus == NULL || focus == p) {
    switch (waitingFor) {
    case nextIns:
      cmdCounter--;
      break;
    case nextSucc:
      switch (op_code(*pc)) {
      case succ:
        cmdCounter--;
        break;
      case kawlO:
      case kawl:
        cmdCounter++;
        break;
      default:
        ;
      }
      break;
    case nextBreak: /* nothing to do here */
    case nextFail:
      break;
    }

    if (tracing || cmdCounter <= 0) {
      byte pref[MAX_SYMB_LEN];
      ptrPo Lits = codeLits(code);

      strMsg(pref, NumberOf(pref), "%w "RED_ESC_ON "[%d]" RED_ESC_OFF " %w", &prefix, pcCount, &Lits[0]);
      dissass(pref, code, pc, a, y, S, mode, B, hBase, H);

      outMsg(logFile, "\ncPC=%w[%d],B=%x,SB=%x,T=%x,Y=%x[%d],", &codeLits(ccode)[0], cpc - codeIns(ccode), B,
          SB, T, y, envSize(cpc));
      if (Svalid > 0)
        outMsg(logFile, "S=%x(%d),", S, Svalid);
      else if (mode == writeMode)
        outMsg(logFile, "S=%x,", H);
      outMsg(logFile, "trail=%x", trail);
      if (!identical(p->proc.trigger, emptyList))
        outMsg(logFile, ",%d triggered", ListLen(deRefI(&p->proc.trigger)));
      outMsg(logFile, "\nH=%x[%d], ", H, (ptrPo) p->proc.heap.end - H);
      outMsg(logFile, "global=%x[%d]\n", globalHeap.create, globalHeap.end - globalHeap.create);
      flushFile(logFile);
    }

    if (cmdCounter <= 0) { /* do we need to stop? */
      while (debugging && cmdCounter <= 0) { /* prompt the user */
        byte *ln = cmdLine;
        outMsg(logFile, " => ");
        flushOut();

        retCode res = inByte(stdIn, &ch);

        if (ch != '\n' && res == Ok) {
          do {
            *ln++ = ch;
            res = inByte(stdIn, &ch);
          } while (ch != '\n' && res == Ok);
          *ln++ = '\0';
        }

        switch (cmdLine[0]) {
        case ' ':
          cmdCounter = cmdCount(cmdLine + 1);
          waitingFor = nextIns;
          tracing = True;
          break;
        case 'n':
          cmdCounter = cmdCount(cmdLine + 1);
          waitingFor = nextIns;
          tracing = False;
          break;
        case 'N':
          cmdCounter = cmdCount(cmdLine + 1);
          switch (op_code(*pc)) {
          case kawlO:
          case kawl:
          case lkawlO:
          case lkawl:
          case dlkawlO:
          case dlkawl:
            waitingFor = nextSucc;
            break;
          default:
            waitingFor = nextIns;
          }
          tracing = False;
          break;

        case '\n':
          cmdCounter = 1;
          waitingFor = nextIns;
          break;

        case 'x': /* wait for a success */
          cmdCounter = cmdCount(cmdLine + 1);
          waitingFor = nextSucc;
          break;

        case 'f':
          focus = p;
          outMsg(logFile, "Focussing on program %w\n", &p->proc.thread);
          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          break;

        case 'F':
          pthread_mutex_unlock(&debugMutex);
          return Fail;

        case 'u':
          focus = NULL;
          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          break;

        case 'q':
          outMsg(logFile, "terminating lo session");
          lo_exit(0);
          break;

        case 'c':
          cmdCounter = cmdCount(cmdLine + 1);
          waitingFor = nextBreak;
          tracing = False;
          break;

        case 't':
          waitingFor = nextBreak;
          tracing = True;
          cmdCounter = 1;
          break;

        case 'S':
          SymbolDebug = True;
          debugging = False;
          interactive = True;
          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          break;

        case 'a': { /* dump an argument register */
          showReg(a, "A", parseInteger(&cmdLine[1], uniStrLen(&cmdLine[1])));
          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          continue;
        }

        case 'y': { /* dump a local variable */
          integer off = parseInteger(&cmdLine[1], uniStrLen(&cmdLine[1]));

          outMsg(logFile, "Y[%ld] = %w\n", off, &y[-off]);
          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          continue;
        }

        case 'r': { /* show all registers */
          unsigned int i;
          int Ylen = envSize(cpc);

          for (i = 1; i <= B->AX; i++)
            outMsg(logFile, "A[%d]=%w\n", i, &a[i]);

//          for (i = 1; i <= Ylen; i++)
//            outMsg(logFile, "%Y[%d]=%w\n", i, &y[-i]);

          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          continue;
        }

        case 'P': { /* Display all processes */
          displayProcesses();
          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          continue;
        }
        case 's': /* Show a stack trace of this process */
          p->proc.B = B;
          p->proc.C = (callPo) y;
          p->proc.cPC = cpc;
          stackTrace(p);
          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          continue;

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': {
          cmdCounter = cmdCount(cmdLine);
          waitingFor = nextIns;
          continue;
        }

        case 'i': { /* Show the following instructions */
          long count = cmdCount(cmdLine + 1);
          insPo tmpPc = pc;
          insPo limit = &code->data[code->size];

          while (count-- > 0 && tmpPc < limit) {
            tmpPc = dissass(NULL, codeV(prog), tmpPc, a, y, S, dummyMode, B, NULL, NULL);
            outMsg(logFile, "\n");
          }
          strMsg(cmdLine, NumberOf(cmdLine), "n\n"); /* default to next instruction */
          continue;
        }

        default:
          outMsg(logFile, "'n' = step, 'N' = step over, 'c' = continue, 't' = trace mode, 'q' = stop\n");
          outMsg(logFile, "'x' = step until success, 'F' = force backtrack\n");
          outMsg(logFile, "'<n>' = step <n>\n");
          outMsg(logFile, "'i <count> = list <count> instructions\n");
          outMsg(logFile, "'S' = symbolic mode\n");
          outMsg(logFile, "'r' = show registers, 'a <n>' = show A[n], 'y <n>' = show Y[n]\n");
          outMsg(logFile, "'s' = show stack trace\n");
          outMsg(logFile, "'P' = show all processes\n");
          outMsg(logFile, "'f' = focus on this process\n");
          outMsg(logFile, "'u' = unfocus \n");

          continue;
        }
      }
      pthread_mutex_unlock(&debugMutex);
      return Ok;
    }
  }
  pthread_mutex_unlock(&debugMutex);
  return Ok;
}

void dC(ptrI w){
  outMsg(logFile,"%w\n",&w);
  flushOut();
}

#endif /* EXECTRACE */

