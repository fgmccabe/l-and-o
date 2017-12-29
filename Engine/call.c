/* Special call to top-level program loaded at boot-time
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
#include "lo.h"			/* main header file */

// __call(package,entry,arguments) -- list of strings
// results in the call:
// package.entry(arguments)
//

static retCode populateArgs(ptrI Args, ptrPo R, int arity);

retCode g__call(processPo P, ptrPo a) {
  ptrI PRG = deRefI(&a[1]);
  ptrI AR = deRefI(&a[2]);
  ptrI ARGS = deRefI(&a[3]);

  if (isvar(PRG) || isvar(AR) || isvar(ARGS))
    return liberror(P, "__call", eINSUFARG);
  else if (!IsString(PRG) || !IsInt(AR) || !isGroundTerm(&ARGS))
    return liberror(P, "__call", eINVAL);
  else {
    strBuffPo pS = stringV(PRG);
    char prog[MAX_SYMB_LEN];
    copyString2Buff(prog, NumberOf(prog), pS);

    uint16 arity = (uint16) integerVal(intV(AR));

    ptrI PROG = programLbl((char *) prog, arity);

    if (PROG != 0 && IsProgram(PROG)) {
      retCode ret = populateArgs(ARGS, (ptrPo) &P->proc.A[1], arity);

      if (ret == Ok) {
        P->proc.cPC = P->proc.PC;                 /* start stacking stuff */
        P->proc.cPROG = P->proc.PROG;
        P->proc.cSB = P->proc.SB;
        P->proc.SB = P->proc.B;

        P->proc.PROG = ProgramOf(PROG);  /* We have a new program to call */
        P->proc.PC = FirstInstruction(P->proc.PROG);

        return Error;       /* We cant use OK, because that checks for GCmap */
      }
    }
    strMsg((char *) &P->proc.errorMsg, NumberOf(P->proc.errorMsg), "%s/%d not defined", prog, arity);
    return raiseError(P,  (char*)&P->proc.errorMsg, eCODE);
  }
}

retCode populateArgs(ptrI Ls, ptrPo R, int arity) {
  for (int ix = 0; ix < arity && ix < LO_REGS && IsList(Ls);) {
    ptrPo h = listHead(objV(Ls));
    ptrI C = deRefI(h);
    *R++ = C;
    Ls = deRefI(h + 1);
  }
  if (IsNil(Ls))
    return Ok;
  else
    return Fail;
}

// __is(pred,arg) -- one argument
// results in the call:
// pred(arg)

retCode g__is(processPo P, ptrPo a) {
  ptrI prog = deRefI(&a[1]);

  if (isvar(prog))
    return liberror(P, "__is", eINSUFARG);
  else if (!IsProgram(prog)) {
    char msg[MAX_SYMB_LEN];
    strMsg(msg, NumberOf(msg), "%w not defined", &prog);
    return raiseError(P, msg, eCODE);
  } else {
    ptrI arg = deRefI(&a[2]);

    P->proc.A[1] = arg;               /* Pass in the argument */

    P->proc.cPC = P->proc.PC;    /* start stacking stuff */
    P->proc.cPROG = P->proc.PROG;
    P->proc.cSB = P->proc.SB;
    P->proc.SB = P->proc.B;

    P->proc.PROG = ProgramOf(prog);                 /* We have a new program to call */
    P->proc.PC = FirstInstruction(P->proc.PROG);

    return Error;       /* We cant use OK, because that checks for GCmap */
  }
}

// __defined(package,entry) -- look for a defined symbol in package
//
retCode g__defined(processPo P, ptrPo a) {
  ptrI entry = deRefI(&a[1]);
  ptrI ar = deRefI(&a[2]);

  if (isvar(entry) || isvar(ar))
    return liberror(P, "__defined", eINSUFARG);
  else if (!IsString(entry) || !IsInt(ar))
    return liberror(P, "__defined", eINVAL);
  else {
    char resolved[MAX_MSG_LEN];      /* compute the entrypoint symbol */

    copyString2Buff(resolved,NumberOf(resolved),stringV(entry));

    integer arity = integerVal(intV(ar));

    switchProcessState(P, in_exclusion);

    ptrI sym = programLbl(resolved, (uint16)arity);

    setProcessRunnable(P);

    if (sym!=0 && IsProgram(sym))
      return Ok;
    else
      return Fail;
  }
}

/*
 * Used in aiding debugging
 */
void showCall(processPo P, char *prefix, ptrI prog, ptrPo args, long arity) {
  int i;
  char buffer[1024];
  ioPo out = O_IO(fixedStringBuffer(buffer, NumberOf(buffer)));

  outMsg(out, "%s %w: %w(", prefix, &P->proc.thread, &prog);

  for (i = 0; i < arity; i++)
    outMsg(out, "%w%s", args++, (i < arity - 1 ? "," : ""));

  outMsg(out, ")\n%_");

  long len;
  char * text = getTextFromBuffer(&len, O_BUFFER(out));

  outText(logFile, text, len);
  closeFile(out);
  flushOut();
}

void showOCall(processPo P, ptrPo obj, ptrPo call, ptrPo this) {
  if (deRefI(obj) == deRefI(this))
    outMsg(logFile, "%w: %w.%w\n%_", &P->proc.thread, obj, call);
  else
    outMsg(logFile, "%w: %w.%w/%w\n%_", &P->proc.thread, obj, call, this);
}
