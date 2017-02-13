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
retCode g__call(processPo P, ptrPo a) {
  ptrI pkg = deRefI(&a[1]);
  ptrI entry = deRefI(&a[2]);

  if (isvar(entry) || isvar(pkg))
    return liberror(P, "__call", eINSUFARG);
  else if (!IsString(entry) || !IsString(pkg))
    return liberror(P, "__call", eINVAL);
  else if (!isLoaded(pkg)) {
    byte errMsg[MAX_MSG_LEN];

    strMsg(errMsg, NumberOf(errMsg), "%U not loaded", StringVal(stringV(pkg)));
    return raiseError(P, (string) errMsg, eCODE);
  } else {
    byte resolved[MAX_MSG_LEN];      /* compute the entrypoint symbol */
    strMsg(resolved, NumberOf(resolved), "%U@%U", StringVal(stringV(pkg)), StringVal(stringV(entry)));

    switchProcessState(P, in_exclusion);
    ptrI prog = newProgramLbl(resolved, 0);
    setProcessRunnable(P);

    if (!IsProgram(prog)) {
      strMsg(resolved, NumberOf(resolved), "%U@%U not defined", StringVal(stringV(pkg)), StringVal(stringV(entry)));
      return raiseError(P, resolved, eCODE);
    } else {
      ptrI args = deRefI(&a[3]);

      P->proc.A[1] = args;               /* Pass in the argument list of strings */

      P->proc.cPC = P->proc.PC;                 /* start stacking stuff */
      P->proc.cPROG = P->proc.PROG;
      P->proc.cSB = P->proc.SB;
      P->proc.SB = P->proc.B;

      P->proc.PROG = ProgramOf(prog);  /* We have a new program to call */
      P->proc.PC = FirstInstruction(P->proc.PROG);

      return Error;       /* We cant use OK, because that checks for GCmap */
    }
  }
}

// __is(pred,arg) -- one argument
// results in the call:
// pred(arg)

retCode g__is(processPo P, ptrPo a) {
  ptrI prog = deRefI(&a[1]);

  if (isvar(prog))
    return liberror(P, "__is", eINSUFARG);
  else if (!IsProgram(prog)) {
    byte msg[MAX_SYMB_LEN];
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
  ptrI pkg = deRefI(&a[1]);
  ptrI entry = deRefI(&a[2]);

  if (isvar(entry) || isvar(pkg))
    return liberror(P, "__defined", eINSUFARG);
  else if (!IsString(entry) || !IsString(pkg))
    return liberror(P, "__defined", eINVAL);
  else {
    byte resolved[MAX_MSG_LEN];      /* compute the entrypoint symbol */

    strMsg(resolved, NumberOf(resolved), "%U@%U", StringVal(stringV(pkg)), StringVal(stringV(entry)));

    switchProcessState(P, in_exclusion);

    ptrI sym = newProgramLbl(resolved, 0);

    setProcessRunnable(P);

    if (IsProgram(sym))
      return Ok;
    else
      return Fail;
  }
}

/*
 * Used in aiding debugging
 */
void showCall(processPo P, ptrI prog, ptrPo args, long arity) {
  int i;
  byte buffer[1024];
  ioPo out = O_IO(fixedStringBuffer(buffer, NumberOf(buffer)));

  outMsg(out, "%w: %w(", &P->proc.thread, &prog);

  for (i = 0; i < arity; i++)
    outMsg(out, "%w%s", args++, (i < arity - 1 ? "," : ""));

  outMsg(out, ")\n%_");

  long len;
  string text = getTextFromBuffer(&len, O_BUFFER(out));

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
