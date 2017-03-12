/*
 Dissassembly of L&O instructions
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
#include "esc.h"

#undef instruction
#define instruction(Nm, Cd, A1, A2, Cmt) \
  case Nm:                      /* Cmt */\
  return outIns(logFile,#Nm,A1,A2,code,pc,pcx,a,y,S,mode,B,hBase,hLimit);
#undef lastInstruction
#define lastInstruction

static insPo outIns(ioPo out, char *Nm, opAndSpec A1, opAndSpec A2, codePo code, insPo pc, insWord pcx,
    ptrPo a, ptrPo y, ptrPo S, rwmode mode, choicePo B, ptrPo hBase, ptrPo hLimit);

/* disassemble instruction at pc */
insPo dissass(byte *pref, codePo code, insPo pc, ptrPo a, ptrPo y, ptrPo S, rwmode mode, choicePo B,
    ptrPo hBase, ptrPo hLimit) {
  register insWord pcx = *pc;

  if (pref != NULL)
    outMsg(logFile, "%U", pref);

  switch (pcx & op_mask) {
#include "instructions.h"

  default: /* illegal instruction */
    outMsg(logFile, "unknown[%x]", pcx);
    return pc + 1;
  }
}

void showInstructions(codePo code, long pc, long count) {
  insPo basePc = codeIns(code);

  for(long ix=0;ix<count;ix++){
    dissass(NULL, code, &basePc[pc+ix], NULL, NULL, NULL, dummyMode, NULL, NULL, NULL);
    outChar(logFile,'\n');
  }

  flushFile(logFile);
}

static char *showOpAnd(ioPo out, char *sep, opAndSpec A, insWord pcx, rwmode mode, ptrPo a, ptrPo y, ptrPo S,
    ptrPo Lits, ptrPo hBase, ptrPo hLimit) {
  switch (A) {
  case nOp:                             // No operand
    return sep;
  case iAh:                             // input argument register in upper slot (0..255)
    if (mode == dummyMode)
      outMsg(out, "%sA[%d]", sep, op_h_val(pcx));
    else
      outMsg(out, "%sA[%d]=%,3w", sep, op_h_val(pcx), &a[op_h_val(pcx)]);
    return ",";
  case oAh:                             // output argument register in upper slot (0..255)
    outMsg(out, "%sA[%d]", sep, op_h_val(pcx));
    return ",";
  case iAm:                             // input argument register in middle slot (0..255)
    if (mode == dummyMode)
      outMsg(out, "%sA[%d]", sep, op_m_val(pcx));
    else
      outMsg(out, "%sA[%d]=%,3w", sep, op_m_val(pcx), &a[op_m_val(pcx)]);
    return ",";
  case oAm:                             // output argument register in middle slot (0..255)
    outMsg(out, "%sA[%d]", sep, op_m_val(pcx));
    return ",";
  case iAl:                             // input argument register in lower slot (0..255)
    if (mode == dummyMode)
      outMsg(out, "%sA[%d]", sep, op_l_val(pcx));
    else
      outMsg(out, "%sA[%d]=%,3w", sep, op_l_val(pcx), &a[op_l_val(pcx)]);
    return ",";
  case oAl:                             // output argument register in lower slot (0..255)
    outMsg(out, "%sA[%d]", sep, op_l_val(pcx));
    return ",";

  case iLh:                             // input local variable offset (0..255)
    if (mode == dummyMode)
      outMsg(out, "%sY[%d]", sep, op_h_val(pcx));
    else
      outMsg(out, "%sY[%d]=%,3w", sep, op_h_val(pcx), &y[-op_h_val(pcx)]);
    return ",";
  case iLm:                             // input local variable offset (0..255)
    if (mode == dummyMode)
      outMsg(out, "%sY[%d]", sep, op_m_val(pcx));
    else
      outMsg(out, "%sY[%d]=%,3w", sep, op_m_val(pcx), &y[-op_m_val(pcx)]);
    return ",";
  case iLl:                             // input local variable offset (0..255)
    if (mode == dummyMode)
      outMsg(out, "%sY[%d]", sep, op_l_val(pcx));
    else
      outMsg(out, "%sY[%d]=%,3w", sep, op_l_val(pcx), &y[-op_l_val(pcx)]);
    return ",";
  case iLc:                             // input local variable offset (0..65535)
    if (mode == dummyMode)
      outMsg(out, "%sY[%d]", sep, op_o_val(pcx));
    else
      outMsg(out, "%sY[%d]=%,3w", sep, op_o_val(pcx), &y[-op_o_val(pcx)]);
    return ",";
  case oLh:                             // output local variable offset (0..255)
    outMsg(out, "%sY[%d]", sep, op_h_val(pcx));
    return ",";
  case oLm:                             // output local variable offset (0..255)
    outMsg(out, "%sY[%d]", sep, op_m_val(pcx));
    return ",";
  case oLl:                             // output local variable offset (0..255)
    outMsg(out, "%sY[%d]", sep, op_l_val(pcx));
    return ",";
  case oLc:                             // output local variable offset  (0..65535)
    outMsg(out, "%sY[%d]", sep, op_o_val(pcx));
    return ",";
  case iSt:                             // input at current structure pointer
    if (mode == readMode)
      outMsg(out, "%sS++=%,3w", sep, S);
    else
      outMsg(out, "%sS++", sep);
    return ",";
  case oSt:                             // output to current structure pointer
    outMsg(out, "%sS++", sep);
    return ",";
  case oAr:                             // Output arity in upper slot
  case uAr:                             // Arity in upper slot
    outMsg(out, "%s#%d", sep, op_sh_val(pcx));
    return ",";
  case uLt:                             // small literal in upper slot (-128..127)
    outMsg(out, "%s%d", sep, op_sh_val(pcx));
    return ",";
  case Ltl:                             // 16bit literal (-32768..32767)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
    outMsg(out, "%s%d", sep, op_o_val(pcx));
    return ",";
  case Es:                              // escape code (0..65535)
    outMsg(out, "%s%s", sep, escapeName(op_o_val(pcx)));
    return ",";
  case pcr:                             // program counter relative offset (-32768..32767)
    outMsg(out, "%spc%+d", sep, op_so_val(pcx));
    return ",";
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    outMsg(out, "%spc%+d", sep, op_ll_val(pcx));
    return ",";
  case ltl:                             // literal number (0..65535)
    outMsg(out, "%s%,3w", sep, &Lits[op_o_val(pcx)]);
    return ",";
  default:
    syserr("Problem in generating showing type");
    return NULL;
  }
}

static insPo outIns(ioPo out, char *Nm, opAndSpec A1, opAndSpec A2, codePo code, insPo pc, insWord pcx,
    ptrPo a, ptrPo y, ptrPo S, rwmode mode, choicePo B, ptrPo hBase, ptrPo hLimit) {
  char *sep = " ";
  ptrPo Lits = codeLits(code);

  outMsg(out, "[%d]: %s", pc - code->data, Nm);

  sep = showOpAnd(out, sep, A1, pcx, mode, a, y, S, Lits, hBase, hLimit);
  sep = showOpAnd(out, sep, A2, pcx, mode, a, y, S, Lits, hBase, hLimit);

  return pc + 1;
}

