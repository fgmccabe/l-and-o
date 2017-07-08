/*
  Code verifier module -- check that a code segment satisfies basic 
  sanity constraints.
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
#include <stdio.h>
#include "disass.h"
#include "esc.h"

typedef struct {
  logical inited;    //  True if cell has real value
  logical read;      //  Has this cell been read?
} Var, *varPo;

typedef struct _segment_ *segPo;

typedef struct _segment_ {
  Var args[LO_REGS];
  long lCount;                      //  number of locals in use
  varPo locals;                     //  entry state for this segment
  uint16 arity;                     //  Arity of the code
  short strcount;                   //  number of valid structure references
  long litCount;                    //  number of literals
  ptrPo Lits;                       //  Array of literals
  unsigned int lclHp;               //  how much local heap can we allocate?
  unsigned int gblHp;               //  how much global heap can we allocate?
  logical inited;                   //  Has the segment's variables been initialized?
  logical checked;                  //  Has this segment been checked?
  logical allocating;               //  Are we allocating variables?
  codePo cde;                       //  Pointer to the code structure itself
  long pc;                          //  base intruction of this segment
  long insCount;                    //  No. of instructions in this segment
  segPo prev;                       //  previous physical segment
  segPo next;                       //  next physical segment
  segPo alt;
  unsigned long entryPoints;        //  how many entry points are there here?
} segRecord;

typedef struct {
  segPo *table;        //  vector of ptrs into the list of segments
  unsigned int top;      //  current top of the vector
  unsigned int size;      //  current size of the vector
} *segVectPo;

#ifdef VERIFYTRACE
extern logical traceVerify;
static void showSegs(segPo root);
#endif

static poolPo segpool = NULL;
static int noOfSegments(segPo root);
static void showSeg(segPo seg);

#undef lastInstruction
#define lastInstruction

static segPo initVerify(codePo cde, unsigned long length) {
  if (segpool == NULL)
    segpool = newPool(sizeof(segRecord), 16);

  {
    segPo seg = (segPo) allocPool(segpool);
    unsigned int i;

    for (i = 0; i < NumberOf(seg->args); i++) {
      seg->args[i].inited = False;
      seg->args[i].read = False;
    }

    seg->cde = cde;
    seg->pc = 0;
    seg->arity = codeArity(cde);
    seg->insCount = length;
    seg->next = seg->prev = NULL;
    seg->alt = NULL;
    seg->entryPoints = 1;
    seg->checked = False;
    seg->inited = False;
    seg->allocating = False;
    seg->locals = NULL;
    seg->lCount = 0;
    seg->strcount = 0;
    seg->litCount = codeLitCount(cde);
    seg->Lits = codeLits(cde);
    return seg;
  }
}

static void clearSegs(segPo seg) {
  while (seg != NULL) {
    segPo next = seg->next;

    if (seg->locals != NULL)
      free(seg->locals);

    freePool(segpool, seg);

    seg = next;
  }
}

static segPo findSeg(segPo seg, long pc) {
  if (pc < seg->pc) {
    while (seg != NULL && pc < seg->pc)
      seg = seg->prev;
  } else {
    while (seg != NULL && pc >= seg->pc + seg->insCount)
      seg = seg->next;
  }

  if (seg == NULL || pc < seg->pc || pc >= seg->pc + seg->insCount)
    return NULL;
  return seg;
}

static segPo splitSeg(segPo root, long pc, long tgt, logical isAlt) {
  segPo base = findSeg(root, pc);
  segPo seg = findSeg(root, tgt);

  if (tgt == seg->pc) {
    seg->entryPoints++;
    if (isAlt) {
      assert(base->alt == NULL);
      base->alt = seg;
    }

    return seg;                  // Nothing to do here
  } else {
    segPo new = (segPo) allocPool(segpool);
    segPo next = seg->next;
    unsigned int i;

    *new = *seg;                  // Copy everything across

    seg->insCount = tgt - seg->pc;
    if (isAlt) {
      assert(base->alt == NULL);
      base->alt = new;
    }
    for (i = 0; i < NumberOf(seg->args); i++) {
      new->args[i].inited = True;
      new->args[i].read = False;
    }
    new->insCount -= seg->insCount;
    new->entryPoints = 1;
    new->checked = False;
    new->inited = False;
    new->allocating = seg->allocating;
    new->locals = NULL;
    new->lCount = 0;
    new->strcount = 0;
    new->litCount = root->litCount;
    new->Lits = root->Lits;
    new->alt = NULL;
    new->pc = tgt;
    new->prev = seg;
    new->next = next;
    new->cde = seg->cde;
    seg->next = new;
    if (next != NULL)
      next->prev = new;

    return new;
  }
}

#ifdef VERIFYTRACE

static void showSegs(segPo root) {
  int segNo = 0;
  segPo seg = root;

  outMsg(logFile, "%d segments\n", noOfSegments(root));

  while (seg != NULL) {
    if (seg->alt != NULL) {
      int altSegNo = 0;
      segPo s = root;

      while (s != NULL && s->pc < seg->alt->pc) {
        s = s->next;
        altSegNo++;
      }
      assert(s == seg->alt);

      outMsg(logFile, "segment: %d (%d) [%d-%d](%d), alt=%d [%d]\n",
             segNo, seg->entryPoints, seg->pc, seg->pc + seg->insCount, seg->insCount, altSegNo, s->pc);
    } else
      outMsg(logFile, "segment: %d (%d) [%d-%d](%d)\n", segNo, seg->entryPoints, seg->pc, seg->pc + seg->insCount,
             seg->insCount);
    segNo++;

    assert(seg->next == NULL || seg->pc + seg->insCount == seg->next->pc);

    seg = seg->next;
  }
  flushFile(logFile);
}

void showSeg(segPo seg) {
  unsigned int i;
  unsigned int max = NumberOf(seg->args) - 1;

  outMsg(logFile, "segment (%d) [%d-%d] ", seg->entryPoints, seg->pc, seg->pc + seg->insCount);

  while (max > 0 && !seg->args[max].inited)
    max--;

  for (i = 0; i <= max; i++)
    outMsg(logFile, " A[%d]%s", i, seg->args[i].inited ? "*" : "");
  if (seg->locals != NULL)
    for (i = 1; i <= seg->lCount; i++)
      outMsg(logFile, " Y[%d]%s", i, seg->locals[i].inited ? "*" : "");
  outMsg(logFile, "\n");
  flushFile(logFile);
}

#endif

static retCode testBreak(segPo seg, long pc, insWord pcx, opAndSpec A, char *errorMsg, long msgLen) {
  switch (A) {
    case nOp:                             // No operand
    case iAh:                             // input argument register in upper slot (0..255)
    case oAh:                             // output argument register in upper slot (0..255)
    case iAm:                             // input argument register in middle slot (0..255)
    case iAl:                             // input argument register in lower slot (0..255)
    case oAl:                             // input argument register in lower slot (0..255)
    case oAm:                             // output argument register in middle slot (0..255)
    case iLh:                             // input local variable offset (0..255)
    case iLm:                             // input local variable offset (0..255)
    case iLl:                             // input local variable offset (0..255)
    case iLc:                             // input local variable offset (0..65535)
    case oLh:                             // output local variable offset (0..255)
    case oLm:                             // output local variable offset (0..255)
    case oLl:                             // output local variable offset (0..255)
    case oLc:                             // output local variable offset  (0..65535)
    case iSt:                             // input at current structure pointer
    case oSt:                             // output to current structure pointer
    case uAr:                             // Arity in upper slot
    case oAr:                             // Arity in upper slot
    case uLt:                             // small literal in upper slot (-128..127)
    case Ltl:                              // 16bit literal (-32768..32767)
    case vSz:                             // Size of local variable vector
    case lSz:                             // Size of local variable vector
    case Es:                              // escape code (0..65535)
    case ltl:                             // literal offset (0..65535)
      return Ok;
    case pcr: {                            // program counter relative offset (-32768..32767)
      segPo alt = splitSeg(seg, pc - 1, pc + op_o_val(pcx), True);
      if (alt == NULL || splitSeg(seg, pc - 1, pc, False) == NULL) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid destination address %d @ %d" RED_ESC_OFF, op_o_val(pcx), pc);
        return Error;
      } else
        return Ok;
    }
    case pcl: {                            // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
      segPo alt = splitSeg(seg, pc - 1, pc + op_ll_val(pcx), True);
      if (alt == NULL || splitSeg(seg, pc - 1, pc, False) == NULL) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid destination address %d @ %d" RED_ESC_OFF, op_o_val(pcx), pc);
        return Error;
      } else
        return Ok;
    }
    default: {
      strMsg(errorMsg, msgLen, RED_ESC_ON "invalid address %d" RED_ESC_OFF, pc);
      return Error;
    }
  }
}

#undef instruction
#define instruction(Mn, O, A1, A2, Cmt)\
  case Mn:\
    ret = checkInstruction(seg,oPc,pc,pcx,A1,A2,errorMsg,msgLen);\
    continue;

static retCode
checkInstruction(segPo seg, long opc, long pc, insWord pcx, opAndSpec A1, opAndSpec A2, char *errorMsg, long msgLen);

static retCode checkSegment(segPo seg, char * name, char *errorMsg, long msgLen) {
  long pc = seg->pc;
  long limit = pc + seg->insCount;
  retCode ret = Ok;

#ifdef VERIFYTRACE
  if (traceVerify) {
    outMsg(logFile, "On entry: ");
    showSeg(seg);
  }
#endif

  seg->checked = True;
  insPo basePc = codeIns(seg->cde);

  while (ret == Ok && pc < limit) {
    long oPc = pc;
    insWord pcx = basePc[pc++];

    switch (op_code(pcx)) {
#include "instructions.h"

      default:
        strMsg(errorMsg, msgLen, RED_ESC_ON "illegal instruction at %d" RED_ESC_OFF, pc);
        return Error;
    }
  }
#ifdef VERIFYTRACE
  if (traceVerify) {
    outMsg(logFile, "On exit:  ");
    showSeg(seg);
  }
#endif

  return Ok;
}

int noOfSegments(segPo root) {
  int i;

  for (i = 0; root != NULL; i++, root = root->next);
  return i;
}

static retCode mergeSegVars(segPo seg, segPo next, char *errorMsg, long msgLen) {
  unsigned int i;

  if (next->locals == NULL && seg->locals != NULL) {
    next->lCount = seg->lCount;
    next->locals = (varPo) malloc(sizeof(Var) * (next->lCount + 1));

    for (i = 0; i <= next->lCount; i++)
      next->locals[i] = seg->locals[i];
  } else if (next->lCount > seg->lCount) {
    strMsg(errorMsg, msgLen, RED_ESC_ON "improper reallocation of locals" RED_ESC_OFF);
    return Error;
  } else if (seg->locals != NULL) {
    for (i = 0; i <= next->lCount; i++) {
      next->locals[i].inited &= seg->locals[i].inited;
      next->locals[i].read |= seg->locals[i].read;
    }
  }
  for (i = 0; i < NumberOf(seg->args); i++) {
    next->args[i].inited &= seg->args[i].inited;
    next->args[i].read |= seg->args[i].read;
  }

  return Ok;
}

static retCode checkSegments(segPo root, char * name, char *errorMsg, long msgLen) {
  int count = noOfSegments(root);
  segPo stack[count];
  int top = 0;

  stack[top++] = root;
  retCode ret = Ok;

  while (ret == Ok && top > 0) {
    segPo seg = stack[--top];
    ret = checkSegment(seg, name, errorMsg, msgLen);

    if (ret != Ok)
      return ret;

    if (seg->next != NULL) {
      segPo next = seg->next;

      if (!next->checked) {
        if ((ret = mergeSegVars(seg, next, errorMsg, msgLen)) != Ok)
          return ret;

        if (--next->entryPoints == 0)
          stack[top++] = next;
      }
    }
    if (seg->alt != NULL) {
      segPo alt = seg->alt;

      if (!alt->checked) {
        if ((ret = mergeSegVars(seg, alt, errorMsg, msgLen)) != Ok)
          return ret;

        if (--alt->entryPoints == 0)
          stack[top++] = alt;
      }
    }
  }

  for (segPo seg = root; seg != NULL; seg = seg->next)
    if (!seg->checked) {
      strMsg(errorMsg, msgLen, RED_ESC_ON "unreachable segment %s @ %x" RED_ESC_OFF, name, seg->pc);
      return Error;
    }

  return Ok;
}

static retCode checkInOperand(segPo seg, long opc, long pc, insWord pcx, opAndSpec A, char *errorMsg, long msgLen) {
  switch (A) {
    case nOp:                             // No operand
      return Ok;
    case iAh: {                            // input argument register in upper slot (0..255)
      int regNo = op_h_val(pcx);          // Pick up input register name

      if (regNo > LO_REGS || !seg->args[regNo].inited) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access unset argument register A[%d] @ %d" RED_ESC_OFF, regNo, opc);
        return Error;
      } else
        seg->args[regNo].read = True;
      return Ok;
    }
    case oAh:                             // output argument register in upper slot (0..255)
      return Ok;
    case iAm: {                            // input argument register in middle slot (0..255)
      int regNo = op_m_val(pcx);          // Pick up input register name

      if (regNo > LO_REGS || !seg->args[regNo].inited) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access unset argument register A[%d] @ %d" RED_ESC_OFF, regNo, opc);
        return Error;
      } else
        seg->args[regNo].read = True;
      return Ok;
    }
    case oAm:                             // output argument register in middle slot (0..255)
      return Ok;
    case iAl: {                            // input argument register in lower slot (0..255)
      int regNo = op_l_val(pcx);          // Pick up input register name

      if (regNo > LO_REGS || !seg->args[regNo].inited) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access unset argument register A[%d] @ %d" RED_ESC_OFF, regNo, pc);
        return Error;
      } else
        seg->args[regNo].read = True;
      return Ok;
    }
    case oAl:                             // output argument register in lower slot (0..255)
      return Ok;
    case iLh: {                            // variable in upper slot
      short lcNo = op_h_val(pcx);           // Pick up local variable

      if (seg->lCount < lcNo || !seg->locals[lcNo].inited) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access unset local Y[%d] @ %d" RED_ESC_OFF, lcNo, opc);
        return Error;
      } else
        seg->locals[lcNo].read = True;
      return Ok;
    }
    case iLm: {                            // variable in midle slot
      short lcNo = op_m_val(pcx);           // Pick up local variable

      if (seg->lCount < lcNo || !seg->locals[lcNo].inited) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access unset local Y[%d] @ %d" RED_ESC_OFF, lcNo, pc);
        return Error;
      } else
        seg->locals[lcNo].read = True;
      return Ok;
    }
    case iLl: {                            // variable in lower slot
      short lcNo = op_l_val(pcx);           // Pick up local variable

      if (seg->lCount < lcNo || !seg->locals[lcNo].inited) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access unset local Y[%d] @ %d" RED_ESC_OFF, lcNo, opc);
        return Error;
      } else
        seg->locals[lcNo].read = True;
      return Ok;
    }
    case iLc: {                            // variable in local variable (0..65535)
      short lcNo = op_o_val(pcx);           // Pick up local variable

      if (seg->lCount < lcNo || !seg->locals[lcNo].inited) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access unset local Y[%d] @ %d" RED_ESC_OFF, lcNo, opc);
        return Error;
      } else
        seg->locals[lcNo].read = True;
      return Ok;
    }
    case oLh:        //  variable in upper slot
    case oLm:        //  variable in middle slot
    case oLl:        //  variable in lower slot
    case oLc:                             // output local variable offset  (0..65535)
      return Ok;

    case iSt:                             // input at current structure pointer
    case oSt:                             // output to current structure pointer
      if (seg->strcount == 0) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "too many accesses to structure @ %d" RED_ESC_OFF, opc);
        return Error;
      }
      seg->strcount--;

      return Ok;
    case uAr: {                            // Arity in upper slot
      int regNo = op_h_val(pcx);          // Pick up register name
      int i;

      for (i = 1; i <= regNo; i++)
        if (!seg->args[i].inited) {
          strMsg(errorMsg, msgLen, RED_ESC_ON "uninitialized argument A[%d] @ %d" RED_ESC_OFF, regNo, opc);
          return Error;
        }

      return Ok;
    }
    case oAr:        //  Output arity in upper slot
    case uLt:                             // small literal in upper slot (-128..127)
    case Ltl:                              // 16bit literal (-32768..32767)
    case vSz:
      return Ok;

    case lSz: {                            // Size of local variable vector
      /*    unsigned int i;
      int count = op_so_val(pcx);


          if(count>0){
        if(seg->locals==NULL)
          return "locals not allocated yet";
        else{
          for(i=1;i<=count;i++){
            if(!seg->locals[i].inited){
              static char msg[256];

              sprintf(msg,"local Y[%d/%d] not initialized",i,count);
              return msg;
            }
          }
        }
      }
      */
      return Ok;
    }

    case Es: {                             // escape code (0..65535)
      uint16 esc = op_o_val(pcx);

      if (!validEscape(esc, op_h_val(pcx))) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid escape code [%s/%d] @ %d" RED_ESC_OFF, escapeName(esc), op_h_val(pcx), opc);
        return Error;
      }
      return Ok;
    }
    case pcr:                             // program counter relative offset (-32768..32767)
    case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
      return Ok;
    case ltl: {                            // literal number (0..65535)
      short lit = op_o_val(pcx);

      if (seg->litCount <= lit) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access invalid literal %d @ %d" RED_ESC_OFF, lit, opc);
        return Error;
      }

      return Ok;
    }
    default: {
      strMsg(errorMsg, msgLen, RED_ESC_ON "Problem in checking opcode type: 0x%x @ %d" RED_ESC_OFF, pcx, opc);
      return Error;
    }
  }
}

static retCode checkOutOperand(segPo seg, long opc, long pc, insWord pcx, opAndSpec A, char *errorMsg, long msgLen) {
  switch (A) {
    case nOp:                             // No operand
      return Ok;
    case iAh:                             // input argument register in upper slot (0..255)
      return Ok;
    case oAh: {                            // output argument register in upper slot (0..255)
      int regNo = op_h_val(pcx);          // Pick up input register name

      if (regNo > LO_REGS) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to set non-existent register: A[%d] @ %d" RED_ESC_OFF, regNo, opc);
        return Error;
      } else
        seg->args[regNo].inited = True;
      return Ok;
    }
    case iAm:                             // input argument register in middle slot (0..255)
      return Ok;
    case oAm: {                            // output argument register in middle slot (0..255)
      int regNo = op_m_val(pcx);          // Pick up input register name

      if (regNo > LO_REGS) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to set non-existent register: A[%d] @ %d" RED_ESC_OFF, regNo, opc);
        return Error;
      } else
        seg->args[regNo].inited = True;
      return Ok;
    }
    case iAl:                             // input argument register in lower slot (0..255)
      return Ok;
    case oAl: {                            // output argument register in lower slot (0..255)
      int regNo = op_l_val(pcx);          // Pick up input register name

      if (regNo > LO_REGS) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to set non-existent register: A[%d]@ %d" RED_ESC_OFF, regNo, opc);
        return Error;
      } else
        seg->args[regNo].inited = True;
      return Ok;
    }
    case iLh:                             // input local variable upper slot
    case iLm:                             // input local variable middle slot
    case iLl:                             // input local variable lower slot
    case iLc:                             // input local variable offset (0..65535)
      return Ok;
    case oLh: {                            // output local variable upper slot
      short lcNo = op_h_val(pcx);           // Pick up local variable

      if (seg->lCount < lcNo) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to set out of bounds variable: Y[%d] @ %d" RED_ESC_OFF, lcNo, opc);
        return Error;
      } else
        seg->locals[lcNo].inited = True;
      return Ok;
    }
    case oLm: {                            // output local variable middle slot
      short lcNo = op_m_val(pcx);           // Pick up local variable

      if (seg->lCount < lcNo) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to set out of bounds variable: Y[%d] @ %d" RED_ESC_OFF, lcNo, opc);
        return Error;
      } else
        seg->locals[lcNo].inited = True;
      return Ok;
    }
    case oLl: {                            // output local variable lower slot
      short lcNo = op_l_val(pcx);           // Pick up local variable

      if (seg->lCount < lcNo) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to set out of bounds variable: Y[%d] @ %d" RED_ESC_OFF, lcNo, opc);
        return Error;
      } else
        seg->locals[lcNo].inited = True;
      return Ok;
    }
    case oLc: {                            // output local variable offset  (0..65535)
      short lcNo = op_o_val(pcx);           // Pick up local variable

      if (seg->lCount < lcNo) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to set out of bounds variable: Y[%d] @ %d" RED_ESC_OFF, lcNo, opc);
        return Error;
      } else
        seg->locals[lcNo].inited = True;
      return Ok;
    }
    case iSt:                             // input at current structure pointer
    case oSt:                             // output to current structure pointer
      return Ok;
    case uAr: {                            // Arity in upper slot
      unsigned short regNo = op_h_val(pcx);          // Pick up register name
      int i;

      for (i = regNo + 1; i < LO_REGS; i++)
        seg->args[i].inited = False;
      return Ok;
    }
    case oAr: {                            // Arity in upper slot
      int regNo = op_h_val(pcx);          // Pick up register name
      int i;

      for (i = 1; i <= regNo; i++)
        seg->args[i].inited = True;

      for (i = regNo + 1; i < LO_REGS; i++)
        seg->args[i].inited = False;
      return Ok;
    }

    case uLt:                             // small literal in upper slot (-128..127)
    case Ltl:                              // 16bit literal (-32768..32767)
      return Ok;
    case vSz: {                            // Size of local variable vector
      unsigned int i;
      long count = seg->lCount = op_so_val(pcx);

      if (count >= 0) {
        if (seg->locals != NULL)
          seg->locals = (varPo) realloc(seg->locals, sizeof(Var) * (count + 1));
        else
          seg->locals = (varPo) malloc(sizeof(Var) * (count + 1));

        for (i = 0; i <= count; i++) {
          seg->locals[i].inited = False;
          seg->locals[i].read = False;
        }
      }

      return Ok;
    }
    case lSz:                            // Size of local variable vector
      return Ok;

    case Es:                              // escape code (0..65535)
    case pcr:                             // program counter relative offset (-32768..32767)
    case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    case ltl:                             // literal number (0..65535)
      return Ok;
    default: {
      strMsg(errorMsg, msgLen, RED_ESC_ON "Problem in checking opcode type: %x @ %d" RED_ESC_OFF, pcx, opc);
      return Error;
    }
      return Ok;
  }
}

static retCode
checkInstruction(segPo seg, long opc, long pc, insWord pcx, opAndSpec A1, opAndSpec A2, char *errorMsg, long msgLen) {
  opCode op = op_code(pcx);

  retCode ret = checkInOperand(seg, opc, pc, pcx, A1, errorMsg, msgLen);

  if (ret == Ok)
    ret = checkInOperand(seg, opc, pc, pcx, A2, errorMsg, msgLen);
  if (ret == Ok)
    ret = checkOutOperand(seg, opc, pc, pcx, A1, errorMsg, msgLen);
  if (ret == Ok)
    ret = checkOutOperand(seg, opc, pc, pcx, A2, errorMsg, msgLen);

  // We have to hack the special aspects for now
  if (ret == Ok) {
    switch (op) {
      case alloc:
        seg->allocating = True;
        break;
      case dealloc:
      case dlkawlO:
      case dlkawl:
        seg->lCount = 0;
        free(seg->locals);
        seg->locals = NULL;

        if (!seg->allocating) {
          strMsg(errorMsg, msgLen, RED_ESC_ON "dealloc without corresponding alloc @ %d" RED_ESC_OFF, opc);
          return Error;
        }
        break;
      case succ:
      case lkawl:
      case lkawlO:
        if (seg->allocating) {
          strMsg(errorMsg, msgLen, RED_ESC_ON "succ without corresponding dealloc @ %d" RED_ESC_OFF, opc);
          return Error;
        }
      case trycl:
      case tryme:
      case retry:
      case trust:
      case retryme:
      case trustme: {
        unsigned int i;
        for (i = 1; i <= seg->arity; i++) {
          seg->args[i].inited = True;
          seg->args[i].read = False;
        }
        break;
      }
      case uAcns:
      case uScns:
      case mAcns:
      case mScns:
      case cAcns:
      case cScns: {
        short litNo = op_o_val(pcx);

        if (seg->litCount <= litNo) {
          strMsg(errorMsg, msgLen, RED_ESC_ON "attempted to access invalid literal %d @ %d" RED_ESC_OFF, litNo, opc);
          return Error;
        } else {
          ptrI lit = seg->Lits[litNo];
          register clssPo class = (clssPo) objV(lit);
          seg->strcount = (short) classArity(class);
          break;
        }
      }
      case vdYY:
      case clYY: {
        unsigned int i;
        unsigned short low = op_o_val(pcx);
        unsigned short hi = low + op_h_val(pcx);

        for (i = low; i < hi; i++)
          seg->locals[i].inited = True;
        break;
      }
      case vdAA: {
        unsigned int i;
        unsigned short low = op_h_val(pcx);
        unsigned short hi = low + op_o_val(pcx);

        for (i = low; i < hi; i++)
          seg->args[i].inited = True;
        break;
      }
      default:;
    }
  }

#ifdef VERIFYTRACE
  if (traceVerify && ret != Ok) {
    outMsg(logFile, "Problem %s in instruction:\n", errorMsg);
    showInstructions(seg->cde, opc, 1);
    flushFile(logFile);
  }
#endif

  return ret;
}

static pthread_mutex_t verifyMutex = PTHREAD_MUTEX_INITIALIZER;

#undef instruction
#define instruction(Op, Cde, A1, A2, Cmt)\
    case Op:\
      ret=testBreak(segs,pc,pcx,A1,errorMsg,msgLen);\
      if(ret==Ok)\
        ret=testBreak(segs,pc,pcx,A2,errorMsg,msgLen);\
      continue;

retCode verifyCode(ptrI prog, char *name, char *errorMsg, long msgLen) {
  pthread_mutex_lock(&verifyMutex);  //  We synchronize all verification

  codePo cde = codeV(prog);
  int i;
  segPo segs = initVerify(cde, codeInsCount(cde));

  uint16 arity = segs->arity = codeArity(cde);

  for (i = 1; i <= arity; i++) {
    segs->args[i].inited = True;
    segs->args[i].read = False;
  }

  segs->litCount = codeLitCount(cde);

  long codeLength = codeInsCount(cde);
  insPo basePc = codeIns(cde);
  long pc = 0;
  retCode ret = Ok;

#ifdef VERIFYTRACE
  if (traceVerify) {
    showInstructions(cde, segs->pc, segs->insCount);
  }
#endif

  while (ret == Ok && pc < codeLength) {
    insWord pcx = basePc[pc++];
    switch (op_code(pcx)) {
#include "instructions.h"

      default:
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid instruction at %s:%d" RED_ESC_OFF, name, pc);
        return Error;
    }
  }

#ifdef VERIFYTRACE
  if (traceVerify)
    showSegs(segs);
#endif

  if (ret == Ok)
    ret = checkSegments(segs, name, errorMsg, msgLen);

  clearSegs(segs);

  pthread_mutex_unlock(&verifyMutex);  //  We can now allow others to verify

  return ret;
}

