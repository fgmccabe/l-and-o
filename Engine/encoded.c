/*
 Term encoding and decoding functions
 Copyright (c) 2016, 2017. Francis G. McCabe

 Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the
 License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied. See the License for the specific language governing
 permissions and limitations under the License.
 */

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <base64.h>

#include "lo.h"
#include "signature.h"
#include "term.h"
#include "encoded.h"             /* pick up the term encoding definitions */

/* Decode a term encoded message ... from a file stream */

typedef struct _encoding_support_ {
  ptrPo lbls;       /* The table of labels */
  long maxlbl;      /* How big is our table */
  ptrPo vars;       /* The table of variables */
  long maxvar;      /* How big is the variable table */
  string errorMsg;  /* Place to put error messages */
  long msgSize;     /* How big is that buffer */
  heapPo R;         /* Where should the roots go? */
} EncodeSupport, *encodePo;

/*
 * Decode a structure from an input stream
 */
static retCode decode(ioPo in, encodePo S, heapPo H, ptrPo tgt, bufferPo strBuffer);
static retCode decInt(ioPo in, encodePo S, integer *ii);
static retCode decFlt(ioPo in, encodePo S, double *dx);
static retCode decodeText(ioPo in, encodePo S, bufferPo buffer);
static retCode decodeName(ioPo in, encodePo S, bufferPo buffer);

static retCode codedEstimate(ioPo in, encodePo S, integer *amnt, integer *perm);

retCode decodeTerm(ioPo in, heapPo H, heapPo R, ptrPo tgt, string errorMsg,
                   long msgSize) {
  EncodeSupport support = {NULL, 0, NULL, 0, errorMsg, msgSize, R};

  logical isBlocking = (objectHasClass(O_OBJECT(in), fileClass) ? isFileBlocking(O_FILE(in)) : False);
  logical isAsynch = (objectHasClass(O_OBJECT(in), fileClass) ? isFileAsynch(O_FILE(in)) : False);

  if (!isBlocking)
    configureIo(O_FILE(in), turnOnBlocking);
  if (isAsynch)
    configureIo(O_FILE(in), disableAsynch);

  codePoint ch;

  again:
  switch (inChar(in, &ch)) {
    case Eof:
      return Eof;
    default:
      strMsg(errorMsg, msgSize, "stream prematurely ended");
      return Error;
    case Interrupt:
      goto again;
    case Ok: {
      if (ch != trmString) {
        strMsg(errorMsg, msgSize, "invalid lead-in in code sequence");
        goto error_exit;
      } else {
        codePoint delim;

        switch (inChar(in, &delim)) {
          case Ok: {
            strBufPo buffer = openStrOutput((string) "", utf8Encoding);
            while (True) {
              switch (inChar(in, &ch)) {
                case Ok:
                  if (ch == delim) {
                    rewindStr(buffer);

                    integer amnt, perm = 0;

                    retCode res = codedEstimate(O_IO(buffer), &support, &amnt, &perm);

                    //logMsg(logFile,"Estimate of space requirements: %d heap, %d permanent",perm);

                    if (perm > 0 || H == &globalHeap)
                      lockGlobal(); /* lock access to the global heap */
                    if (res == Ok) {
                      if (H != &globalHeap) {
                        res = reserveSpace(&globalHeap, (size_t) perm);
                        if (res == Ok)
                          res = reserveSpace(H, (size_t) amnt);
                      } else
                        res = reserveSpace(&globalHeap, (size_t) (perm + amnt));
                    }
                    if (res == Ok) {
                      rewindStr(buffer); /* re-read from string buffer */
                      bufferPo tmpBuffer = newStringBuffer();

                      res = decode(O_IO(buffer), &support, H, tgt, tmpBuffer);

                      closeFile(O_IO(buffer));
                      closeFile(tmpBuffer);

                      if (support.lbls != NULL)
                        free(support.lbls);
                      if (support.vars != NULL)
                        free(support.vars);
                    }
                    if (perm > 0 || H == &globalHeap)
                      unlockGlobal(); /* release access to the global heap */

                    if (!isBlocking)
                      configureIo(O_FILE(in), turnOffBlocking);
                    if (isAsynch)
                      configureIo(O_FILE(in), enableAsynch);
                    return res;
                  } else {
                    outChar(O_IO(buffer), ch);
                    continue;
                  }
                case Eof:
                  strMsg(errorMsg, msgSize, "unexpected eof");
                  goto error_exit;
                case Error:
                default:
                  strMsg(errorMsg, msgSize, "stream prematurely ended");
                  goto error_exit;
              }
            }
          }
          default:
            strMsg(errorMsg, msgSize, "stream prematurely ended");
            goto error_exit;
        }
      }
    }
  }
  error_exit:
  if (!isBlocking)
    configureIo(O_FILE(in), turnOffBlocking);
  if (isAsynch)
    configureIo(O_FILE(in), enableAsynch);
  return Error;
}

static logical isDigit(codePoint ch) {
  return (logical) (ch >= '0' && ch <= '9');
}

static int digitVal(codePoint ch) {
  return (int) (ch - '0');
}

retCode decInt(ioPo in, encodePo S, integer *ii) {
  codePoint ch;
  integer result = 0;

  again:
  switch (inChar(in, &ch)) {
    case Ok:
      if (ch == '-') {
        retCode ret = decInt(in, S, ii);
        *ii = -(*ii);
        return ret;
      } else if (isDigit(ch)) {
        result = digitVal(ch); // First digit of number

        while (True) {
          switch (inChar(in, &ch)) {
            case Ok:
              if (isDigit(ch)) {
                result = result * 10 + digitVal(ch);
                continue;
              } else {
                *ii = result;
                return Ok;
              }
            case Eof: {
              *ii = result;
              return Ok;
            }
            default:
              strMsg(S->errorMsg, S->msgSize, "stream prematurely ended");
              return Error;
          }
        }
      } else {
        strMsg(S->errorMsg, S->msgSize, "stream prematurely ended");
        return Error;
      }
    case Eof:
    default:
      strMsg(S->errorMsg, S->msgSize, "stream prematurely ended");
      return Error;
  }
}

static inline long mx(long a, long b) {
  if (a > b)
    return a;
  else
    return b;
}

/*
 Warning: caller assumes responsibility for ensuring that tgt is a valid root
 */
retCode decode(ioPo in, encodePo S, heapPo H, ptrPo tgt, bufferPo tmpBuffer) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch (ch) {
    case trmVar: { /* an unbound variable */
      integer vno;

      if ((res = decInt(in, S, &vno)) != Ok)
        return res;

      if (vno >= 0) {
        if (vno >= S->maxvar) {
          if (S->vars == NULL) { /* First variable in table */
            S->maxvar = mx(128, vno);
            S->vars = (ptrPo) malloc(sizeof(ptrI) * S->maxvar);
            memset(S->vars, 0, sizeof(ptrI) * S->maxvar); /* clear variables */
          } else {
            long newmax = mx(S->maxvar + (S->maxvar >> 1), vno); /* 50% growth */
            ptrPo newvars = (ptrPo) realloc(S->vars, sizeof(ptrI) * newmax);

            if (newvars == NULL) {
              syserr("no space");
              return Error;
            } else {
              for (long i = S->maxvar; i < newmax; i++)
                newvars[i] = 0; /* clear the new entry */

              S->vars = newvars;
              S->maxvar = newmax;
            }
          }
        }

        if (S->vars[vno] == 0) {
          if (inHeap(H, (objPo) tgt))
            S->vars[vno] = unBind(tgt); /* initialize the variable */
          else
            S->vars[vno] = *tgt = allocateVar(H);
        } else
          *tgt = S->vars[vno]; /* copy in the variable binding */
      } else
        unBind(tgt); /* A void variable? */

      return Ok;
    }
    case trmInt: {
      integer i;
      if ((res = decInt(in, S, &i)) != Ok)
        return res;
      *tgt = allocateInteger(H, i);
      return Ok;
    }
    case trmFlt: {
      double dx;
      if ((res = decFlt(in, S, &dx)) != Ok)
        return res;
      *tgt = allocateFloat(H, dx);
      return Ok;
    }

    case trmSym: {
      clearBuffer(tmpBuffer);
      if ((res = decodeName(in, S, O_IO(tmpBuffer))) == Ok) {
        long len;
        outByte(O_IO(tmpBuffer), 0); // terminate the name
        *tgt = newUniSymbol(getTextFromBuffer(&len, tmpBuffer));
      }
      return res;
    }

    case trmString: { /* A literal string */

      if ((res = decodeText(in, S, O_IO(tmpBuffer))) == Ok) {
        uint64 len;
        string buff = getTextFromBuffer(&len, tmpBuffer);
        *tgt = allocateString(H, buff, (long) len);
      }
      return res;
    }

    case trmStrct: { /* We have a class definition structure */
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decodeName(in, S, O_IO(tmpBuffer))) == Ok) {
        uint64 len;
        outByte(O_IO(tmpBuffer), 0); // terminate the name
        *tgt = newClassDef(getTextFromBuffer(&len, tmpBuffer), arity);
      }
      return res;
    }

    case trmPrg: { /* We have a program label */
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decodeName(in, S, tmpBuffer)) == Ok) {
        uint64 len;
        outByte(O_IO(tmpBuffer), 0); // terminate the name
        *tgt = newProgramLbl(getTextFromBuffer(&len, tmpBuffer), arity);
      }
      return res;
    }

    case trmCns: {
      ptrI class = kvoid;
      rootPo root = gcAddRoot(S->R, &class);
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decode(in, S, H, &class, tmpBuffer)) != Ok)
        return res;

      gcAddRoot(S->R, &class); /* Keep the pointer to the class */

      if (res == Ok) {
        objPo obj = allocate(H, (size_t) (arity + 1));

        obj->class = class;

        *tgt = objP(obj);

        ptrI el = kvoid;
        integer i;

        gcAddRoot(S->R, &el);

        for (i = 0; i < arity; i++) {
          if ((res = decode(in, S, H, &el, tmpBuffer)) != Ok) /* read each element of term */
            break; /* we might need to skip out early */
          else {
            ptrPo arg = nthArg(obj, i);

            *arg = el; /* stuff in the new element */
          }
        }
      }

      gcRemoveRoot(S->R, root); /* clear the GC root */
      return res;
    }

    default: {
      strMsg(S->errorMsg, S->msgSize, "invalid encoding");
      return Error;
    }
  }
}

static retCode decodeName(ioPo in, encodePo S, bufferPo buffer) {
  codePoint delim;
  clearBuffer(buffer);

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while ((ret = inChar(in, &ch)) == Ok && ch != delim) {
      outChar(O_IO(buffer), ch);
    }
    return ret;
  }
}

static retCode decodeText(ioPo in, encodePo S, bufferPo buffer) {
  codePoint delim;
  clearBuffer(buffer);

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while ((ret = inChar(in, &ch)) == Ok && ch != delim) {
      if (ch == '\\')
        ret = inChar(in, &ch);
      outChar(O_IO(buffer), ch);
    }
    return ret;
  }
}




/* swap bytes in the little endian game */
static inline void SwapBytes(unsigned long *x) {
  *x = ((*x & 0xff) << 8) | ((*x >> 8) & 0xff) | ((*x & 0x00ff0000L) << 8)
       | ((*x & 0xff000000L) >> 8);
}

static inline void SwapWords(unsigned long *x) {
  *x = (*x & 0x0000ffffL) << 16 | (*x & 0xffff0000L) >> 16;
}

// Decode a code segment.
// Each segment consists of
// a. The program structure object being defined
// b. A string containing the code as a base64 encoded string.
// c. A tuple of literals associated with the code segment
// All wrapped up as a #code structure.

retCode decodeCodeSegment(ioPo in,encodePo S,heapPo H,ptrPo tgt) {
codePoint ch;

  retCode ret = inChar(in,&ch);

  if(ch==trmCns){
    ptrI prg;
    rootPo root = gcAddRoot(S->R, &prg);
    bufferPo tmp = newStringBuffer();

    ret = decode(in,S,H,&prg,tmp);




  } else
    return Error;




  bufferPo in = fixedStringBuffer(buffer, buffLeng);
  retCode ret = decode64(in, tmp);
  rewindBuffer(tmp);

  integer codeCount = (bufferSize(tmp) / SIZEOF_INT) - 1;
  int32 signature = (inB(O_IO(tmp)) & 0xff) << 24 | (inB(O_IO(tmp)) & 0xff) << 16 | (inB(O_IO(tmp)) & 0xff) << 8 |
              (inB(O_IO(tmp)) & 0xff); // verify correct code signature

  heapPo H = &globalHeap;
  ptrI pc = *tgt = permCode(codeCount, litCount);

  long i;
  insPo cd = FirstInstruction(pc);
  ptrI el = kvoid;
  rootPo root = gcAddRoot(S->R, &pc); /* in case of GC ... */

  gcAddRoot(S->R, &el); /* we need a temporary pointer */

  /* get the instructions */
  for (i = 0; i < codeCount; i++)
    cd[i] = (inB(O_IO(tmp))&0xff) << 24 | (inB(O_IO(tmp))&0xff) << 16
            |(inB(O_IO(tmp))&0xff) << 8 | (inB(O_IO(tmp))&0xff);

  /* Now convert the main code to handle little endians etc */
  if (signature == SIGNATURE) {
  } /* endian load same as endian save */
  else if (signature == SIGNBSWAP) { /* swap bytes keep words */
    unsigned long *xx = (unsigned long *) FirstInstruction(pc);
    long cnt = codeCount;
    for (; cnt--; xx++)
      SwapBytes(xx);
  } else if (signature == SIGNWSWAP) { /* swap words keep bytes */
    unsigned long *xx = (unsigned long *) FirstInstruction(pc);
    long cnt = codeCount;
    for (; cnt--; xx++)
      SwapWords(xx);
  } else if (signature == SIGNBWSWP) { /* swap words and bytes */
    unsigned long *xx = (unsigned long *) FirstInstruction(pc);
    long cnt = codeCount;
    for (; cnt--; xx++) {
      SwapWords(xx);
      SwapBytes(xx);
    }
  }

  codeV(pc)->arity = (unsigned short)arity; /* set the arity of the program */

  // Pick up the literal table
  for (i = 0; i < litCount; i++) {
    res = decode(in, S, H, &el);
    updateCodeLit(codeV(pc), i, el);
  }

  gcRemoveRoot(S->R, root); /* we're all done!! */
  if (res == Ok && enableVerify) /* Except we have to verify ... */
    res = verifyCode(pc);
  return res;

}

static retCode decodeCode(ioPo in, encodePo S, unsigned long signature,
                          ptrPo tgt) {
  integer size;
  retCode res;
  integer arity;
  heapPo H = &globalHeap;
  integer litCount;

  if ((res = decInt(in, S, &size, inB(in))) != Ok)
    return res;

  if ((res = decInt(in, S, &arity, inB(in))) != Ok)
    return res;

  if ((res = decInt(in, S, &litCount, inB(in))) != Ok)
    return res;

  ptrI pc = *tgt = permCode(size, litCount);
  long i;
  insPo cd = FirstInstruction(pc);
  ptrI el = kvoid;
  rootPo root = gcAddRoot(S->R, &pc); /* in case of GC ... */

  gcAddRoot(S->R, &el); /* we need a temporary pointer */

  /* get the instructions */
  for (i = 0; i < size; i++)
    cd[i] = (inB(in) & 0xff) << 24 | (inB(in) & 0xff) << 16
            | (inB(in) & 0xff) << 8 | (inB(in) & 0xff);

  /* Now convert the main code to handle little endians etc */
  if (signature == SIGNATURE) {
  } /* endian load same as endian save */
  else if (signature == SIGNBSWAP) { /* swap bytes keep words */
    unsigned long *xx = (unsigned long *) FirstInstruction(pc);
    long cnt = size;
    for (; cnt--; xx++)
      SwapBytes(xx);
  } else if (signature == SIGNWSWAP) { /* swap words keep bytes */
    unsigned long *xx = (unsigned long *) FirstInstruction(pc);
    long cnt = size;
    for (; cnt--; xx++)
      SwapWords(xx);
  } else if (signature == SIGNBWSWP) { /* swap words and bytes */
    unsigned long *xx = (unsigned long *) FirstInstruction(pc);
    long cnt = size;
    for (; cnt--; xx++) {
      SwapWords(xx);
      SwapBytes(xx);
    }
  }

  codeV(pc)->arity = arity; /* set the arity of the program */

  // Pick up the literal table
  for (i = 0; i < litCount; i++) {
    res = decode(in, S, H, &el);
    updateCodeLit(codeV(pc), i, el);
  }

  gcRemoveRoot(S->R, root); /* we're all done!! */
  if (res == Ok && enableVerify) /* Except we have to verify ... */
    res = verifyCode(pc);
  return res;
}

/*
 Estimate amount of heap space needed
 */
static retCode estimateCode(ioPo in, encodePo S, integer *amnt, integer *perm);
static retCode estimate(ioPo in, encodePo S, integer *amnt, integer *perm);

static retCode codedEstimate(ioPo in, encodePo S, integer *amnt, integer *perm) {
  *amnt = 0;
  *perm = 0;

  return estimate(in, S, amnt, perm);
}

static retCode estimateName(ioPo in, long *len);
static retCode estimateText(ioPo in, long *len);

static retCode estimate(ioPo in, encodePo S, integer *amnt, integer *perm) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch (ch) {
    case trmAnon:
      (*amnt) += VariableCellCount;

      return Ok;
    case trmVar: { /* an unbound variable */
      integer vno;

      if ((res = decInt(in, S, &vno)) != Ok)
        return res;

      (*amnt) += VariableCellCount;

      return Ok;
    }
    case trmInt: {
      integer i;
      if ((res = decInt(in, S, &i)) != Ok)
        return res;
      (*amnt) += CellCount(sizeof(integerRec));
      return Ok;
    }
    case trmFlt: {
      double dx;
      if ((res = decFlt(in, S, &dx)) != Ok)
        return res;
      (*amnt) += CellCount(sizeof(floatRec));
      return Ok;
    }

    case trmSym: {
      long length;
      if ((res = estimateName(in, &length)) == Ok) {
        (*perm) += CellCount(sizeof(symbolRec) + (length) * sizeof(byte));
      }
      return res;
    }

    case trmString: { /* A literal string */
      long length;

      if ((res = estimateText(in, &length)) == Ok) {
        (*perm) += CellCount(sizeof(stringRec) + (length + 1) * sizeof(byte));
      }
      return res;
    }

    case trmStrct: { /* We have a class definition structure */
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      long length;
      if ((res = estimateName(in, &length)) == Ok) {
        (*perm) += CellCount(sizeof(clssRec) + (length) * sizeof(byte));
      }

      return res;
    }

    case trmPrg: { /* We have a program label */
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      long length;
      if ((res = estimateName(in, &length)) == Ok) {
        (*perm) += CellCount(sizeof(programRec) + (length) * sizeof(byte));
      }

      return res;
    }

    case trmCns: {
      ptrI class = kvoid;
      rootPo root = gcAddRoot(S->R, &class);
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      res = estimate(in, S, amnt, perm); /* pick up the class constructor */

      if (res == Ok) {
        integer i;

        for (i = 0; res == Ok && i < arity; i++)
          res = estimate(in, S, amnt, perm); /* read each constructor element */
      }

      (*amnt) += arity + 1;
      return res;
    }

    default: {
      strMsg(S->errorMsg, S->msgSize, "invalid encoding");
      return Error;
    }
  }
}

static retCode estimateName(ioPo in, long *length) {
  codePoint delim;
  long len = 0;

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while ((ret = inChar(in, &ch)) == Ok && ch != delim) {
      len += codePointSize(ch);
    }
    *length = len;
    return ret;
  }
}

static retCode estimateText(ioPo in, long *length) {
  codePoint delim;
  long len = 0;

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while ((ret = inChar(in, &ch)) == Ok && ch != delim) {
      len += codePointSize(ch);
      if (ch == '\\')
        ret = inChar(in, &ch);
      len += codePointSize(ch);
    }
    *length = len;
    return ret;
  }
}

static retCode display(ioPo in, encodePo S, ioPo out);
static retCode displayName(ioPo in, ioPo out);
static retCode displayText(ioPo in, ioPo out);

retCode displayEncoded(ioPo out, byte *buffer, long len) {
  ioPo str = O_IO(openByteBuffer(buffer, len));
  byte errorMsg[1024];
  long msgSize = NumberOf(errorMsg);
  EncodeSupport support = {NULL, 0, NULL, 0, errorMsg, msgSize};
  retCode ret = display(str, &support, out);

  flushFile(out);

  closeFile(str);
  return ret;
}

retCode sE(byte *buffer, long len) {
  return displayEncoded(logFile, buffer, len);
}

static retCode display(ioPo in, encodePo S, ioPo out) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch (ch) {
    case trmAnon:
      return outMsg(out, "_");
    case trmVar: { /* an unbound variable */
      integer vno;

      if ((res = decInt(in, S, &vno)) != Ok)
        return res;

      return outMsg(out, "_%ld", vno);
    }
    case trmInt: {
      integer i;
      if ((res = decInt(in, S, &i)) != Ok)
        return res;
      return outMsg(out, "%ld", i);
    }
    case trmFlt: {
      double dx;
      if ((res = decFlt(in, S, &dx)) != Ok)
        return res;
      return outMsg(out, "%f", dx);
    }

    case trmSym: {
      return displayName(in, out);
    }

    case trmString: { /* A literal string */
      res = outChar(out, '\"');
      if (res == Ok)
        res = displayText(in, out);
      if (res == Ok)
        res = outChar(out, '\"');
      return res;
    }

    case trmStrct: { /* We have a class definition structure */
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if (res == Ok)
        res = displayName(in, out);

      if (res == Ok)
        res = outChar(out, '/');

      if (res == Ok)
        res = outMsg(out, "%ld", arity);
      return res;
    }

    case trmPrg: { /* We have a program label */
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if (res == Ok)
        res = displayName(in, out);

      if (res == Ok)
        res = outChar(out, '%');

      if (res == Ok)
        res = outMsg(out, "%ld", arity);
      return res;
    }

    case trmCns: {
      ptrI class = kvoid;
      rootPo root = gcAddRoot(S->R, &class);
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if (res == Ok)
        res = display(in, S, out);

      if (res == Ok)
        res = outStr(out, "(");

      if (res == Ok) {
        integer i;
        char *sep = "";

        for (i = 0; res == Ok && i < arity; i++) {
          res = outStr(out, sep);
          sep = ", ";
          if (res == Ok)
            res = display(in, S, out);
        }
        if (res == Ok)
          res = outStr(out, ")");
      }
      return res;
    }

    default: {
      strMsg(S->errorMsg, S->msgSize, "invalid encoding");
      return Error;
    }
  }
}

static retCode displayName(ioPo in, ioPo out) {
  codePoint delim;

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while (ret == Ok && (ret = inChar(in, &ch)) == Ok && ch != delim) {
      ret = outChar(out, ch);
    }
    return ret;
  }
}

static retCode displayText(ioPo in, ioPo out) {
  codePoint delim;

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while (ret == Ok && (ret = inChar(in, &ch)) == Ok && ch != delim) {
      ret = outChar(out, ch);

      if (ch == '\\') {
        ret = inChar(in, &ch);
        if (ret == Ok)
          ret = outChar(out, ch);
      }
    }
    return ret;
  }
}

static retCode skipName(ioPo in, encodePo S) {
  codePoint delim;

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while ((ret = inChar(in, &ch)) == Ok && ch != delim);
    return ret;
  }
}

static retCode skipText(ioPo in, encodePo S) {
  codePoint delim;

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while (ret == Ok && (ret = inChar(in, &ch)) == Ok && ch != delim) {
      if (ch == '\\')
        ret = inChar(in, &ch);
    }
    return ret;
  }
}

static retCode skipTrm(ioPo in, encodePo S) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch (ch) {
    case trmAnon:
      return Ok;
    case trmVar: { /* an unbound variable */
      integer vno;

      return decInt(in, S, &vno);
    }
    case trmInt: {
      integer i;
      return decInt(in, S, &i);
    }
    case trmFlt: {
      double dx;
      return decFlt(in, S, &dx);
    }

    case trmSym: {
      return skipName(in, S);
    }

    case trmString:
      return skipText(in, S);

    case trmStrct: { /* We have a class definition structure */
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      return skipName(in, S);
    }

    case trmPrg: { /* We have a program label */
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      return skipName(in, S);
    }

    case trmCns: {
      integer arity;

      if ((res = decInt(in, S, &arity)) != Ok) /* How many arguments in the class */
        return res;

      res = skipTrm(in, S);

      if (res == Ok) {
        integer i;

        for (i = 0; res == Ok && i < arity; i++)
          res = skipText(in, S); /* skip each constructor element */
      }

      return res;
    }

    default: {
      strMsg(S->errorMsg, S->msgSize, "invalid encoding");
      return Error;
    }
  }
}

retCode skipEncoded(ioPo in, string errorMsg, long msgLen) {
  EncodeSupport support = {NULL, 0, NULL, 0, errorMsg, msgLen, NULL};

  return skipTrm(in, &support);
}
