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
#include <string.h>
#include <base64.h>
#include "lo.h"
#include "term.h"
#include "encodedP.h"             /* pick up the term encoding definitions */

/* Decode a term encoded message ... from a file stream */


/*
 * Decode a structure from an input stream
 */

static retCode estimate(ioPo in, integer *amnt, integer *perm);

retCode decodeTerm(ioPo in, heapPo H, heapPo R, ptrPo tgt, string errorMsg,
                   long msgSize) {
  EncodeSupport support = {NULL, 0, errorMsg, msgSize, R};

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
            bufferPo buffer = newStringBuffer();
            while (True) {
              switch (inChar(in, &ch)) {
                case Ok:
                  if (ch == delim) {
                    rewindBuffer(buffer);

                    integer amnt, perm = 0;

                    retCode res = estimate(O_IO(buffer), &amnt, &perm);

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
                      rewindBuffer(buffer); /* re-read from string buffer */
                      bufferPo tmpBuffer = newStringBuffer();

                      res = decode(O_IO(buffer), &support, H, tgt, tmpBuffer);

                      closeFile(O_IO(buffer));
                      closeFile(O_IO(tmpBuffer));

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

retCode decInt(ioPo in, integer *ii) {
  codePoint ch;
  integer result = 0;

  switch (inChar(in, &ch)) {
    case Ok:
      if (ch == '-') {
        retCode ret = decInt(in, ii);
        *ii = -(*ii);
        return ret;
      } else if (isDigit(ch)) {
        result = digitVal(ch); // First digit of number

        while (True) {
          byte chb;
          switch (inByte(in, &chb)) {
            case Ok:
              if (isDigit(chb)) {
                result = result * 10 + digitVal(chb);
                continue;
              } else {
                *ii = result;
                return putBackByte(in,chb);
              }
            case Eof: {
              *ii = result;
              return Ok;
            }
            default:
              return Eof;
          }
        }
        break;
      } else {
        return Eof;
      }
    case Eof:
    default:
      return Eof;
  }
}

retCode decFlt(ioPo in, double *dx) {
  bufferPo tmpBuffer = newStringBuffer();
  retCode ret = decodeName(in, tmpBuffer);

  if (ret == Ok) {
    long len;
    *dx = parseNumber(getTextFromBuffer(&len, tmpBuffer), len);
  }
  return ret;
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

      if ((res = decInt(in, &vno)) != Ok)
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
      if ((res = decInt(in, &i)) != Ok)
        return res;
      *tgt = allocateInteger(H, i);
      return Ok;
    }
    case trmFlt: {
      double dx;
      if ((res = decFlt(in, &dx)) != Ok)
        return res;
      *tgt = allocateFloat(H, dx);
      return Ok;
    }

    case trmSym: {
      if ((res = decodeName(in, tmpBuffer)) == Ok) {
        long len;
        *tgt = newEnumSymbol(getTextFromBuffer(&len, tmpBuffer));
      }
      return res;
    }

    case trmString: { /* A literal string */
      if ((res = decodeText(in, tmpBuffer)) == Ok) {
        long len;
        string buff = getTextFromBuffer(&len, tmpBuffer);
        *tgt = allocateString(H, buff, len);
      }
      return res;
    }

    case trmStrct: { /* We have a class definition structure */
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decodeName(in, tmpBuffer)) == Ok) {
        long len;
        *tgt = newClassDef(getTextFromBuffer(&len, tmpBuffer), arity);
      }
      return res;
    }

    case trmPrg: { /* We have a program label */
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decodeName(in, tmpBuffer)) == Ok) {
        long len;
        *tgt = newProgramLbl(getTextFromBuffer(&len, tmpBuffer), (unsigned short)arity);
      }
      return res;
    }

    case trmCns: {
      ptrI class = kvoid;
      rootPo root = gcAddRoot(S->R, &class);
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
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

retCode decodeName(ioPo in, bufferPo buffer) {
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

retCode decodeText(ioPo in, bufferPo buffer) {
  codePoint delim;
  clearBuffer(buffer);

  retCode ret = inChar(in, &delim);

  if (ret != Ok)
    return ret;
  else {
    codePoint ch;
    while (ret == Ok && (ret = inChar(in, &ch)) == Ok && ch != delim) {
      if (ch == '\\')
        ret = inChar(in, &ch);
      outChar(O_IO(buffer), ch);
    }
    return ret;
  }
}

/*
 Estimate amount of heap space needed
 */

typedef struct {
  integer amnt;
  integer perm;
} Estimation;

static retCode nullEstimate(void *cl) {
  return Ok;
}

static retCode estimateVar(integer vno, void *cl) {
  Estimation *info = (Estimation *) cl;
  info->perm += VariableCellCount;
  return Ok;
}

static retCode estimateInt(integer vno, void *cl) {
  Estimation *info = (Estimation *) cl;
  info->perm += IntegerCellCount;
  return Ok;
}

static retCode estimateFlt(double dx, void *cl) {
  Estimation *info = (Estimation *) cl;
  info->perm += FloatCellCount;
  return Ok;
}

static retCode estimateName(string nm, void *cl) {
  Estimation *info = (Estimation *) cl;

  long length = uniStrLen(nm);

  info->perm += CellCount(sizeof(clssRec) + (length + 1) * sizeof(byte));
  return Ok;
}

static retCode estimateString(string nm, void *cl) {
  Estimation *info = (Estimation *) cl;

  long length = uniStrLen(nm);

  info->perm += CellCount(sizeof(stringRec) + (length + 1) * sizeof(byte));
  return Ok;
}

static retCode estimateStrct(string nm, integer arity, void *cl) {
  Estimation *info = (Estimation *) cl;

  long length = uniStrLen(nm);

  info->perm += CellCount(sizeof(clssRec) + (length + 1) * sizeof(byte));
  return Ok;
}

static retCode estimatePrg(string nm, integer arity, void *cl) {
  Estimation *info = (Estimation *) cl;

  long length = uniStrLen(nm);

  info->perm += CellCount(sizeof(programRec) + (length + 1) * sizeof(byte));
  return Ok;
}

static retCode estimateCns(integer arity, void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += arity + 1;
  return Ok;
}

/*
 Estimate amount of heap space needed
 */
retCode estimate(ioPo in, integer *amnt, integer *perm) {
  Estimation info = {0, 0};

  DecodeCallBacks estimateCB = {
    nullEstimate,           // startDecoding
    nullEstimate,           // endDecoding
    estimateVar,            // decVar
    estimateInt,            // decInt
    estimateFlt,            // decFlt
    estimateName,           // decEnum
    estimateString,         // decString
    estimateStrct,          // decStruct
    estimatePrg,            // decPrg
    estimateCns             // decCon
  };

  retCode ret = streamDecode(in, &estimateCB, &info);

  if (ret == Ok) {
    *amnt = info.amnt;
    *perm = info.perm;
  }

  return ret;
}

static retCode display(ioPo in, encodePo S, ioPo out);
static retCode displayName(ioPo in, ioPo out);
static retCode displayText(ioPo in, ioPo out);

retCode displayEncoded(ioPo out, byte *buffer, long len) {
  ioPo str = O_IO(fixedStringBuffer(buffer, len));
  byte errorMsg[1024];
  long msgSize = NumberOf(errorMsg);
  EncodeSupport support = {NULL, 0, errorMsg, msgSize, &globalHeap};
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

      if ((res = decInt(in, &vno)) != Ok)
        return res;

      return outMsg(out, "_%ld", vno);
    }
    case trmInt: {
      integer i;
      if ((res = decInt(in, &i)) != Ok)
        return res;
      return outMsg(out, "%ld", i);
    }
    case trmFlt: {
      double dx;
      if ((res = decFlt(in, &dx)) != Ok)
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

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
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

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
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

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
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

      gcRemoveRoot(S->R, root);
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

static retCode decodeStream(ioPo in, decodeCallBackPo cb, void *cl, bufferPo buff);

retCode streamDecode(ioPo in, decodeCallBackPo cb, void *cl) {
  bufferPo strBuffer = newStringBuffer();
  retCode ret = cb->startDecoding(cl);

  if (ret == Ok)
    ret = decodeStream(in, cb, cl, strBuffer);

  if (ret == Ok)
    cb->endDecoding(cl);

  closeFile(O_IO(strBuffer));
  return ret;
}

static retCode decodeStream(ioPo in, decodeCallBackPo cb, void *cl, bufferPo buff) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch (ch) {
    case trmAnon:
      return cb->decVar(0, cl);
    case trmVar: { /* an unbound variable */
      integer vno;

      res = decInt(in, &vno);
      if (res == Ok)
        res = cb->decVar(vno, cl);
      return res;
    }
    case trmInt: {
      integer i;
      res = decInt(in, &i);
      if (res == Ok)
        res = cb->decInt(i, cl);
      return res;
    }
    case trmFlt: {
      double dx;
      res = decFlt(in, &dx);
      if (res == Ok)
        res = cb->decFlt(dx, cl);
      return res;
    }

    case trmSym: {
      clearBuffer(buff);
      res = decodeName(in, buff);

      if (res == Ok) {
        long len;
        string nm = getTextFromBuffer(&len, buff);
        res = cb->decEnum(nm, cl);
      }
      return res;
    }

    case trmString: {
      clearBuffer(buff);
      res = decodeName(in, buff);

      if (res == Ok) {
        long len;
        string nm = getTextFromBuffer(&len, buff);
        res = cb->decString(nm, cl);
      }
      return res;
    }

    case trmStrct: { /* We have a class definition structure */
      integer arity;
      clearBuffer(buff);

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      res = decodeName(in, buff);

      if (res == Ok) {
        long len;
        string nm = getTextFromBuffer(&len, buff);
        res = cb->decStruct(nm, arity, cl);
      }
      return res;
    }

    case trmPrg: { /* We have a program label */
      integer arity;
      clearBuffer(buff);

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      res = decodeName(in, buff);

      if (res == Ok) {
        long len;
        string nm = getTextFromBuffer(&len, buff);
        res = cb->decPrg(nm, arity, cl);
      }
      return res;
    }

    case trmCns: {
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if (res == Ok)
        res = cb->decCons(arity, cl);

      if (res == Ok)
        res = decodeStream(in, cb, cl, buff); // Handle the operator of the cons

      if (res == Ok) {
        integer i;

        for (i = 0; res == Ok && i < arity; i++)
          res = decodeStream(in, cb, cl, buff);
      }

      return res;
    }

    default:
      return Error;
  }
}

static retCode skipFlag(void *cl) {
  return Ok;
}

static retCode skipInt(integer ix, void *cl) {
  return Ok;
}

static retCode skipFlt(double dx, void *cl) {
  return Ok;
}

static retCode skipString(string sx, void *cl) {
  return Ok;
}

static retCode skipStrct(string nm, integer ar, void *cl) {
  return Ok;
}

static DecodeCallBacks skipCB = {
  skipFlag,           // startDecoding
  skipFlag,           // endDecoding
  skipInt,            // decVar
  skipInt,            // decInt
  skipFlt,            // decFlt
  skipString,         // decEnum
  skipString,         // decString
  skipStrct,          // decStruct
  skipStrct,          // decPrg
  skipInt             // decCons
};

retCode skipEncoded(ioPo in, string errorMsg, long msgLen) {
  switch (streamDecode(in, &skipCB, NULL)) {
    case Ok:
      return Ok;
    case Error:
      strMsg(errorMsg, msgLen, "problem in decoding");
      return Error;
    case Eof:
      strMsg(errorMsg, msgLen, "unexpected EOF");
      return Error;
    default:
      strMsg(errorMsg, msgLen, "problem in decoding");
      return Error;
  }
}
