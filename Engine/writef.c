/*
  Data value and formatted write programs
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
#include <limits.h>

#include "lo.h"
#include "term.h"
#include "tuples.h"

/*
 * write a cell in a basic format
 * The depth argument limits the depth that tuples are printed to
 */

retCode wStringChr(ioPo f, codePoint ch) {
  switch (ch) {
    case '\a':
      return outStr(f, "\\a");
    case '\b':
      return outStr(f, "\\b");
    case '\x7f':
      return outStr(f, "\\d");
    case '\x1b':
      return outStr(f, "\\e");
    case '\f':
      return outStr(f, "\\f");
    case '\n':
      return outStr(f, "\\n");
    case '\r':
      return outStr(f, "\\r");
    case '\t':
      return outStr(f, "\\t");
    case '\v':
      return outStr(f, "\\v");
    case '\\':
      return outStr(f, "\\\\");
    case '\"':
      return outStr(f, "\\\"");
    default:
      if (ch < ' ')
        return outMsg(f, "\\+%x;", ch);
      else
        return outChar(f, ch);
  }
}

static retCode outC(ioPo f, ptrPo x, long depth, int prec, logical alt);

retCode outCell(ioPo f, ptrPo x, long depth, int prec, logical alt) {
  if (x != NULL)
    return outC(f, x, depth, prec, alt);
  else
    return outStr(f, "(NULL)");
}

static retCode outC(ioPo f, ptrPo x, long depth, int prec, logical alt) {
  ptrI vx = *(x = deRef(x));
  objPo p = objV(vx);
  retCode r = Ok;

  switch (ptg(vx)) {
    case varTg:
      if (isSuspVar((ptrPo) vx))
        r = outMsg(f, "_*%x", vx);
      else
        r = outMsg(f, "_%x", vx);
      break;
    case objTg: {
      ptrI class = p->class;

      if (isfwd(class)) {
        outMsg(f, "*");
        class = *((ptrPo) objV(class));
      }

      if (class == integerClass)
        r = outInteger(f, integerVal((integerPo) p), 10, 0, prec, ' ', False, (string) "", False);
      else if (class == floatClass)
        r = outFloat(f, floatVal((floatPo) p));
      else if (class == symbolClass) {
        symbPo s = (symbPo) p;
        string sym = SymVal(s);

        r = outChar(f, '\'');

        while (r == Ok && *sym != 0)
          r = wStringChr(f, *sym++);
        if (r == Ok)
          r = outChar(f, '\'');
      } else if (class == stringClass) {
        string src = stringVal((stringPo) p);

        long pos = 0;
        long end = uniStrLen(src);

        r = outChar(f, '\"');

        while (pos < end) {
          codePoint ch;
          r = nxtPoint(src, &pos, end, &ch);
          if (r == Ok)
            r = wStringChr(f, ch);
        }

        if (r == Ok)
          r = outChar(f, '\"');
      } else if (class == classClass) {
        clssPo cl = (clssPo) p;
        string clName = className(cl);

        r = outText(f, clName, uniStrLen(clName));
        if (r == Ok)
          r = outChar(f, '/');
        if (r == Ok)
          r = outInteger(f, cl->arity, 10, 0, prec, ' ', False, (string) "", False);
      } else if (class == listClass) {
        if (depth > 0) {
          long maxLen = (prec != 0 ? prec * 2 : INT_MAX); /* How many elements to show */

          r = outChar(f, '[');

          while (r == Ok && IsList(vx)) {
            ptrPo ll = listHead(objV(vx));

            r = outC(f, ll++, depth - 1, prec, alt);

            vx = deRefI(ll);

            if (r == Ok && IsList(vx)) {
              if (maxLen-- <= 0) {
                r = outStr(f, "...");        /* only show a certain length */

                goto exit_list;  /* what a hack, need a double break */
              } else
                r = outChar(f, ',');
            }
          }
          if (r == Ok && !identical(vx, emptyList)) {
            r = outStr(f, ",..");

            if (r == Ok)
              r = outC(f, &vx, depth - 1, prec, alt);
          }
          exit_list:
          if (r == Ok)
            r = outStr(f, "]");
        } else
          r = outStr(f, "[...]");
      } else if (isTupleClass(class)) {
        if (depth > 0) {
          char *sep = "";

          outChar(f, '(');

          long arity = tupleArity(p);

          for (long ix = 0; ix < arity; ix++) {
            r = outStr(f, sep);
            sep = ", ";
            if (r == Ok)
              r = outC(f, nthArg(p, ix), depth - 1, prec, alt);
          }

          if (r == Ok)
            r = outChar(f, ')');
        } else
          r = outStr(f, "(...)");
      } else if (IsTermClass(class)) {
        string name = objectClassName(p);

        r = outMsg(f, "%U", name);

        if (depth > 0) {
          long arity = objectArity(p);

          if (arity > 0) {
            ptrPo a = objectArgs(p);
            long i;
            char *sep = "";

            outChar(f, '(');

            for (i = 0; r == Ok && i < arity; i++, a++) {
              r = outStr(f, sep);
              sep = ", ";
              if (r == Ok)
                r = outC(f, a, depth - 1, prec, alt);
            }
            if (r == Ok)
              r = outChar(f, ')');
          }
        } else
          r = outStr(f, "(...)");
      } else if (IsSpecialClass(class)) {
        specialClassPo sClass = (specialClassPo) objV(class);
        r = sClass->outFun(sClass, f, p);
      }
      return r;
    }

    case fwdTg: {             /* special case to help with debugging */
      outMsg(f, "[0x%x]-->", x);
      return outC(f, (ptrPo) p, depth - 1, prec, alt);
    }

    default:
      outMsg(f, "illegal cell found at [%x]", p);
      return Error;
  }

  return r;
}

void dc(ptrPo trm) {
  if (trm != NULL)
    outMsg(logFile, "0x%x -> %,20w\n", trm, trm);
  else
    outMsg(logFile, "NULL\n");
  flushFile(logFile);
}

void dO(objPo trm) {
  ptrI T = objP(trm);
  outMsg(logFile, "0x%x : %,20w\n", trm, &T);
  flushFile(logFile);
}
