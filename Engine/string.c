/*
  String handling functions for the Go! engine
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"		/* pick up standard configuration header */
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "go.h"
#include "term.h"

static long strSizeFun(specialClassPo class, objPo o);
static comparison strCompFun(specialClassPo class, objPo o1, objPo o2);
static retCode strOutFun(specialClassPo class, ioPo out, objPo o);
static retCode strScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o);
static objPo strCopyFun(specialClassPo class, objPo dst, objPo src);
static uinteger strHashFun(specialClassPo class, objPo o);

void initStringClass(void) {
  stringClass = newSpecialClass("go.stdlib#string", strSizeFun, strCompFun,
                                strOutFun, strCopyFun, strScanFun, strHashFun);
}

static long strSizeFun(specialClassPo class, objPo o) {
  assert(o->class == stringClass);
  stringPo s = (stringPo) o;

  return CellCount(sizeof(stringRec) + (uniStrLen(s->data) + 1) * sizeof(byte));
}

static comparison strCompFun(specialClassPo class, objPo o1, objPo o2) {
  if (o1 == o2)
    return same;
  else if (o1->class == stringClass && o2->class == stringClass) {
    stringPo s1 = (stringPo) o1;
    stringPo s2 = (stringPo) o2;

    int comp = uniCmp(StringVal(s1), StringVal(s2));

    if (comp == 0)
      return same;
    else if (comp < 0)
      return smaller;
    else
      return bigger;
  }
  else
    return incomparible;
}

static retCode strOutFun(specialClassPo class, ioPo out, objPo o) {
  stringPo s = (stringPo) o;
  string sym = StringVal(s);
  retCode r = outChar(out, '\'');

  while (r == Ok && *sym != 0)
    r = wStringChr(out, *sym++);
  if (r == Ok)
    r = outChar(out, '\'');
  return r;
}

static retCode strScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o) {
  return Ok;
}

static objPo strCopyFun(specialClassPo class, objPo dst, objPo src) {
  long size = strSizeFun(class, src);
  memmove((void *) dst, (void *) src, size * sizeof(ptrI));

  return (objPo) (((ptrPo) dst) + size);
}

static uinteger strHashFun(specialClassPo class, objPo o) {
  assert(o->class == stringClass);

  stringPo s = (stringPo) o;
  return uniHash(StringVal(s));
}

ptrI allocateCString(heapPo H, const char *m) {
  return allocateString(H, (string) m, strlen(m));
}

ptrI allocateString(heapPo H, string m, long count) {
  size_t len = CellCount(sizeof(stringRec) + (count + 1) * sizeof(byte));
  stringPo new = (stringPo) allocate(H, len);

  new->class = stringClass;
  new->size = count;
  strncpy((char *) new->data, (char *) m, count);
  return objP(new);
}

retCode g__stringOf(processPo P, ptrPo a) {
  ptrI Data = deRefI(&a[1]);
  ptrI Width = deRefI(&a[2]);
  ptrI Prec = deRefI(&a[3]);
  heapPo H = &P->proc.heap;

  if (isvar(Width) || !isInteger(objV(Width)))
    return liberror(P, "__stringOf", eINTNEEDD);
  else if (isvar(Prec) || !isInteger(objV(Prec)))
    return liberror(P, "__stringOf", eINTNEEDD);
  else {
    long width = integerVal(intV(Width));
    long prec = integerVal(intV(Prec));
    string buffer = (prec < 0 ?
                     (byte *) malloc(sizeof(byte) * (-prec + 1))
                              : NULL);
    ioPo str = O_IO(openStrOutput((string) "", utf8Encoding));
    retCode ret = outCell(str, &Data, prec == 0 ? INT_MAX / 4 : prec, 0, False);

    if (ret != Ok) {
      if (buffer != NULL)
        free(buffer);
      return liberror(P, "__stringOf", eINVAL);
    }
    else {
      uint64 len;
      string txt = getStrText(O_STRING(str), &len);

      if (width != 0) {
        long sLen = labs(width) + 1;
        byte text[sLen];

        if (width > 0) {                    /* right padded */
          string p;
          uint64 w = width - len;

          uniNCpy(text, sLen, txt, (long) len);
          p = text + len;
          while (w-- > 0)
            *p++ = ' ';                   /* pad out with spaces */
          *p = 0;
        }
        else {
          string p = text;
          long w = -width - (integer) len;
          while (w-- > 0)
            *p++ = ' ';                   /* left pad with spaces */
          if (labs(width) > len)
            uniNCpy(p, sLen - (p - text), txt, (long) len);
          else
            uniNCpy(p, sLen - (p - text), txt + len + width, -width);
        }

        {
          ptrI txtList = allocateString(H, text, labs(width));

          closeFile(str);  /* close the file down */
          if (buffer != NULL)
            free(buffer);
          return funResult(P, a, 4, txtList);
        }
      }
      else {
        ptrI txtList = kvoid;
        rootPo root = gcAddRoot(&P->proc.heap, &txtList);
        ret = closeOutString(str, &P->proc.heap, &txtList);

        gcRemoveRoot(&P->proc.heap, root);
        if (buffer != NULL)
          free(buffer);

        if (ret == Ok)
          return equal(P, &a[4], &txtList);
        else
          return ret;
      }
    }
  }
}

/*
 Trim a string to be a particular width
 negative width = right justified 
*/
retCode g__trim(processPo P, ptrPo a) {
  ptrI Data = deRefI(&a[1]);
  ptrI Width = deRefI(&a[2]);

  if (isvar(Width) || !isInteger(objV(Width)))
    return liberror(P, "__trim", eINTNEEDD);
  else if (isvar(Data))
    return liberror(P, "__trim", eINSUFARG);
  else if (!IsString(Data))
    return liberror(P, "__trim", eINVAL);
  else {
    long width = integerVal(intV(Width));
    string data = stringVal(stringV(Data));
    long len = uniStrLen(data);
    long awidth = labs(width);
    heapPo H = &P->proc.heap;

    if (width == 0 || awidth == len)
      return equal(P, &a[1], &a[3]);      /* just return the string itself */
    else {
      byte buff[MAX_SYMB_LEN];
      string buffer = (width > NumberOf(buff) ? (byte *) malloc(sizeof(byte) * awidth) : buff);

      if (width < 0) {                      /* right justified */
        long cnt = len - awidth;           /* how much we have to step into the string */
        string l = &data[cnt];

        cnt = awidth - len;               /* the number of pad characters */
        while (cnt > 0)
          buffer[--cnt] = ' ';
        cnt = awidth - len;
        if (cnt < 0)
          cnt = 0;
        strncpy((char *) &buffer[cnt], (char *) l, awidth - cnt);     // plop in the string contents
      }
      else {
        strncpy((char *) buffer, (char *) data, awidth);

        if (len < width) {
          for (long i = len; i < width; i++)
            buffer[i] = ' ';
        }
      }

      {
        ptrI txtList = allocateString(H, buffer, awidth);

        if (buffer != buff)
          free(buffer);

        return funResult(P, a, 3, txtList);
      }
    }
  }
}

// Prepare a string to be formatted in a differently sized field

static retCode strPrepare(string tgt, long tLen, string src, long sLen,
                          codePoint pad, logical left, long width) {
  long i, j;

  if (width == 0)
    width = sLen;

  if (width >= tLen)
    return Error;      /* requested width too large */

  long gaps = width - sLen;    /* How many gap fillers */

  if (left) {        /* left aligned */
    if (gaps < 0) {        /* We have to trim, lose trailing */
      for (i = 0; i < width; i++)
        tgt[i] = src[i];
      tgt[i] = '\0';      /* terminate */
    }
    else {
      for (i = 0; i < sLen; i++)
        tgt[i] = src[i];
      while (i < width)
        appendCodePoint(tgt, &i, tLen, pad);
      tgt[i] = '\0';
    }
  }
  else {          /* right aligned */
    if (gaps < 0) {
      for (j = 0, i = sLen + gaps; i < sLen; i++, j++) /* lose the left part of the source */
        tgt[j] = src[i];
      tgt[j] = '\0';
    }
    else {        /* extra pad on the left */
      for (j = 0, i = gaps; i > 0; i--)
        appendCodePoint(tgt, &j, tLen, pad);
      for (i = 0; i < sLen; j++, i++)
        tgt[j] = src[i];
      tgt[j] = '\0';
    }
  }
  return Ok;
}

retCode g_num2str(processPo P, ptrPo a) {
  ptrI a1 = deRefI(&a[1]);
  ptrI a2 = deRefI(&a[2]);
  ptrI a3 = deRefI(&a[3]);
  ptrI a4 = deRefI(&a[4]);
  ptrI a5 = deRefI(&a[5]);

  if (isvar(a1) || isvar(a2) || isvar(a3) || isvar(a4) || isvar(a5))
    return liberror(P, "num2str", eINSUFARG);
  else {
    integer width = isFloat(objV(a2)) ? (integer) floatVal(floatV(a2)) : integerVal(intV(a2));
    integer prec = isFloat(objV(a3)) ? (integer) floatVal(floatV(a3)) : integerVal(intV(a3));
    logical left = (logical) (width > 0);
    byte buffer[128];
    ioPo out = O_IO(openBufferStr(buffer, NumberOf(buffer)));
    retCode res = outDouble(out, NumberVal(objV(a1)),
                            (char) (identical(a5, trueClass) ? 'g' : 'f'),
                            abs((int) width), (int) prec, ' ', left, (string) "", identical(a4, trueClass));

    if (res == Ok) {
      uint64 len;
      string text = getStrText(O_STRING(out), &len);
      ptrI rslt = allocateString(&P->proc.heap, text, (long) len);
      closeFile(out);

      return funResult(P, a, 6, rslt);
    }
    else
      return liberror(P, "num2str", eIOERROR);
  }
}

retCode g_int2str(processPo P, ptrPo a) {
  ptrI a1 = deRefI(&a[1]);
  ptrI a2 = deRefI(&a[2]);
  ptrI a3 = deRefI(&a[3]);
  ptrI a4 = deRefI(&a[4]);

  if (isvar(a1) || isvar(a2) || isvar(a3) || isvar(a4))
    return liberror(P, "int2str", eINTNEEDD);
  else {
    integer val = isFloat(objV(a1)) ? (integer) floatVal(floatV(a1)) : integerVal(intV(a1));
    integer base = isFloat(objV(a2)) ? (integer) floatVal(floatV(a2)) : integerVal(intV(a2));
    logical sign = (logical) (base < 0);
    integer width = isFloat(objV(a3)) ? (integer) floatVal(floatV(a3)) : integerVal(intV(a3));
    codePoint pad = IntVal(a4);
    logical left = (logical) (width < 0);
    byte buffer[128];
    byte result[128];

    strMsg(buffer, NumberOf(buffer), "%d", val);

    retCode ret = strPrepare(result, NumberOf(result), buffer, uniStrLen(buffer),
                             pad, left, labs(width));

    if (ret == Ok) {
      ptrI rslt = allocateString(&P->proc.heap, result, uniStrLen(result));

      return funResult(P, a, 5, rslt);
    }
    else
      return liberror(P, "int2str", eIOERROR);
  }
}

retCode g_explode(processPo P, ptrPo a) {
  ptrI Str = deRefI(&a[1]);

  if (isvar(Str))
    return liberror(P, "explode", eINSUFARG);
  else if (!IsString(Str))
    return liberror(P, "explode", eINVAL);
  else {
    string src = stringVal(stringV(Str));
    long srcLen = uniStrLen(src);

    byte buff[MAX_SYMB_LEN];
    string buffer = (srcLen > NumberOf(buff) ? (byte *) malloc(sizeof(byte) * srcLen) : buff);
    strncpy((char *) buffer, (char *) src, srcLen); // Copy out the string in case of GC

    ptrI out = emptyList;
    ptrI el = kvoid;
    heapPo H = &P->proc.heap;
    rootPo root = gcAddRoot(H, &out);

    gcAddRoot(H, &el);

    long pos = srcLen;

    while (pos > 0) {
      codePoint ch;
      if (prevPoint(buffer, &pos, &ch) != Ok)
        return liberror(P, "explode", eINVAL);
      el = allocateInteger(H, ch);
      out = consLsPair(H, el, out);
    }

    gcRemoveRoot(H, root);
    return funResult(P, a, 2, out);
  }
}

retCode g_implode(processPo P, ptrPo a) {
  ptrI Ls = deRefI(&a[1]);

  if (!isGroundTerm(&Ls))
    return liberror(P, "implode", eINSUFARG);
  else {
    long sLen = 4 * ListLen(Ls) + 1;
    long pos = 0;
    byte text[sLen];

    while (IsList(Ls)) {
      ptrPo h = listHead(objV(Ls));
      ptrI C = deRefI(h);

      if (isobj(C) && IsInt(C)) {
        if (pos < sLen) {
          appendCodePoint(text, &pos, sLen, (codePoint) IntVal(C));
          Ls = deRefI(h + 1);
        }
        else
          return liberror(P, "implode", eINVAL);
      }
    }
    switchProcessState(P, in_exclusion);

    ptrI result = allocateString(&P->proc.heap, text, pos);

    setProcessRunnable(P);
    return funResult(P, a, 2, result);
  }
}

retCode closeOutString(ioPo f, heapPo H, ptrPo tgt) {
  uint64 len;
  string buff = getStrText(O_STRING(f), &len);
  ptrI str = allocateString(H, buff, (size_t) len);

  *deRef(tgt) = str;

  return closeFile(f);
}
