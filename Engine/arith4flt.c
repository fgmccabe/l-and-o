/*
  Floating Point Arithmetic functions for the L&O system
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
#include <errno.h>		/* system error doubles */

static comparison fltCompFun(specialClassPo class, objPo o1, objPo o2);

static long ftSizeFun(specialClassPo class, objPo o);
static retCode ftOutFun(specialClassPo class, ioPo out, objPo o);
static retCode ftScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o);
static uinteger fltHash(double n);
static uinteger ftHashFun(specialClassPo class, objPo o);
static objPo ftCopyFun(specialClassPo class, objPo dst, objPo src);

void initFloatClass(void) {
  floatClass = newSpecialClass("lo.core#float", ftSizeFun, fltCompFun,
                               ftOutFun, ftCopyFun, ftScanFun, ftHashFun);
}

static comparison fltCompFun(specialClassPo class, objPo o1, objPo o2) {
  if (o1->class == floatClass && o2->class == floatClass) {
    double f1 = floatVal((floatPo) o1);
    double f2 = floatVal((floatPo) o2);

    if (f1 == f2)
      return same;
    else if (f1 < f2)
      return smaller;
    else
      return bigger;
  } else
    return incomparible;
}

static long ftSizeFun(specialClassPo class, objPo o) {
  return CellCount(sizeof(floatRec));
}

static retCode ftOutFun(specialClassPo class, ioPo out, objPo o) {
  double f = floatVal((floatPo) o);

  return outFloat(out, f);
}

static retCode ftScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o) {
  return Ok;
}

static objPo ftCopyFun(specialClassPo class, objPo dst, objPo src) {
  floatPo fDst = (floatPo) dst;
  floatPo fSrc = (floatPo) src;

  *fDst = *fSrc;
  return (objPo) (fDst + 1);
}

static uinteger ftHashFun(specialClassPo class, objPo o) {
  return fltHash(floatVal((floatPo) o));
}

/* Simple arithmetic predicates */
retCode g_integral(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "integral", eINSUFARG);
  else if (isInteger(objV(x)))
    return Ok;
  else
    return Fail;
}

/* Floating point inequalities */

retCode g__flt_lt(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_flt_lt", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isFloat(A1) || !isFloat(A2))
      return liberror(P, "_flt_lt", eNUMNEEDD);
    else if (floatVal((floatPo) A1) < floatVal((floatPo) A2))
      return Ok;
    else
      return Fail;
  }
}

retCode g__flt_ge(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_flt_ge", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isFloat(A1) || !isFloat(A2))
      return liberror(P, "_flt_ge", eNUMNEEDD);
    else if (floatVal((floatPo) A1) >= floatVal((floatPo) A2))
      return Ok;
    else
      return Fail;
  }
}

retCode g__flt_plus(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if (!isvar(x) && !isvar(y)) {
    if (isFloat(A1) && isFloat(A2)) {
      ptrI ans = allocateFloat(&P->proc.heap, FloatVal(A1) + FloatVal(A2));

      return equal(P, &ans, &a[3]);
    } else
      return liberror(P, "_flt_plus", eNUMNEEDD);
  } else
    return liberror(P, "_flt_plus", eINSUFARG);
}

retCode g__flt_minus(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if (!isvar(x) && !isvar(y)) {
    if (isFloat(A1) && isFloat(A2)) {
      ptrI ans = allocateFloat(&P->proc.heap, FloatVal(A1) - FloatVal(A2));

      return equal(P, &ans, &a[3]);
    } else
      return liberror(P, "_flt_minus", eNUMNEEDD);
  } else
    return liberror(P, "_flt_minus", eINSUFARG);
}

retCode g__flt_times(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if (!isvar(x) && !isvar(y)) {
    if (isFloat(A1) && isFloat(A2)) {
      ptrI ans = allocateFloat(&P->proc.heap, FloatVal(A1) * FloatVal(A2));

      return equal(P, &ans, &a[3]);
    } else
      return liberror(P, "_flt_times", eNUMNEEDD);
  } else
    return liberror(P, "_flt_times", eINSUFARG);
}

retCode g__flt_div(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if (!isvar(x) && !isvar(y)) {
    if (isFloat(A1) && isFloat(A2)) {
      ptrI ans = allocateFloat(&P->proc.heap, FloatVal(A1) / FloatVal(A2));

      return equal(P, &ans, &a[3]);
    } else
      return liberror(P, "_flt_div", eNUMNEEDD);
  } else
    return liberror(P, "_flt_div", eINSUFARG);
}

retCode g__flt_mod(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if (!isvar(x) && !isvar(y)) {
    if (isFloat(A1) && isFloat(A2)) {
      ptrI ans = allocateFloat(&P->proc.heap, fmod(FloatVal(A1), FloatVal(A2)));

      return equal(P, &ans, &a[3]);
    } else
      return liberror(P, "_flt_mod", eNUMNEEDD);
  } else
    return liberror(P, "_flt_mod", eINSUFARG);
}

retCode g__flt_abs(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI z = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "_flt_abs", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (!isFloat(A1))
      return liberror(P, "_flt_abs", eNUMNEEDD);
    else {
      double rslt = fabs(floatVal((floatPo) A1));

      if (isvar(z)) {
        ptrI R = allocateFloat(&P->proc.heap, rslt);

        bindVar(P, deRef(&a[2]), R);
        return Ok;
      } else if (floatVal(floatV(z)) == rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

static uinteger fltHash(double n) {
  union {
    double n;
    uinteger i;
  } c;
  c.n = n;
  return c.i;
}

retCode g__flt_hash(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI z = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "_flt_hash", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (!isFloat(A1))
      return liberror(P, "_flt_hash", eNUMNEEDD);
    else {
      uinteger hash = fltHash(floatVal((floatPo) A1));

      if (isvar(z)) {
        ptrI R = allocateInteger(&P->proc.heap, hash);

        bindVar(P, deRef(&a[2]), R);
        return Ok;
      } else if (integerVal(intV(z)) == hash)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g__flt2int(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI z = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "_flt2int", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (!isFloat(A1))
      return liberror(P, "_flt2int", eNUMNEEDD);
    else {
      integer rslt = (integer) (floatVal((floatPo) A1));

      if (isvar(z)) {
        ptrI R = allocateInteger(&P->proc.heap, rslt);

        bindVar(P, deRef(&a[2]), R);
        return Ok;
      } else if (integerVal(intV(z)) == rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g__str2flt(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (IsString(x)) {
    char * src = stringVal(stringV(x));
    double reslt = parseNumber(src, uniStrLen(src));

    if (isvar(y)) {    /* check the output argument */
      ptrI Ans = allocateFloat(&P->proc.heap, reslt);

      bindVar(P, deRef(&a[2]), Ans);
      return Ok;
    } else if (isFloat(objV(y)) && FloatVal(objV(y)) == reslt)
      return Ok;
    else
      return Fail;
  } else
    return liberror(P, "_str2flt", eINSUFARG); /* dont support inverse mode */
}

retCode g__flt2str(processPo P, ptrPo a) {
  ptrI a1 = deRefI(&a[1]);
  ptrI a2 = deRefI(&a[2]);
  ptrI a3 = deRefI(&a[3]);
  ptrI a4 = deRefI(&a[4]);
  ptrI a5 = deRefI(&a[5]);

  if (isvar(a1) || isvar(a2) || isvar(a3) || isvar(a4) || isvar(a5))
    return liberror(P, "_flt2str", eINSUFARG);
  else if (!isFloat(objV(a1)) || !isInteger(objV(a2)) || !isInteger(objV(a3)) || !isInteger(objV(a4)))
    return liberror(P, "_flt2str", eINVAL);
  else {
    integer width = integerVal(intV(a2));
    integer prec = integerVal(intV(a3));
    logical left = (logical) (width > 0);
    char buffer[128];
    FloatDisplayMode displayMode;

    switch (integerVal(intV(a4))) {
      case 'g':displayMode = general;
        break;
      case 'e':displayMode = scientific;
        break;
      default:displayMode = general;
    }

    retCode res = formatDouble(buffer, NumberOf(buffer), FloatVal(objV(a1)), displayMode, (int) prec,  "",
                               identical(a4, trueClass));

    if (res == Ok) {
      long len;
      ptrI rslt = allocateString(&P->proc.heap, buffer, uniStrLen(buffer));

      return funResult(P, a, 6, rslt);
    } else
      return liberror(P, "num2str", eIOERROR);
  }
}

retCode g__flt_format(processPo P, ptrPo a) {
  ptrI a1 = deRefI(&a[1]);
  ptrI a2 = deRefI(&a[2]);
  ptrI a3 = deRefI(&a[3]);

  if (isvar(a1) || isvar(a2))
    return liberror(P, "_flt_format", eINSUFARG);
  else if (!isFloat(objV(a1)) || !isString(objV(a2)))
    return liberror(P, "_flt_format", eINVAL);
  else if (!isvar(a3))
    return liberror(P, "_flt_format", eVARNEEDD);
  else {
    char buffer[MAX_MSG_LEN];

    strBuffPo frmtP = stringV(a2);
    char * format = stringVal(frmtP);
    long fLen = stringLen(frmtP);
    long endPos;

    if (formattedFloat(floatVal(floatV(a1)), buffer, &endPos, NumberOf(buffer), format, fLen) == Ok) {
      ptrI rslt = allocateString(&P->proc.heap, buffer, endPos);
      return funResult(P, a, 3, rslt);
    } else
      return liberror(P, "_flt_format", eIOERROR);
  }
}

retCode g__pwr(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  objPo A1 = objV(x);
  ptrI y = deRefI(&a[2]);
  objPo A2 = objV(y);

  if (!isvar(x) && !isvar(y)) {
    double N = FloatVal(A1);
    double power = FloatVal(A2);
    double Rslt;

    errno = 0;      /* clear errno prior to computation */
    Rslt = pow(N, power);  /* allow for checks of the answer */

    if (errno != 0)
      return liberror(P, "pwr", eINVAL);
    else {
      ptrI ans = allocateFloat(&P->proc.heap, Rslt);

      return equal(P, &a[3], &ans);
    }
  } else
    return liberror(P, "pow", eINSUFARG);
}

retCode g_sqrt(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double square = FloatVal(A1);

      if (square < 0)
        return liberror(P, "sqrt", eINVAL);  /* square root of negative */
      else {
        double ans = sqrt(square);

        if (isvar(y)) {
          ptrI Ans = allocateFloat(&P->proc.heap, ans);

          bindVar(P, deRef(&a[2]), Ans);
          return Ok;
        } else if (isFloat(objV(y))) {
          if (floatVal(floatV(y)) == ans)
            return Ok;
          else
            return Fail;
        } else
          return Fail;
      }
    } else
      return Fail;
  } else
    return liberror(P, "sqrt", eINSUFARG); /* dont support inverse mode */
}

/* exponential e**x */
retCode g_exp(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = floatVal((floatPo) A1);
      double ans;

      errno = 0;    /* clear errno prior to computation */
      ans = exp(num1);    /* allow for checks of the answer */

      if (errno != 0) {
        if (errno == EDOM || errno == ERANGE)
          return liberror(P, "exp", eRANGE);
        else
          return liberror(P, "exp", eINVAL);
      }

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && floatVal((floatPo) objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "exp", eINSUFARG); /* dont support inverse mode */
}

/* logarithm loge(x) */
retCode g_log(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double ans;

      errno = 0;    /* clear errno prior to computation */
      ans = log(num1);    /* allow for checks of the answer */

      if (errno != 0) {
        if (errno == EDOM || errno == ERANGE)
          return liberror(P, "log", eRANGE);
        else
          return liberror(P, "log", eINVAL);
      }

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "log", eINSUFARG); /* dont support inverse mode */
}

/* logarithm log10(x) */
retCode g_log10(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double ans;

      errno = 0;    /* clear errno prior to computation */
      ans = log10(num1);  /* allow for checks of the answer */

      if (errno != 0) {
        if (errno == EDOM || errno == ERANGE)
          return liberror(P, "log10", eRANGE);
        else
          return liberror(P, "log10", eINVAL);
      }

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "log10", eINSUFARG); /* dont support inverse mode */
}

retCode g_pi(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1) && FloatVal(A1) == PI)
      return Ok;
    else
      return Fail;
  } else {
    ptrI Ans = allocateFloat(&P->proc.heap, PI);

    bindVar(P, deRef(&a[1]), Ans);
    return Ok;
  }
}

retCode g_trunc(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "trunc", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double ans = roundNumber(FloatVal(A1));

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  }
}

retCode g_floor(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "floor", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double ans = floor(FloatVal(A1));

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  }
}

retCode g_ceil(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "ceil", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double ans = ceil(FloatVal(A1));

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  }
}

/* Trigonometric functions */
retCode g_sin(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double ans = sin(num1);

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "sin", eINSUFARG); /* dont support inverse mode */
}

retCode g_cos(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double ans = cos(num1);

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateNumber(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "cos", eINSUFARG); /* dont support inverse mode */
}

retCode g_tan(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double ans = tan(num1);

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateNumber(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "tan", eINSUFARG); /* dont support inverse mode */
}

retCode g_asin(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double ans;

      errno = 0;    /* clear errno prior to computation */
      ans = asin(num1);    /* allow for checks of the answer */

      if (errno != 0) {
        if (errno == EDOM)
          return liberror(P, "asin", eRANGE);
        else
          return liberror(P, "asin", eINVAL);
      }

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateNumber(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "asin", eINSUFARG); /* dont support inverse mode */
}

retCode g_acos(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double ans;

      errno = 0;    /* clear errno prior to computation */
      ans = acos(num1);    /* allow for checks of the answer */

      if (errno != 0) {
        if (errno == EDOM)
          return liberror(P, "acos", eRANGE);
        else
          return liberror(P, "acos", eINVAL);
      }

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateNumber(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "acos", eINSUFARG); /* dont support inverse mode */
}

retCode g_atan(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double ans = atan(num1);

      if (isvar(y)) {    /* check the output argument */
        ptrI Ans = allocateNumber(&P->proc.heap, ans);

        bindVar(P, deRef(&a[2]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "atan", eINSUFARG); /* dont support inverse mode */
}

/*
 * srand(n) - reinitialize the random doubles generator
 */

retCode g_srand(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double i = FloatVal(A1);

      if (i < 0)
        return liberror(P, "srand", eRANGE);
      else {
        srand((unsigned int) i);
        return Ok;
      }
    } else
      return Fail;
  } else
    return liberror(P, "srand", eINSUFARG);
}

/* rand(X) => Random No. in range [0..X) */

retCode g_rand(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);

      if (num1 > 0) {
        double ans = num1 * ((double) rand()) / (RAND_MAX + 1.0);

        if (isvar(y)) {    /* check the output argument */
          ptrI Ans = allocateNumber(&P->proc.heap, ans);

          bindVar(P, deRef(&a[2]), Ans);
          return Ok;
        } else if (isFloat(objV(y)) && FloatVal(objV(y)) == ans)
          return Ok;
        else
          return Fail;
      } else
        return liberror(P, "rand", eINVAL);  /* invalid argument */
    } else
      return Fail;
  } else
    return liberror(P, "rand", eINSUFARG); /* inverse mode doesnt make sense */
}


/*
 * Functions to assist in the manipulations of the fp double 
 */

/*  multiply A[1] by A[2]=integral power of 2, used in generating fp */

retCode g__ldexp(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (!isvar(x) && !isvar(y)) {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (isFloat(A1) && isFloat(A2)) {
      double num1 = FloatVal(A1);
      double ans = ldexp(num1, (int) FloatVal(A2));

      if (isvar(z)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, ans);

        bindVar(P, deRef(&a[3]), Ans);
        return Ok;
      } else if (isFloat(objV(y)) && FloatVal(objV(z)) == ans)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "ldexp", eINSUFARG); /* dont support inverse mode */
}

/* Convert fp to fractional and integral exponent parts */
retCode g__frexp(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      int exp;
      double frac = frexp(num1, &exp);

      if (isvar(y)) {    /* check the output arguments */
        ptrI Ans = allocateFloat(&P->proc.heap, frac);

        bindVar(P, deRef(&a[2]), Ans);
      } else if (isFloat(objV(y)) && FloatVal(objV(y)) != frac)
        return Fail;

      if (isvar(z)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, exp);

        bindVar(P, deRef(&a[3]), Ans);
        return Ok;
      } else if (isFloat(objV(z)) && FloatVal(objV(z)) == exp)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "frexp", eINSUFARG); /* dont support inverse mode */
}

/* Convert fp to fractional and integral parts */
retCode g__modf(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isFloat(A1)) {
      double num1 = FloatVal(A1);
      double intgrl;
      double frac = modf(num1, &intgrl);

      if (isvar(y)) {    /* check the output arguments */
        ptrI Ans = allocateFloat(&P->proc.heap, intgrl);

        bindVar(P, deRef(&a[2]), Ans);
      } else if (isFloat(objV(y)) && FloatVal(objV(z)) != intgrl)
        return Fail;

      if (isvar(z)) {    /* check the output argument */
        ptrI Ans = allocateFloat(&P->proc.heap, frac);

        bindVar(P, deRef(&a[3]), Ans);
        return Ok;
      } else if (isFloat(objV(z = deRefI(&a[3]))) && FloatVal(objV(z)) == frac)
        return Ok;
      else
        return Fail;
    } else
      return Fail;
  } else
    return liberror(P, "modf", eINSUFARG); /* dont support inverse mode */
}
