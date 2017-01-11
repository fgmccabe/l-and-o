/*
  Integer Arithmetic functions for the L&O system
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
#include "config.h"		/* pick up standard configuration header */
#include <math.h>
#include <stdlib.h>
#include <errno.h>		/* system error doubles */
#include "lo.h"

static long inSizeFun(specialClassPo class, objPo o);
static comparison intCompFun(specialClassPo class, objPo o1, objPo o2);

static retCode inOutFun(specialClassPo class, ioPo out, objPo o);
static retCode inScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o);
static uinteger inHashFun(specialClassPo class, objPo o);
static objPo inCopyFun(specialClassPo class, objPo dst, objPo src);

void initIntegerClass(void) {
  integerClass = newSpecialClass("lo.core#integer", inSizeFun, intCompFun,
                                 inOutFun, inCopyFun, inScanFun, inHashFun);
}

static long inSizeFun(specialClassPo class, objPo o) {
  return CellCount(sizeof(integerRec));
}

static comparison intCompFun(specialClassPo class, objPo o1, objPo o2) {
  if (o1->class == integerClass && o2->class == integerClass) {
    integer i1 = integerVal((integerPo) o1);

    integer i2 = integerVal((integerPo) o2);

    if (i1 == i2)
      return same;
    else if (i1 < i2)
      return smaller;
    else
      return bigger;
  } else
    return incomparible;
}

static retCode inOutFun(specialClassPo class, ioPo out, objPo o) {
  integer i = integerVal((integerPo) o);

  return outInteger(out, i, 10, 0, 0, ' ', False, (string) "", False);
}

static retCode inScanFun(specialClassPo class, specialHelperFun helper, void *c, objPo o) {
  return Ok;
}

static objPo inCopyFun(specialClassPo class, objPo dst, objPo src) {
  integerPo iDst = (integerPo) dst;
  integerPo iSrc = (integerPo) src;

  *iDst = *iSrc;
  return (objPo) (iDst + 1);
}

static uinteger inHashFun(specialClassPo class, objPo o) {
  integer val = ((integerPo) o)->i;
  return (uint64) val;
}

/* Integer arithmetic */
retCode g__int_plus(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_int_plus", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_int_plus", eINTNEEDD);
    else {
      integer rslt = integerVal((integerPo) A1) + integerVal((integerPo) A2);

      if (isvar(z)) {
        ptrI R = allocateInteger(&P->proc.heap, rslt);

        bindVar(P, deRef(&a[4]), R);
        return Ok;
      } else if (integerVal(intV(z)) == rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g__int_minus(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_int_minus", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_int_minus", eINTNEEDD);
    else {
      integer rslt = integerVal((integerPo) A1) - integerVal((integerPo) A2);

      if (isvar(z)) {
        ptrI R = allocateInteger(&P->proc.heap, rslt);

        bindVar(P, deRef(&a[4]), R);
        return Ok;
      } else if (integerVal(intV(z)) == rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g__int_times(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_int_times", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_int_times", eINTNEEDD);
    else {
      integer rslt = integerVal((integerPo) A1) * integerVal((integerPo) A2);

      if (isvar(z)) {
        ptrI R = allocateInteger(&P->proc.heap, rslt);

        bindVar(P, deRef(&a[4]), R);
        return Ok;
      } else if (integerVal(intV(z)) == rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g__int_div(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_int_div", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_int_div", eINTNEEDD);
    else if (integerVal((integerPo) A2) == 0)
      return liberror(P, "_int_div", eDIVZERO);
    else {
      integer rslt = integerVal((integerPo) A1) / integerVal((integerPo) A2);

      if (isvar(z)) {
        ptrI R = allocateInteger(&P->proc.heap, rslt);

        bindVar(P, deRef(&a[4]), R);
        return Ok;
      } else if (integerVal(intV(z)) == rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g__int_mod(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_int_mod", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_int_mod", eINTNEEDD);
    else {
      integer rslt = integerVal((integerPo) A1) % integerVal((integerPo) A2);

      if (isvar(z)) {
        ptrI R = allocateInteger(&P->proc.heap, rslt);

        bindVar(P, deRef(&a[3]), R);
        return Ok;
      } else if (integerVal(intV(z)) == rslt)
        return Ok;
      else
        return Fail;
    }
  }
}

retCode g__int_abs(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI z = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "_int_abs", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (!isInteger(A1))
      return liberror(P, "_int_abs", eINTNEEDD);
    else {
      integer rslt = llabs(integerVal((integerPo) A1));

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

retCode g__int_lt(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_int_lt", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_int_lt", eINTNEEDD);
    else if (integerVal((integerPo) A1) < integerVal((integerPo) A2))
      return Ok;
    else
      return Fail;
  }
}

retCode g__int_ge(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_int_ge", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_int_lt", eINTNEEDD);
    else if (integerVal((integerPo) A1) >= integerVal((integerPo) A2))
      return Ok;
    else
      return Fail;
  }
}

retCode g__int2flt(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI z = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "_int2flt", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (!isInteger(A1))
      return liberror(P, "_int2flt", eINTNEEDD);
    else {
      double rslt = (double) (integerVal((integerPo) A1));

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



/* irand(X) => Random integer in range [0..X) */

retCode g_irand(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (!isvar(x)) {
    objPo A1 = objV(x);

    if (isInteger(A1)) {
      double num1 = (double)integerVal((integerPo)A1);

      if (roundNumber(num1) == num1 && num1 > 0) {
        integer ans = (integer) (num1 * ((double) rand()) / (RAND_MAX + 1.0));

        if (isvar(y)) {    /* check the output argument */
          ptrI Ans = allocateInteger(&P->proc.heap, ans);

          bindVar(P, deRef(&a[2]), Ans);
          return Ok;
        } else if (isInteger(objV(y)) && IntVal(y) == ans)
          return Ok;
        else
          return Fail;
      } else
        return liberror(P, "irand", eINVAL); /* invalid argument */
    } else
      return Fail;
  } else
    return liberror(P, "irand", eINSUFARG); /* inverse mode doesnt make sense */
}

/* Bitwise arithmetic operators */
retCode g__band(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_band", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_band", eINTNEEDD);
    else if (isvar(z)) {
      ptrI R = allocateInteger(&P->proc.heap, integerVal((integerPo) A1) & integerVal((integerPo) A2));

      bindVar(P, deRef(&a[3]), R);
      return Ok;
    } else if ((integerVal((integerPo) A1) & integerVal((integerPo) A2)) == integerVal(intV(z)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__bor(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_bor", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_bor", eINTNEEDD);
    else if (isvar(z)) {
      ptrI R = allocateInteger(&P->proc.heap, integerVal((integerPo) A1) | integerVal((integerPo) A2));

      bindVar(P, deRef(&a[3]), R);
      return Ok;
    } else if ((integerVal((integerPo) A1) | integerVal((integerPo) A2)) == integerVal(intV(z)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__bxor(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_bxor", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_bxor", eINTNEEDD);
    else if (isvar(z)) {
      ptrI R = allocateInteger(&P->proc.heap, integerVal((integerPo) A1) ^ integerVal((integerPo) A2));

      bindVar(P, deRef(&a[3]), R);
      return Ok;
    } else if ((integerVal((integerPo) A1) ^ integerVal((integerPo) A2)) == integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__blsl(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_blsl", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_blsl", eINTNEEDD);
    else if (isvar(z)) {
      ptrI R = allocateInteger(&P->proc.heap, integerVal((integerPo) A1) << integerVal((integerPo) A2));

      bindVar(P, deRef(&a[3]), R);

      return Ok;
    } else if ((integerVal((integerPo) A1) << integerVal((integerPo) A2)) == integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__blsr(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_blsr", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_blsr", eINTNEEDD);
    else if (isvar(z)) {
      ptrI R = allocateInteger(&P->proc.heap,
                               ((unsigned long long) integerVal((integerPo) A1)) >> integerVal((integerPo) A2));

      bindVar(P, deRef(&a[3]), R);
      return Ok;
    } else if ((integerVal((integerPo) A1) >> integerVal((integerPo) A2)) == integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__basr(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);
  ptrI z = deRefI(&a[3]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_basr", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger(A2))
      return liberror(P, "_basr", eINTNEEDD);
    else if (isvar(z)) {
      ptrI R = allocateInteger(&P->proc.heap, integerVal((integerPo) A1) >> integerVal((integerPo) A2));

      bindVar(P, deRef(&a[3]), R);
      return Ok;
    } else if ((integerVal((integerPo) A1) >> integerVal((integerPo) A2)) == integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__bnot(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x))
    return liberror(P, "_bnot", eINSUFARG);
  else {
    objPo A1 = objV(x);

    if (!isInteger(A1))
      return liberror(P, "_bnot", eINTNEEDD);
    else if (isvar(y)) {
      ptrI R = allocateInteger(&P->proc.heap, ~integerVal((integerPo) A1));

      bindVar(P, deRef(&a[2]), R);
      return Ok;
    } else if (~integerVal((integerPo) A1) == integerVal(intV(y)))
      return Ok;
    else
      return Fail;
  }
}

// It is quite important for this to be as fast as possible
retCode g__nthb(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);
  ptrI y = deRefI(&a[2]);

  if (isvar(x) || isvar(y))
    return liberror(P, "_nthb", eINSUFARG);
  else {
    objPo A1 = objV(x);
    objPo A2 = objV(y);

    if (!isInteger(A1) || !isInteger((A2)))
      return liberror(P, "_nthb", eINTNEEDD);
    else {
      uinteger XX = (uinteger) (integerVal((integerPo) A1));
      uinteger Off = (uinteger) (integerVal((integerPo) A2));

      if (XX & (1 << Off))
        return Ok;
      else
        return Fail;
    }
  }
}
