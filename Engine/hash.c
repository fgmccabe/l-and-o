/*
  Hash table management functions
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "go.h"

/*
 * Function to compute the hashcode of a term
 */

static retCode termHash(ptrPo p, uinteger *hash);

retCode g_hash_term(processPo P, ptrPo a) {
  uinteger hash = 0;
  retCode ret = termHash(&a[1], &hash);

  if (ret == Ok) {
    ptrI val = allocateInteger(&P->proc.heap, hash);
    return equal(P, &val, &a[2]);
  }
  else
    return ret;
}

// Compute the hash code of a term
retCode termHash(ptrPo p, uinteger *hash) {
  ptrI xx = deRefI(p);

  switch (ptg(xx)) {
    case varTg:
      return Fail;                        /* cant hash variables */
    case objTg: {
      objPo o = objV(xx);
      clssPo class = classOf(o);

      if (isSpecialClass(class)) {
        specialClassPo sClass = (specialClassPo) class;
        *hash += sClass->hashFun(sClass, o);
        return Ok;
      }
      else {
        *hash += class->hash;
        long ix;

        ptrPo a = objectArgs(o);
        long arity = objectArity(o);
        retCode ret = Ok;

        for (ix = 0; ret == Ok && ix < arity; ix++, a++)
          ret = termHash(a, hash);
        return ret;
      }
    }
    default:
      syserr("unexpected word in termhash");
      return Error;
  }
}
