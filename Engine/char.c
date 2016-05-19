/* 
 Character handling and Unicode character predicates
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

#include "go.h"
#include "hashTable.h"
#include "eval.h"
#include "unicode.h"
#include "char.h"


/*
 * Character type predicates
 */

retCode g__isCcChar(processPo P, ptrPo a) /* Other, Control */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isCcChar", eINSUFARG);
  else {
    if (isCcChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isCfChar(processPo P, ptrPo a) /* Other, Format */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isCfChar", eINSUFARG);
  else {
    if (isCfChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isCnChar(processPo P, ptrPo a) /* Other, Unassigned */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isCnChar", eINSUFARG);
  else {
    if (isCnChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isCoChar(processPo P, ptrPo a) /* Other, Private */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isCoChar", eINSUFARG);
  else {
    if (isCoChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isCsChar(processPo P, ptrPo a) /* Other, surrogate */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isCsChar", eINSUFARG);
  else {
    if (isCsChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isLlChar(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isLlChar", eINSUFARG);
  else {
    if (isLlChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isLmChar(processPo P, ptrPo a) /* Letter, modifier */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isLmChar", eINSUFARG);
  else {
    if (isLmChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isLoChar(processPo P, ptrPo a) /* Letter, other */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isLoChar", eINSUFARG);
  else {
    if (isLoChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isLtChar(processPo P, ptrPo a) /* Letter, titlecase */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isLtChar", eINSUFARG);
  else {
    if (isLtChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isLuChar(processPo P, ptrPo a) /* Letter, uppercase */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isLuChar", eINSUFARG);
  else {
    if (isLuChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isMcChar(processPo P, ptrPo a) /* Mark, spacing combining */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isMcChar", eINSUFARG);
  else {
    if (isMcChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isMeChar(processPo P, ptrPo a) /* Mark, enclosing */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isMeChar", eINSUFARG);
  else {
    if (isMeChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isMnChar(processPo P, ptrPo a) /* Mark, non spacing */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isMnChar", eINSUFARG);
  else {
    if (isMnChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isNdChar(processPo P, ptrPo a) /* Number, decimal digit */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isNdChar", eINSUFARG);
  else {
    if (isNdChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isNlChar(processPo P, ptrPo a) /* Number, letter */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isNlChar", eINSUFARG);
  else {
    if (isNlChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isNoChar(processPo P, ptrPo a) /* Number, other */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isNoChar", eINSUFARG);
  else {
    if (isNoChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isPcChar(processPo P, ptrPo a) /* Punctuation, connector */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isPcChar", eINSUFARG);
  else {
    if (isPcChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isPdChar(processPo P, ptrPo a) /* Punctuation, dash */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isPdChar", eINSUFARG);
  else {
    if (isPdChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isPeChar(processPo P, ptrPo a) /* Punctuation, close */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isPeChar", eINSUFARG);
  else {
    if (isPeChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isPfChar(processPo P, ptrPo a) /* Punctuation, final quote */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isPfChar", eINSUFARG);
  else {
    if (isPfChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isPiChar(processPo P, ptrPo a) /* Punctuation, initial quote */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isPiChar", eINSUFARG);
  else {
    if (isPiChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isPoChar(processPo P, ptrPo a) /* Punctuation, other */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isPoChar", eINSUFARG);
  else {
    if (isPoChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isPsChar(processPo P, ptrPo a) /* Punctution, open */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isPsChar", eINSUFARG);
  else {
    if (isPsChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isScChar(processPo P, ptrPo a) /* Symbol, currency */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isScChar", eINSUFARG);
  else {
    if (isScChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isSkChar(processPo P, ptrPo a) /* Symbol, modifier */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isSkChar", eINSUFARG);
  else {
    if (isSkChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isSmChar(processPo P, ptrPo a) /* Symbol, math */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isSmChar", eINSUFARG);
  else {
    if (isSmChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isSoChar(processPo P, ptrPo a) /* Symbol, other */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isSoChar", eINSUFARG);
  else {
    if (isSoChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isZlChar(processPo P, ptrPo a) /* Separator, line */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isZlChar", eINSUFARG);
  else {
    if (isZlChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isZpChar(processPo P, ptrPo a) /* Separator, paragraph */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isZpChar", eINSUFARG);
  else {
    if (isZpChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isZsChar(processPo P, ptrPo a) /* Separator, space */
{
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isZsChar", eINSUFARG);
  else {
    if (isZsChar(IntVal(x)))
      return Ok;
    else
      return Fail;
  }
}

retCode g__isLetterChar(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__isLetterChar", eINSUFARG);
  else {
    codePoint ch = IntVal(x);

    if (isLetterChar(ch))
      return Ok;
    else
      return Fail;
  }
}

retCode g__digitCode(processPo P, ptrPo a) {
  ptrI x = deRefI(&a[1]);

  if (isvar(x))
    return liberror(P, "__digitCode", eINSUFARG);
  else {
    codePoint ch = IntVal(x);

    if (isNdChar(ch)) {
      ptrI ans = allocateInteger(&P->proc.heap, digitValue(ch));
      return equal(P, &a[2], &ans);
    } else
      return Fail;
  }
}
