/*
  Tuple type schema
  Copyright (c) 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#include "lo.h"
#include "tpl.h"

ptrI tupleClass(integer arity) {
  byte lbl[MAX_SYMB_LEN];
  strMsg(lbl, NumberOf(lbl), "()%d", arity);
  return newClassDef(lbl, arity);
}

extern objPo newTuple(heapPo H, integer arity) {
  return allocateObject(H, tupleClass(arity));
}

ptrI tuplePair(heapPo H, ptrI left, ptrI right) {
  rootPo root = gcAddRoot(H, &left);
  gcAddRoot(H, &right);

  objPo tpl = allocateObject(H, tupleClass(2));
  tpl->args[0] = left;
  tpl->args[1] = right;

  gcRemoveRoot(H, root);
  return objP(tpl);
}

static logical isTplClass(clssPo cl);

logical isTuplePair(ptrPo t, ptrPo left, ptrPo right) {
  objPo tpl = objV(t);

  if (isTplClass(classOf(tpl))) {
    if (tupleArity(tpl) == 2) {
      *left = tpl->args[0];
      *right = tpl->args[1];
      return True;
    } else
      return False;
  } else
    return False;
}

logical IsTuple(ptrI t) {
  objPo tpl = objV(t);

  return isTplClass(classOf(tpl));
}

integer tupleArity(objPo t) {
  return objectArity(t);
}

ptrPo nthTplEl(objPo tpl, integer pos) {
  assert(isTplClass(classOf(tpl)));

  return nthArg(tpl, pos);
}

logical isTplClass(clssPo cl) {
  string clName = className(cl);
  if (uniIsLitPrefix(clName, "()")) {
    string ar = clName + strlen("()");
    while (*ar) {
      byte ch = *ar++;
      if (!(ch >= '0' && ch <= '9'))
        return False;
    }
    return True;
  } else
    return False;
}

logical isTupleClass(ptrI cl) {
  return isTplClass((clssPo) objV(cl));
}


