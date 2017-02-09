//
// Created by Francis McCabe on 2/8/17.
//

#include "lo.h"
#include "tuples.h"

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

integer tupleArity(objPo t) {
  return objectArity(t);
}

ptrPo nthEl(ptrPo p, integer pos) {
  objPo tpl = objV(p);

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
  return  isTplClass((clssPo)objV(cl));
}


