/*
  Cons Lists
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "listsP.h"
#include "utils.h"
#include <assert.h>
#include <listsP.h>

static uint64 listHash(objectPo o);

static logical listEquality(objectPo o1, objectPo o2);

static void listInit(objectPo o, va_list *args);

static void eraseList(objectPo o);

ListClassRec ListClass = {
  {
    (classPo) &ObjectClass,
    "list",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    eraseList,
    listInit,
    sizeof(ListRecord),
    listHash,
    listEquality,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo listClass = (classPo) &ListClass;

ListRecord EmptyList = {
  {(classPo) &ListClass,
    LARGE_INT64
  },
  {NULL,
    NULL}
};

listPo emptyList = &EmptyList;

void listInit(objectPo o, va_list *args) {
  listPo l = O_LIST(o);
  l->list.head = va_arg(*args, objectPo);
  l->list.tail = va_arg(*args, listPo);

  incReference(l->list.head);
  incReference(O_OBJECT(l->list.tail));
}

void eraseList(objectPo o) {
  listPo l = O_LIST(o);

  decReference(l->list.head);
  decReference(O_OBJECT(l->list.tail));
}

static uint64 listHash(objectPo o) {
  listPo l = O_LIST(o);
  if (l == emptyList)
    return 0;
  else
    return hashCode(head(l)) * 37 + listHash(O_OBJECT(tail(l)));
}

static logical listEquality(objectPo o1, objectPo o2) {
  listPo l1 = O_LIST(o1);
  listPo l2 = O_LIST(o2);

  while (l1 != emptyList && l2 != emptyList) {
    if (!equals(l1->list.head, l2->list.head))
      return False;
    l1 = tail(l1);
    l2 = tail(l2);
  }

  return (logical) (l1 == emptyList && l2 == emptyList);
}

void *head(listPo list) {
  return list->list.head;
}

listPo tail(listPo list) {
  return list->list.tail;
}

listPo cons(objectPo head, listPo tail) {
  return O_LIST(newObject(listClass, head, tail));
}

listPo tack(objectPo head, listPo list) {
  if (list == emptyList)
    return cons(head, list);
  else {
    listPo l = list;
    while (l->list.tail != emptyList)
      l = l->list.tail;
    l->list.tail = cons(head, emptyList);
    return list;
  }
}

objectPo listNthElement(listPo list, int32 ix) {
  while (list != emptyList && ix-- > 0)
    list = tail(list);
  if (list == emptyList)
    return Null;
  else
    return head(list);
}

retCode processList(listPo list, listFun fun, void *cl) {
  retCode ret = Ok;
  while (ret == Ok && list != emptyList) {
    ret = fun(head(list), cl);
    list = tail(list);
  }
  return ret;
}

void *findInList(listPo list, listTest test, void *cl) {
  while (list != emptyList) {
    if (test(head(list), cl))
      return head(list);
    list = tail(list);
  }
  return Null;
}

long listCount(listPo list) {
  long count = 0;
  while (list != emptyList) {
    count++;
    list = tail(list);
  }
  return count;
}

listPo removeElements(listPo l, listTest test, void *cl) {
  listPo l1 = l;
  listPo tl = emptyList;
  while (l1 != emptyList) {
    if (test(head(l1), cl)) {
      if (tl == emptyList) {    /* top of list */
        assert(l1 == l);
        l1 = l = tail(l);
      }
      else {
        l1 = tail(l1);
        tl->list.tail = l1;      /* chop out l1 */
      }
    }
    else {
      tl = l1;
      l1 = tail(l1);
    }
  }
  return l;
}

listPo filter(listPo l, listTest test, void *cl) {
  if (l == emptyList)
    return l;
  else if (test(head(l), cl)) {
    return cons(head(l), filter(tail(l), test, cl));
  }
  else
    return filter(tail(l), test, cl);
}

void *listFold(listPo l, folder f, void *state) {
  while(l!=emptyList){
    state = f(head(l),state);
    l = tail(l);
  }
  return state;
}
