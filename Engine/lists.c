/*
  List handling functions for Go!
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

ptrI permLsPair(heapPo H,ptrI head,ptrI tail)
{
  rootPo root = gcAddRoot(H,&head);

  gcAddRoot(H,&tail);

  objPo new = permObject(H,listClass);

  new->args[0] = head;
  new->args[1] = tail;

  gcRemoveRoot(H,root);

  return objP(new);
}

long ListLen(ptrI xx)
{
  long count = 0;

  while(IsList(xx)){
    xx = deRefI(listTail(objV(xx)));
    count++;
  }

  if(identical(xx,emptyList))
    return count;
  else
    return -count;
}

