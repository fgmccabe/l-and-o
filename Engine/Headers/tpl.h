/*
  Header file giving the interface for tuples
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
#ifndef _LO_TUPLES_H_
#define _LO_TUPLES_H_

#include "word.h"

extern ptrI tupleClass(integer arity);
extern objPo newTuple(heapPo H, integer arity);
extern ptrI tuplePair(heapPo H, ptrI left, ptrI right);
extern logical isTuplePair(ptrPo t, ptrPo left, ptrPo right);

extern logical IsTuple(ptrI t);
extern logical isTupleClass(ptrI cl);
extern ptrPo nthTplEl(objPo p, integer pos);

extern integer tupleArity(objPo t);

#endif
