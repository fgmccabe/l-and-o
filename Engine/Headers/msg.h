/*
  Header file for message handling
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
  */
#ifndef _ENGINE_MSG_H_
#define _ENGINE_MSG_H_

#include "word.h"

/* message processing functions */
void initMailBoxes(void);               /* initialize message queues etc. */

logical IsRealMailBox(ptrI M);
processPo mailBoxOwner(ptrI M);
integer mailBoxSequence(ptrI M);

ptrI newMailBox(ptrI owner);
void closeMailBox(ptrI M);

retCode showMailBox(ioPo f,ptrI M);

#endif
