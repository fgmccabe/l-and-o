/* 
  Spinlock interface
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _LOCK_H_
#define _LOCK_H_

#include "word.h"

typedef struct {
  long count;				/* The lock recursion count */
  pthread_t owner;			/* The current owner of the lock */
  pthread_mutex_t mutex;		/* The mutex itself */
  pthread_cond_t cond;			/* Condition variable */
} GoLock, *lockPo;

lockPo newLock(void);

retCode acquireLock(lockPo l,double tmOut);
retCode releaseLock(lockPo l);
retCode waitLock(lockPo l,double tmOut);

#endif
