/*
  Support a multi-wait for global GC
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <stdlib.h>
#include "lo.h"

/*
 * When a thread has been suspended for G/C, it will be woken up again by
 * the G/C thread when that has completed
 */

/* This is called by a process when it has been asked to suspend,
   and it is now safe to do so.
   Major constraint is that no request to allocate permanent memory permitted
   between the initial request and this response
*/

logical checkForPause(processPo P)
{
  lock(O_LOCKED(P));
  if(P->proc.pauseRequest){   /* pauseRequest may only be set by this process */
    process_state currState = P->proc.state;

    P->proc.state = wait_rendezvous;
    pthread_cond_broadcast(&P->proc.cond); /* We are ready to pause */

    while(P->proc.pauseRequest)
      pthread_cond_wait(&P->proc.cond,&P->lock.mutex);
    P->proc.state = currState;		/* restart ... */
    unlock(O_LOCKED(P));
    return True;
  }
  unlock(O_LOCKED(P));
  return False;
}

void setProcessRunnable(processPo P)
{
  lock(O_LOCKED(P));
  if(P->proc.pauseRequest){
    P->proc.state = wait_rendezvous;
    pthread_cond_broadcast(&P->proc.cond); /* We are ready to pause */

    while(P->proc.pauseRequest)
      pthread_cond_wait(&P->proc.cond,&P->lock.mutex);
  }
  P->proc.state = runnable;
  unlock(O_LOCKED(P));
}

void switchProcessState(processPo P,process_state state)
{
  lock(O_LOCKED(P));
  if(P->proc.pauseRequest){
    P->proc.state = wait_rendezvous;
    pthread_cond_broadcast(&P->proc.cond); /* We are ready to pause */

    while(P->proc.pauseRequest)
      pthread_cond_wait(&P->proc.cond,&P->lock.mutex);
  }
  P->proc.state = state;
  unlock(O_LOCKED(P));
}



// This is called by the GC active thread to pause a given thread

static retCode pauseThread(processPo P,void *c)
{
  pthread_t thrID = ps_threadID(P);
  pthread_t self = (pthread_t)c;

  if(!pthread_equal(thrID,self)){
    lock(O_LOCKED(P));
    P->proc.pauseRequest = True;

    if(ps_state(P)==runnable){
      while(ps_state(P)==runnable)	/* Wait for the synchronization */
	pthread_cond_wait(&P->proc.cond,&P->lock.mutex);
    }
    unlock(O_LOCKED(P));
  }
  return Ok;
}

static pthread_mutex_t suspMutex = PTHREAD_MUTEX_INITIALIZER;

void pauseAllThreads(pthread_t self)
{
  // assert(pthread_self()==gcThread);	/* Only the GC thread should call this */

  pthread_mutex_lock(&suspMutex);	/* We should never need this */
  processProcesses(pauseThread,(void*)self);
  pthread_mutex_unlock(&suspMutex);
}

static retCode resumeThread(processPo P,void *c)
{
  pthread_t self = (pthread_t)c;

  lock(O_LOCKED(P));
  P->proc.pauseRequest=False;
  
  if(!pthread_equal(ps_threadID(P),self) && ps_state(P)==wait_rendezvous)
    pthread_cond_signal(&P->proc.cond);

  unlock(O_LOCKED(P));
  return Ok;
}

void resumeAllThreads(pthread_t self)
{
  // assert(pthread_self()==gcThread);	/* Only the GC thread should call this */

  pthread_mutex_lock(&suspMutex);	/* We should never need this */
  processProcesses(resumeThread,(void*)self);
  pthread_mutex_unlock(&suspMutex);
}

