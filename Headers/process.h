/* 
  Go! process record structure
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _PROCESS_H_
#define _PROCESS_H_

#include "heap.h"                       /* access heap mgt interface */
#include "managedP.h"
#include <pthread.h>

typedef enum {
  quiescent,				/* process has yet to execute */
  runnable,			    /* process is running or available to run */
  wait_io,				/* process is waiting for I/O */
  wait_term,		/* process is waiting for another thread to terminate */
  wait_timer,				/* waiting for an interval times */
  wait_lock,				// Waiting for a lock to be released
  wait_child,                           // Wait for a child process
  wait_rendezvous,		       /* Waiting for a rendezvous to release */
  in_exclusion,				/* In an exclusion zone */
  dead                                  /* process has died */
} process_state;

/* A call record */

typedef struct _trail_rec_ *trailPo;
typedef struct _choice_rec_ *choicePo;
typedef struct _call_rec_ *callPo;

typedef struct _call_rec_ {
  insPo cPC;				/* parent program counter */
  ptrI cPROG;                           /* continuation program  */
  callPo cC;                            /* parent environment */
  choicePo cSB;                         /* where to cut to in parent */
} CallRec;

/* definition of a choice point record */
 
typedef struct _trail_rec_ {
  ptrPo var;                            /* variable that was bound */
  ptrI val;                             /* last value held by the variable */
} TrailRec;

typedef struct _choice_rec_ {
  integer AX;                           /* no. of arguments saved */
  insPo PC;                             /* next clause to try */
  insPo cPC;                            /* back up continuation pointer */
  choicePo B;                           /* previous choice point */
  choicePo cSB;                         /* continuation cut point */
  callPo C;                             /* back up call record */
  ptrI cPROG;                           /* continuation program  */
  ptrI PROG;                            /* current program  */
  trailPo trail;                        /* current trail level */
  choicePo T;				                    /* error recovery trap */
  ptrPo H;                              /* value of the heap stack at choice point */
} ChoiceRec;

/* Object and class interface */

typedef struct{
  long liveProcesses;			              /* Number of live processes */
  pthread_key_t processKey;		          /* Special key for thread specific */
} ProcessClassPartRec;


typedef struct {
  ptrPo sBase;                          /* Base of on-board stack */
  ptrPo sTop;                           /* Limit of on-board stack */
  HeapRec heap;                         /* Stack heap record */
  ptrI A[GO_REGS];                      /* Copies of the argument registers */
  choicePo B;                           /* Current choice point */
  choicePo SB;                          /* Where to cut to */
  choicePo cSB;                         /* Continuation slashback point */
  choicePo T;				/* Trap recovery */
  trailPo trail;                        /* Current trail point */
  callPo C;                             /* Current call frame */
  callPo cC;                            /* Continuation call frame */
  insPo PC;                             /* Current program counter */
  insPo cPC;                            /* Continuation pointer */
  ptrI cPROG;                           /* continuation program  */
  ptrI PROG;                            /* current program  */
  ptrI trigger;                         /* triggered variables */
  unsigned short F;			/* Trigger flag */

  byte errorMsg[1024];		// Current error message
  ptrI errorCode;			/* Error code of this process */

  pthread_t threadID;			/* What is the posix thread ID? */
  pthread_cond_t cond;			/* Condition variable attached  */
  ptrI thread;                          /* The thread structure of this process */
  void *cl;                             /* Client specific data */
  process_state state;                 /* What is the status of this process? */
  logical pauseRequest;	       /* Has a pause of this process been requested? */
  process_state savedState;		/* Saved state of this process? */
} ProcessRec;

/*
 *  Set up process as a lockable object
 */
typedef struct _process_ {
  ObjectRec object;
  LockObjectRec lock;
  ManagedRec managed;
  ProcessRec proc;
} ProcessObjectRecord;

typedef struct _process_class_ {
  ObjectClassRec object;
  LockClassPart lock;
  ManagedClassPartRec managedPart;   /* The managed part of the process class */
  ProcessClassPartRec proc;
} ProcessClassRec;

extern classPo processClass;

#ifdef VERIFY_OBJECT
#define O_PROCESS(c) ((processPo)(checkCast((c),processClass)))
#else
#define O_PROCESS(c) ((processPoPo)(c))
#endif
  
extern long LiveProcesses;              /* Number of executing processes */

typedef retCode (*procProc)(processPo p,void *cl);

extern ptrI commandLine(heapPo H);      /* command line arguments */
extern ptrI cmdLineOptions(heapPo H);

retCode processProcesses(procProc p,void *cl);
processPo getProcessOfThread(void);

pthread_t ps_threadID(processPo p);
processPo ps_suspend(processPo p,process_state reason);
processPo ps_resume(register processPo p, register logical fr); /* resume process */
void ps_kill(processPo p);		  /* kill process */

void switchProcessState(processPo P,process_state state);
void setProcessRunnable(processPo P);

void pauseAllThreads(pthread_t except);
void resumeAllThreads(pthread_t except);
logical checkForPause(processPo P);

void *ps_client(processPo p);	/* Get the process's client information */
void *ps_set_client(processPo p,void *cl);

process_state ps_state(processPo p);

processPo runerr(processPo); /* non-fatal error */

processPo rootProcess(ptrI thread,ptrI boot,string classPath);
void bootstrap(string entry,logical debug,string bootfile,string cwd);
	       
void displayProcesses(void);
void displayProcess(processPo p);
void stackTrace(processPo p);

retCode extendStack(processPo p,int sfactor,int hfactor,long hmin);

ptrI buildDieEnv(void);
ptrI buildRootEnv(void);
ptrI buildExitEnv(void);
ptrI buildLoaderEnv(void);

void verifyProc(processPo p);
void initThreadClass(void);

#endif
