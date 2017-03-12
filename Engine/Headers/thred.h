//
// Created by Francis McCabe on 2/5/17.
//

#ifndef LANDO_THREAD_H
#define LANDO_THREAD_H

#include "config.h"
#include "proc.h"
#include "word.h"
#include "lo.h"

typedef struct _thread_record_ {
  ptrI class;      /* == threadClass */
  processPo process;
} threadRec, *threadPo;

#define ThreadCellCount CellCount(sizeof(threadRec))

extern ptrI threadClass;    /* threadClass is a specialClass */

void initThreadClass(void);

void setProcess(ptrI T,processPo P);
processPo getProcessVal(ptrI P);
void clearProcess(ptrI P);

ptrI newThread(void);

#endif //LANDO_THREAD_H
