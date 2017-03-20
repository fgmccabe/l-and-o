/* 
  Symbol related definitions for the L&O engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _LO_SYMBOLS_H_
#define _LO_SYMBOLS_H_

#include <string.h>

/*
 Standard classes
*/
extern ptrI thingClass;
extern ptrI thingProg;
extern ptrI classClass;

extern ptrI errorClass;
extern ptrI filePtrClass;
extern ptrI udpPtrClass;

/* Standard symbols */

extern ptrI kvoid;
extern ptrI emptyList;

extern ptrI dieProg;                    /* program just dies */
extern ptrI exitProg;                   /* program that succeeds out of process */
extern ptrI trapProg;                   /* default trap handler */

extern ptrI trueClass,falseClass;

extern ptrI kmainThread;		            /* The main thread */

extern ptrI kstart;                     /* entry point for new threads */
extern ptrI kdelay;                     /* delay response handler */

extern ptrI doResume[LO_REGS];          /* array of resume exit points */

#endif

