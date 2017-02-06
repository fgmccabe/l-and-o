/* 
  Statistics collection interface
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _STATS_COLLECT_H_
#define _STATS_COLLECT_H_

#include "logical.h"		/* import a definition of true and false */
#include "integer.h"
#include "retcode.h"
#include "opcodes.h"		/* The definitions of the opcodes */

extern logical traceCount;	/* Are we counting instructions? */
extern long pcCount;		/* number of instructions executed */

extern void countEscape(insWord PCX);
void dumpInsCount(void);

extern long pcCount;
extern long insCount[256];
extern long escCount[256];


#endif /* _STATS_COLLECT_H_ */


