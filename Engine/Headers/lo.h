/* 
  Public header file
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _LO_H_
#define _LO_H_

#include <stdlib.h>
#include "config.h"
#include "dbgflags.h"		/* standard debugging flags */
#include "local.h"		  /* localization */
#include "retcode.h"
#include "unicode.h"
#include "file.h"
#include "formio.h"
#include "stringBuffer.h"

#include "word.h"		    /* standard definition of a cell & access fns */
#include "char.h"

#include "heap.h"
#include "process.h"

#include "global.h"
#include "symbols.h"		/* standard symbols available to the engine */
#include "errors.h"		  /* standard error codes */
#include "str.h"        /* String manipulation */
#include "arith.h"
#include "code.h"
#include "list.h"
#include "vars.h"
#include "eval.h"



#ifndef NULL
#define NULL            ((void*)0) /* The NULL pointer */
#endif

#undef NumberOf
#define NumberOf(a) (sizeof(a)/sizeof(a[0]))

#undef ElementSize
#define ElementSize(a) (sizeof(a[0]))

typedef retCode (*funpo)(processPo p,ptrPo args); /* Escape function pointer */

void runGo(register processPo P);

#include "dict.h"
#include "signals.h"

#define EXIT_SUCCEED 0		/* Normal exit */
#define EXIT_FAIL 1		    /* Failing exit */

#endif
