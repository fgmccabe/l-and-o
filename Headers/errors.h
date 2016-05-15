/* 
  Standard error number definitions
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"		/* pick up standard configuration header */

void initErrorSymbols(void);
retCode liberror(processPo P,char *name,ptrI code);
ptrI errorString(heapPo H,ptrI code);

extern ptrI eINSUFARG,                  // Insufficiently instatiated argument
  eINTNEEDD,                            /* Integer required */
  eNUMNEEDD,                            /* Number required */
  eVARNEEDD,                            /* Unbound variable required */
  eSPACE,                               /* Out of heap space */
  eUNIFY,                               /* Incomparible values in unification */
  eOCCUR,                               // Occurs check
  eCODE,                                // Attempt to execute unbound variable
  eDIVZERO,                             /* Division by zero */
  eLSTNEEDD,                            /* List needed */
  eTPLNEEDD,                            /* Tuple needed */
  eCHRNEEDD,                            /* Character needed */
  eSTRNEEDD,                            /* String needed */
  eSYMNEEDD,                            /* Symbol needed */
  eINVAL,                               /* invalid argument */
  eRANGE,                               /* out of range argument */
  eNOPERM,                              /* permission denied */
  eNOFILE,                              /* file not found */
  eNOTDIR,				/* not a directory */
  eCFGERR,                              /* configuration problem */
  eEOF,                                 /* read past end-of-file */
  eIOERROR,                             /* Error on i/o */
  eABORT,                               /* Abort process */
  eNOTFND,                              /* Not found */
  eCONNECT,                             /* Problem with connection */
  eHANDLE,                              // Not a legal handle
  eFAIL,                                /* unexpected failure */
  eINVCODE,                             // Invalid code type
  eASSIGN,                              // invalid attempt at assignment
  eSYSTEM,                              // system overflow
  eDEAD,                                // Deadlock detected
  eTIME,                                // Timeout detected
  eDUPLICATE,                           // Duplicate requested
  eNOIMPL,                              // Not implemented
  eNOTENUF,                             /* not enough arguments */
  eINTRUPT;				// Interrupted

#ifdef GOXLIB
extern ptrI eNOX;			/* no X connection */
#endif


