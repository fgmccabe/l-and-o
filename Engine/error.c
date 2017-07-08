/* 
  Error handling and exiting functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "lo.h"

ptrI errorClass;      /* standard error class */

/* error symbols */
ptrI eINSUFARG,             /* Insufficiently instatiated argument */
  eINTNEEDD,        /* Integer required */
  eNUMNEEDD,        /* Number required */
  eVARNEEDD,        /* Unbound variable required */
  eSPACE,        /* Out of heap space */
  eUNIFY,        /* Incomparible values in unification */
  eOCCUR,        // Occurs check violation
  eCODE,        // Attempt to execute unbound variable
  eDIVZERO,        /* Division by zero */
  eLSTNEEDD,        /* List needed */
  eTPLNEEDD,        /* Tuple needed */
  eSYMNEEDD,        /* Symbol needed */
  eCHRNEEDD,        /* Character needed */
  eINVAL,        /* invalid argument */
  eRANGE,        /* out of range of tuple */
  eNOPERM,        /* permission denied */
  eNOFILE,        /* file not found */
  eNOTDIR,        /* not a directory */
  eCFGERR,        /* configuration problem */
  eEOF,          /* read past end-of-file */
  eIOERROR,        /* Error on i/o */
  eABORT,        /* Abort process */
  eNOTFND,        /* Not found */
  eCONNECT,        /* Cant connect */
  eFAIL,        /* unexpected failure */
  eSTRNEEDD,        /* String needed */
  eHANDLE,        // Not a legal handle
  eINVCODE,        // Invalid code type
  eASSIGN,        // Invalid assignment
  eSYSTEM,        // system overflow
  eDEAD,        // Deadlock detected
  eTIME,        // timeout detected
  eDUPLICATE,        // Duplicate request
  eNOIMPL,        /* feature not implemented*/
  eNOTENUF,                             /* not enough arguments */
  eINTRUPT;        // Interrupted

void initErrorSymbols(void) {
  // Standard error codes
  eINSUFARG = newEnumSym("eINSUFARG");
  eVARNEEDD = newEnumSym("eVARNEEDD");
  eINTNEEDD = newEnumSym("eINTNEEDD");
  eNUMNEEDD = newEnumSym("eNUMNEEDD");
  eSPACE = newEnumSym("eSPACE");
  eUNIFY = newEnumSym("eUNIFY");
  eOCCUR = newEnumSym("eOCCUR");
  eCODE = newEnumSym("eCODE");
  eDIVZERO = newEnumSym("eDIVZERO");
  eLSTNEEDD = newEnumSym("eLSTNEEDD");
  eTPLNEEDD = newEnumSym("eTPLNEEDD");
  eSYMNEEDD = newEnumSym("eSYMNEEDD");
  eCHRNEEDD = newEnumSym("eCHRNEEDD");
  eSTRNEEDD = newEnumSym("eSTRNEEDD");
  eHANDLE = newEnumSym("eHANDLE");
  eINVAL = newEnumSym("eINVAL");
  eRANGE = newEnumSym("eRANGE");
  eNOPERM = newEnumSym("eNOPERM");
  eNOFILE = newEnumSym("eNOFILE");
  eNOTDIR = newEnumSym("eNOTDIR");
  eCFGERR = newEnumSym("eCFGERR");
  eEOF = newEnumSym("eEOF");
  eIOERROR = newEnumSym("eIOERROR");
  eABORT = newEnumSym("eABORT");
  eNOTFND = newEnumSym("eNOTFND");
  eCONNECT = newEnumSym("eCONNECT");
  eFAIL = newEnumSym("eFAIL");
  eINVCODE = newEnumSym("eINVCODE");
  eASSIGN = newEnumSym("eASSIGN");
  eSYSTEM = newEnumSym("eSYSTEM");
  eDEAD = newEnumSym("eDEAD");
  eTIME = newEnumSym("eTIME");
  eDUPLICATE = newEnumSym("eDUPLICATE");
  eNOIMPL = newEnumSym("eNOIMPL");
  eNOTENUF = newEnumSym("eNOTENUF");
  eINTRUPT = newEnumSym("eINTRUPT");

  errorClass = newClassDef((char *) "lo.stdlib#error", 2);
}

ptrI errorString(heapPo H, ptrI code) {
  if (!isvar(code)) {
    if (code == eINSUFARG)
      return allocateCString(H, "Insufficiently instantiated argument");
    else if (code == eINTNEEDD)
      return allocateCString(H, "Integer required");
    else if (code == eNUMNEEDD)
      return allocateCString(H, "Number required");
    else if (code == eVARNEEDD)
      return allocateCString(H, "Unbound variable required");
    else if (code == eSPACE)
      return allocateCString(H, "Out of heap space");
    else if (code == eUNIFY)
      return allocateCString(H, "Incomparible values in unification");
    else if (code == eDIVZERO)
      return allocateCString(H, "Division by zero");
    else if (code == eLSTNEEDD)
      return allocateCString(H, "List needed");
    else if (code == eTPLNEEDD)
      return allocateCString(H, "Tuple needed");
    else if (code == eSYMNEEDD)
      return allocateCString(H, "Symbol needed");
    else if (code == eSTRNEEDD)
      return allocateCString(H, "String required");
    else if (code == eCHRNEEDD)
      return allocateCString(H, "Character required");
    else if (code == eINVAL)
      return allocateCString(H, "invalid argument");
    else if (code == eNOPERM)
      return allocateCString(H, "permission denied");
    else if (code == eNOFILE)
      return allocateCString(H, "file not found");
    else if (code == eNOTDIR)
      return allocateCString(H, "not a directory");
    else if (code == eCFGERR)
      return allocateCString(H, "configuration problem");
    else if (code == eEOF)
      return allocateCString(H, "read past end-of-file");
    else if (code == eIOERROR)
      return allocateCString(H, "error on i/o");
    else if (code == eABORT)
      return allocateCString(H, "process aborted");
    else if (code == eNOTFND)
      return allocateCString(H, "not found");
    else if (code == eCODE)
      return allocateCString(H, "undefined program");
    else if (code == eFAIL)
      return allocateCString(H, "unexpected failure");
    else if (code == eHANDLE)
      return allocateCString(H, "not a valid handle");
    else if (code == eINVCODE)
      return allocateCString(H, "incorrect code type");
    else if (code == eASSIGN)
      return allocateCString(H, "assignment not allowed");
    else if (code == eDEAD)
      return allocateCString(H, "deadlock detected");
    else if (code == eSYSTEM)
      return allocateCString(H, "system overflow");
    else if (code == eDUPLICATE)
      return allocateCString(H, "duplicate request");
    else if (code == eNOIMPL)
      return allocateCString(H, "feature not implemented");
    else if (code == eNOTENUF)
      return allocateCString(H, "insufficient arguments given");
    else if (code == eCONNECT)
      return allocateCString(H, "cannot connect to host");
    else if (code == eINTRUPT)
      return allocateCString(H, "interrupted");
    else {
      char buf[MAX_MSG_LEN];

      strMsg(buf, NumberOf(buf), "Unknown error code: %w", &code);
      return allocateString(H, buf, uniStrLen(buf));
    }
  } else {
    char buf[MAX_MSG_LEN];

    strMsg(buf, NumberOf(buf), "Invalid error code: %w", &code);
    return allocateString(H, buf, uniStrLen(buf));
  }
}

retCode g__errorcode(processPo P, ptrPo a) {
  ptrI S = errorString(&P->proc.heap, deRefI(&a[1]));

  return equal(P, &S, &a[2]);
}



