//
// Created by Francis McCabe on 2/3/17.
//

#ifndef LANDO_ENCODEDP_H
#define LANDO_ENCODEDP_H

#include "encoded.h"

typedef struct _encoding_support_ {
  ptrPo vars;       /* The table of variables */
  long maxvar;      /* How big is the variable table */
  string errorMsg;  /* Place to put error messages */
  long msgSize;     /* How big is that buffer */
  heapPo R;         /* Where should the roots go? */
} EncodeSupport, *encodePo;

retCode decode(ioPo in, encodePo S, heapPo H, ptrPo tgt, bufferPo strBuffer);

retCode decInt(ioPo in, integer *ii);
retCode decFlt(ioPo in, double *dx);
retCode decodeText(ioPo in, bufferPo buffer);
retCode decodeName(ioPo in, bufferPo buffer);

typedef retCode (*intProc)(integer ix, void *cl);
typedef retCode (*fltProc)(double dx, void *cl);
typedef retCode (*nameProc)(string sx,void*cl);
typedef retCode (*stringProc)(string sx,integer len,void*cl);
typedef retCode (*strctProc)(string nm,integer ar,void*cl);
typedef retCode (*flagProc)(void *cl);

typedef struct {
  flagProc startDecoding;
  flagProc endDecoding;
  intProc decVar;
  intProc decInt;
  fltProc decFlt;
  nameProc decEnum;
  stringProc decString;
  strctProc decStruct;
  strctProc decPrg;
  intProc decCons;
} DecodeCallBacks, *decodeCallBackPo;

retCode streamDecode(ioPo in,decodeCallBackPo cb,void *cl);

#endif //LANDO_ENCODEDP_H
