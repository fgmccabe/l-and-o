//
// Created by Francis McCabe on 2/3/17.
//

#ifndef LANDO_ENCODEDP_H
#define LANDO_ENCODEDP_H

#include "encoded.h"

typedef struct _encoding_support_ {
  ptrPo lbls;       /* The table of labels */
  long maxlbl;      /* How big is our table */
  ptrPo vars;       /* The table of variables */
  long maxvar;      /* How big is the variable table */
  string errorMsg;  /* Place to put error messages */
  long msgSize;     /* How big is that buffer */
  heapPo R;         /* Where should the roots go? */
} EncodeSupport, *encodePo;

retCode decode(ioPo in, encodePo S, heapPo H, ptrPo tgt, bufferPo strBuffer);

retCode decInt(ioPo in, encodePo S, integer *ii);
retCode decFlt(ioPo in, encodePo S, double *dx);
retCode decodeText(ioPo in, bufferPo buffer);
retCode decodeName(ioPo in, bufferPo buffer);

retCode skipTrm(ioPo in, encodePo S);

#endif //LANDO_ENCODEDP_H
