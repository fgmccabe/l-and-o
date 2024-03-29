/*
  Term encoding format definitions
  (c) 2000-2017 F.G. McCabe

  Contact: Francis McCabe <frankmccabe@mac.com>
*/

#ifndef _TERM_ENCODED_H_
#define _TERM_ENCODED_H_

typedef enum {
  trmAnon = 'a',
  trmVar = 'v',
  trmInt = 'x',
  trmFlt = 'd',
  trmSym = 'e',
  trmString = 's',
  trmStrct = 'o',
  trmPrg = 'p',
  trmCns = 'n'
} trmSig;

retCode decodeTerm(ioPo in, heapPo H, heapPo R, ptrPo tgt, char * errorMsg, long msgSize);
retCode skipEncoded(ioPo in, char * errorMsg, long msgLen);
retCode copyEncoded(ioPo in, ioPo out, char * errorMsg, long msgLen);

retCode encodeInt(ioPo out,integer ix);
retCode encodeFlt(ioPo out,double dx);
retCode encodeEnum(ioPo out,char * sx);
retCode encodeStrng(ioPo out,char * dx,integer len);
retCode encodeStrct(ioPo out, char * sx, integer ar);
retCode encodePrg(ioPo out, char * sx, integer ar);
retCode encodeCons(ioPo out, integer ar);

#endif
