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
  trmClass = 'o',
  trmPrg = 'p',
  trmTpl = 'u',
  trmCns = 'n'
} trmSig;

#endif
