#ifndef _UNICODE_P_H
#define _UNICODE_P_H

/*
  Unicode interface
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "unicode.h"

/* The various ranges in the UTF-8 encoding are as follows:
  
  0x00000000 - 0x0000007F:
  0xxxxxxx

  0x00000080 - 0x000007FF:
  110xxxxx 10xxxxxx
*/
#define U80 ((0x6)<<5)
#define M80 ((0x7)<<5)
#define UC80(x) (((x)&M80)==U80)
#define UX80(x) ((x)&(~M80))

#define UR ((0x2)<<6)
#define MR ((0x3)<<6)
#define UCR(x) (((x)&MR)==UR)
#define UXR(x) ((x)&(~MR))

/*

  0x00000800 - 0x0000FFFF:
  1110xxxx 10xxxxxx 10xxxxxx

*/
#define U800 ((0xe)<<4)
#define M800 ((0xf)<<4)
#define UC800(x) (((x)&M800)==U800)
#define UX800(x) ((x)&(~M800))

/*
 0x10000 - 0x1fffff
*/
#define U1000 ((0xf0)<<3)
#define M1000 (0x7)
#define UC1000(x) (((x)&M1000)==U1000)
#define UX1000(x) ((x)&(~M1000))
/*
  The  xxx  bit  positions  are  filled with the bits of the
  character code number in binary representation.  Only  the
  shortest  possible  multibyte sequence which can represent
  the code number of the character can be used.
*/

 typedef enum{
  Cc,  Cf,  Cn,  Co,  Cs,
  Ll,  Lm,  Lo,  Lt,  Lu,
  Mc,  Me,  Mn,
  Nd,  Nl,  No,
  Pc,  Pd,  Pe,  Pf,  Pi,  Po,  Ps,
  Sc,  Sk,  Sm,  So,
  Zl,  Zp,  Zs, Other
} UniCharCategory;

extern UniCharCategory genCatTbl[];

extern long uniByteLen(const string s);

#define MAXUNICODE (1<<20)

#endif
