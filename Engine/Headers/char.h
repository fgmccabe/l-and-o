/*
  Header file for the character management of Go! engine
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _GO_CHAR_H_
#define _GO_CHAR_H_

#include "word.h"

retCode wStringChr(ioPo f,codePoint ch);

retCode g_isCcChar(processPo P,ptrPo a);         /* Other, Control */
retCode g_isCfChar(processPo P,ptrPo a);         /* Other, Format */
retCode g_isCnChar(processPo P,ptrPo a);         /* Other, Unassigned */
retCode g_isCoChar(processPo P,ptrPo a);         /* Other, Private */
retCode g_isCsChar(processPo P,ptrPo a);         /* Other, surrogate */
retCode g_isLlChar(processPo P,ptrPo a);         /* Letter, lowercase */
retCode g_isLmChar(processPo P,ptrPo a);         /* Letter, modifier */
retCode g_isLoChar(processPo P,ptrPo a);         /* Letter, other */
retCode g_isLtChar(processPo P,ptrPo a);         /* Letter, titlecase */
retCode g_isLuChar(processPo P,ptrPo a);         /* Letter, uppercase */
retCode g_isMcChar(processPo P,ptrPo a);         /* Mark, spacing combining */
retCode g_isMeChar(processPo P,ptrPo a);         /* Mark, enclosing */
retCode g_isMnChar(processPo P,ptrPo a);         /* Mark, non spacing */
retCode g_isNdChar(processPo P,ptrPo a);         /* Number, decimal digit */
retCode g_isNlChar(processPo P,ptrPo a);         /* Number, letter */
retCode g_isNoChar(processPo P,ptrPo a);         /* Number, other */
retCode g_isPcChar(processPo P,ptrPo a);         /* Punctuation, connector */
retCode g_isPdChar(processPo P,ptrPo a);         /* Punctuation, dash */
retCode g_isPeChar(processPo P,ptrPo a);         /* Punctuation, close */
retCode g_isPfChar(processPo P,ptrPo a);         /* Punctuation, final quote */
retCode g_isPiChar(processPo P,ptrPo a);         /* Punctuation, initial quote */
retCode g_isPoChar(processPo P,ptrPo a);         /* Punctuation, other */
retCode g_isPsChar(processPo P,ptrPo a);         /* Punctution, open */
retCode g_isScChar(processPo P,ptrPo a);         /* Symbol, currency */
retCode g_isSkChar(processPo P,ptrPo a);         /* Symbol, modifier */
retCode g_isSmChar(processPo P,ptrPo a);         /* Symbol, math */
retCode g_isSoChar(processPo P,ptrPo a);         /* Symbol, other */
retCode g_isZlChar(processPo P,ptrPo a);         /* Separator, line */
retCode g_isZpChar(processPo P,ptrPo a);         /* Separator, paragraph */
retCode g_isZsChar(processPo P,ptrPo a);         /* Separator, space */
retCode g_isLetterChar(processPo P,ptrPo a);
retCode g_digitCode(processPo P,ptrPo a);
retCode g_charOf(processPo P,ptrPo a);

#endif
