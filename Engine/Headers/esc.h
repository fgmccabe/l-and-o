/*
  Interface to the escape management functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/


#ifndef _ENGINE_ESC_H_
#define _ENGINE_ESC_H_

#include "word.h"

void install_escapes(void);
funpo escapeCode(uint16 code);
char *escapeName(int code);
int escapeOpcode(unsigned int code);
funpo getescape(int code);
logical validEscape(unsigned int code, unsigned short arity);
void showEscape(processPo P, char *prefix, int code, ptrPo args, long arity);
logical validEscape(unsigned int code, unsigned short arity);
void ScanEscapes(void);

void showCall(processPo P,ptrI prog,ptrPo args,long arity);
void showOCall(processPo P,ptrPo obj,ptrPo call,ptrPo this);


#undef escape
#define escape(name,secr,pr,spec,cmnt) \
extern retCode g_##name(processPo,ptrPo);

#include "escapes.h"
#undef escape

#endif
