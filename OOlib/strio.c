/*
  String file handling functions 
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"		/* pick up standard configuration header */
#include "formioP.h"

#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include "stringBuffer.h"

string strMsg(string buffer, long len, char *fmt, ...) {
  bufferPo f = fixedStringBuffer(buffer, len);

  va_list args;      /* access the generic arguments */
  va_start(args, fmt);    /* start the variable argument sequence */

  __voutMsg(O_IO(f), (unsigned char *) fmt, args);  /* Display into the string buffer */

  va_end(args);
  outByte(O_IO(f), 0);                /* Terminate the string */

  closeFile(O_IO(f));
  return buffer;
}

string strAppend(string buffer, long len, char *fmt, ...) {
  string buff = uniEndStr(buffer);
  long blen = len - (buff - buffer);

  bufferPo f = fixedStringBuffer(buffer, len);
  bufferStepForward(f, blen);

  va_list args;      /* access the generic arguments */
  va_start(args, fmt);    /* start the variable argument sequence */

  __voutMsg(O_IO(f), (unsigned char *) fmt, args);  /* Display into the string buffer */

  va_end(args);
  outChar(O_IO(f), '\0');                /* Terminate the string */

  closeFile(O_IO(f));
  return buffer;
}
