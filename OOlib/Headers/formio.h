/* 
  High level I/O handling functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _IO_FORMIO_H_
#define _IO_FORMIO_H_

#include "io.h"

retCode outInteger(ioPo f, integer i, uint16 base, int width, int precision,
                   codePoint pad, logical left, char *prefix, logical sign);
retCode outDouble(ioPo out, double x, char mode, int width, int precision,
                  codePoint pad, logical left, char *prefix, logical sign);
retCode formattedFloat(double dx, char *out, long *endPos, long outLen, char *frmt, long formatLen);
retCode formattedLong(integer ix, char *out, long *endPos, long outLen, char *frmt, long formatLen);
retCode outMsg(ioPo f, char *fmt, ...);
retCode logMsg(ioPo out, char *fmt, ...);

retCode outUniString(ioPo f, char *str, long len, long width, int precision,
                     codePoint pad, logical leftPad, logical alt);
retCode outInt(ioPo f, integer i);
retCode outFloat(ioPo out, double x);
retCode outUStr(ioPo f, char *str);

long int2StrByBase(char *str, integer i, long pos, uint16 base);

typedef enum {
  fractional, scientific, general
} FloatDisplayMode;

retCode formatDouble(char *out, long outLen, double x, FloatDisplayMode displayMode, int precision, char *prefix,
                     logical sign);

integer parseInteger(char *s, long len);
double parseNumber(char *s, long len);

char *strMsg(char *buffer, long len, char *fmt, ...);
char *strAppend(char *buffer, long len, char *fmt, ...);

#endif
