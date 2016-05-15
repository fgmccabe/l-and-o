/* 
  String File library (private header)
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 

#ifndef _IO_STRING_P_H_
#define _IO_STRING_P_H_

#include "ioP.h"
#include "iostr.h"
#include <stdarg.h>

typedef struct {
} StringClassPartRec;

typedef struct _string_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;              /* the io part of the class information */
  StringClassPartRec stringPart;
} StringClassRec;

extern StringClassRec StringClass; /* the standard pointer to an String class record */

typedef struct _string_part_{           /* The string specific part of a string object */
  byte *buffer;                         /* The data buffer */
  long pos;
  long len;
  logical resizeable;                   /* Is this string object resizeable? */
} StringPart;

typedef struct _string_buffer_ {
  ObjectRec object;                     /* object level of the io structure */
  IoPart io;                            /* Io level of io object */
  StringPart string;                    /* String level of string object */
} StringObject;

#endif
