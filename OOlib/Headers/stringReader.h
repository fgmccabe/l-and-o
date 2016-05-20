/*
  stringReader.h
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef OOLIB_HEADERS_STRINGREADER_H_
#define OOLIB_HEADERS_STRINGREADER_H_

#include "io.h"

typedef struct _string_reader_ *readerPo;
extern classPo readerClass;

readerPo openStringReader(string buffer, long len, ioEncoding encoding);

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c, classPo class);

#define O_READER(c) ((readerPo)(checkCast((c),readerClass)))
#else
#define O_READER(c) ((readerPo)(c))
#endif

#endif /* OOLIB_HEADERS_STRINGREADER_H_ */
