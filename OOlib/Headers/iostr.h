/*
  String I/O handling functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _IO_STR_H_
#define _IO_STR_H_

#include "io.h"

typedef struct _string_buffer_ *strBufPo;
extern classPo strBufferClass;

strBufPo openInStr(string buffer, long len, ioEncoding encoding);
strBufPo openStrOutput(string name,ioEncoding encoding);
strBufPo openByteBuffer(string buffer,long len);
strBufPo openBufferStr(string buffer, long len);
strBufPo openByteInput(string buffer, long len);

string strMsg(byte *buffer,long len,char *fmt,...);
string strAppend(byte *buffer,long len,char *fmt,...);
string getStrText(strBufPo f,uint64 *len);
retCode emptyOutStr(strBufPo f);
retCode rewindStr(strBufPo in);
long getStrPos(strBufPo s);

#ifdef VERIFY_OBJECT
objectPo checkCast(void *c,classPo class);

#define O_STRING(c) ((strBufPo)(checkCast((c),strBufferClass)))
#else
#define O_STRING(c) ((strBufPo)(c))
#endif

#endif
