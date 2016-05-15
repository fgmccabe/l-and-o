/*
  String file handling functions 
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"		/* pick up standard configuration header */
#include "ioStringP.h"
#include "formioP.h"

#include <assert.h>
#include <stdlib.h>
#include <ctype.h>

/* Set up the string file class */

static void initStringClass(classPo class, classPo req);

static void StringDestroy(objectPo o);

static void StringInit(objectPo list, va_list *args);

static retCode stringInBytes(ioPo f, byte *ch, long count, long *actual);

static retCode stringOutChar(ioPo f, codePoint ch);

static retCode stringUngetChar(ioPo f, codePoint ch);

static retCode stringOutBytes(ioPo f, byte *b, long count, long *actual);

static retCode stringBackByte(ioPo f, byte b);

static retCode stringAtEof(ioPo io);

static retCode stringInReady(ioPo f);

static retCode stringOutReady(ioPo f);

static retCode stringFlusher(ioPo f, long count);

static retCode stringSeek(ioPo f, long count);

static retCode stringClose(ioPo f);

StringClassRec StringClass = {
  {
    (classPo) &IoClass,                      /* parent class is io object */
    "string",                               /* this is the string class */
    NULL,
    initStringClass,                        /* String class initializer */
    O_INHERIT_DEF,                          /* String object element creation */
    StringDestroy,                          /* String objectdestruction */
    O_INHERIT_DEF,                          /* erasure */
    StringInit,                             /* initialization of a string object */
    sizeof(StringObject),                   /* size of a string object */
    NULL,                                  /* pool of string values */
    O_INHERIT_DEF,                        // No special hash function
    O_INHERIT_DEF,                        // No special equality
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {
    stringInBytes,                        /* inByte  */
    stringOutBytes,                       /* outBytes  */
    stringBackByte,                       /* backByte */
    stringAtEof,        /* at end of file? */
    stringInReady,                        /* readyIn  */
    stringOutReady,                       /* readyOut  */
    stringFlusher,                        /* flush  */
    stringSeek,                           /* seek  */
    stringClose                           /* close  */
  }
};

classPo strBufferClass = (classPo) &StringClass;

static void initStringClass(classPo class, classPo req) {
}

// IO initialization should already be done at this point
static void StringInit(objectPo o, va_list *args) {
  strBufPo f = O_STRING(o);

  // Set up the buffer pointers
  f->string.pos = 0;
  setEncoding(O_IO(f), va_arg(*args, ioEncoding)); /* set up the encoding */
  f->string.buffer = va_arg(*args, string);
  f->string.len = va_arg(*args, long);   /* set up the buffer */
  f->io.mode = va_arg(*args, ioState);   /* set up the access mode */
  f->string.resizeable = va_arg(*args, logical); /* is this string resizeable? */
}

static void StringDestroy(objectPo o) {
  strBufPo str = O_STRING(o);
  if (str->string.resizeable)
    free(str->string.buffer);
}

// Implement class string functions

static retCode stringSeek(ioPo io, long count) {
  strBufPo f = O_STRING(io);

  if (count >= 0 && count < f->string.pos) {
    f->string.pos = count;
    return Ok;
  }
  else
    return Fail;
}

static retCode stringInBytes(ioPo io, byte *ch, long count, long *actual) {
  retCode ret = Ok;
  long remaining = count;
  strBufPo f = O_STRING(io);

  while (remaining > 0) {
    if (f->string.pos >= f->string.len) {
      if (remaining == count)
        ret = Eof;
      break;
    }
    else {
      *ch++ = f->string.buffer[f->string.pos++];
      remaining--;
    }
  }
  *actual = count - remaining;

  return ret;
}

static retCode stringOutBytes(ioPo io, byte *b, long count, long *actual) {
  strBufPo f = O_STRING(io);

  if (f->string.pos + count >= f->string.len) {
    if (f->string.resizeable) {
      long nlen = f->string.len + (f->string.len >> 1) + count; /* allow for some growth */
      string nbuff = realloc(f->string.buffer, sizeof(byte) * nlen);
      if (nbuff != NULL) {
        f->string.buffer = nbuff;
        f->string.len = nlen;
      }
      else
        syserr("could not allocate more space for string");
    }
    else {
      return Ok;      /* Silently drop actual output */
    }
  }

  for (int ix = 0; ix < count; ix++)
    f->string.buffer[f->string.pos++] = b[ix];
  return Ok;
}

static retCode stringBackByte(ioPo io, byte b) {
  strBufPo f = O_STRING(io);

  if (f->string.pos > 0) {
    f->string.buffer[--f->string.pos] = b;
    return Ok;
  }
  else
    return Error;
}

static retCode stringAtEof(ioPo io) {
  strBufPo f = O_STRING(io);

  if (f->string.pos < f->string.len)
    return Ok;
  else
    return Eof;
}

static retCode stringInReady(ioPo io) {
  strBufPo f = O_STRING(io);

  if (f->string.pos < f->string.len)
    return Ok;
  else
    return Eof;
}

static retCode stringOutReady(ioPo io) {
  strBufPo f = O_STRING(io);

  if (f->string.pos < f->string.len)
    return Ok;
  else {
    if (f->string.resizeable)
      return Ok;
    else
      return Fail;
  }
}

static retCode stringFlusher(ioPo io, long count) {
  return Ok;
}

static retCode stringClose(ioPo io) {
  destroyObject(O_OBJECT(io)); /* this will get rid of all the string objects attributes */
  return Ok;
}

strBufPo openInStr(string buffer, long len, ioEncoding encoding) {
  byte strName[] = {'<', 's', 't', 'r', '>', 0};
  return O_STRING(newObject(strBufferClass, strName, encoding, buffer, len, ioREAD, False));
}

strBufPo openStrInput(string name, string buffer, long len, ioEncoding encoding) {
  return O_STRING(newObject(strBufferClass, name, encoding, buffer, len, ioREAD, False));
}

strBufPo openByteInput(string buffer, long len) {
  return O_STRING(newObject(strBufferClass, "", rawEncoding, buffer, len, ioREAD, False));
}

retCode rewindStr(strBufPo in) {
  in->string.pos = 0;
  in->io.inBpos = in->io.inCpos = 0;
  return Ok;
}

strBufPo openOutStr(ioEncoding encoding) {
  byte strName[] = {'<', 's', 't', 'r', '>', 0};
  string buffer = (string) malloc(sizeof(byte) * 128);

  return O_STRING(newObject(strBufferClass, strName, encoding, buffer, 128, ioWRITE, True));
}

strBufPo openStrOutput(string name, ioEncoding encoding) {
  int len = 128;      /* initial length of buffer */
  string buffer = (string) malloc(sizeof(byte) * len);
  return O_STRING(newObject(strBufferClass, name, encoding, buffer, len, ioWRITE, True));
}

strBufPo openBufferStr(string buffer, long len) {
  byte strName[] = {'<', 's', 't', 'r', '>', 0};

  return O_STRING(newObject(strBufferClass, strName, utf8Encoding, buffer, len, ioWRITE, False));
}

strBufPo openByteBuffer(string buffer,long len){
  byte strName[] = {'<', 's', 't', 'r', '>', 0};

  return O_STRING(newObject(strBufferClass, strName, rawEncoding, buffer, len, ioWRITE, False));

}

strBufPo openIoStr(ioEncoding encoding) {
  byte strName[] = {'<', 's', 't', 'r', '>', 0};
  string buffer = (string) malloc(sizeof(byte) * 128);

  return O_STRING(newObject(strBufferClass, strName, encoding, buffer, 128, ioREAD | ioWRITE, True));
}

retCode emptyOutStr(strBufPo str) {
  if (str->string.pos > 0)
    return Fail;
  else
    return Ok;
}

string getStrText(strBufPo s, uint64 *len) {
  *len = s->string.pos;

  return s->string.buffer;
}

long getStrPos(strBufPo s) {
  return s->string.pos;
}

string strMsg(string buffer, long len, char *fmt, ...) {
  strBufPo f = openBufferStr(buffer, len);

  va_list args;      /* access the generic arguments */
  va_start(args, fmt);    /* start the variable argument sequence */

  __voutMsg(O_IO(f), (unsigned char *) fmt, args);  /* Display into the string buffer */

  va_end(args);
  outChar(O_IO(f), '\0');                /* Terminate the string */

  closeFile(O_IO(f));
  return buffer;
}

string strAppend(string buffer, long len, char *fmt, ...) {
  string buff = uniEndStr(buffer);
  long blen = len - (buff - buffer);

  strBufPo f = openBufferStr(buff, blen);

  va_list args;      /* access the generic arguments */
  va_start(args, fmt);    /* start the variable argument sequence */

  __voutMsg(O_IO(f), (unsigned char *) fmt, args);  /* Display into the string buffer */

  va_end(args);
  outChar(O_IO(f), '\0');                /* Terminate the string */

  closeFile(O_IO(f));
  return buffer;
}
