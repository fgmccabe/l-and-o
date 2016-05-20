/*
 StringReader.c
 Copyright (c) 2016. Francis G. McCabe

 Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the
 License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied. See the License for the specific language governing
 permissions and limitations under the License.
 */

#include "stringReaderP.h"

static void initReaderClass(classPo class, classPo req);
static void ReaderDestroy(objectPo o);
static void ReaderInit(objectPo list, va_list *args);
static retCode readerInBytes(ioPo f, byte *ch, long count, long *actual);
static retCode readerBackByte(ioPo f, byte b);
static retCode readerAtEof(ioPo io);
static retCode readerInReady(ioPo f);
static retCode readerFlusher(ioPo f, long count);
static retCode readerSeek(ioPo f, long count);
static retCode readerClose(ioPo f);

ReaderClassRec ReaderClass =
  {
    { (classPo) &IoClass, /* parent class is io object */
    "reader", /* this is the reader class */
    NULL, initReaderClass, /* Reader class initializer */
    O_INHERIT_DEF, /* Reader object element creation */
    ReaderDestroy, /* Reader objectdestruction */
    O_INHERIT_DEF, /* erasure */
    ReaderInit, /* initialization of a reader object */
    sizeof(ReaderObject), /* size of a reader object */
    NULL, /* pool of reader values */
    O_INHERIT_DEF,                        // No special hash function
        O_INHERIT_DEF,                        // No special equality
        PTHREAD_ONCE_INIT, /* not yet initialized */
      PTHREAD_MUTEX_INITIALIZER },
    { },
    { readerInBytes, /* inByte  */
    NULL, /* outBytes  */
    readerBackByte, /* backByte */
    readerAtEof, /* at end of file? */
    readerInReady, /* readyIn  */
    NULL, /* readyOut  */
    readerFlusher, /* flush  */
    readerSeek, /* seek  */
    readerClose
    /* close  */
    } };

classPo strBufferClass = (classPo) &ReaderClass;

static void initReaderClass(classPo class, classPo req) {
}

// IO initialization should already be done at this point
static void ReaderInit(objectPo o, va_list *args) {
  readerPo f = O_READER(o);

  // Set up the buffer pointers
  f->reader.pos = 0;
  setEncoding(O_IO(f), va_arg(*args, ioEncoding)); /* set up the encoding */
  f->reader.buffer = va_arg(*args, byte*);
  f->reader.len = va_arg(*args, long); /* set up the buffer */
  f->io.mode = ioREAD; /* set up the access mode */
  f->io.encoding = utf8Encoding;
}

static void ReaderDestroy(objectPo o) {
  readerPo str = O_READER(o);
}

// Implement class reader functions

static retCode readerSeek(ioPo io, long count) {
  readerPo f = O_READER(io);

  if (count >= 0 && count < f->reader.pos) {
    f->reader.pos = count;
    return Ok;
  } else
    return Fail;
}

static retCode readerInBytes(ioPo io, byte *ch, long count, long *actual) {
  retCode ret = Ok;
  long remaining = count;
  readerPo f = O_READER(io);

  while (remaining > 0) {
    if (f->reader.pos >= f->reader.len) {
      if (remaining == count)
        ret = Eof;
      break;
    } else {
      *ch++ = f->reader.buffer[f->reader.pos++];
      remaining--;
    }
  }
  *actual = count - remaining;

  return ret;
}

static retCode readerBackByte(ioPo io, byte b) {
  readerPo f = O_READER(io);

  if (f->reader.pos > 0) {
    f->reader.buffer[--f->reader.pos] = b;
    return Ok;
  } else
    return Error;
}

static retCode readerAtEof(ioPo io) {
  readerPo f = O_READER(io);

  if (f->reader.pos < f->reader.len)
    return Ok;
  else
    return Eof;
}

static retCode readerInReady(ioPo io) {
  readerPo f = O_READER(io);

  if (f->reader.pos < f->reader.len)
    return Ok;
  else
    return Eof;
}

static retCode readerFlusher(ioPo io, long count) {
  return Ok;
}

static retCode readerClose(ioPo io) {
  destroyObject(O_OBJECT(io)); /* this will get rid of all the reader objects attributes */
  return Ok;
}

readerPo openStringReader(string buffer, long len, ioEncoding encoding) {
  byte strName[] =
    { '<', 's', 't', 'r', '>', 0 };
  return O_READER(newObject(strBufferClass, strName, encoding, buffer, len));
}

retCode rewindReader(readerPo in) {
  in->reader.pos = 0;
  in->io.inBpos = in->io.inCpos = 0;
  return Ok;
}
