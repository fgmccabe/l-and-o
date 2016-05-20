/*
 * stringReaderP.h
 *
 *  Created on: May 19, 2016
 *      Author: fgm
 */

#ifndef OOLIB_HEADERS_STRINGREADERP_H_
#define OOLIB_HEADERS_STRINGREADERP_H_

#include "stringReader.h"
#include "ioP.h"

typedef struct {
} ReaderClassPartRec;

typedef struct _string_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;
  ReaderClassPartRec readerPart;
} ReaderClassRec;

extern ReaderClassRec ReaderClass;

typedef struct _string_reader_part_ { /* The string specific part of a string object */
  byte *buffer; /* The data buffer */
  long pos;
  long len;
} ReaderPart;

typedef struct _string_reader_ {
  ObjectRec object; /* object level of the io structure */
  IoPart io; /* Io level of io object */
  ReaderPart reader; /* Reader part */
} ReaderObject;

#endif /* OOLIB_HEADERS_STRINGREADERP_H_ */
