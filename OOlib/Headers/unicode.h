/* 
  Unicode interface
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 

#ifndef _IO_UNICODE_H_
#define _IO_UNICODE_H_

#include "config.h"
#include "integer.h"
#include "retcode.h"
#include "logical.h"

typedef unsigned int32 codePoint; /* underlying code point is actually up to 20 bits */
typedef byte *string;  /* A string is a pointer to a utf8 sequence */

typedef enum {rawEncoding,
	      utf8Encoding,
        unknownEncoding} ioEncoding;

logical isChar(codePoint ch);	/* Is character a legal codePoint char? */

logical isCcChar(codePoint ch);
logical isCfChar(codePoint ch);
logical isCnChar(codePoint ch);
logical isCoChar(codePoint ch);
logical isCsChar(codePoint ch);
logical isLlChar(codePoint ch);
logical isLmChar(codePoint ch);
logical isLoChar(codePoint ch);
logical isLtChar(codePoint ch);
logical isLuChar(codePoint ch);
logical isMcChar(codePoint ch);
logical isMeChar(codePoint ch);
logical isMnChar(codePoint ch);
logical isNdChar(codePoint ch);
logical isNlChar(codePoint ch);
logical isNoChar(codePoint ch);
logical isPcChar(codePoint ch);
logical isPdChar(codePoint ch);
logical isPeChar(codePoint ch);
logical isPfChar(codePoint ch);
logical isPiChar(codePoint ch);
logical isPoChar(codePoint ch);
logical isPsChar(codePoint ch);
logical isScChar(codePoint ch);
logical isSkChar(codePoint ch);
logical isSmChar(codePoint ch);
logical isSoChar(codePoint ch);
logical isZlChar(codePoint ch);
logical isZpChar(codePoint ch);
logical isZsChar(codePoint ch);

logical isLetterChar(codePoint ch);
logical isSpaceChar(codePoint ch);
int digitValue(codePoint ch);

codePoint lowerOf(codePoint ch);
codePoint upperOf(codePoint ch);

long countCodePoints(string src,long start,long end);
long uniCodeCount(string src);

long advanceCodePoint(string src,long start,long end,long count);
retCode nxtPoint(string src,long *start,long end,codePoint *code);
retCode prevPoint(string src, long *pos, codePoint *code);

logical isUniIdentifier(string id);

unsigned long uniStrLen(const string s);
retCode uniCpy(string dest,long len,const string src);
retCode uniNCpy(string dest,long len,const string src,long sLen);
int uniCmp(string s1,string s2);
int uniNCmp(string s1,string s2,long l);
retCode uniInsert(string dest,long len,const string src);
retCode appendCodePoint(string dest, long *pos, long len, codePoint ch);
retCode uniTack(string dest,long len,const char *src);
retCode uniAppend(string dest, long *pos, long len, string src);

long uniIndexOf(string s,long len,long from,codePoint c);
long uniLastIndexOf(string s,long len,codePoint c);
string uniSubStr(string s,long len,long from,long cnt,string buff,long bLen);

string uniSearchAny(string s,long len,string term);
string uniLast(string s,long l,codePoint c);
string uniDuplicate(string s);
logical uniIsLit(string s1,char *s2);
logical uniIsLitPrefix(string s1,char *s2);
string uniEndStr(string s);
uinteger uniHash(const string name);
retCode uniLower(string s,long sLen,string d,long dLen);

#ifndef uniEOF
#define uniEOF (0xffff)
#endif

#ifndef uniBOM                          // Byte Order mark
#define uniBOM (0xfeff)
#define uniBOMhi (0xfe)
#define uniBOMlo (0xff)
#endif


#ifndef uniSentinel                     // This marks a stream as a UTF16
#define uniSentinel (0xfeff)
#endif

#ifndef uniRevSentinel                  // This marks a stream as a byte swapped UTF16
#define uniRevSentinel (0xfffe)
#endif

#endif
