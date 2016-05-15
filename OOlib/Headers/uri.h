#ifndef _IO_URI_H_
#define _IO_URI_H_

#include "unicode.h"
#include "io.h"

ioPo openURI(string uri, ioEncoding encoding);
ioPo createURI(string url, ioEncoding encoding);

string resolveURI(string base, string url, string buffer, long len);
string relativizeRI(string base, string url, string buffer, long len);
retCode checkRoot(string sys, string root, string user);
retCode parseURI(string uri, string scheme, long sLen,
                 string user, long uLen, string pass, long pLen,
                 string host, long hLen, long *port,
                 string path, long tLen,
                 string query, long qLen, string fragment, long fLen);
string defaultURI(string base);
string grabURI(string url);

typedef ioPo (*transducer)(string uri, ioEncoding encoding);

void registerTransducer(string scheme, transducer trans);

#endif
