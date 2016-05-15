/*
  URI parsing functions
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

#include "config.h"
#include "uri.h"
#include "unicode.h"
#include <stdlib.h>

static int defaultPort(string scheme);

static void clear(string text, long len);

retCode parseURI(string uri, string scheme, long sLen,
                 string user, long uLen, string pass, long pLen,
                 string host, long hLen, long *port,
                 string path, long tLen,
                 string query, long qLen, string fragment, long fLen) {
  long uriLen = uniStrLen(uri);
  long colonPos = uniIndexOf(uri, uriLen, 0, ':');

  if (colonPos > 0) {
    uniSubStr(uri, uriLen, 0, colonPos, scheme, sLen); /* Extract the scheme */
    uri = &uri[colonPos + 1];
    uriLen -= colonPos + 1;      /* bump the start of the uri */

    if (uniIsLitPrefix(uri, "//")) {
      uri = &uri[2];
      uriLen -= 2;      /* move over the // */
      // We have an authority ...
      long atPos = uniIndexOf(uri, uriLen, 0, '@');
      if (atPos > 0) {
        uniSubStr(uri, uriLen, 2, atPos, user, uLen);
        uri = &uri[atPos + 1];
        uriLen -= atPos + 1;
      }
      else
        clear(user, uLen);    /* No user component */
      long portPos = uniIndexOf(uri, uriLen, 0, ':');
      if (portPos >= 0) {
        uniSubStr(uri, uriLen, 0, portPos++, host, hLen);
        long p = 0;
        while (portPos < uriLen && isNdChar(uri[portPos]))
          p = p * 10 + digitValue(uri[portPos++]);
        *port = p;
        uri = &uri[portPos];
        uriLen -= portPos;
      }
      else {
        long slPos = uniIndexOf(uri, uriLen, 0, '/');
        if (slPos > 0) {      /* We found a slash ... */
          uniSubStr(uri, uriLen, 0, slPos, host, hLen);
          uri = &uri[slPos];
          uriLen -= slPos;
        }
        else {        /* No path or anything */
          uniNCpy(host, hLen, uri, uriLen);
          uri = &uri[uriLen];
          uriLen = 0;
        }
        *port = defaultPort(scheme);
      }
    }
    else {        /* No authority */
      clear(user, uLen);
      clear(host, hLen);
      *port = defaultPort(scheme);
    }
  }
  else {          /* unknown or implicit scheme */
    clear(scheme, sLen);
    clear(user, uLen);
    clear(host, hLen);
    *port = -1;
  }

  long queryPos = uniIndexOf(uri, uriLen, 0, '?'); /* Is there a query? */
  if (queryPos > 0) {
    uniSubStr(uri, uriLen, 0, queryPos, path, pLen);
    uri = &uri[queryPos + 1];
    uriLen -= queryPos + 1;

    long fragPos = uniIndexOf(uri, uriLen, 0, '#'); /* Is there a fragment? */
    if (fragPos > 0) {
      uniSubStr(uri, uriLen, 0, fragPos, query, qLen);
      uniSubStr(uri, uriLen, fragPos + 1, uriLen - fragPos - 1, fragment, fLen);
    }
    else {
      clear(fragment, fLen);
      uniNCpy(query, qLen, uri, uriLen);
    }
  }
  else {          /* No query, is there a fragment? */
    long fragPos = uniIndexOf(uri, uriLen, 0, '#');
    clear(query, qLen);
    if (fragPos > 0) {
      uniSubStr(uri, uriLen, 0, fragPos, path, pLen);
      uniSubStr(uri, uriLen, fragPos + 1, uriLen - fragPos - 1, fragment, fLen);
    }
    else {
      uniNCpy(path, pLen, uri, uriLen);
      clear(fragment, fLen);
    }
  }
  return Ok;
}

static void clear(string txt, long len) {
  *txt = 0;
}

static int defaultPort(string scheme) {
  if (uniIsLit(scheme, "http"))
    return 80;
  else if (uniIsLit(scheme, "ftp"))
    return 21;
  else if (uniIsLit(scheme, "nntp"))
    return 119;
  else if (uniIsLit(scheme, "telnet"))
    return 23;
  else if (uniIsLit(scheme, "gopher"))
    return 70;        // default port for GOPHER
  else if (uniIsLit(scheme, "wais"))
    return 210;
  else if (uniIsLit(scheme, "prospero"))
    return 1525;
  else
    return -1;
}
