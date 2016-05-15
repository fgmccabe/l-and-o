/*
  URI management functions
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

#include "fileP.h"
#include "uri.h"
#include "iostr.h"
#include "hashTable.h"

#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <pwd.h>
#include <formio.h>

static hashPo transducers = NULL;

static ioPo openFileURI(string uri, ioEncoding encoding);

void initUri() {
  if (transducers == NULL) {
    transducers = NewHash(11, (hashFun) uniHash, (compFun) uniCmp, NULL);

    registerTransducer(uniDuplicate((unsigned char *) "file"), openFileURI);
  }
}

void registerTransducer(string scheme, transducer trans) {
  hashPut(transducers, uniDuplicate(scheme), trans);
}

static inline void copyOut(string s, string d, string buff, long len) {
  while (s < d && len-- > 0)
    *buff++ = *s++;

  if (len > 0)
    *buff = '\0';    /* terminate the output string */
}

static logical nonEmpty(string s) {
  return *s != 0 ? True : False;
}

string resolveURI(string base, string url, string buffer, long len) {
  byte scheme[MAXFILELEN];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXFILELEN], frag[MAXLINE];
  long port;

  if (parseURI(url, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path),
               query, NumberOf(query), frag, NumberOf(frag)) == Ok &&
      nonEmpty(scheme)) {
    copyOut(url, uniEndStr(url), buffer, len);
    return buffer;      /* The uri is already resolved */
  }
  else {
    byte bscheme[MAXLINE];
    byte buser[MAXFILELEN];
    byte bpass[MAXFILELEN];
    byte bhost[MAXFILELEN];
    byte bpath[MAXLINE];
    byte bquery[MAXLINE];
    byte bfrag[MAXLINE];
    long bport;
    string pt;

    if (parseURI(base, bscheme, NumberOf(bscheme),
                 buser, NumberOf(buser), bpass, NumberOf(bpass),
                 bhost, NumberOf(bhost), &bport, bpath, NumberOf(bpath),
                 bquery, NumberOf(bquery), bfrag, NumberOf(bfrag)) != Ok
        || uniIsLit(bscheme, "unknown")) {
      copyOut(url, uniEndStr(url), buffer, len);
      return buffer;
    }

    if ((pt = uniLast(bpath, NumberOf(bpath), '/')) != NULL)
      pt[0] = '\0';    /* chop off trailing stuff of base path */
    else {
      pt = &bpath[uniStrLen(bpath)];
      while (pt > bpath && *pt != '/' && *pt != ':')
        pt--;
      pt[0] = '\0';
    }

    if (uniStrLen(bhost) != 0) {
      if (bport != -1)
        strMsg(buffer, len, "%U://%U:%d/%U/%U", bscheme, bhost, bport, bpath, path);
      else
        strMsg(buffer, len, "%U://%U/%U/%U", bscheme, bhost, bpath, path);
    }
    else
      strMsg(buffer, len, "%U:%U/%U", bscheme, bpath, path);

    if (uniStrLen(query) != 0)
      strAppend(buffer, len, "?%U", query);

    if (uniStrLen(frag) != 0)
      strAppend(buffer, len, "#%U", frag);

    return buffer;
  }
}

ioPo openURI(string url, ioEncoding encoding) {
  byte scheme[MAXLINE];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXLINE], frag[MAXLINE];
  long port;

  if (parseURI(url, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path),
               query, NumberOf(query), frag, NumberOf(frag)) != Ok)
    return NULL;
  else {
    transducer trans = (transducer) hashGet(transducers, scheme);

    if (trans != NULL)
      return trans(url, encoding);
    else
      return NULL;
  }
}

ioPo createURI(string url, ioEncoding encoding) {
  byte scheme[MAXLINE];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXLINE], frag[MAXLINE];
  long port;

  if (parseURI(url, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path),
               query, NumberOf(query), frag, NumberOf(frag)) != Ok)
    return NULL;
  else {
    syserr("not implemented");
    return NULL;
  }
}

ioPo openFileURI(string uri, ioEncoding encoding) {
  byte scheme[MAXLINE];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXLINE], frag[MAXLINE];
  long port;

  if (parseURI(uri, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path),
               query, NumberOf(query), frag, NumberOf(frag)) == Ok)
    return openInFile(path, encoding);
  else
    return NULL;
}

string defaultURI(string base) {
  if (base != NULL)
    return base;
  else {
    static byte CWD[MAXLINE] = {0};

    if (uniStrLen(CWD) == 0) {
      char Buff[MAXLINE];
      char *pwd = getcwd(Buff, NumberOf(Buff));

      assert(pwd != NULL);

      strMsg(CWD, NumberOf(CWD), "file:%s/", pwd);
    }

    return CWD;
  }
}

retCode checkRoot(string sys, string root, string user) {
  byte rootScheme[MAXLINE];
  byte rootUser[MAXFILELEN], rootPass[MAXFILELEN];
  byte rootHost[MAXFILELEN], rootPath[MAXFILELEN];
  byte rootQuery[MAXLINE], rootFrag[MAXLINE];
  long rootPort;

  byte userScheme[MAXLINE];
  byte userUser[MAXFILELEN], userPass[MAXFILELEN];
  byte userHost[MAXFILELEN], userPath[MAXFILELEN];
  byte userQuery[MAXLINE], userFrag[MAXLINE];
  long userPort;

  if (parseURI(root, rootScheme, NumberOf(rootScheme),
               rootUser, NumberOf(rootUser),
               rootPass, NumberOf(rootPass),
               rootHost, NumberOf(rootHost), &rootPort,
               rootPath, NumberOf(rootPath),
               rootQuery, NumberOf(rootQuery),
               rootFrag, NumberOf(rootFrag)) != Ok)
    return Error;

  if (parseURI(user, userScheme, NumberOf(userScheme),
               userUser, NumberOf(userUser),
               userPass, NumberOf(userPass),
               userHost, NumberOf(userHost), &userPort,
               userPath, NumberOf(userPath),
               userQuery, NumberOf(userQuery),
               userFrag, NumberOf(userFrag)) != Ok)
    return Error;

  if (uniIsLit(userScheme, "sys")) {
    byte fname[MAXFILELEN];

    strMsg(fname, NumberOf(fname), "%U/%U", sys, userPath);
    return checkRoot(sys, root, fname);
  }
  else if (uniCmp(userScheme, rootScheme) == 0) {
    string uPath = userPath;

    /* In the first phase, we try to expand out the user path */
    if (*uPath == '~') {    /* Look for file relative to home */
      uPath++;
      if (*uPath == '/' || *uPath == '\0') {
        char *home = getenv("HOME");
        if (home != NULL) {
          uniInsert(uPath, NumberOf(userPath) - (uPath - userPath), (string) home);
        }
      }
      else {
        char *rest, usr[512];
        struct passwd *pwd;

        strncpy((char *) uPath, (char *) usr, NumberOf(usr));  /* Extract the user's name */
        if ((rest = strchr(usr, '/')) != NULL) {
          *rest = '\0';

          pwd = getpwnam(usr);

          if (pwd != NULL) {
            byte uHome[MAXFILELEN];
            strncpy(pwd->pw_dir, (char *) uHome, NumberOf(uHome));
            uniTack(uHome, NumberOf(uHome), "/");
            uniInsert(uPath, NumberOf(userPath) - (uPath - userPath), uHome); /* Append rest of name onto home dir */
          }
          else
            return Fail;                 /* no user! */
        }
        else
          return Fail;                  /* user but no file! */
      }
    }

    if (*uPath == '/') {      /* Absolute path name -- must match root */
      if (uniNCmp(uPath, rootPath, uniStrLen(rootPath)) != 0)
        return Fail;                    /* Absolute, but doesnt match root */
      else
        return Ok;                      /* We are OK */
    }
    else {        /* Relative file name */
      {
        string e = uniEndStr(rootPath);
        if (e != NULL && e > rootPath && e[-1] == '/')
          e[-1] = 0;                      /* chop off trailing / from cwd */
      }
      while (True) {      /* consume .. and . from file name */
        if (uniIsLitPrefix(uPath, "../")) {    /* We must back up one directory */
          string pt;

          if (uniStrLen(rootPath) == 0)
            return Fail;                /* we backed up too far */
          pt = uniLast(rootPath, NumberOf(rootPath), '/');
          if (pt == NULL)
            rootPath[0] = '\0';            /* The last entry in the CWD */
          else
            *pt = '\0';                   /* nibble off a piece of the CWD */
          uPath += 3;      /* step over the ../ segment */
          continue;                     /* there may be more than one ../ */
        }
        else if (uniIsLitPrefix(uPath, "./")) {
          uPath += 2;                     /* step over */
          continue;
        }
        else
          break;
      }
      return Ok;
    }
  }
  else
    return Fail;
}

retCode urlPresent(string sys, string url) {
  byte scheme[MAXLINE];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXLINE], frag[MAXLINE];
  long port;

  if (parseURI(url, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path),
               query, NumberOf(query), frag, NumberOf(frag)) != Ok)
    return Error;
  else {
    if (uniIsLit(scheme, "http"))
      return Error;                   // Not implemented yet
    else if (uniIsLit(scheme, "file") || uniIsLit(scheme, "unknown"))
      return filePresent(path) ? Ok : Fail;       // For now we ignore the host
    else if (uniIsLit(scheme, "sys")) {
      byte fname[MAXFILELEN];

      strMsg(fname, NumberOf(fname), "%U/%U", sys, path);
      return urlPresent(sys, fname);
    }
    else
      return Error;
  }
}

retCode rmURL(string sys, string url) {
  byte scheme[MAXLINE];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXLINE], frag[MAXLINE];
  long port;

  if (parseURI(url, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path),
               query, NumberOf(query), frag, NumberOf(frag)) != Ok)
    return Error;
  else {
    if (uniIsLit(scheme, "http"))
      return Error;                   // Not implemented yet
    else if (uniIsLit(scheme, "file") || uniIsLit(scheme, "unknown"))
      return rmFile(path);
    else if (uniIsLit(scheme, "sys")) {
      logMsg(logFile, "not permitted");
      return Error;
    }
    else
      return Error;
  }
}

retCode mvURL(string sys, string url, string nurl) {
  byte scheme[MAXLINE];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXLINE], frag[MAXLINE];
  byte nscheme[MAXLINE];
  byte nuser[MAXFILELEN], npass[MAXFILELEN];
  byte nhost[MAXFILELEN], npath[MAXLINE];
  byte nquery[MAXLINE], nfrag[MAXLINE];
  long port, nport;

  if (parseURI(url, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path), query, NumberOf(query),
               frag, NumberOf(frag)) != Ok)
    return Error;
  else if (parseURI(nurl, nscheme, NumberOf(nscheme),
                    nuser, NumberOf(nuser), npass, NumberOf(npass),
                    nhost, NumberOf(nhost), &nport, npath, NumberOf(npath),
                    nquery, NumberOf(nquery), nfrag, NumberOf(nfrag)) != Ok)
    return Error;
  else if (uniCmp(scheme, nscheme) != 0)
    return Error;
  else if (uniIsLit(scheme, "http"))
    return Error;                   // Not implemented
  else if (uniIsLit(scheme, "file") || uniIsLit(scheme, "unknown"))
    return mvFile(path, npath);
  else if (uniIsLit(scheme, "sys")) {
    logMsg(logFile, "not permitted");
    return Error;
  }
  else
    return Error;
}

string grabURI(string url) {
  ioPo in = openURI(url, utf8Encoding);
  ioPo str = O_IO(openStrOutput(url, utf8Encoding));
  uint64 len;
  string text;

  while (isFileAtEof(in) != Eof)
    outChar(str, inCh(in));
  outChar(str, 0);

  text = getStrText(O_STRING(str), &len);

  text = uniDuplicate(text);

  closeFile(in);
  closeFile(str);

  return text;
}
