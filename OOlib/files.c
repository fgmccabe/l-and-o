/*
  File management functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"		/* Invoke configuration header */
#include "fileP.h"
#include "uri.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/dirent.h>

#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>		/* Where OPEN_MAX is defined */
#include <signal.h>

retCode rmFile(string name) {
  tryAgain:
  if (unlink((const char *) name) != -1)
    return Ok;
  else
    switch (errno) {
      case EINTR:
        goto tryAgain;
      default:
        return Error;
    }
}

retCode mvFile(string from, string to) {

  tryAgain:
  if (rename((const char *) from, (const char *) to) != -1)
    return Ok;
  else
    switch (errno) {
      case EINTR:
        goto tryAgain;
      default:
        return Error;
    }
}

retCode processDirectory(string url, dirProc proc, void *cl) {
  byte scheme[MAXLINE];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXFILELEN], frag[MAXLINE];
  long port;

  if (parseURI(url, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path),
               query, NumberOf(query), frag, NumberOf(frag)) == Ok &&
      uniIsLit(scheme, "file")) {
    struct stat buf;

    if (stat((const char*)path, &buf) == -1)
      return Error;    /* File not found */
    else if (!S_ISDIR(buf.st_mode))
      return Fail;
    else {
      DIR *dir = opendir((const char *) path);
      if (dir != NULL) {
        struct dirent *entry = readdir(dir);
        retCode ret = Ok;

        while (ret == Ok && entry != NULL) {
          byte dirName[MAXLINE];
          if (strncmp(entry->d_name, ".", entry->d_namlen) != 0 &&
              strncmp(entry->d_name, "..", entry->d_namlen) != 0) {
            int ix = 0;
            while (ix < entry->d_namlen && ix < MAXLINE - 1) {
              dirName[ix] = (byte)entry->d_name[ix];
            }
            dirName[ix] = 0;

            fileType type;
            switch (entry->d_type) {
              case DT_REG:
                type = regularFileType;
                break;
              case DT_DIR:
                type = dirFileType;
                break;
              case DT_LNK:
                type = linkFileType;
                break;
              case DT_SOCK:
                type = socketFileType;
                break;
              default:
                type = unknownFileType;
            }

            ret = proc(dirName, type, cl);
          }
          entry = readdir(dir);
        }
        closedir(dir);
        return ret;
      }
      else
        return Error;
    }
  }
  else
    return Error;
}

retCode isRegularFile(string fname) {
  struct stat buf;

  if (stat((const char *) fname, &buf) == -1)
    return Fail;    /* File not found */
  else if (S_ISDIR(buf.st_mode))
    return Fail;
  else
    return Ok;
}

retCode isDirectory(string name) {
  struct stat buf;

  if (stat((const char *) name, &buf) == -1)
    return Error;    /* File not found */
  else if (S_ISDIR(buf.st_mode))
    return Ok;
  else
    return Fail;
}

fileType typeOfFile(string url) {
  byte scheme[MAXLINE];
  byte user[MAXFILELEN], pass[MAXFILELEN];
  byte host[MAXFILELEN], path[MAXLINE];
  byte query[MAXFILELEN], frag[MAXLINE];
  long port;

  if (parseURI(url, scheme, NumberOf(scheme),
               user, NumberOf(user), pass, NumberOf(pass),
               host, NumberOf(host), &port, path, NumberOf(path),
               query, NumberOf(query), frag, NumberOf(frag)) == Ok &&
      uniIsLit(scheme, "file")) {
    struct stat buf;

    if (stat((const char*)path, &buf) == -1)
      return unknownFileType;    /* File not found */
    else if (S_ISDIR(buf.st_mode))
      return dirFileType;
    else if (S_ISREG(buf.st_mode))
      return regularFileType;
    else if (S_ISLNK(buf.st_mode))
      return linkFileType;
    else if (S_ISSOCK(buf.st_mode))
      return socketFileType;
  }
  return unknownFileType;
}

/* Special macro for Windows 95 */
#define FILE_ACCESS_MODE F_OK|R_OK

/* Check if a file is present or not */
logical filePresent(string name) {
  if (access((const char *) name, FILE_ACCESS_MODE) == 0)
    return True;
  else
    return False;
}
