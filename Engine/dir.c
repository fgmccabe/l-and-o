/* 
  Directory and file handling functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>
#include <dirent.h>

#include <sys/stat.h>
#include <errno.h>

#include "lo.h"

// Report current directory
retCode g__cwd(processPo P, ptrPo a) {
  switchProcessState(P, wait_io);
  char *cwd = getcwd(NULL, 0);           /* compute current working directory */
  setProcessRunnable(P);

  if (cwd != NULL) {
    ptrI CWD = allocateCString(&P->proc.heap, cwd);

    free(cwd);
    return equal(P, &CWD, &a[1]);
  } else
    return Error;
}

// Change current directory
retCode g__cd(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__cd", eSTRNEEDD);
  else {
    tryAgain:
    switchProcessState(P, wait_io);

    char *strData = (char *) stringVal(stringV(t1));

    if (chdir(strData) != -1) {
      setProcessRunnable(P);
      return Ok;
    } else {
      setProcessRunnable(P);

      switch (errno) {
        case EINTR:
          goto tryAgain;
        case EACCES:
        case EPERM:
          return liberror(P, "__cd", eNOPERM);
        case EBUSY:
          return liberror(P, "__cd", eFAIL);
        case ENOTDIR:
          return liberror(P, "__cd", eNOTDIR);
        case ENOENT:
        case ELOOP:
          return liberror(P, "__cd", eINVAL);
        default:
          return liberror(P, "__cd", eIOERROR);
      }
    }
  }
}

// Delete a file

retCode g__rm(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__rm", eSTRNEEDD);
  else {
    tryAgain:
    switchProcessState(P, wait_io);

    char *strData = (char *) stringVal(stringV(t1));
    if (unlink(strData) != -1) {
      setProcessRunnable(P);
      return Ok;
    } else {
      setProcessRunnable(P);
      switch (errno) {
        case EINTR:
          goto tryAgain;
        case EACCES:
        case EPERM:
          return liberror(P, "__rm", eNOPERM);
        case EBUSY:
          return liberror(P, "__rm", eFAIL);
        case ENOENT:
        default:
          return liberror(P, "__rm", eIOERROR);
      }
    }
  }
}

// Rename file

retCode g__mv(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);
  ptrI t2 = deRefI(&a[2]);

  if (!IsString(t1) || !IsString(t2))
    return liberror(P, "__mv", eSTRNEEDD);
  else {
    tryAgain:
    switchProcessState(P, wait_io);

    char *fn1 = (char *) stringVal(stringV(t1));
    char *fn2 = (char *) stringVal(stringV(t2));

    if (rename(fn1, fn2) != -1) {
      setProcessRunnable(P);
      return Ok;
    } else {
      setProcessRunnable(P);
      switch (errno) {
        case EINTR:
          goto tryAgain;
        case EACCES:
          return liberror(P, "__mv", eNOPERM);
        case EBUSY:
        case ENOENT:
          return liberror(P, "__mv", eFAIL);
        default:
          return liberror(P, "__mv", eIOERROR);
      }
    }
  }
}

/*
 * mkdir : create a new directory
 */

retCode g__mkdir(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);
  ptrI t2 = deRefI(&a[2]);

  if (!IsString(t1))
    return liberror(P, "__mkdir", eSTRNEEDD);
  else {
    mode_t acmode = (mode_t) integerVal(intV(t2));

    tryAgain:
    switchProcessState(P, wait_io);

    char *str = (char *) stringVal(stringV(t1));

    if (mkdir(str, acmode) == -1) {
      setProcessRunnable(P);
      switch (errno) {
        case EINTR:
          goto tryAgain;
        case EEXIST:
          return liberror(P, "__mkdir", eFAIL);
        default:
          return liberror(P, "__mkdir", eINVAL);
      }
    } else {
      setProcessRunnable(P);
      return Ok;
    }
  }
}

/*
 * rmdir : remove a directory
 */
retCode g__rmdir(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__rmdir", eSTRNEEDD);
  else {
    tryAgain:
    switchProcessState(P, wait_io);

    char *str = (char *) stringVal(stringV(t1));

    if (rmdir(str) == 0) {
      setProcessRunnable(P);
      return Ok;
    } else {
      setProcessRunnable(P);
      switch (errno) {
        case EINTR:
          goto tryAgain;
        case EACCES:
        case EPERM:
          return liberror(P, "__rmdir", eNOPERM);
        case EBUSY:
        case ENOENT:
          return liberror(P, "__rmdir", eFAIL);
        default:
          return liberror(P, "__rmdir", eIOERROR);
      }
    }
  }
}

/*
 * chmod : set permissions on a file or directory
 */

retCode g__chmod(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);
  ptrI t2 = deRefI(&a[2]);

  if (!IsString(t1))
    return liberror(P, "__chmod", eSTRNEEDD);
  else {
    mode_t acmode = (mode_t) integerVal(intV(t2));

    tryAgain:
    switchProcessState(P, wait_io);

    char *str = (char *) stringVal(stringV(t1));

    if (chmod(str, acmode) == -1) {
      setProcessRunnable(P);
      switch (errno) {
        case EINTR:
          goto tryAgain;    /* A mega hack */
        case EACCES:
          return liberror(P, "__chmod", eNOPERM);
        case EPERM:
          return liberror(P, "__chmod", eNOPERM);
        default:
          return liberror(P, "__chmod", eNOPERM);
      }
    }
    setProcessRunnable(P);
  }
  return Ok;
}

/*
 * fmode : gets permissions of a file or directory as a mode string
 */

retCode g__file_mode(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__fmode", eSTRNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "__fmode", eVARNEEDD);
  else {
    struct stat buf;

    tryAgain:
    switchProcessState(P, wait_io);
    char *str = (char *) stringVal(stringV(t1));

    if (stat(str, &buf) == -1) {
      setProcessRunnable(P);

      switch (errno) {
        case EINTR:
          goto tryAgain;
        case ENOTDIR:
          return liberror(P, "__fmode", eNOFILE);
        case ENAMETOOLONG:
          return liberror(P, "__fmode", eINVAL);
        case ENOENT:
          return liberror(P, "__fmode", eNOTFND);
        case EACCES:
          return liberror(P, "__fmode", eNOPERM);
        case ELOOP:
          return liberror(P, "__fmode", eINVAL);
        case EIO:
          return liberror(P, "__fmode", eIOERROR);
        case EFAULT:
          return liberror(P, "__fmode", eINVAL);
        default:
          return liberror(P, "__fmode", eNOTFND);
      }
    } else {
      ptrI modes = allocateInteger(&P->proc.heap, buf.st_mode);

      setProcessRunnable(P);
      return equal(P, &modes, &a[2]);
    }
  }
}

/*
 * file_type check out the type of the file
 */

retCode g__file_type(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__file_type", eSTRNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "__file_type", eVARNEEDD);
  else {
    struct stat buf;

    tryAgain:
    switchProcessState(P, wait_io);

    char *str = (char *) stringVal(stringV(t1));
    if (stat(str, &buf) == -1) {
      setProcessRunnable(P);

      switch (errno) {
        case EINTR:
          goto tryAgain;
        case ENOTDIR:
          return liberror(P, "__file_type", eNOFILE);
        case ENAMETOOLONG:
          return liberror(P, "__file_type", eINVAL);
        case ENOENT:
          return liberror(P, "__file_type", eNOTFND);
        case EACCES:
          return liberror(P, "__file_type", eNOPERM);
        case ELOOP:
          return liberror(P, "__file_type", eINVAL);
        case EIO:
          return liberror(P, "__file_type", eIOERROR);
        case EFAULT:
          return liberror(P, "__file_type", eINVAL);
        default:
          return liberror(P, "__file_type", eNOTFND);
      }
    }

    setProcessRunnable(P);

    if (S_ISFIFO(buf.st_mode))
      return equal(P, &kfifo, &a[2]);
    else if (S_ISCHR(buf.st_mode))
      return equal(P, &kcharfile, &a[2]);
    else if (S_ISDIR(buf.st_mode))
      return equal(P, &kdir, &a[2]);
    else if (S_ISBLK(buf.st_mode))
      return equal(P, &kblock, &a[2]);
    else if (S_ISREG(buf.st_mode))
      return equal(P, &kplain, &a[2]);
    else if (S_ISLNK(buf.st_mode))
      return equal(P, &ksymlink, &a[2]);
    else
      return liberror(P, "__file_type", eINVAL);
  }
}

/*
 * ffile(file)
 * succeeds if file is present, false otherwise 
 */

/* Special macro for Windows 95 */
#define FILE_ACCESS_MODE F_OK|R_OK

/* Check if a file is present or not */
static retCode filePresent(string name) {
  if (access((const char *) name, FILE_ACCESS_MODE) == 0)
    return Ok;
  else
    return Fail;
}


retCode g__file_present(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__file_present", eSTRNEEDD);

  switchProcessState(P, wait_io);
  retCode present = filePresent(stringVal(stringV(t1)));
  setProcessRunnable(P);

  return present;
}

/*
 * __file_size() - return file size 
 */

retCode g__file_size(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__file_size", eSTRNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "__file_size", eVARNEEDD);
  else {
    struct stat buf;

    tryAgain:
    switchProcessState(P, wait_io);

    char *str = (char *) stringVal(stringV(t1));

    if (stat(str, &buf) == -1) {
      setProcessRunnable(P);

      switch (errno) {
        case EINTR:
          goto tryAgain;
        case ENOTDIR:
          return liberror(P, "__file_size", eNOTDIR);
        case ENAMETOOLONG:
          return liberror(P, "__file_size", eINVAL);
        case ENOENT:
          return liberror(P, "__file_size", eNOTFND);
        case EACCES:
          return liberror(P, "__file_size", eNOPERM);
        case ELOOP:
          return liberror(P, "__file_size", eINVAL);
        case EIO:
          return liberror(P, "__file_size", eIOERROR);
        case EFAULT:
          return liberror(P, "__file_size", eINVAL);
        default:
          return liberror(P, "__file_size", eNOTFND);
      }
    } else {
      ptrI details = allocateInteger(&P->proc.heap, buf.st_size);

      setProcessRunnable(P);
      return equal(P, &details, &a[2]);
    }
  }
}

/*
 * __file_date(file,access,modified,created) - return file creation and modification
 */

retCode g__file_date(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__file_date", eSTRNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "__file_date", eVARNEEDD);
  else {
    struct stat buf;

    tryAgain:
    switchProcessState(P, wait_io);

    char *str = (char *) stringVal(stringV(t1));

    if (stat(str, &buf) == -1) {
      setProcessRunnable(P);

      switch (errno) {
        case EINTR:
          goto tryAgain;
        case ENOTDIR:
          return liberror(P, "__file_date", eNOFILE);
        case ENAMETOOLONG:
          return liberror(P, "__file_date", eINVAL);
        case ENOENT:
          return liberror(P, "__file_date", eNOTFND);
        case EACCES:
          return liberror(P, "__file_date", eNOPERM);
        case ELOOP:
          return liberror(P, "__file_date", eINVAL);
        case EIO:
          return liberror(P, "__file_date", eIOERROR);
        case EFAULT:
          return liberror(P, "__file_date", eINVAL);
        default:
          return liberror(P, "__file_date", eNOTFND);
      }
    } else {
      setProcessRunnable(P);

      ptrI details = allocateInteger(&P->proc.heap, buf.st_atime);
      retCode ret = equal(P, &details, &a[2]);

      if (ret == Ok) {
        details = allocateInteger(&P->proc.heap, buf.st_mtime);
        ret = equal(P, &details, &a[3]);
      }

      if (ret == Ok) {
        details = allocateInteger(&P->proc.heap, buf.st_ctime);
        ret = equal(P, &details, &a[4]);
      }
      return ret;
    }
  }
}

/*
 * __file_modified(file,modified) - return file  modification
 */

retCode g__file_modified(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__file_modified", eSTRNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "__file_modified", eVARNEEDD);
  else {
    struct stat buf;

    tryAgain:
    switchProcessState(P, wait_io);

    char *str = (char *) stringVal(stringV(t1));

    if (stat(str, &buf) == -1) {
      setProcessRunnable(P);

      switch (errno) {
        case EINTR:
          goto tryAgain;
        case ENOTDIR:
          return liberror(P, "__file_date", eNOFILE);
        case ENAMETOOLONG:
          return liberror(P, "__file_date", eINVAL);
        case ENOENT:
          return liberror(P, "__file_date", eNOTFND);
        case EACCES:
          return liberror(P, "__file_date", eNOPERM);
        case ELOOP:
          return liberror(P, "__file_date", eINVAL);
        case EIO:
          return liberror(P, "__file_date", eIOERROR);
        case EFAULT:
          return liberror(P, "__file_date", eINVAL);
        default:
          return liberror(P, "__file_date", eNOTFND);
      }
    } else {
      setProcessRunnable(P);

      ptrI details = allocateInteger(&P->proc.heap, buf.st_mtime);
      return equal(P, &details, &a[2]);
    }
  }
}

/*
 * ls lists files in a directory
 */
retCode g__ls(processPo P, ptrPo a) {
  ptrI t1 = deRefI(&a[1]);

  if (!IsString(t1))
    return liberror(P, "__ls", eSTRNEEDD);
  else if (!isvar(deRefI(&a[2])))
    return liberror(P, "__ls", eVARNEEDD);
  else {
    DIR *directory;

    switchProcessState(P, wait_io);
    char *str = (char *) stringVal(stringV(t1));

    if ((directory = opendir(str)) == NULL) {
      setProcessRunnable(P);
      switch (errno) {
        case EACCES:
        case EMFILE:
        case ENFILE:
          return liberror(P, "__ls", eNOPERM);
        case ENOENT:
          return liberror(P, "__ls", eNOTFND);
        case ENAMETOOLONG:
        case ENOTDIR:
          return liberror(P, "__ls", eINVAL);
        default:
          return liberror(P, "__ls", eNOTFND);
      }
    } else {
      ptrI dir = emptyList;
      ptrI dirEntry = emptyList;
      ptrI name = emptyList;
      rootPo root = gcAddRoot(&P->proc.heap, &dir);
      struct dirent *ent;
      heapPo H = &P->proc.heap;

      gcAddRoot(H, &dirEntry);
      gcAddRoot(H, &name);

      while ((ent = readdir(directory)) != NULL) {
        /* skip special entries "." and ".." */
        if (strcmp(ent->d_name, ".") != 0 && strcmp(ent->d_name, "..") != 0) {
          name = allocateCString(H, ent->d_name);

          dir = consLsPair(H, name, dir); /* directory ends up in reverse order */
        }
      }

      closedir(directory);              /* Close the directory stream */

      gcRemoveRoot(H, root);

      setProcessRunnable(P);
      return equal(P, &a[2], &dir);
    }
  }
}
