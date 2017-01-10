/* 
  File I/O part of the I/O library
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 

#ifndef _FILEIO_H_
#define _FILEIO_H_

#include "config.h"

void initFileIo(void);
void initFiles(void);
void setUpAsyncIO(int fd);

extern byte loSysPath[];		/* L&O installation point */

ioPo openSocketFile(char *name,int sock);
retCode filePerms(char *file,unsigned long *mode);
logical executableFile(char *file);
ioEncoding pickEncoding(ptrI k);

ioPo filePtr(ptrI p);
ptrI allocFilePtr(ioPo file);

retCode load_code_file(char *name,ptrPo tgt);

typedef enum { input, output } ioMode;
 
retCode attachProcessToFile(ioPo f,processPo p,ioMode mode);
void detachProcessFromFile(ioPo f,processPo p);
void detachProcessFromIo(processPo p);

#include <sys/time.h>
int set_in_fdset(fd_set *set);
int set_out_fdset(fd_set *set);
void trigger_io(fd_set *inset,fd_set *outset,int max);

#endif
