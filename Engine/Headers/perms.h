/*
  Permissions handling header for the L&O run-time system
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _PERMS_H_
#define _PERMS_H_

#include "config.h"		/* pick up standard configuration header */
#include "lo.h"			/* Main header file */

logical inPermissions(ptrI perms,ptrI perm);
ptrI permitedAmount(ptrI perms,ptrI perm);
ptrI defaultPerms(ptrI baseURI);

ptrI generateKey(char *prefix);
ptrI generateUniKey(char * prefix);

extern ptrI
  pFORK,			// Permission to fork
  pIO,				// Permission to perform IO
  pSENDMSG,			// Permission to send messages
  pRECVMSG,			// Permission to receive messages
  pFILE,			// Permission to perform file operations
  pROOTURL,			// Permission to access root URL
  pIOKEY;			// Key for accessing the file system

#endif
