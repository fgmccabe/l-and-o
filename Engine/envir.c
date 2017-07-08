/*
  Access to the environment variables
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.

*/
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "lo.h"			/* main header file */
#include "tpl.h"

retCode g_getenv(processPo P, ptrPo a) {
  ptrI k = deRefI(&a[1]);

  if (isvar(k))
    return liberror(P, "getenv", eINSUFARG);
  else if (!IsString(k))
    return liberror(P, "getenv", eINVAL);
  else {
    char * key = stringVal(stringV(k));
    char *val = getenv((char*)key);

    if (val != NULL) {
      ptrI L = allocateCString(&P->proc.heap, val);

      return equal(P, &a[3], &L);
    }
    else
      return equal(P, &a[2], &a[3]);
  }
}

retCode g_setenv(processPo P, ptrPo a) {
  ptrI k = deRefI(&a[1]);
  ptrI v = deRefI(&a[2]);

  if (isvar(k) || !IsString(k) || !IsString(v))
    return liberror(P, "setenv", eINVAL);
  else {
    char * key = stringVal(stringV(k));
    char * val = stringVal(stringV(v));

    if (setenv((char *) key,  val, 1) == 0)
      return Ok;
    else
      return liberror(P, "setenv", eSPACE);
  }
}

retCode g_envir(processPo P, ptrPo a) {
  extern char **environ;
  int i, cnt;
  ptrI lst = emptyList;
  ptrI el = kvoid;
  ptrI ky = kvoid;
  ptrI vl = kvoid;
  heapPo H = &P->proc.heap;
  rootPo root = gcAddRoot(H, &lst);

  gcAddRoot(H, &el);
  gcAddRoot(H, &ky);
  gcAddRoot(H, &vl);

  for (cnt = 0; environ[cnt] != NULL; cnt++);

  switchProcessState(P, in_exclusion);

  for (i = cnt - 1; i > 0; i--) {
    char *pt = strchr(environ[i], '=');

    if (pt != NULL) {
      *pt = '\0';           /* Split off the key from the value */

      ky = allocateCString(H, environ[i]);
      vl = allocateCString(H, pt + 1);
      el = tuplePair(H,ky,vl);

      lst = consLsPair(H, el, lst);
      *pt = '=';          /* restore the value */
    }
  }

  setProcessRunnable(P);
  gcRemoveRoot(H, root);
  return equal(P, &lst, &a[1]);
}

retCode g_getlogin(processPo P, ptrPo a) {
  ptrI k = deRefI(&a[1]);

  if (!isvar(k))
    return liberror(P, "getlogin", eVARNEEDD);
  else {
    ptrI Login = allocateCString(&P->proc.heap, getlogin());

    return equal(P, &a[1], &Login);
  }
}
