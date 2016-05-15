/* 
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <string.h>
#include "go.h"

static char **argsv=NULL;	/* Store the command line list */
static int argcnt=0;

void init_args(char **argv, int argc, int start)
{
  argsv = &argv[start];
  argcnt = argc-start;
}

ptrI commandLine(heapPo H)
{
  int i;
  ptrI alist=emptyList;
  ptrI ag = kvoid;
  rootPo root = gcAddRoot(H,&alist);

  gcAddRoot(H,&ag);

  for(i=argcnt-1;i>=0;i--){
    ag = allocateCString(H,argsv[i]);
    alist = consLsPair(H,ag,alist);       /* construct in reverse order */
  }

  gcRemoveRoot(H,root);                   /* clear off additional roots */
  return alist;
}

retCode g__command_line(processPo P,ptrPo args)
{
  ptrI alist = commandLine(&P->proc.heap);
  rootPo root = gcAddRoot(&P->proc.heap,&alist);
  retCode ret = equal(P,&args[1],&alist);

  gcRemoveRoot(&P->proc.heap,root);
  return ret;
}

retCode g__command_opts(processPo P,ptrPo args)
{
  ptrI alist = cmdLineOptions(&P->proc.heap);
  rootPo root = gcAddRoot(&P->proc.heap,&alist);
  retCode ret = equal(P,&args[1],&alist);

  gcRemoveRoot(&P->proc.heap,root);
  return ret;
}
