/*
  System interface escapes
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
#include "config.h"		/* pick up standard configuration header */
#include <string.h>		/* String functions */
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>		/* system error numbers */
#include "lo.h"
#include "fileio.h"
#include "tpl.h"

retCode g__exit(processPo P, ptrPo a) {
  exit((int) FloatVal(objV(deRefI(&a[1]))));
}

/* This is used to attach a shell to a process, so that when the child terminates
   becomes available, the right process is kicked off
*/

retCode g__shell(processPo P, ptrPo a) {
  switchProcessState(P, wait_io);

  ptrI pth = deRefI(&a[1]);
  ptrI ags = deRefI(&a[2]);
  ptrI env = deRefI(&a[3]);

  long aLen = ListLen(ags);
  long eLen = ListLen(env);

  if (!IsString(pth))
    return liberror(P, "__shell", eSTRNEEDD);
  else if (isvar(ags) || aLen < 0 || isvar(env) || eLen < 0)
    return liberror(P, "__shell", eINSUFARG);
  else {
    strBuffPo str = stringV(pth);

    char cmd[MAX_MSG_LEN];
    copyString2Buff(cmd,NumberOf(cmd),str);

    if (access((char *) cmd, F_OK | R_OK | X_OK) != 0) {
      setProcessRunnable(P);
      return liberror(P, "__shell", eNOTFND);
    } else if (!executableFile((char *) cmd)) {
      setProcessRunnable(P);
      return liberror(P, "__shell", eNOPERM);
    } else {
      char **argv = (char **) calloc((size_t) (aLen + 2), sizeof(char *));
      char **envp = (char **) calloc((size_t) (eLen + 1), sizeof(char *));
      int pid;
      long i;

      argv[0] =  cmd;

      for (i = 1; IsList(ags); i++, ags = deRefI(listTail(objV(ags)))) {
        ptrPo l = listHead(objV(ags));
        strBuffPo sp = stringV(deRefI(l));
        char * s = stringVal(sp);
        long al = stringLen(sp);

        if (al < 0)
          return liberror(P, "__shell", eINSUFARG);
        else {
          argv[i] = strdup((char *) s);
        }
      }

      argv[i] = NULL;

      for (i = 0; IsList(env); i++, env = deRefI(listTail(objV(env)))) {
        ptrPo l = listHead(objV(env));
        ptrI El = deRefI(l);
        ptrI var, val;
        long al;

        if (!isTuplePair(&El, &var, &val) || !IsString(deRefI(&val)) || !IsString(deRefI(&var)))
          return liberror(P, "__shell", eINSUFARG);
        else {
          strBuffPo kp = stringV(deRefI(&var));
          char * k = stringVal(kp);
          long kl = stringLen(kp);
          strBuffPo vp = stringV(deRefI(&val));
          char * v = stringVal(vp);
          long vl = stringLen(vp);
          long bSize = kl+vl + 10;
          char str[bSize];

          strMsg(str, bSize, "%U = %U", k, v);

          envp[i] = strdup((char *) str);
        }
      }

      envp[i] = NULL;

      switchProcessState(P, wait_child);  /* We are now waiting for a child */

      if ((pid = fork()) == 0) {
        // child process, terminating after execve
        execve((char *) cmd, argv, envp);
        // abnormal termination -- should never get here
        _exit(127);
      } else {
        // parent process (agent)
        for (i = 1; argv[i] != NULL; i++)  // argv[0] is a local string
          free(argv[i]);

        for (i = 0; envp[i] != NULL; i++)
          free(envp[i]);

        free(argv);
        free(envp);

        do {
          int childStatus;
          int res = waitpid(pid, &childStatus, 0);

          setProcessRunnable(P);  /* now we can run */

          if (res < 0) {
            switch (errno) {
              case ECHILD:
                return liberror(P, "__shell", eNOTFND);
              case EFAULT:
                return liberror(P, "__shell", eINVAL);
              case EINTR:
              default:
                continue;
            }
          } else if (WIFEXITED(childStatus)) { /* exited normally */
            ptrI r = allocateInteger(&P->proc.heap,
                                     WEXITSTATUS(childStatus));

            return equal(P, &a[4], &r);
          } else if (WIFSIGNALED(childStatus))
            return liberror(P, "__shell", eINTRUPT);
        } while (True);
      }
    }
  }
}

    
