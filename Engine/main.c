/* 
  Main program for the L&O run-time system
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"
#include <unistd.h>
#include <string.h>
#include <sys/param.h>
#include <stdlib.h>
#include "lo.h"		/* Main header file */
#include "fileio.h"
#include "clock.h"
#include "debug.h"
#include "esc.h"

logical debugging = False;  // instruction tracing option 
logical interactive = False;  // Whether it should be interactive 
logical enableVerify = True;  // true if we wish to enable code verification 
logical SymbolDebug = False;  // symbolic debugging generation 
logical traceVerify = False;  // true if tracing code verification 
logical traceMessage = False;  // true if tracing message passing 
logical tracePut = False;  // true if tracing term freeze 
logical traceLock = False;  /* true if tracing locks */
#ifdef XTRACE
#ifdef LOXLIB
logical traceX = False;			/* True if tracing X windows stuff */
#endif
#endif

byte loSysPath[MAX_MSG_LEN] = {0};      // Pointer to L&O's installation point
static byte loCWD[MAX_MSG_LEN] = {0};
static byte classPath[MAX_MSG_LEN] = {0};  // Go class path string 
static byte entryPoint[MAX_MSG_LEN] = {0};  // Go entry point class 
static byte debugPkg[MAX_MSG_LEN] = {0};  // Standard debug package 

static struct {
  codePoint option; // The name of the option
  char value[MAX_MSG_LEN];           /* the value of the option */
} Options[32];  // An array of them 
static int optCount = 0;                /* How many do we have? */

#include "version.h"		/* Version ID for the evaluator */

char copyRight[] = "(c) 2016 F.G.McCabe\nApache 2.0 licence";

static long initHeapSize = 200 * 1024;
long initStackHeapSize = 1024;

static void splitFirstArg(int argc, char **argv, int *newArgc, char ***newArgv) {
  /* 
     Splits the first command-line argument into multiple
     arguments if it starts with "-%". The
     delimiter is the first character following the percent sign.
     For example: "-%%-pdir1%-pdir2" will be split into
     two arguments "-pdir1", "-pdir2".

     This helps to work around the limitation that #! scripts
     can pass a maximum of one argument to the interpreter
     under certain operating systems (eg. Linux).
  */

  *newArgc = argc;
  *newArgv = argv;

  if (argc < 2)
    return;

  if (strncmp(argv[1], "-%", 2) == 0) {
    char delimiter = argv[1][2];
    int extra = 0, arg = 1;
    char *p;

    /* Count number of & in passed arguments */
    p = argv[1] + 3;
    do {
      char *q = strchr(p, delimiter);
      if (q == NULL)
        break;

      p = q + 1;
      extra++;
    } while (*p != '\0');


    /* We didn't find any delimiters */
    if (extra == 0)
      return;

    /* Make the extra arguments */
    *newArgc = argc + extra;
    *newArgv = (char **) malloc(*newArgc * sizeof(char *));
    (*newArgv)[0] = argv[0];

    p = argv[1] + 3;
    do {
      char *q = strchr(p, delimiter);
      if (q == NULL) {
        (*newArgv)[arg++] = p;
        break;
      }
      else {
        size_t len = (size_t) (q - p);
        char *data = (char *) malloc(len + 1);

        strncpy(data, p, len);
        data[len] = '\0';
        (*newArgv)[arg++] = data;
        p = q + 1;
      }
    } while (True);
  }
}

int getOptions(int argc, char **argv) {
  int opt;
  extern char *optarg;
  extern int optind;

  splitFirstArg(argc, argv, &argc, &argv);

  for (; optCount < NumberOf(Options) &&
         (opt = getopt(argc, argv, GNU_GETOPT_NOPERMUTE "n:m:M:D:P:d:gG:x:vVh:s:L:R:")) >= 0; optCount++) {
    Options[optCount].option = (codePoint)opt;     /* store the option */

    if (optarg != NULL) {
      strncpy(Options[optCount].value, optarg, NumberOf(Options[optCount].value));
    }
    else
      Options[optCount].value[0] = '\0';

    switch (opt) {
      case 'D': {      /* turn on various debugging options */
        char *c = optarg;

        while (*c) {
          switch (*c++) {
            case 'e':    /* Escape call tracing */
#ifdef EXECTRACE
              traceEscapes = True;
              continue;
#else
            logMsg(logFile,"Escape tracing not enabled\n");
            return -1;
#endif

            case 'd':    /* single step instruction tracing */
#ifdef EXECTRACE
              debugging = True;
              continue;
#else
            logMsg(logFile,"Instruction-level debugging not enabled\n");
            return -1;
#endif

            case 'v':    /* turn on verify tracing */
#ifdef VERIFYTRACE
              traceVerify = True;
              continue;
#else
            logMsg(logFile,"code verification not enabled\n");
            return -1;
#endif

            case 'm':    /* trace memory allocations  */
#ifdef MEMTRACE
              if (traceMemory)
                stressMemory = True;
              else
                traceMemory = True;
              continue;
#else
            logMsg(logFile,"memory tracing not enabled");
            return -1;
#endif

            case 'l':    /* trace synch locks */
#ifdef LOCKTRACE
              traceLock = True;
              continue;
#else
            logMsg(logFile,"sync tracing not enabled");
            return -1;
#endif

            case 'p':    /* trace put-style operations */
#ifdef EXECTRACE
              tracePut = True;
              continue;
#else
            logMsg(logFile,"put tracing not enabled");
            return -1;
#endif

            case 'G':    /* Internal symbolic tracing */
#ifdef EXECTRACE
              SymbolDebug = True;
              interactive = False;
              continue;
#else
            logMsg(logFile,"tracing not enabled");
            return -1;
#endif

            case 'g':    /* Internal symbolic debugging */
              SymbolDebug = True;
              interactive = True;
              continue;

            case 'I':
#ifdef STATSTRACE
              traceCount = True;
              atexit(dumpInsCount);
              break;
#else
            logMsg(logFile,"instruction counting not enabled");
            return -1;
#endif

#if 0
            case 'x':		/* turn on tracing of X windows */
#ifdef XTRACE
#ifdef LOXLIB
              traceX=True;
#else
              logMsg(logFile,"X not enabled\n");
              return -1;
#endif
              continue;
#else
              logMsg(logFile,"X tracing not enabled\n");
              return -1;
#endif
#endif

            case '*':    /* trace everything */
#ifdef ALLTRACE
              traceEscapes = True;
              debugging = True;
              interactive = True;
              traceVerify = True;
              traceCount = True;
              traceMessage = True;
              if (traceMemory)
                stressMemory = True;
              else
                traceMemory = True;
              tracePut = True;              /* term freeze */
#else
            logMsg(logFile,"debugging not enabled\n");
            return -1;
#endif
            default:
              ;
          }
        }
        break;
      }

      case 'g': {
        SymbolDebug = True;  /* turn on symbolic debugging */
        interactive = True;       // Initially its also interactive
        break;
      }

      case 'G': {        /* non-default debugging package */
        strMsg(debugPkg, NumberOf(debugPkg), "%s", optarg);
        break;
      }

      case 'P': {
        byte buff[MAX_MSG_LEN];

        strMsg(buff, NumberOf(buff), "%s:%U", optarg, classPath);
        uniCpy(classPath, NumberOf(classPath), buff);

        break;
      }

      case 'm': {                          /* modify the entry point */
        strMsg(entryPoint, NumberOf(entryPoint), "lo.boot@%s", optarg);
        break;
      }

      case 'd': {                      /* non-standard initial working directory */
        strMsg(loCWD, NumberOf(loCWD), "%s", optarg);
        break;
      }

      case 'R': {                          /* fix the random seed */
        srand((unsigned int) atoi(optarg));
        break;
      }

      case 'L': {
        byte fn[MAX_MSG_LEN];
        strncpy((char*)fn, optarg, NumberOf(fn));

        if (initLogfile(fn) != Ok) {
          logMsg(logFile, "log file %s not found", optarg);
          return -1;
        }
        break;
      }

      case 'v':                           /* Display version ID */
        outMsg(logFile, "%s", version);
        outMsg(logFile, "%s", copyRight);
        break;

      case 'V':                      /* Turn on (will be off) code verification */
        enableVerify = (logical) !enableVerify;
        break;

      case 'h':                           /* set up heap size */
        initHeapSize = atoi(optarg) * 1024;
        break;

      case 's':                           /* set up initial size of a thread */
        initStackHeapSize = atoi(optarg) * 1024;
        break;

      default:
        break;                            /* ignore options we dont understand */
    }
  }
  return optind;
}

/*
 * Go evaluator main program
 */
int main(int argc, char **argv) {
  int narg;

#ifdef HAVE_LOCALECONV
  setlocale(LC_ALL,"");		/* set up locale */
#endif

#ifdef LOCALEDIR
  bindtextdomain(PACKAGE,LOCALEDIR);
  textdomain(PACKAGE);
#endif

  initLogfile((string) "-");

  {
    char *dir = getenv("LO_DIR"); /* pick up the installation directory */
    char cbuff[MAXPATHLEN];
    char *cwd = getcwd(cbuff, NumberOf(cbuff)); /* compute current starting directory */

    if (dir == NULL)
      dir = LODIR;                  /* Default installation path */

    strMsg(loSysPath, NumberOf(loSysPath), "%s", dir);

    if (cwd == NULL)
      syserr("cant determine current directory");
    else {
      strMsg(loCWD, NumberOf(loCWD), "%s/", cwd);
      strMsg(classPath, NumberOf(classPath), "%s:%s/", dir, cwd);
    }
  }

  strMsg(entryPoint, NumberOf(entryPoint), "lo.boot@__main"); /* standard entry point */

  if ((narg = getOptions(argc, argv)) < 0) {
    outMsg(logFile, _("usage: %s [-v] [-m mainclass] [-L log] [-g host:port] [-V] [-P classpath]"
#ifdef ALLTRACE
                        " [-D debugopts]"
#endif
                        " [-h sizeK] [-s sizeK] [-d rootdir] args ...\n"), argv[0]);
    exit(1);
  }

  /* IMPORTANT -- Keep the order of these set up calls */
  initFileIo();        /* Set up special file handling */
  initGlobal(initHeapSize);    /* start up the global space */
  initClass();        /* Initialize the class handlers */
  initPrograms();      /* Initialize program handling */
  initDict();        /* Start up the dictionaries */
  install_escapes();      /* Initialize the escape table */
  initFiles();        /* initialize file tables */
  init_args(argv, argc, narg);    /* Initialize the argument list */
  init_time();        /* Initialize time stuff */

  setupSignals();

  bootstrap(entryPoint, SymbolDebug, classPath, loCWD);

  return EXIT_SUCCEED;          /* exit the lo system cleanly */
}

/*
 * This is here for convenience ...
 */
ptrI cmdLineOptions(heapPo H) {
  ptrI options = emptyList;
  rootPo root = gcAddRoot(H, &options);
  ptrI pair = kvoid;
  ptrI tmp = kvoid;
  int i;

  gcAddRoot(H, &pair);
  gcAddRoot(H, &tmp);

  for (i = 0; i < optCount; i++) {
    pair = objP(allocateObject(H, commaClass));  /* a new option pair */

    tmp = allocateInteger(H,Options[i].option);
    updateArg(objV(pair), 0, tmp);

    tmp = newSymbol(Options[i].value);
    updateArg(objV(pair), 1, tmp);

    options = consLsPair(H, pair, options);
  }

  gcRemoveRoot(H, root);

  return options;
}
