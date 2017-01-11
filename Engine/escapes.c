/*
  Install standard escapes into L&O system
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
#include "lo.h"
#include "signature.h"          // Access the signature definitions
#include "esc.h"
#include "escodes.h"

#define MAXESC 1024    /* initial size of the escape hash table */

#ifdef EXECTRACE
logical traceEscapes = False;  /* Trace escape calls */
#endif

typedef struct {
  funpo escape_code;            // What is the escape function code?
  logical priv;                 // Execute in priviliged mode only?
  int arity;
  char *name;                   // Name of this escape function
  char *spec;
} EscapeTemplate, *escpo;

static EscapeTemplate escFuns[MAXESC]; // The escape tables themselves

static void install_escape(char *escape_fn, funpo escape_code, int code,
                           logical pr, char *spec);

#undef constr
#define constr(A, B, C)

#undef tdf
#define tdf(A, S, B)

#undef escape
#define escape(name, secr, pr, spec, cmnt) {\
  install_escape("##name##",g_##name,Esc##name,pr,spec);\
};

/* Set up the escape table */
// This must be executed before any threads are spawned off.
void install_escapes(void) {
  int i;

  for (i = 0; i < MAXESC; i++)
    escFuns[i].escape_code = NULL; /* Clear the table */

#include "escapes.h"

#undef escape
}

static integer scanInt(char **sig) {
  if (**sig == '-') {
    (*sig)++;
    return -scanInt(sig);
  }
  else {
    char ch = *(*sig)++;
    integer ii = 0;
    while (ch >= '0' && ch <= '9') {
      ii = ii * 10 + (ch - '0');
      ch = *(*sig)++;
    }

    return ii;
  }
}

// This is NOT v. safe, but is only called on internal code
static char *skipName(char *sig) {
  char qt = *sig++;
  while (*sig != qt) {
    sig++;
  }
  return ++sig;
}

static char *skipSig(char *tp) {
  assert(tp != NULL);

  switch (*tp++) {
    case integer_sig:
    case float_sig:
    case string_sig:
    case void_sig:
    case logical_sig:
      return tp;
    case kvar_sig:
      return skipName(tp);
    case kfun_sig:
      scanInt(&tp);
      return skipName(tp);
    case type_sig:
      return skipName(tp);
    case poly_sig: {
      tp = skipSig(tp);
      integer ar = scanInt(&tp);
      while (ar-- > 0)
        tp = skipSig(tp);
      return tp;
    }
    case list_sig:
      return skipSig(tp);
    case funct_sig: {
      integer ar = scanInt(&tp);

      while (ar-- > 0)
        tp = skipSig(tp);
      return skipSig(tp);
    }
    case grammar_sig: {
      integer ar = scanInt(&tp);

      while (ar-- > 0)
        tp = skipSig(tp);
      return skipSig(tp);
    }
    case pred_sig: {
      integer ar = scanInt(&tp);

      while (ar-- > 0)
        tp = skipSig(tp + 1);
      return tp;
    }
    case tuple_sig: {
      integer ar = scanInt(&tp);

      while (ar-- > 0)
        tp = skipSig(tp);
      return tp;
    }
    case forall_sig:
      return skipSig(skipSig(tp + 1));

    case face_sig: {
      integer ar = scanInt(&tp);     // the number of elements in the interface

      while (ar-- > 0) {
        tp = skipSig(skipName(tp));
      }
      return tp;
    }

    default:
      return NULL;
  }
}

static int sigArity(char *spec) {
  integer ar;
  switch (*spec) {
    case funct_sig:
      ar = scanInt(&spec);
      return ar + 1;
    case pred_sig:
      ar = scanInt(&spec);
      return ar;
    case grammar_sig:
      ar = scanInt(&spec);
      return ar + 2;
    case forall_sig:
      return sigArity(skipSig(spec + 2));
    default:
      assert(False);
      return 0;
  }
}

/* Install a symbol into the procedure escapes table */
static void install_escape(char *escape_fn, funpo escape_code, int code,
                           logical pr, char *spec) {
  assert(escFuns[code].escape_code == NULL);

  escpo e = &escFuns[code];
  e->escape_code = escape_code;
  e->priv = pr;
  e->name = strdup(escape_fn);
  e->arity = sigArity(spec);
}

funpo escapeCode(unsigned int code) {
  assert(code >= 0 && code < NumberOf(escFuns));
  return escFuns[code].escape_code;
}

char *escapeName(int code) {
  assert(code >= 0 && code < NumberOf(escFuns));

  return escFuns[code].name;
}

funpo getescape(int code) {
  assert(code >= 0 && code < NumberOf(escFuns));

  escpo e = &escFuns[code];
  return e->escape_code;
}

logical validEscape(unsigned int code, int arity) {
  if (code >= NumberOf(escFuns))
    return False;
  else if (escFuns[code].escape_code == NULL || escFuns[code].arity != arity)
    return False;
  else
    return True;
}

void showEscape(processPo P, int code, ptrPo args, long arity) {
  int i;
  byte buffer[1024];
  ioPo out = O_IO(openBufferStr(buffer, NumberOf(buffer)));

  outMsg(out, "%w: ", &P->proc.thread);
  outMsg(out, "%s(", escapeName(code));

  for (i = 0; i < arity; i++)
    outMsg(out, "%w%s", args++, (i < arity - 1 ? "," : ""));

  outMsg(out, ")\n%_");

  uint64 len;
  string text = getStrText(O_STRING(out), &len);

  outText(logFile, text, len);
  closeFile(out);
}

/* garbage collection function to handle type structures of escape funs */
void ScanEscapes(void) {
}

void markEscapes(void) {
}
