/*
  Install standard escapes into Go! system
  Copyright (c) 2016. Francis G. McCabe

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
#include "go.h"
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
#define escape(name, code, secr, pr, spec, cmnt) {\
  install_escape("##name##",g_##name,code,pr,spec);\
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

static char *skipSig(char *tp) {
  assert(tp != NULL);

  switch (*tp++) {
    case integer_sig:
    case float_sig:
    case number_sig:
    case symbol_sig:
    case string_sig:
    case top_sig:
    case logical_sig:
    case opaque_sig:
    case type_sig:
      return tp;
    case list_sig:
      return skipSig(tp);
    case funct_sig: {
      int ar = *tp++;

      while (ar-- > 0)
        tp = skipSig(tp);
      return skipSig(tp);
    }
    case grammar_sig: {
      int ar = *tp++;

      while (ar-- > 0)
        tp = skipSig(tp);
      return skipSig(tp);
    }
    case proc_sig: {
      int ar = *tp++;

      while (ar-- > 0)
        tp = skipSig(tp + 1);
      return tp;
    }
    case action_sig: {
      int ar = *tp++;

      while (ar-- > 0)
        tp = skipSig(tp);
      return tp;
    }
    case tuple_sig: {
      int ar = *tp++;

      while (ar-- > 0)
        tp = skipSig(tp);
      return tp;
    }
    case forall_sig:
      return skipSig(skipSig(tp + 1));
    case poly_sig: {
      char delim = *tp++;

      while (*tp++ != delim);

      int ar = *tp++;

      while (ar-- > 0)
        tp = skipSig(tp);
      return tp;
    }
    case face_sig: {
      int ar = *tp++;                     // the number of elements in the interface

      while (ar-- > 0) {
        tp = skipSig(tp);
      }
      return tp;
    }

    default:
      return NULL;
  }
}

static int sigArity(char *spec) {
  switch (*spec) {
    case funct_sig:
      return (int) (spec[1]) + 1;
    case proc_sig:
      return (int) (spec[1]);
    case action_sig:
      return (int) (spec[1]);
    case grammar_sig:
      return (int) (spec[1]) + 2;
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
