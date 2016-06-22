/* Generate a module, in either prolog or L&O, that knows about escape codes */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <getopt.h>
#include "signature.h"
#include "stringBuffer.h"
#include "formio.h"
#include <ctype.h>

enum {
  genProlog, genLO
} genMode = genProlog;
char *prefix = NULL;

int getOptions(int argc, char **argv) {
  int opt;
  extern char *optarg;
  extern int optind;

  while ((opt = getopt(argc, argv, "pc:")) >= 0) {
    switch (opt) {
      case 'p':
        genMode = genProlog;
        break;
      case 'c':
        genMode = genLO;
        prefix = optarg;
        break;
      default:;
    }
  }
  return optind;
}

static void prologEscapeTypes(FILE *);
static void prologIsEscape(FILE *);
static void loEscapeTypes(FILE *);
static void loIsEscape(FILE *);

int main(int argc, char **argv) {
  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  } else {
    FILE *out = stdout;

    if (narg < argc)
      out = fopen(argv[narg], "w");

    fprintf(out, "/* Automatically generated, do not edit */\n\n");

    switch (genMode) {
      case genProlog:
        fprintf(out, ":-module(escapes,[isEscape/1,escapeType/2]).\n\n");
        prologEscapeTypes(out);
        prologIsEscape(out);
        break;
      case genLO:
        fprintf(out, "%s{\n", prefix);
        fprintf(out, "\n  import lo.\n");
        fprintf(out, "  import lo.comp.types.\n\n");
        loEscapeTypes(out);
        loIsEscape(out);
        fprintf(out, "\n}.\n");
        break;
    }
  }
}

static void dumpStdType(char *name, bufferPo out);
static void dumpStr(char *str, bufferPo out);
static char *dName(char *sig, bufferPo out);
static char *dSequence(char *sig, bufferPo out);
static char *dFields(char *sig, bufferPo out);

static char *dumpSig(char *sig, bufferPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case integer_sig:
      dumpStdType("lo.arith*integer", out);
      break;
    case float_sig:
      dumpStdType("lo.arith*float", out);
      break;
    case string_sig:
      dumpStdType("lo.string*string", out);
      break;
    case logical_sig:
      dumpStdType("lo.logical*logical", out);
      break;
    case kvar_sig:
      outStr(O_IO(out), "kVar(");
      sig = dName(sig, out);
      outStr(O_IO(out), ")");
      break;
    case anon_sig:
      outStr(O_IO(out), "topType");
      break;
    case top_sig:
      outStr(O_IO(out), "topType");
      break;
    case void_sig:
      outStr(O_IO(out), "voidType");
      break;
    case type_sig:
      outStr(O_IO(out), "type(");
      sig = dName(sig, out);
      outStr(O_IO(out), ")");
      break;
    case poly_sig:
      outStr(O_IO(out), "typeExp(");
      sig = dName(sig, out);
      outStr(O_IO(out), ",");

      sig = dSequence(sig, out);
      outStr(O_IO(out), ")");
      break;
    case tuple_sig:
      outStr(O_IO(out), "tupleType(");
      sig = dSequence(sig, out);
      outStr(O_IO(out), ")");
      break;
    case funct_sig:
      outStr(O_IO(out), "funType(");
      sig = dSequence(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), ")");
      break;
    case pred_sig:
      outStr(O_IO(out), "predType(");
      sig = dSequence(sig, out);
      outStr(O_IO(out), ")");
      break;
    case grammar_sig:
      outStr(O_IO(out), "grammarType(");
      sig = dSequence(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), ")");
      break;
    case class_sig:
      outStr(O_IO(out), "classType(");
      sig = dSequence(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), ")");
      break;
    case face_sig:
      outStr(O_IO(out), "faceType(");
      sig = dFields(sig, out);
      outStr(O_IO(out), ")");
      break;
    case list_sig:
      outStr(O_IO(out), "typeExp(");
      dumpStr("lo.list*list", out);
      outStr(O_IO(out), ",[");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), "])");
      break;
    case forall_sig:
      outStr(O_IO(out), "univType(");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), ")");
      break;
    case constrained_sig:
      outStr(O_IO(out), "constrained(");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpSig(sig, out);
      outStr(O_IO(out), ")");
      break;
    default:
      fprintf(stderr, "illegal signature %s\n", sig);
      exit(99);
  }
  return sig;
}

static void dumpStdType(char *name, bufferPo out) {
  outMsg(O_IO(out), "type(");
  dumpStr(name, out);
  outMsg(O_IO(out), ")");
}

static char *dInt(char *sig,int *len) {
  char K = *sig;
  int Ln = 0;
  while(isdigit(K)){
    Ln = Ln*10+digittoint(K);
    K = *++sig;
  }
  *len = Ln;
  return sig;
}

static char *dSequence(char *sig, bufferPo out) {
  int ar;
  sig = dInt(sig,&ar);
  char *sep = "";
  outStr(O_IO(out), "[");
  while (ar-- > 0) {
    outStr(O_IO(out), sep);
    sig = dumpSig(sig, out);
    sep = ",";
  }
  outStr(O_IO(out), "]");
  return sig;
}

static char *dFields(char *sig, bufferPo out) {
  int ar;
  sig = dInt(sig,&ar);
  char *sep = "";
  outStr(O_IO(out), "[");
  while (ar-- > 0) {
    outStr(O_IO(out), sep);
    outStr(O_IO(out), "(");
    sig = dName(sig, out);
    outStr(O_IO(out), ",");
    sig = dumpSig(sig, out);
    outStr(O_IO(out), ")");
    sep = ",";
  }
  outStr(O_IO(out), "]");
  return sig;
}

static void dumpStr(char *str, bufferPo out) {
  outByte(O_IO(out), '"');
  while (*str != '\0') {
    char c = *str++;
    switch (c) {
      case '\'':
      case '"':
      case '\\':
        outByte(O_IO(out), '\\');
        outByte(O_IO(out), (byte) c);
        break;
      default:
        outByte(O_IO(out), (byte) c);
    }
  }
  outByte(O_IO(out), '"');
}

static char *dName(char *sig, bufferPo out) {
  char delim = *sig++;
  outByte(O_IO(out), '"');
  while (*sig != delim && *sig != '\0') {
    outByte(O_IO(out), *sig++);
  }
  outByte(O_IO(out), '"');
  return sig+1;
}

#undef escape
#define escape(name, code, priv, secr, type, cmt) genLoEsc(out,buffer,#name,type,cmt);

static void genLoEsc(FILE *out, bufferPo buffer, char *name, char *sig, char *cmt) {
  outStr(O_IO(buffer), "  escapeType(");
  dumpStr(name, buffer);
  outStr(O_IO(buffer), ") => ");
  dumpSig(sig, buffer);
  outStr(O_IO(buffer), ".\n");

  long len;
  char *text = (char *) getTextFromBuffer(&len, buffer);
  fprintf(out, "%s", text);
  clearBuffer(buffer);
}

static void loEscapeTypes(FILE *out) {
  bufferPo buffer = newStringBuffer();

  fprintf(out, "  escapeType:(string)=>type.\n");

#include "escapes.h"

  closeFile(O_IO(buffer));
}

#undef escape
#define escape(name, code, priv, secr, type, cmt) genLoIsEsc(out,buffer,#name);

static void genLoIsEsc(FILE *out, bufferPo buffer, char *name) {
  outStr(O_IO(buffer), "  isEscape(");
  dumpStr(name, buffer);
  outStr(O_IO(buffer), ").\n");

  long len;
  char *text = (char *) getTextFromBuffer(&len, buffer);
  fprintf(out, "%s", text);
  clearBuffer(buffer);
}

static void loIsEscape(FILE *out) {
  bufferPo buffer = newStringBuffer();

  fprintf(out, "\n  isEscape:(string){}.\n");

#include "escapes.h"

  closeFile(O_IO(buffer));
}

#undef escape
#define escape(name, code, priv, secr, type, cmt) genPrologEsc(out,buffer,#name,type,cmt);

static void genPrologEsc(FILE *out, bufferPo buffer, char *name, char *sig, char *cmt) {
  outStr(O_IO(buffer), "escapeType(");
  dumpStr(name, buffer);
  outStr(O_IO(buffer), ",");
  dumpSig(sig, buffer);
  outStr(O_IO(buffer), ").\n");

  long len;
  char *text = (char *) getTextFromBuffer(&len, buffer);
  fprintf(out, "%s", text);
  clearBuffer(buffer);
}

static void prologEscapeTypes(FILE *out) {
  bufferPo buffer = newStringBuffer();

#include "escapes.h"

  closeFile(O_IO(buffer));
}

#undef escape
#define escape(name, code, priv, secr, type, cmt) genPrIsEsc(out,buffer,#name);

static void genPrIsEsc(FILE *out, bufferPo buffer, char *name) {
  outStr(O_IO(buffer), "isEscape(");
  dumpStr(name, buffer);
  outStr(O_IO(buffer), ").\n");

  long len;
  char *text = (char *) getTextFromBuffer(&len, buffer);
  fprintf(out, "%s", text);
  clearBuffer(buffer);
}

static void prologIsEscape(FILE *out) {
  bufferPo buffer = newStringBuffer();

#include "escapes.h"

  closeFile(O_IO(buffer));
}
