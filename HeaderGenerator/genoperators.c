#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include "trie.h"
#include "pool.h"

/* Generate a Prolog or L&O module, that knows about the standard operators */

static void genInfix(FILE *out, char *op, int left, int prior, int right, char *cmt);
static void genPrefix(FILE *out, char *op, int prior, int right, char *cmt);
static void genPostfix(FILE *out, char *op, int left, int prior, char *cmt);

#undef lastOp
#define lastOp sep = "\t";

static char *pC(char *buff, long *ix, char c);

static char *pS(char *buff, char *s) {
  char *p = buff;
  long ix = 0;

  while (*s != '\0') {
    pC(buff, &ix, *s++);
  }
  buff[ix] = '\0';
  return buff;
}

static char *pC(char *buff, long *ix, char c) {
  switch (c) {
    case '\'':
    case '"':
    case '\\':
      buff[(*ix)++] = '\\';
    default:
      buff[(*ix)++] = c;
      buff[*ix] = '\0';
  }
  return buff;
}

enum {
  genProlog, genLO
} genMode = genProlog;

typedef struct {
  char *name;
  char *cmt;
} OpRecord, *operatorPo;

char *prefix = NULL;
static triePo tokenTrie;
static poolPo opPool;

static void initTries() {
  tokenTrie = emptyTrie();

  opPool = newPool(sizeof(OpRecord), 128);
}

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

static void dumpFollows(char *K, void *V, void *cl) {
  FILE *out = (FILE *) cl;

  char *prefix = (char *) calloc(sizeof(char), strlen(K));
  for (int ix = 0; ix < strlen(K) - 1; ix++)
    prefix[ix] = K[ix];
  char last = K[strlen(K) - 1];
  char b1[32], b2[32], b3[32];

  long ix = 0;
  if (genMode == genProlog)
    fprintf(out, "  follows('%s',\'%s\','%s').\n", pS(&b1[0], prefix), pC(b2, &ix, last), pS(b3, K));
  else
    fprintf(out, "  follows(\"%s\",0c%s,\"%s\").\n", pS(&b1[0], prefix), pC(b2, &ix, last), pS(b3, K));
}

static void dumpOperator(char *K, void *V, void *cl) {
  operatorPo op = (operatorPo) V;
  FILE *out = (FILE *) cl;
  char b1[32], b2[32];
  if (op != NULL) {
    if (genMode == genProlog)
      fprintf(out, "  final('%s',\"%s\").\t /* %s */\n", pS(b1, K), pS(b2, op->name), op->cmt);
    else
      fprintf(out, "  final(\"%s\").\t /* %s */\n", pS(b1, K), op->cmt);
  }
}

logical isAlphaNumeric(char *p){
  if(*p!='\0' && isalpha(*p++)){
    while(*p!='\0' && isalnum(*p++))
      ;
    return True;
  }
  return False;
}

int main(int argc, char **argv) {
  initTries();
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
        fprintf(out,
                ":-module(operators,[infixOp/4,prefixOp/3,postfixOp/3,isOperator/2,follows/3,final/2]).\n\n");
        break;
      case genLO:
        fprintf(out, "%s{\n", prefix);
        fprintf(out, "  import lo.string.\n");
        fprintf(out, "  import lo.arith.\n\n");
        fprintf(out, "  infixOp:(string,integer,integer,integer){}.\n");
        fprintf(out, "  prefixOp:(string,integer,integer){}.\n");
        fprintf(out, "  postfixOp:(string,integer,integer){}.\n\n");
        break;
    }

#undef lastInfOp
#define lastInfOp
#undef lastPreOp
#define lastPreOp
#undef lastPostOp
#define lastPostOp

#undef infixOp
#define infixOp(op, left, prior, right, cmt) genInfix(out,op,left,prior,right,cmt);

#undef prefixOp
#define prefixOp(op, prior, right, cmt) genPrefix(out,op,prior,right,cmt);

#undef postfixOp
#define postfixOp(op, left, prior, cmt) genPostfix(out,op,left,prior,cmt);

#include "operators.h"

    fprintf(out, "\n  /* Define isOperator */");

    switch (genMode) {
      case genLO:
        fprintf(out, "\n  isOperator:(string,integer){}.\n");
        break;
      default:
      ;
    }

    fprintf(out, "  isOperator(Op,Pr) :- prefixOp(Op,Pr,_).\n");
    fprintf(out, "  isOperator(Op,Pr) :- infixOp(Op,_,Pr,_).\n");
    fprintf(out, "  isOperator(Op,Pr) :- postfixOp(Op,_,Pr).\n");

    switch (genMode) {
      case genLO:
        fprintf(out, "\n  follows:(string,integer,string){}.\n");
        break;
      default:
      ;
    }

    processTrie(tokenTrie, dumpFollows, out);

    switch (genMode) {
      case genLO:
        fprintf(out, "\n  final:(string){}.\n");
        break;
      default:
      ;
    }

    processTrie(tokenTrie, dumpOperator, out);

    switch (genMode) {
      case genProlog:
        break;
      case genLO:
        fprintf(out, "}.\n");
        break;
    }

    fclose(out);
    exit(0);
  }
}

static void genInfix(FILE *out, char *op, int left, int prior, int right, char *cmt) {
  char b[32];
  fprintf(out, "  infixOp(\"%s\",%d,%d,%d).\t /* %s */\n", pS(b, op), left, prior, right, cmt);
  operatorPo opRecord = (operatorPo) allocPool(opPool);
  opRecord->name = op;
  opRecord->cmt = cmt;
  if(!isAlphaNumeric(op))
    addToTrie(op, opRecord, tokenTrie);
}

static void genPrefix(FILE *out, char *op, int prior, int right, char *cmt) {
  char b[32];
  fprintf(out, "  prefixOp(\"%s\",%d,%d).\t /* %s */\n", pS(b, op), prior, right, cmt);
  operatorPo opRecord = (operatorPo) allocPool(opPool);
  opRecord->name = op;
  opRecord->cmt = cmt;
  if(!isAlphaNumeric(op))
    addToTrie(op, opRecord, tokenTrie);
}

static void genPostfix(FILE *out, char *op, int left, int prior, char *cmt) {
  char b[32];
  fprintf(out, "  postfixOp(\"%s\",%d,%d).\t /* %s */\n", pS(b, op), left, prior, cmt);
  operatorPo opRecord = (operatorPo) allocPool(opPool);
  opRecord->name = op;
  opRecord->cmt = cmt;
  if(!isAlphaNumeric(op))
    addToTrie(op, opRecord, tokenTrie);
}
