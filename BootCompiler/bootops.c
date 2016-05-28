#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Generate a Prolog module, that knows about the standard operators */

static void genInfix(FILE *out,char *op,int left,int prior,int right,char *cmt);
static void genPrefix(FILE *out,char *op,int prior,int right,char *cmt);
static void genPostfix(FILE *out,char *op,int left,int prior,char *cmt);

#undef lastOp
#define lastOp sep = "\t";

static char *pS(char *s)
{
  static char buff[1024];
  char *p = buff;
  
  while(*s!='\0'){
    if(*s=='\\')
      *p++='\\';
    *p++=*s++;
  }
  *p++='\0';
  return buff;
}

int main(int argc,char **argv)
{
  FILE *out=stdout;

  if(argc>=2)
    out = fopen(argv[1],"w");

  fprintf(out,"/* Automatically generated, do not edit */\n");
  fprintf(out,":-module('operators',[infixOp/4,prefixOp/3,postfixOp/3]).\n");

#undef lastInfOp
#define lastInfOp
#undef lastPreOp
#define lastPreOp
#undef lastPostOp
#define lastPostOp


#undef infixOp
#define infixOp(op,left,prior,right,cmt) genInfix(out,op,left,prior,right,cmt);

#undef prefixOp
#define prefixOp(op,prior,right,cmt) genPrefix(out,op,prior,right,cmt);

#undef postfixOp
#define postfixOp(op,left,prior,cmt) genPostfix(out,op,left,prior,cmt);
#include "operators.h"

  fclose(out);
  exit(0);
}

static void genInfix(FILE *out,char *op,int left,int prior,int right,char *cmt)
{
  fprintf(out,"    infixOp(\"%s\",%d,%d,%d).\t-- %s\n",pS(op),left,prior,right,cmt);
}

static void genPrefix(FILE *out,char *op,int prior,int right,char *cmt)
{
  fprintf(out,"    prefixOp(\"%s\",%d,%d)%s\t-- %s\n",pS(op),prior,right,cmt);
}

static void genPostfix(FILE *out,char *op,int left,int prior,char *cmt)
{
  fprintf(out,"    postfixOp(\"%s\",%d,%d).\t-- %s\n",pS(op),left,prior,cmt);
}
