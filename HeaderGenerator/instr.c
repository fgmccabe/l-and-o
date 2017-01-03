#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#include "opcodes.h"
#include "stringBuffer.h"
#include "formio.h"

#undef instruction
#define instruction(M,O,A1,A2,cmt)  genIns(out,#M,A1,A2,cmt,term);

#undef lastInstruction
#define lastInstruction term = ".";

static void genIns(FILE *out,char *op,opAndSpec A1,opAndSpec A2,char *cmt,char *term);

char *prefix = "";

int getOptions(int argc, char **argv) {
  int opt;
  extern char *optarg;
  extern int optind;

  while ((opt = getopt(argc, argv, "c:")) >= 0) {
    switch (opt) {
      case 'c':
        prefix = optarg;
        break;
      default:;
    }
  }
  return optind;
}

int main(int argc,char **argv)
{
  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  } else {
    FILE *out=stdout;
    char *term = "";

    if (narg < argc)
      out = fopen(argv[narg], "w");

    fprintf(out,"/* Automatically generated, do not edit */\n");

    fprintf(out, "%s{\n", prefix);
    fprintf(out, "  import lo.\n");


    fprintf(out,"  public type instruction ::=              -- type defining the opcodes\n");
    fprintf(out,"     iLbl(string)               -- label in code stream\n");

  #include "instructions.h"
      
    fprintf(out, "\n}.\n");
    
    exit(0);
  }
}

static char *genOpAnd(FILE *f,char *sep,opAndSpec A)
{
  switch(A){
  case nOp:                             // No operand
    return sep;
  case iAh:                             // input argument register in upper slot (0..255)
  case oAh:                             // output argument register in upper slot (0..255)
  case iAm:                             // input argument register in middle slot (0..255)
  case oAm:                             // output argument register in middle slot (0..255)
  case iAl:                             // input argument register in lower slot (0..255)
  case oAl:                             // output argument register in lower slot (0..255)
  case iLh:				/* input local, offset 0..255 */
  case iLm:				/* input local, offset 0..255 */
  case iLl:				/* input local, offset 0..255 */
  case iLc:                             // input local variable offset (0..65535)
  case oLh:				/* output local offet 0..255 */
  case oLm:				/* output local offet 0..255 */
  case oLl:				/* output local offet 0..255 */
  case oLc:                             // output local variable offset  (0..65535)
    fprintf(f,"%sinteger",sep);
    return ",";
  case iSt:                             // input at current structure pointer
  case oSt:                             // output to current structure pointer
    return sep;
  case oAr:				/* Result arity in upper slot */
  case uAr:                             // Arity in upper slot
  case uLt:                             // small literal in upper slot (-128..127)
  case vSz:                             // Size of local variable vector
  case lSz:                             // Size of local variable vector
  case cSz:             		            // Structure size
  case Ltl:                             // 16 bit integer offset
    fprintf(f,"%sinteger",sep);
    return ",";
  case Es:                              // escape code (0..65535)
    fprintf(f,"%sstring",sep);
    return ",";
  case pcr:                             // program counter relative offset (-32768..32767)
  case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
  case ltl:                             // literal number (0..65535)
    fprintf(f,"%sstring",sep);
    return ",";
  default:
    printf("Problem in generating opcode type\n");
    exit(11);
  }
}

static char *capitalize(char *str); 
static void genIns(FILE *out,char *op,opAndSpec A1,opAndSpec A2,char *cmt,char *term)
{
  char *sep = "(";
  
  fprintf(out,"   | i%s",capitalize(op));
  
  sep = genOpAnd(out,sep,A1);
  sep = genOpAnd(out,sep,A2);
  
  if(strcmp(sep,",")==0)
    fprintf(out,")%s\t\t-- %s\n",term,cmt);
  else
    fprintf(out,"%s\t\t-- %s\n",term,cmt);
}

static char *capitalize(char *str){
  static char buffer[128];
  strcpy(buffer,str);
  if(buffer[0]>='a' && buffer[0]<='z'){
    buffer[0] = 'A'+(buffer[0]-'a');
  }
  return buffer;
}
