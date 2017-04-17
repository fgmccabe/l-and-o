#include <stdio.h>
#include <stdlib.h>

// Utility program to generate the opcodes.h header file

#undef instruction
#define instruction(mnem,op,A1,A2,cmnt) fprintf(out," "#mnem "= " #op",\n");

#define lastInstruction

int main(int argc, char **argv) {
  FILE *out = stdout;

  if (argc >= 2)
    out = fopen(argv[1], "w");

  fprintf(out, "/* Automatically generated, do not edit */\n");
  fprintf(out, "typedef enum {\n");
#include "instructions.h" /* Pick up the instructions specification */
  fprintf(out, "illegalOp\n");
  fprintf(out, "} opCode;");
#undef instruction

  fclose(out);
  exit(0);
}
