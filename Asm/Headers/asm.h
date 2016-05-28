#ifndef _ASM_H_
#define _ASM_H_

#include "config.h"
#include "assem.h"

extern logical debugAssem;
extern logical debugParse;

extern int getOptions(int argc, char **argv);

extern void setOutputFile(string path);

extern retCode parseContent(string path, string outPath);

extern void setHome(string home);

#endif
