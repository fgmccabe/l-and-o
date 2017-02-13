/*
   Public declarations for the dictionary functions for April
*/
#ifndef _DICT_H_
#define _DICT_H_

void initDict();			/* Initialize the dictionary */

ptrI newEnumSym(const char *name);
ptrI newEnumSymbol(const string name);

void restartDictionary(globalGcPo G);

#endif
