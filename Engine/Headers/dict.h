/*
   Public declarations for the dictionary functions for April
*/
#ifndef _DICT_H_
#define _DICT_H_

void initSymbolClass(void);
void initDict();			/* Initialize the dictionary */

ptrI newSymbol(const char *name);
ptrI newEnumSym(const char *name);
ptrI newUniSymbol(const string name);

void installSymbol(symbPo s);

void restartDictionary(globalGcPo G);

#endif
