#ifndef _CATALOG_H_
#define _CATALOG_H_

#include "unicode.h"

typedef struct _catalog_ *catalogPo;

extern string catalogResolve(catalogPo cat,string url);
extern retCode addCatalogEntry(catalogPo cat,string name,string url);
extern string catalogUrl(catalogPo cat);

extern catalogPo newCatalog();
extern void setCatalogBase(catalogPo cat,string url);
extern void setCatalogVersion(catalogPo cat,string version);
#endif

