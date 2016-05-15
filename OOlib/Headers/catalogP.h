#ifndef _CATALOG_P_H_
#define _CATALOG_P_H_

#include "config.h"
#include "catalog.h"
#include "ixTree.h"

typedef string (*resolverFn)(catalogPo cat,string name);
typedef retCode (*addFn)(catalogPo cat,string name,string url);

typedef struct _catalog_ {
  string url;
  resolverFn resolver;
  addFn adder;
  treePo contents;
} CatalogRec;


extern void initCatalog();
extern void dumpCatalog(catalogPo cat);
extern catalogPo createCatalog(string url,resolverFn resolver,addFn adder);

#endif



