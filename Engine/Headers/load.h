//
// Created by Francis McCabe on 2/23/17.
//

#ifndef LANDO_LOAD_H
#define LANDO_LOAD_H

#include "word.h"

typedef struct _package_record_ {
  char packageName[1024];
  char version[1024];
} PackageRec, *packagePo;

void initPackages();

packagePo loadedPackage(char * package);

char * pkgName(packagePo pkg);
char * pkgVers(packagePo pkg);
char * loadedVersion(char * package);

#endif //LANDO_LOAD_H
