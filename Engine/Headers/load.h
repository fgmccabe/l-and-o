//
// Created by Francis McCabe on 2/23/17.
//

#ifndef LANDO_LOAD_H
#define LANDO_LOAD_H

#include "word.h"

typedef struct _package_record_ {
  byte packageName[1024];
  byte version[1024];
} PackageRec, *packagePo;

void initPackages();

packagePo loadedPackage(string package);

string pkgName(packagePo pkg);
string pkgVers(packagePo pkg);
string loadedVersion(string package);

#endif //LANDO_LOAD_H
