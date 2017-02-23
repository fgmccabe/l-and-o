//
// Created by Francis McCabe on 2/23/17.
//

#ifndef LANDO_LOAD_H
#define LANDO_LOAD_H

#include "word.h"

typedef struct {
  byte packageName[1024];
  byte version[1024];
} PackageRec, *packagePo;

void initPackages();

packagePo loadedPackage(string package);

string loadedVersion(string package);

retCode packageIsLoaded(string package, string version);

#endif //LANDO_LOAD_H
