//
// Created by Francis McCabe on 2/1/17.
//

#include <unicode.h>

#ifndef LANDO_MANIFEST_H
#define LANDO_MANIFEST_H

typedef struct {
  byte packageName[1024];
  byte version[1024];
} PackageRec, *packagePo;

typedef struct _manifest_entry_ *manifestEntryPo;
typedef struct _manifest_version_ *manifestVersionPo;

manifestEntryPo manifestEntry(string package);

string packageCodeFile(string package,string version);

string loadedVersion(string package);

retCode packageIsLoaded(string package, string version);

retCode loadManifest(string dir);

#endif //LANDO_MANIFEST_H
