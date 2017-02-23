//
// Created by Francis McCabe on 2/1/17.
//

#include <unicode.h>

#ifndef LANDO_MANIFEST_H
#define LANDO_MANIFEST_H

typedef struct _manifest_entry_ *manifestEntryPo;
typedef struct _manifest_version_ *manifestVersionPo;
typedef struct _manifest_file_name_ *manifestFilePo;

manifestEntryPo manifestEntry(string package);

string packageCodeFile(string package, string version, byte *flNm, long flLen);

string manifestResource(string package,string version,string kind,byte *fl,long flLen);

retCode loadManifest(string dir);

#endif //LANDO_MANIFEST_H
