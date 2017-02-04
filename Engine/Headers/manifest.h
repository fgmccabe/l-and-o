//
// Created by Francis McCabe on 2/1/17.
//

#include <unicode.h>

#ifndef LANDO_MANIFEST_H
#define LANDO_MANIFEST_H

typedef struct _manifest_ *manifestPo;
typedef struct _manifest_entry_ *manifestEntryPo;

manifestPo newManifest();
manifestEntryPo newManifestEntry(string package,string version,string source,string code);

string manifestSource(manifestPo manifest,string package);
string manifestCode(manifestPo manifest,string package);


#endif //LANDO_MANIFEST_H
