//
// Created by Francis McCabe on 2/1/17.
//

#include <unicode.h>

#ifndef LANDO_MANIFEST_H
#define LANDO_MANIFEST_H

typedef struct _manifest_entry_ *manifestEntryPo;

manifestEntryPo newManifestEntry(string package,string version,string source,string code);

manifestEntryPo manifestEntry(string package);


#endif //LANDO_MANIFEST_H
