//
// Created by Francis McCabe on 2/1/17.
//

#include <unicode.h>

#ifndef LANDO_MANIFEST_H
#define LANDO_MANIFEST_H

typedef struct _manifest_entry_ *manifestEntryPo;
typedef struct _manifest_version_ *manifestVersionPo;
typedef struct _manifest_file_name_ *manifestFilePo;

manifestEntryPo manifestEntry(char * package);

char * packageCodeFile(char *package, char *version, char *flNm, long flLen);

char * manifestResource(char *package, char *version, char *kind, char *fl, long flLen);

retCode loadManifest(char * dir);

#endif //LANDO_MANIFEST_H
