//
// Created by Francis McCabe on 2/1/17.
//

#ifndef LANDO_MANIFEST_H
#define LANDO_MANIFEST_H

typedef struct _manifest_ *manifestPo;
typedef struct _manifest_entry_ *manifestEntryPo;

manifestPo newManifest();
manifestEntryPo newManifestEntry();

#endif //LANDO_MANIFEST_H
