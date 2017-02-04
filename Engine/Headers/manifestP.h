//
// Created by Francis McCabe on 2/1/17.
//

#ifndef LANDO_MANIFESTP_H
#define LANDO_MANIFESTP_H

#include <unicode.h>
#include "manifest.h"
#include "pool.h";
#include "hashTable.h";

typedef struct _manifest_ {
  hashPo entries;
} ManifestRecord;

typedef struct _manifest_entry_ {
  string package;
  string version;
  string source;
  string code;
} ManifestEntryRecord;

#endif //LANDO_MANIFESTP_H
