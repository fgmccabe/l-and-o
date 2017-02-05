//
// Created by Francis McCabe on 2/1/17.
//

#ifndef LANDO_MANIFESTP_H
#define LANDO_MANIFESTP_H

#include <unicode.h>
#include "manifest.h"
#include "hashTable.h"
#include "lo.h"

typedef struct _manifest_entry_ {
  byte package[MAX_SYMB_LEN];
  hashPo versions;
  manifestVersionPo deflt;
  manifestVersionPo loaded;
} ManifestEntryRecord;

typedef struct _manifest_version_ {
  byte version[MAXFILELEN];
  logical isDefault;
  byte source[MAX_SYMB_LEN]; // File name of source of package
  byte code[MAX_SYMB_LEN];   // File name of package code
} ManifestVersionRecord;


#endif //LANDO_MANIFESTP_H
