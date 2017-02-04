//
// Created by Francis McCabe on 2/1/17.
//

#ifndef LANDO_MANIFESTP_H
#define LANDO_MANIFESTP_H

#include <unicode.h>
#include "manifest.h"
#include "pool.h"
#include "hashTable.h"


typedef struct {
  byte packageName[MAX_SYMB_LEN];
  byte version[MAX_SYMB_LEN];
} PackageRec, *packagePo;

typedef struct _manifest_entry_ {
  PackageRec package;
  byte source[MAX_SYMB_LEN]; // File name of source of package
  byte code[MAX_SYMB_LEN];   // File name of package code
} ManifestEntryRecord;



#endif //LANDO_MANIFESTP_H
