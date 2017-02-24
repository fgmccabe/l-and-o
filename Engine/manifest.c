/*
  Manifest & repository handling
  Copyright (c) 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#include <jsonEvent.h>
#include <process.h>
#include "manifestP.h"
#include "load.h"

// Use the JSON event parser to parse a manifest file and build up a manifest structure

static retCode startManifest(void *cl);
static retCode endManifest(void *cl);
static retCode startCollection(void *cl);
static retCode endCollection(void *cl);
static retCode startArray(void *cl);
static retCode endArray(void *cl);
static retCode startEntry(const char *name, void *cl);
static retCode endEntry(const char *name, void *cl);
static retCode numEntry(double dx, void *cl);
static retCode boolEntry(logical trueVal, void *cl);
static retCode txtEntry(const char *name, void *cl);
static retCode nullEntry(void *cl);
static retCode errorEntry(const char *name, void *cl);

JsonCallBacks manEvents = {
  startManifest,
  endManifest,
  startCollection,
  endCollection,
  startArray,
  endArray,
  startEntry,
  endEntry,
  txtEntry,
  numEntry,
  boolEntry,
  nullEntry,
  errorEntry
};

static poolPo manifestPool = NULL;
static poolPo versionPool = NULL;
static poolPo filePool = NULL;

static hashPo manifest;

static byte repoDir[MAXFILELEN];

void initManifest() {
  manifestPool = newPool(sizeof(ManifestEntryRecord), 128);
  versionPool = newPool(sizeof(ManifestVersionRecord), 128);
  filePool = newPool(sizeof(ManifestFileRecord), 128);
  manifest = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}


// Manage the manifest

manifestEntryPo newManifestEntry(string name) {
  manifestEntryPo entry = (manifestEntryPo) allocPool(manifestPool);
  uniCpy((string) &entry->package, NumberOf(entry->package), name);
  entry->versions = NewHash(1, (hashFun) uniHash, (compFun) uniCmp, NULL);
  entry->deflt = NULL;
  hashPut(manifest, &entry->package, entry);
  return entry;
}

manifestEntryPo manifestEntry(string package) {
  return (manifestEntryPo) hashGet(manifest, package);
}

manifestEntryPo getEntry(string name) {
  manifestEntryPo entry = (manifestEntryPo) hashGet(manifest, (void *) name);

  if (entry == NULL) {
    entry = newManifestEntry((string) name);
    hashPut(manifest, &entry->package, entry);
  }
  return entry;
}

manifestVersionPo newVersion(string version) {
  manifestVersionPo vEntry = (manifestVersionPo) allocPool(versionPool);
  uniCpy((string) &vEntry->version, NumberOf(vEntry->version), version);
  vEntry->resources = NewHash(3, (hashFun) uniHash, (compFun) uniCmp, NULL);
  return vEntry;
}

manifestVersionPo manifestVersion(string package, string version) {
  manifestEntryPo entry = manifestEntry(package);

  if (entry != NULL) {
    if (uniCmp(version, (string) "*") == same && entry->deflt != NULL)
      return entry->deflt;
    else
      return (manifestVersionPo) hashGet(entry->versions, version);
  } else
    return NULL;
}

manifestFilePo newManifestResource(string kind, string fileNm) {
  manifestFilePo f = (manifestFilePo) allocPool(filePool);

  uniCpy((string) &f->kind, NumberOf(f->kind), kind);
  uniCpy((string) &f->fn, NumberOf(f->fn), fileNm);

  return f;
}

void addResource(manifestVersionPo version, string kind, string fileNm) {
  manifestFilePo f = newManifestResource(kind, fileNm);

  hashPut(version->resources, &f->kind, f);
}

string manifestResource(string package, string version, string kind, byte *fl, long flLen) {
  manifestVersionPo v = manifestVersion(package, version);

  if (v != NULL) {
    manifestFilePo f = hashGet(v->resources, kind);

    if (f != NULL) {
      strMsg(fl, flLen, "%s/%s", repoDir, (string) &f->fn);
      return fl;
    } else
      return NULL;
  } else
    return NULL;
}

string packageCodeFile(string package, string version, byte *flNm, long flLen) {
  return manifestResource(package, version, (string) "code", flNm, flLen);
}

typedef enum {
  initial,
  inPackage,
  inVersion,
  inDetail,
  inResource
} ParseState;

static char* stNames[] = {"initial", "inPackage", "inVersion", "inDetail", "inResource"};

typedef struct {
  byte pkg[MAXLINE]; // Package name
  manifestEntryPo entry;
  byte ver[MAXLINE];
  manifestVersionPo version;
  byte kind[MAXFILELEN];
  ParseState state;
} ParsingState, *statePo;

retCode loadManifest(string dir) {
  uniCpy(repoDir, NumberOf(repoDir), dir);
  initManifest();
  initPackages();

  byte manifestName[MAX_MSG_LEN];

  strMsg(manifestName, NumberOf(manifestName), "%s/manifest", dir);

  ioPo inFile = openInFile(manifestName, utf8Encoding);

  if (inFile != NULL) {
    ParsingState info;
    yyparse(inFile, &manEvents, &info);
    return Ok;
  } else
    return Error;
}

retCode startManifest(void *cl) {
  statePo info = (statePo) cl;
  info->state = initial;

  logMsg(logFile, "Starting parse of manifest");
  return Ok;
}

retCode endManifest(void *cl) {
  logMsg(logFile, "Ending parse of manifest");
  return Ok;
}

retCode startCollection(void *cl) {
  statePo info = (statePo) cl;

  logMsg(logFile, "Starting collection, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      info->state = inPackage;
      uniCpy((string) &info->pkg, NumberOf(info->pkg), (string) "");
      break;
    case inPackage:
      uniCpy((string) &info->ver, NumberOf(info->ver), (string) "");
      info->state = inVersion;
      break;
    case inVersion:
      uniCpy((string) &info->kind, NumberOf(info->kind), (string) "");
      info->state = inResource;
      break;
    case inResource:
      break;

    default:
      return Error;
  }
  return Ok;
}

retCode endCollection(void *cl) {
  statePo info = (statePo) cl;

  logMsg(logFile, "Ending collection, state = %s", stNames[info->state]);

  switch (info->state) {
    case initial:
      return Error;
    case inPackage:
      info->state = initial;
      break;
    case inVersion:
      info->state = inPackage;
      break;
    case inResource:
      info->state = inVersion;
      break;
    case inDetail:
      info->state = inResource;
      break;
    default:
      return Error;
  }

  return Ok;
}

retCode startArray(void *cl) {
  return Ok;
}

retCode endArray(void *cl) {
  return Ok;
}

retCode startEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  logMsg(logFile, "Starting entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case initial:
      return Error;
    case inPackage: {
      info->entry = getEntry((string) name);
      break;
    }
    case inVersion:
      uniCpy((string) &info->ver, NumberOf(info->ver), (string) name);
      info->version = newVersion((string)name);
      hashPut(info->entry->versions,&info->version->version,info->version);
      break;
    case inResource:
      uniCpy((string) &info->kind, NumberOf(info->kind), (string) name);
      info->state = inDetail;
      break;
    case inDetail:
      return Error; // expecting a text, not a collection
    default:
      return Error;
  }
  return Ok;
}

retCode endEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  logMsg(logFile, "Ending entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    case inDetail:
      info->state = inResource;
      break;
    case inResource:
    case inVersion:
    case inPackage:
    default:
      break;
  }

  return Ok;
}

retCode numEntry(double dx, void *cl) {
  return Ok;
}

retCode boolEntry(logical trueVal, void *cl) {
  return Ok;
}

retCode txtEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  logMsg(logFile, "Text entry, state = %s, name=%s", stNames[info->state], name);

  switch (info->state) {
    default:
      return Error;
    case inDetail:
      addResource(info->version, (string) &info->kind, (string) name);
      break;
  }
  return Ok;
}

retCode nullEntry(void *cl) {
  return Ok;
}

retCode errorEntry(const char *name, void *cl) {
  logMsg(logFile, "Error: %s", name);
  return Ok;
}

// Implement escapes that access the manifest

retCode g__pkg_is_present(processPo P, ptrPo a) {
  ptrI a1 = deRefI(&a[1]);
  ptrI a2 = deRefI(&a[2]);
  ptrI a3 = deRefI(&a[3]);
  ptrI a4 = deRefI(&a[4]);

  if (isvar(a1) || isvar(a2) || isvar(a3))
    return liberror(P, "_pkg_is_present", eSTRNEEDD);
  else if (!isString(objV(a1)) || !isString(objV(a2)) || !isString(objV(a3)))
    return liberror(P, "_pkg_is_present", eSTRNEEDD);
  else if (!isvar(a4))
    return liberror(P, "_pkg_is_present", eVARNEEDD);
  else {
    stringPo s1 = stringV(a1);
    stringPo s2 = stringV(a2);
    stringPo s3 = stringV(a3);

    byte pkgNm[MAX_SYMB_LEN];
    copyString2Buff(pkgNm, NumberOf(pkgNm), s1);

    byte verNm[MAX_SYMB_LEN];
    copyString2Buff(verNm, NumberOf(verNm), s2);

    byte kndNm[MAX_SYMB_LEN];
    copyString2Buff(kndNm, NumberOf(kndNm), s3);

    byte fn[MAXFILELEN];
    string actualFn = manifestResource(pkgNm, verNm, kndNm, fn, NumberOf(fn));

    if (actualFn == NULL)
      return Fail;
    else if (filePresent(actualFn) != Ok)
      return Fail;
    else {
      ptrI reslt = allocateCString(&P->proc.heap, (char *) actualFn);
      return equal(P, &reslt, &a[4]);
    }
  }
}
