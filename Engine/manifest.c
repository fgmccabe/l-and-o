//
// Created by Francis McCabe on 2/4/17.
//

#include <jsonEvent.h>
#include "manifestP.h"

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
static pthread_once_t once = PTHREAD_ONCE_INIT;

static hashPo manifest;

static void initManifest(void) {
  if (manifestPool == NULL) {
    manifestPool = newPool(sizeof(ManifestEntryRecord), 128);
    versionPool = newPool(sizeof(ManifestVersionRecord), 128);
    manifest = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
  }
}

static void init() {
  pthread_once(&once, initManifest);
}

// Manage the manifest

manifestEntryPo newManifestEntry(string name) {
  manifestEntryPo entry = (manifestEntryPo) allocPool(manifestPool);
  uniCpy((string) &entry->package, NumberOf(entry->package), name);
  entry->versions = NewHash(1, (hashFun) uniHash, (compFun) uniCmp, NULL);
  entry->deflt = NULL;
  entry->loaded = NULL;
  hashPut(manifest, &entry->package, entry);
  return entry;
}

manifestEntryPo manifestEntry(string package) {
  init();
  return (manifestEntryPo) hashGet(manifest, package);
}

manifestVersionPo newVersion(string version, string source, string code) {
  manifestVersionPo vEntry = (manifestVersionPo) allocPool(versionPool);
  uniCpy((string) &vEntry->version, NumberOf(vEntry->version), version);
  uniCpy((string) &vEntry->source, NumberOf(vEntry->source), source);
  uniCpy((string) &vEntry->code, NumberOf(vEntry->code), code);
  vEntry->isDefault = uniCmp(version, (string) "*") == same ? True : False;
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

string packageCodeFile(string package, string version) {
  manifestVersionPo entry = manifestVersion(package, version);

  if (entry != NULL)
    return (string) &entry->code;
  else
    return NULL;
}

string loadedVersion(string package) {
  manifestEntryPo entry = manifestEntry(package);

  if (entry != NULL) {
    manifestVersionPo loaded = entry->loaded;

    if (loaded != NULL)
      return (string) &loaded->version;
  }
  return NULL;
}

retCode packageIsLoaded(string package, string version) {
  manifestEntryPo entry = manifestEntry(package);

  if (entry->loaded != NULL) {
    if (uniCmp((string) &entry->loaded->version, version) != same)
      return Error;
    else
      return Ok;
  } else {
    manifestVersionPo v = hashGet(entry->versions, version);
    if (v == NULL)
      return Error;
    else {
      entry->loaded = v;
      return Ok;
    }
  }
}

typedef enum {
  initial,
  inPackage,
  inVersion,
  inDetail,
  inSource,
  inCode
} ParseState;

typedef struct {
  byte pkg[MAXLINE]; // Package name
  manifestEntryPo entry;
  byte ver[MAXLINE];
  logical defaultVersion;
  byte source[MAXFILELEN];
  byte code[MAXFILELEN];
  ParseState state;
} ParsingState, *statePo;

retCode loadManifest(string repoDir) {
  init();

  byte manifestName[MAX_MSG_LEN];

  strMsg(manifestName, NumberOf(manifestName), "%s/manifest", repoDir);

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
  return Ok;
}

retCode endManifest(void *cl) {
  return Ok;
}

retCode startCollection(void *cl) {
  statePo info = (statePo) cl;

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
      uniCpy((string) &info->source, NumberOf(info->source), (string) "");
      uniCpy((string) &info->code, NumberOf(info->code), (string) "");
      info->state = inDetail;
      break;
    default:
      return Error;
  }
  return Ok;
}

retCode endCollection(void *cl) {
  statePo info = (statePo) cl;

  switch (info->state) {
    case initial:
      return Error;
    case inPackage:
      info->state = initial;
      break;
    case inVersion:
      info->state = inPackage;
      break;
    case inDetail:
    case inSource:
    case inCode:
      info->state = inVersion;
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

  switch (info->state) {
    case initial:
      return Error;
    case inPackage: {
      manifestEntryPo entry = (manifestEntryPo) hashGet(manifest, (void *) name);

      if (entry == NULL)
        entry = newManifestEntry((string) name);

      info->entry = entry;
      break;
    }
    case inVersion:
      uniCpy((string) &info->ver, NumberOf(info->ver), (string) name);
      if (uniCmp((string) name, (string) "*") == same)
        info->defaultVersion = True;
      else
        info->defaultVersion = False;
      break;
    case inDetail:
      if (uniCmp((string) name, (string) "source") == same)
        info->state = inSource;
      else if (uniCmp((string) name, (string) "code") == same)
        info->state = inCode;
      else
        return Error;
      break;
    default:
      return Error;
  }
  return Ok;
}

retCode endEntry(const char *name, void *cl) {
  statePo info = (statePo) cl;

  switch (info->state) {
    case inSource:
    case inCode:
      info->state = inDetail;
      break;
    case inVersion:
      if (info->entry != NULL) {
        manifestVersionPo version = (manifestVersionPo) hashGet(info->entry->versions, info->ver);
        if (version == NULL) {
          version = newVersion((string) &info->ver, (string) &info->source, (string) &info->code);
          hashPut(info->entry->versions, &version->version, version);
          if (version->isDefault)
            info->entry->deflt = version;
        } else {
          // We override -- effectively a reload. Should rarely happen
          uniCpy((string) &version->source, NumberOf(version->source), (string) info->source);
          uniCpy((string) &version->code, NumberOf(version->code), (string) info->code);
        }
      }
      info->state = inPackage;
      break;
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

  switch (info->state) {
    default:
      return Error;

    case inSource:
      uniCpy((string) &info->source, NumberOf(info->source), (string) name);
      break;
    case inCode:
      uniCpy((string) &info->code, NumberOf(info->code), (string) name);
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
