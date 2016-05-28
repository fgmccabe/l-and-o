/*
 * trie structure
 */

#include "trieP.h"
#include "retcode.h"
#include "logical.h"
#include "pool.h"
#include <stdlib.h>
#include <string.h>

static poolPo triePool;

static void init() {
  if (triePool == NULL) {
    triePool = newPool(sizeof(TrieRec), 1024);
  }
}

static triePo emptyTr(char *prefix) {
  init();

  triePo emptyTr = (triePo) allocPool(triePool);

  emptyTr->prefix = prefix;
  emptyTr->follows = NULL;
  emptyTr->value = NULL;
  return emptyTr;
}

triePo emptyTrie() {
  return emptyTr("");
}

static uinteger charHash(void *data) {
  char ch = (char) data;
  return (uinteger) ch;
}

static int charComp(void *l, void *r) {
  char left = (char) l;
  char right = (char) r;

  return left != right;
}

static void addToTr(char *full, char *key, void *value, triePo trie) {
  if (*key == '\0') {
    trie->value = value;
  } else {
    if (trie->follows == NULL) {
      trie->follows = NewHash(6, charHash, charComp, NULL);
    }
    long K = *key;
    triePo follows = (triePo) hashGet(trie->follows, (void *) K);
    if (follows == NULL) {
      char *prefix = (char *) calloc((key - full + 2), sizeof(byte));
      for (int ix = 0; ix < key - full + 1; ix++)
        prefix[ix] = full[ix];
      follows = emptyTr(strdup(prefix));
      hashPut(trie->follows, (void *) K, follows);
    }
    addToTr(full, key + 1, value, follows);
  }
}

void addToTrie(char *key, void *value, triePo trie) {
  addToTr(key, key, value, trie);
}

void *findInTrie(char *key, triePo trie) {
  if (trie == NULL)
    return NULL;
  else if ((*key) == '\0')
    return trie->value;
  else if (trie->follows != NULL) {
    long K = *key;
    triePo next = (triePo) hashGet(trie->follows, (void *) K);
    if (next != NULL)
      return findInTrie(key + 1, next);
    else
      return NULL;
  } else
    return NULL;
}

typedef struct {
  void *cl;
  trieProc proc;
} ClRecord, *clPo;

retCode trieEntryProc(void *n, void *v, void *cl) {
  char K = (char) n;
  triePo T = (triePo) v;
  clPo P = (clPo) cl;

  P->proc(T->prefix, T->value, P->cl);
  if (T->follows != NULL) {
    return ProcessTable(trieEntryProc, T->follows, cl);
  }
  return Ok;
}

void processTrie(triePo trie, trieProc proc, void *cl) {
  if (trie != NULL) {
    ClRecord Cl =
      {cl, proc};
    ProcessTable(trieEntryProc, trie->follows, (void *) &Cl);
  }
}

