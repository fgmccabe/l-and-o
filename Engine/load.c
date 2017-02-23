//
// Created by Francis McCabe on 2/3/17.
//

#include <base64.h>
#include <load.h>

#include "lo.h"
#include "signature.h"
#include "encodedP.h"             /* pick up the term encoding definitions */
#include "manifestP.h"
#include "tpl.h"
#include "hashTable.h"
#include "load.h"

static poolPo packagePool = NULL;
static hashPo loadedPackages = NULL;

void initPackages() {
  packagePool = newPool(sizeof(PackageRec), 128);
  loadedPackages = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}

packagePo loadedPackage(string package) {
  return (packagePo) hashGet(loadedPackages, package);
}

string loadedVersion(string package) {
  packagePo pkg = loadedPackage(package);

  if (pkg != NULL)
    return (string) &pkg->version;

  return NULL;
}

static logical compatiblVersion(string rqVer, string ver);

retCode markLoaded(string package, string version) {
  packagePo pkg = loadedPackage(package);

  if (pkg != NULL && !compatiblVersion((string) &pkg->version, version))
    return Error;
  else {
    pkg = (packagePo) allocPool(packagePool);
    uniCpy((byte *) &pkg->packageName, NumberOf(pkg->packageName), package);
    uniCpy((byte *) &pkg->version, NumberOf(pkg->version), version);
    hashPut(loadedPackages, &pkg->packageName, pkg);
    return Ok;
  }
}

logical isLoaded(ptrI pkg) {
  return (logical) (loadedPackage(StringVal(stringV(pkg))) != NULL);
}

static logical compatiblVersion(string rqVer, string ver) {
  return (logical) (uniCmp(rqVer, (string) "*") == same || uniCmp(rqVer, ver) == same);
}

typedef retCode (*pickupPkg)(string pkgNm, string vers, string errorMsg, long msgLen, void *cl);

static retCode decodePkgName(ioPo in, byte *nm, long nmLen, byte *v, long vLen);
static retCode decodePrgName(ioPo in, byte *nm, long nmLen, integer *arity);
static retCode decodeLoadedPkg(byte *pkgNm, long nmLen, byte *vrNm, long vrLen, bufferPo sigBuffer);
static retCode decodeImportsSig(bufferPo sigBuffer, string errorMsg, long msgLen, pickupPkg pickup, void *cl);

retCode loadSegments(ioPo file, string errorMsg, long msgLen);

static retCode ldPackage(string pkg, string vers, string errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  byte flNm[MAXFILELEN];
  string fn = packageCodeFile(pkg, vers, flNm, NumberOf(flNm));

  if (fn == NULL) {
    outMsg(logFile, "cannot determine code for %s:%s", pkg, vers);
    return Error;
  }

  ioPo file = openInFile(fn, utf8Encoding);

#ifdef EXECTRACE
  if (debugging)
    outMsg(logFile, "loading package %s:%s from file %s\n", pkg, vers, fn);
#endif

  if (file != NULL) {
    retCode ret = Ok;

    byte ch;

    if ((ch = inB(file)) == '#') { /* look for standard #!/.... header */
      if ((ch = inB(file)) == '!') {
        while (inByte(file, &ch) == Ok && ch != NEW_LINE);                      // consume the interpreter statement
      } else {
        putBackByte(file, ch);
        putBackByte(file, '#');
      }
    } else
      putBackByte(file, ch);

    if (fileStatus(file) == Ok) {
      byte pkgNm[MAX_SYMB_LEN];
      byte vrNm[MAX_SYMB_LEN];
      bufferPo sigBuffer = newStringBuffer();

      if ((ret = isLookingAt(file, "s")) == Ok)
        ret = decodeText(file, sigBuffer);

      if (ret == Ok)
        ret = decodeLoadedPkg(pkgNm, NumberOf(pkgNm), vrNm, NumberOf(vrNm), sigBuffer);

      if (ret == Ok && uniCmp((string) pkgNm, pkg) != same) {
        outMsg(logFile, "loaded package: %s not what was expected %w\n", (string) pkgNm, pkg);
        return Error;
      }

      markLoaded(pkgNm, vrNm);

      ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

      if (ret == Ok)
        ret = loadSegments(file, errorMsg, msgSize);
    }

    closeFile(file);

#ifdef EXECTRACE
    if (debugging)
      logMsg(logFile, "package %s loaded\n", pkg);
#endif

    if (ret == Eof)
      return Ok;
    else
      return ret;
  } else {
    strMsg(errorMsg, msgSize, "package %s not found", pkg);
    return Eof;
  }
}

retCode loadPackage(string pkg, string vers, string errorMsg, long msgSize, void *cl) {
  string version = loadedVersion(pkg);

  if (version != NULL) {
    if (!compatiblVersion(vers, version)) {
      outMsg(logFile, "invalid version of package already loaded: %s:%s,"
        "version %s expected\n", pkg, version, vers);
      return Error;
    } else
      return Ok; // already loaded correct version
  } else
    return ldPackage(pkg, vers, errorMsg, msgSize, loadPackage, cl);
}

static retCode
installPackage(string pkgText, long pkgTxtLen, string errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  bufferPo inBuff = fixedStringBuffer(pkgText, pkgTxtLen);

  retCode ret;
  byte pkgNm[MAX_SYMB_LEN];
  byte vrNm[MAX_SYMB_LEN];
  bufferPo sigBuffer = newStringBuffer();

  if ((ret = isLookingAt(O_IO(inBuff), "s")) == Ok)
    ret = decodeText(O_IO(inBuff), sigBuffer);

  if (ret == Ok)
    ret = decodeLoadedPkg(pkgNm, NumberOf(pkgNm), vrNm, NumberOf(vrNm), sigBuffer);

  markLoaded(pkgNm, vrNm);

  ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

  if (ret == Ok)
    ret = loadSegments(O_IO(inBuff), errorMsg, msgSize);

  closeFile(O_IO(inBuff));

#ifdef EXECTRACE
  if (debugging)
    logMsg(logFile, "package %s installed\n", pkgNm);
#endif

  if (ret == Eof)
    return Ok;
  else
    return ret;
}

/*
 * A package signature consists of a tuple of 7 elements:
 * (pkg,imports,fields,types,contracts,implementations)
 *
 * We are only interested in the first two the pkg and the imports.
 */

retCode decodeLoadedPkg(byte *pkgNm, long nmLen, byte *vrNm, long vrLen, bufferPo sigBuffer) {
  rewindBuffer(sigBuffer);

  if (isLookingAt(O_IO(sigBuffer), "n7o7'()7'") == Ok)
    return decodePkgName(O_IO(sigBuffer), pkgNm, nmLen, vrNm, vrLen);
  else
    return Error;
}

static retCode decodeImportsSig(bufferPo sigBuffer, string errorMsg, long msgLen, pickupPkg pickup, void *cl) {
  rewindBuffer(sigBuffer);
  ioPo in = O_IO(sigBuffer);

  if (isLookingAt(in, "n7o7'()7'") == Ok) {
    retCode ret = skipEncoded(in, errorMsg, msgLen);
    if (ret != Ok)
      return ret;

    if (isLookingAt(in, "n") == Ok) {
      integer len;
      ret = decInt(in, &len);

      // The imports are next in the signature
      if (ret == Ok)
        ret = skipEncoded(in, errorMsg, msgLen); // Move over the tuple constructor
      for (integer ix = 0; ret == Ok && ix < len; ix++) {
        ret = isLookingAt(in, "n2o2'import'");

        if (ret == Ok)
          ret = skipEncoded(in, errorMsg, msgLen);  // skip the private/public flag

        byte pkgNm[MAX_SYMB_LEN];
        byte vrNm[MAX_SYMB_LEN];

        if (ret == Ok)
          ret = decodePkgName(in, &pkgNm[0], NumberOf(pkgNm), &vrNm[0], NumberOf(vrNm));

        if (ret == Ok)
          ret = pickup(pkgNm, vrNm, errorMsg, msgLen, cl);
      }
      return ret;
    } else
      return Error;
  } else
    return Error;
}

retCode decodePkgName(ioPo in, byte *nm, long nmLen, byte *v, long vLen) {
  if (isLookingAt(in, "n2o2'pkg's") == Ok) {
    bufferPo pkgB = fixedStringBuffer(nm, nmLen);
    bufferPo vrB = fixedStringBuffer(v, vLen);

    retCode ret = decodeName(O_IO(in), pkgB);

    if (ret == Ok) {
      if (isLookingAt(in, "e'*'") == Ok)
        outStr(O_IO(vrB), "*");
      else if (isLookingAt(in, "s") == Ok) {
        ret = decodeText(O_IO(in), vrB);
      }
    }

    outByte(O_IO(pkgB), 0);
    outByte(O_IO(vrB), 0);

    closeFile(O_IO(pkgB));
    closeFile(O_IO(vrB));
    return ret;
  } else
    return Error;
}

retCode decodePrgName(ioPo in, byte *nm, long nmLen, integer *arity) {
  if (isLookingAt(in, "p") == Ok) {
    retCode ret = decInt(O_IO(in), arity);

    if (ret != Ok)
      return ret;
    else {
      bufferPo pkgB = fixedStringBuffer(nm, nmLen);
      ret = decodeName(O_IO(in), pkgB);
      outByte(O_IO(pkgB), 0);
      closeFile(O_IO(pkgB));
      return ret;
    }
  } else
    return Error;
}

static retCode loadCodeSegment(ioPo in, string errorMsg, long msgSize);

retCode loadSegments(ioPo file, string errorMsg, long msgLen) {
  retCode ret = Ok;

  while (ret == Ok) {
    ret = loadCodeSegment(file, errorMsg, msgLen);
  }

  return ret;
}

/* swap bytes in the little endian game */
static inline void SwapBytes(unsigned long *x) {
  *x = ((*x & 0xff) << 8) | ((*x >> 8) & 0xff) | ((*x & 0x00ff0000L) << 8)
       | ((*x & 0xff000000L) >> 8);
}

static inline void SwapWords(unsigned long *x) {
  *x = (*x & 0x0000ffffL) << 16 | (*x & 0xffff0000L) >> 16;
}

static retCode in32(ioPo in, int32 *tgt) {
  byte b1 = 0, b2 = 0, b3 = 0, b4 = 0;

  retCode ret = inByte(in, &b1);

  if (ret == Ok)
    ret = inByte(in, &b2);

  if (ret == Ok)
    ret = inByte(in, &b3);

  if (ret == Ok)
    ret = inByte(in, &b4);

  if (ret == Ok)
    *tgt = b1 << 24 | b2 << 16 | b3 << 8 | b4;
  return ret;
}

// Decode a code segment.
// Each segment consists of
// a. The program structure object being defined
// b. A string containing the code as a base64 encoded string.
// c. A tuple of literals associated with the code segment
// All wrapped up as a #code structure.

retCode loadCodeSegment(ioPo in, string errorMsg, long msgSize) {
  EncodeSupport sp = {
    NULL,
    0,
    errorMsg,
    msgSize,
    &globalHeap,
  };

  if (isFileAtEof(in) == Eof)
    return Eof;
  else {
    retCode ret = isLookingAt(in, "n3o3'#code'");

    if (ret != Ok) {
      strMsg(errorMsg, msgSize, "invalid code stream");
      return Error;
    }
    byte prgName[MAX_SYMB_LEN];
    integer arity;

    ret = decodePrgName(in, prgName, NumberOf(prgName), &arity);

#ifdef EXECTRACE
    if (debugging) {
      outMsg(logFile, "loading segment %s:%d\n", prgName, arity);
      flushOut();
    }
#endif

    if (ret == Ok && isLookingAt(in, "s") == Ok) {
      bufferPo buff = newStringBuffer();

      ret = decodeText(in, buff); // Pick up the code - as base64 string

      if (ret != Ok || isLookingAt(in, "n") != Ok) { // Look for the tuple of literals
        closeFile(O_IO(buff));
        return ret;
      }

      rewindBuffer(buff); // tmpBufer should contain base64 text

      integer litCount;

      ret = decInt(in, &litCount);

      if (ret == Ok)
        ret = skipEncoded(in, errorMsg, msgSize); // we know this is a tuple structure marker

      if (ret != Ok) {
        closeFile(O_IO(buff));
        return ret;
      } else {
        // Decode the base64 text
        bufferPo cdeBuffer = newStringBuffer();

        ret = decode64(O_IO(cdeBuffer), O_IO(buff));
        rewindBuffer(cdeBuffer);

        if (ret != Ok) {
          closeFile(O_IO(buff));
          closeFile((O_IO(cdeBuffer)));
          return ret;
        } else {
          integer codeCount = (bufferSize(cdeBuffer) / SIZEOF_INT) - 1;
          int32 signature;

          ret = in32(O_IO(cdeBuffer), &signature); // verify correct code signature

          heapPo GH = &globalHeap;
          ptrI pc = permCode((unsigned long) codeCount, (unsigned long) litCount);

          insPo cd = FirstInstruction(pc);
          ptrI el = kvoid;

          rootPo root = gcAddRoot(GH, &pc); /* in case of GC ... */
          gcAddRoot(GH, &el); /* we need a temporary pointer */

          ptrI prg = newProgLbl((char *) prgName, (short) arity);
          gcAddRoot(GH, &prg);

          /* get the instructions */
          for (long i = 0; ret == Ok && i < codeCount; i++)
            ret = in32(O_IO(cdeBuffer), &cd[i]);

          if (ret != Ok) {
            closeFile(O_IO(buff));
            closeFile((O_IO(cdeBuffer)));
            gcRemoveRoot(GH, root); /* clear the GC root */
            return ret;
          } else {
            /* Now convert the main code to handle little endians etc */
            if (signature == SIGNATURE) {
            } /* endian load same as endian save */
            else if (signature == SIGNBSWAP) { /* swap bytes keep words */
              unsigned long *xx = (unsigned long *) FirstInstruction(pc);
              long cnt = codeCount;
              for (; cnt--; xx++)
                SwapBytes(xx);
            } else if (signature == SIGNWSWAP) { /* swap words keep bytes */
              unsigned long *xx = (unsigned long *) FirstInstruction(pc);
              long cnt = codeCount;
              for (; cnt--; xx++)
                SwapWords(xx);
            } else if (signature == SIGNBWSWP) { /* swap words and bytes */
              unsigned long *xx = (unsigned long *) FirstInstruction(pc);
              long cnt = codeCount;
              for (; cnt--; xx++) {
                SwapWords(xx);
                SwapBytes(xx);
              }
            }

            codeV(pc)->arity = (uint16) arity; /* set the arity of the program */

            // Now we find the literals

            for (long i = 0; ret == Ok && i < litCount; i++) {
              if ((ret = decode(in, &sp, GH, &el, buff)) != Ok) /* read each element of term */
                break; /* we might need to skip out early */
              else {
                updateCodeLit(codeV(pc), i, el);
              }
            }

            gcRemoveRoot(GH, root); /* clear the GC root */

            closeFile(O_IO(buff));
            closeFile((O_IO(cdeBuffer)));

            if (ret == Ok)
              defineProg(prg, pc);

            return ret;
          }
        }
      }
    }
    return ret;
  }
}

typedef struct {
  heapPo H;
  ptrPo lst;
  ptrPo el;
  ptrPo pk;
  ptrPo vr;
} BuildSupportRec;

static retCode buildImport(string pkg, string ver, string errorMsg, long msgLen, void *cl) {
  BuildSupportRec *x = (BuildSupportRec *) cl;

  *x->pk = allocateCString(x->H, (char *) pkg);
  *x->vr = allocateCString(x->H, (char *) ver);
  *x->el = tuplePair(x->H, *x->pk, *x->vr);
  *x->lst = consLsPair(x->H, *x->el, *x->lst);
  return Ok;
}

retCode g__install_pkg(processPo P, ptrPo a) {
  ptrI pkgText = deRefI(&a[1]);

  if (isvar(pkgText))
    return liberror(P, "_install_pkg", eINSUFARG);
  else if (!IsString(pkgText))
    return liberror(P, "_install_pkg", eINVAL);
  else {
    switchProcessState(P, wait_io); /* Potentially nec. to wait */

    heapPo H = &P->proc.heap;
    ptrI lst = emptyList;
    ptrI el = kvoid;
    ptrI ky = kvoid;
    ptrI vl = kvoid;
    rootPo root = gcAddRoot(H, &lst);

    gcAddRoot(H, &el);
    gcAddRoot(H, &ky);
    gcAddRoot(H, &vl);

    BuildSupportRec x = {
      H,
      &lst,
      &el,
      &ky,
      &vl
    };

    stringPo text = stringV(pkgText);

    retCode ret = installPackage(stringVal(text), stringLen(text), P->proc.errorMsg,
                            NumberOf(P->proc.errorMsg), buildImport, &x);
    setProcessRunnable(P);

    gcRemoveRoot(H, root);

    switch (ret) {
      case Ok:
        return equal(P, &lst, &a[3]);
      case Error:
      case Eof:
        return liberror(P,"_install_pkg",eINVAL);
      case Fail:
        return Fail;
      case Space:
        outMsg(logFile, "Out of heap space, increase and try again\n%_");
        return liberror(P, "_install_pkg", eSPACE);
      default:
        return liberror(P, "_install_pkg", eINVAL);
    }
  }
}
