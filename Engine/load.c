//
// Created by Francis McCabe on 2/3/17.
//

#include <base64.h>

#include "lo.h"
#include "signature.h"
#include "encodedP.h"             /* pick up the term encoding definitions */
#include "manifestP.h"
#include "hashTable.h"

logical isLoaded(ptrI pkg) {
  return (logical) (loadedVersion(SymVal(symbV(pkg))) != NULL);
}

static logical compatiblVersion(string rqVer, string ver) {
  return (logical) (uniCmp(rqVer, (string) "*") == same || uniCmp(rqVer, ver) == same);
}

static retCode decodePkgSignature(ioPo in, byte *pkgNm, long nmLen, byte *vrNm, long vrLen);

retCode loadSegments(ioPo file, string errorMsg, long msgLen);

retCode loadPkg(string pkg, string vers, string errorMsg, long msgSize) {
  string version = loadedVersion(pkg);

  if (version != NULL) {
    if (!compatiblVersion(vers, version)) {
      outMsg(logFile, "invalid version of package already loaded: %s:%s,"
        "version %s expected\n", pkg, version, vers);
      return Error;
    } else
      return Ok; // already loaded correct version
  } else {
    string fn = packageCodeFile(pkg, vers);

    if (fn == NULL) {
      outMsg(logFile, "cannot determine code for %s:%s", pkg, version);
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
        ret = decodePkgSignature(file, &pkgNm[0], NumberOf(pkgNm), &vrNm[0], NumberOf(vrNm));

        if (ret == Ok && uniCmp((string) pkgNm, pkg) != same) {
          outMsg(logFile, "loaded package: %s not what was expected %w\n", (string) pkgNm, pkg);
          return Error;
        }

        ret = loadSegments(file, errorMsg, msgSize);
      }

      closeFile(file);

#ifdef EXECTRACE
      if (debugging)
        outMsg(logFile, "package %s loaded\n", pkg);
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
}

/*
 * A package signature consists of a tuple of 7 elements:
 * (pkg,imports,fields,types,contracts,implementations)
 *
 * We are only interested in the first two the pkg and the imports.
 */

static retCode decodePkgName(ioPo in, byte *nm, long nmLen, byte *v, long vLen);
static retCode decodePrgName(ioPo in, byte *nm, long nmLen, integer *arity);

static retCode decodeImports(ioPo in, string errorMsg, long msgLen);

retCode decodePkgSignature(ioPo in, byte *pkgNm, long nmLen, byte *vrNm, long vrLen) {
  byte ch;
  retCode ret = inByte(in, &ch);

  if (ret != Ok)
    return ret;
  else if (ch != trmString)
    return Error;
  else {
    bufferPo buffer = newStringBuffer();
    ret = decodeText(in, buffer);

    // The first characters should be fixed by the encoding
    rewindBuffer(buffer);

    if (ret == Ok && isLookingAt(O_IO(buffer), "n7o7'()7'") == Ok) {
      if ((ret = decodePkgName(O_IO(buffer), pkgNm, nmLen, vrNm, vrLen)) == Ok) {
        ret = packageIsLoaded((string) pkgNm, (string) vrNm);

        if (ret == Ok)
          ret = decodeImports(O_IO(buffer), NULL, 0);
      }

    } else
      ret = Error;

    closeFile(O_IO(buffer));
    return ret;
  }
}

retCode decodePkgName(ioPo in, byte *nm, long nmLen, byte *v, long vLen) {
  if (isLookingAt(in, "n2o2'pkg'") == Ok) {
    bufferPo pkgB = fixedStringBuffer(nm, nmLen);
    bufferPo vrB = fixedStringBuffer(v, vLen);

    retCode ret = decodeName(O_IO(in), pkgB);

    if (ret == Ok)
      ret = decodeName(O_IO(in), vrB);

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
      closeFile(O_IO(pkgB));
      return ret;
    }
  } else
    return Error;
}

retCode decodeImports(ioPo in, string errorMsg, long msgLen) {
  if (isLookingAt(in, "n") == Ok) {
    integer len;
    retCode ret = decInt(in, &len);

    if (ret == Ok)
      ret = skipEncoded(in, errorMsg, msgLen); // Move over the tuple constructor
    for (integer ix = 0; ret == Ok && ix < len; ix++) {
      byte pkgNm[MAX_SYMB_LEN];
      byte vrNm[MAX_SYMB_LEN];
      ret = decodePkgName(in, &pkgNm[0], NumberOf(pkgNm), &vrNm[0], NumberOf(vrNm));

      if (ret == Ok)
        ret = loadPkg((string) pkgNm, (string) vrNm, errorMsg, msgLen);
    }
    return ret;
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

    if (ret == Ok && isLookingAt(in,"s")==Ok) {
      bufferPo buff = newStringBuffer();

      ret = decodeText(in,buff); // Pick up the code - as base64 string

      if(ret!=Ok || isLookingAt(in,"c")!=Ok) { // Look for the tuple of literals
        closeFile(O_IO(buff));
        return ret;
      }

      rewindBuffer(buff); // tmpBufer should contain base64 text

      integer litCount;

      ret = decInt(in, &litCount);

      if(ret==Ok)
        ret = skipEncoded(in,errorMsg,msgSize); // we know this is a tuple structure marker

      if(ret!=Ok){
        closeFile(O_IO(buff));
        return ret;
      }else {
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
          ptrI pc = permCode((unsigned long)codeCount, (unsigned long)litCount);

          insPo cd = FirstInstruction(pc);
          ptrI el = kvoid;

          rootPo root = gcAddRoot(GH, &pc); /* in case of GC ... */
          gcAddRoot(GH, &el); /* we need a temporary pointer */

          ptrI prg = newProgLbl((char *) prgName, arity);
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

            codeV(pc)->arity = (unsigned short)arity; /* set the arity of the program */

            // Now we find the literals

            for (long i = 0; ret == Ok && i < litCount; i++) {
              if ((ret = decode(in, &sp, GH, &el, buff)) != Ok) /* read each element of term */
                break; /* we might need to skip out early */
              else {
                updateCodeLit(codeV(pc), i, el);
              }
            }
            gcRemoveRoot(GH, root); /* clear the GC root */

            if (ret != Ok) {
              closeFile(O_IO(buff));
              closeFile((O_IO(cdeBuffer)));
              return ret;
            } else {
              defineProg(prg, pc);

              return ret;
            }
          }
        }
      }
    }
    return ret;
  }
}

