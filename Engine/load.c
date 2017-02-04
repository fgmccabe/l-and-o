//
// Created by Francis McCabe on 2/3/17.
//

#include <base64.h>

#include "lo.h"
#include "signature.h"
#include "encodedP.h"             /* pick up the term encoding definitions */
#include "manifestP.h"

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

retCode decodeCodeSegment(ioPo in, encodePo S, heapPo H, ptrPo tgt) {
  codePoint ch;

  retCode ret = inChar(in, &ch);

  if (ret != Ok)
    return ret;

  if (ch == trmCns) {
    bufferPo tmpBuffer = newStringBuffer();

    ret = decode(in, S, H, tgt, tmpBuffer); // This is the program label

    if (ret != Ok) {
      closeFile(O_IO(tmpBuffer));
      return ret;
    } else {
      assert(IsProgLbl(*tgt));

      if ((ret = inChar(in, &ch)) != Ok) {
        closeFile(O_IO(tmpBuffer));
        return ret;
      } else {
        assert(ch == trmString);

        clearBuffer(tmpBuffer);

        if ((ret = decodeText(in, tmpBuffer)) != Ok) {
          closeFile(O_IO(tmpBuffer));
          return ret;
        } else {
          rewindBuffer(tmpBuffer); // tmpBufer should contain base64 text

          // See how many literals we have -- we dont decode them yet.
          if ((ret = inChar(in, &ch)) != Ok) {
            closeFile(O_IO(tmpBuffer));
            return ret;
          } else {
            assert(ch == trmCns);

            integer litCount;

            ret = decInt(in, S, &litCount);

            if (ret == Ok)ret = skipTrm(in, S); // Skip the tuple struct marker itself.
            if (ret != Ok) {
              closeFile(O_IO(tmpBuffer));
              return ret;
            } else {
              // Decode the base64 text
              bufferPo cdeBuffer = newStringBuffer();

              ret = decode64(O_IO(cdeBuffer), O_IO(tmpBuffer));
              rewindBuffer(cdeBuffer);

              if (ret != Ok) {
                closeFile(O_IO(tmpBuffer));
                closeFile((O_IO(cdeBuffer)));
                return ret;
              } else {
                integer codeCount = (bufferSize(cdeBuffer) / SIZEOF_INT) - 1;
                int32 signature;

                ret = in32(O_IO(cdeBuffer), &signature); // verify correct code signature

                heapPo GH = &globalHeap;
                ptrI pc = permCode(codeCount, litCount);

                long i;
                insPo cd = FirstInstruction(pc);
                ptrI el = kvoid;

                rootPo root = gcAddRoot(S->R, &pc); /* in case of GC ... */
                gcAddRoot(S->R, &el); /* we need a temporary pointer */

                /* get the instructions */
                for (i = 0; ret == Ok && i < codeCount; i++)
                  ret = in32(O_IO(cdeBuffer), &cd[i]);

                if (ret != Ok) {
                  closeFile(O_IO(tmpBuffer));
                  closeFile((O_IO(cdeBuffer)));
                  gcRemoveRoot(S->R, root); /* clear the GC root */
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

                  codeV(pc)->arity = (unsigned short) programArity(objV(*tgt)); /* set the arity of the program */

                  // Now we find the literals

                  for (i = 0; ret == Ok && i < litCount; i++) {
                    if ((ret = decode(in, S, GH, &el, tmpBuffer)) != Ok) /* read each element of term */
                      break; /* we might need to skip out early */
                    else {
                      updateCodeLit(codeV(pc), i, el);
                    }
                  }
                  gcRemoveRoot(S->R, root); /* clear the GC root */

                  if (ret != Ok) {
                    closeFile(O_IO(tmpBuffer));
                    closeFile((O_IO(cdeBuffer)));
                    return ret;
                  } else {
                    defineProg(*tgt, pc);

                    return ret;
                  }
                }
              }
            }
          }
        }
      }
    }
  } else
    return Error;
}

retCode pkgLoader(heapPo H, string path, ptrI request, ptrI rqV, ptrPo loaded, string errorMsg,
                  long msgSize) {
  PackageRec *ldFlag = (PackageRec *) hashGet(loadedPackages, SymVal(symbV(request)));

  if (ldFlag != NULL) {
    if (rqV != emptySymbol)
      if (uniCmp(SymVal(symbV(rqV)), ldFlag->version) == 0)
        return Ok;
      else {
        outMsg(logFile, "invalid version of package already loaded: %w:%U,"
            "version %w expected\n", &request, ldFlag->version, &rqV);
        return Error;
      }
    else
      return Ok;
  } else {
    byte fbuffer[MAX_FILE_LEN];
    string fn = computeClassFileName(path, uniStrLen(path), SymVal(symbV(request)), SymVal(symbV(rqV)),
        fbuffer, NumberOf(fbuffer));
    ioPo file = fn != NULL ? openInFile(fn, rawEncoding) : NULL;
    byte ch;

#ifdef EXECTRACE
    if (debugging)
      outMsg(logFile, "loading package %w from file %U\n", &request, fn);
#endif

    if (file != NULL) {
      retCode ret = Ok;
      ptrI scratch = kvoid;
      ptrI package = kvoid;
      ptrI version = kvoid; /* version of the loaded package */
      ptrI imports = kvoid;
      ptrI defined = kvoid;
      rootPo root = gcAddRoot(H, &scratch);

      gcAddRoot(H, &package);
      gcAddRoot(H, &request);
      gcAddRoot(H, &version);
      gcAddRoot(H, &imports);
      gcAddRoot(H, &defined);

      if ((ch = inB(file)) == '#') { /* look for standard #!/.... header */
        if ((ch = inB(file)) == '!') {
          while (inByte(file, &ch) == Ok && ch != NEW_LINE)
            ;                      // consume the interpreter statement
        } else {
          putBackByte(file, ch);
          putBackByte(file, '#');
        }
      } else
        putBackByte(file, ch);

      if (fileStatus(file) == Ok) {
        ret = decodeTerm(file, &globalHeap, H, &package, errorMsg, msgSize);

        if (package != request) {
          outMsg(logFile, "loaded package: %w not what was expected %w\n", &package, &request);
          return Error;
        }

        ret = decodeTerm(file, &globalHeap, H, &version, errorMsg, msgSize);

        //outMsg(logFile,"package is %w:%w\n",&package,&version);

        if (version != rqV && rqV != emptySymbol && version != universal) {
          outMsg(logFile, "invalid version of package: %w:%w,"
              "version %w expected\n", &request, &version, &rqV);
          return Error;
        }

        packagePo pkgInfo = malloc(sizeof(PackageRec));

        uniCpy(pkgInfo->packageName, NumberOf(pkgInfo->packageName), SymVal(symbV(package)));
        uniCpy(pkgInfo->version, NumberOf(pkgInfo->version), SymVal(symbV(version)));

        hashPut(loadedPackages, pkgInfo->packageName, pkgInfo);

        if (ret == Ok)
          ret = skipEncoded(file, errorMsg, msgSize);

        if (ret == Ok)
          ret = skipEncoded(file, errorMsg, msgSize);

        if (ret == Ok) {
          ret = decodeTerm(file, &globalHeap, H, &imports, errorMsg, msgSize);
          //	  outMsg(logFile,"Imported packages: %w\n",&imports);
        }

        if (ret == Ok) {
          ret = decodeTerm(file, &globalHeap, H, &defined, errorMsg, msgSize); /* Locally defined programs */

          //	  setProperty(H,package,kdefined,defined);
        }

        if (ret == Ok) {
          if (IsList(imports)) {
            ptrI imps = imports;
            gcAddRoot(H, &imps);

            while (IsList(imps)) {
              ptrI entry = deRefI(listHead(objV(imps)));

              if (HasClass(entry, commaClass)) {
                ptrI import = deRefI(nthArg(objV(entry), 0));
                ptrI vers = deRefI(nthArg(objV(entry), 1));
                rootPo subRoot = gcAddRoot(H, &import);

                gcAddRoot(H, &vers);
                gcAddRoot(H, &import);

                ret = pkgLoader(H, path, import, vers, loaded, errorMsg, msgSize);

                gcRemoveRoot(H, subRoot);

                switch (ret) {
                case Ok:
                  break;
                default:
                case Error:
                  outMsg(logFile, "Failed to load package %U, "
                      "[version: %U] requested by %U\n", SymVal(symbV(import)), SymVal(symbV(vers)),
                      SymVal(symbV(request)));
                  break;
                case Space:
                  outMsg(logFile, "Not enough heap space to load package %U, "
                      "[version: %U] requested by %U\n", SymVal(symbV(import)), SymVal(symbV(vers)),
                      SymVal(symbV(request)));
                  break;
                }
              } else
                outMsg(logFile, "invalid version info import package "
                    "spec %w, requested by %w\n", &imports, &request);
              imps = deRefI(listTail(objV(imps)));
            }
          } else if (!identical(imports, emptyList))
            outMsg(logFile, "invalid import package spec %w, "
                "requested by %w\n", &imports, &request);
        }

        while (ret == Ok) {
          ret = decodeTerm(file, &globalHeap, H, &scratch, errorMsg, msgSize);

          if (ret == Ok) {
            if (HasClass(scratch, commaClass)) {
              ptrI prog = deRefI(nthArg(objV(scratch), 1));
              ptrI symb = deRefI(nthArg(objV(scratch), 0));
              if (IsCode(prog) && IsProgLbl(symb)) {
                defineProg(symb, prog);

#ifdef EXECTRACE
                if (debugging)
                  outMsg(logFile, "program %w loaded\n", &symb);
#endif
              } else
                outMsg(logFile, "code expected, not: %#,4w in code file", &scratch);
            } else
              outMsg(logFile, "invalid entry: %#,4w in code file", &scratch);
          }
        }
      }

      if (ret != Space)
        *loaded = permLsPair(H, request, *loaded);

      gcRemoveRoot(H, root);
      closeFile(file);

#ifdef EXECTRACE
      if (debugging)
        outMsg(logFile, "package %w loaded\n", &package);
#endif

      if (ret == Eof)
        return Ok;
      else
        return ret;
    } else {
      strMsg(errorMsg, msgSize, "package %U not found", SymVal(symbV(request)));
      return Eof;
    }
  }
}
