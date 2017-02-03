//
// Created by Francis McCabe on 2/3/17.
//

#include <base64.h>

#include "lo.h"
#include "signature.h"
#include "encodedP.h"             /* pick up the term encoding definitions */

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
