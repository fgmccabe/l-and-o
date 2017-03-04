/*
  Output formatting functions for I/O library
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "io.h"
#include "formioP.h"

#include <float.h>		/* For fp conversion */
#include <limits.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <file.h>
#include <stringBuffer.h>

static retCode outString(ioPo f, byte *str, long len, long width, int precision,
                         codePoint pad, logical leftPad);

retCode outInt(ioPo f, integer i) {
  byte buff[64];
  long len = int2StrByBase(buff, i, 0, 10);

  return outText(f, buff, len);
}

static inline byte hxDgit(integer h) {
  if (h < 10)
    return (byte) (h | '0');
  else
    return (byte) (h + 'a' - 10);
}

static long natural2StrByBase(byte *str, uinteger i, long pos, uint16 base);

long int2StrByBase(byte *str, integer i, long pos, uint16 base) {
  if (i < 0) {
    str[pos++] = '-';
    return natural2StrByBase(str, (uinteger) -i, pos, base);
  } else
    return natural2StrByBase(str, (uinteger) i, pos, base);
}

static long natural2StrByBase(byte *str, uinteger i, long pos, uint16 base) {
  if (i < base)
    str[pos++] = hxDgit(i);
  else {
    pos = natural2StrByBase(str, i / base, pos, base);
    str[pos++] = hxDgit(i % base);
  }
  return pos;
}

retCode outInteger(ioPo f, integer i, uint16 base, int width, int precision,
                   codePoint pad, logical left, string prefix, logical sign) {
  byte iBuff[128];
  retCode ret;

  if (i >= 0 && sign)
    prefix = (string) "+";
  else if (i < 0) {
    prefix = (string) "-";
    i = -i;
  }

  long len = natural2StrByBase(iBuff, (uinteger) i, 0, base);

  ret = outUStr(f, prefix);

  if (ret == Ok)
    ret = outString(f, iBuff, (int) len, width - (int) strlen((char *) prefix), precision, pad, left);

  return ret;
}

static retCode outOctal(ioPo f, integer i, int width, int precision, codePoint pad,
                        logical left, byte *prefix, logical sign, logical alt) {
  byte iBuff[64];

  if (i < 0) {
    i = -i;
    prefix = (string) "-";
  } else if (i >= 0 && sign)
    prefix = (string) "+";

  long len = natural2StrByBase(iBuff, (uinteger) i, 0, 8);

  if (!left)
    pad = ' ';      /* We dont put trailing zeroes */

  retCode ret = outUStr(f, prefix);

  if (ret == Ok)
    ret = outString(f, iBuff, (int) len, width - (int) len, precision, pad, left);

  return ret;
}

static retCode outHex(ioPo f, long i, int width, int precision, codePoint pad,
                      logical left, string prefix, logical sign, logical alt) {
  byte iBuff[64];

  long len = natural2StrByBase(iBuff, (uinteger) i, 0, 16);

  if (alt)
    prefix = (string) "0x";

  if (!left)
    pad = ' ';      /* We dont put trailing zeroes */


  retCode ret = outUStr(f, prefix);

  if (ret == Ok)
    ret = outString(f, iBuff, (int) len, width - (int) len, precision, pad, left);

  return ret;
}

/* Convert a floating point to decimal format */

static const double bit_values[] = {
  1.0E1, 1.0E2, 1.0E4, 1.0E8, 1.0E16, 1.0E32, 1.0E64, 1.0E128, 1.0E256
};

static int number2Str(double x, int precision, byte *dec, long *exp) {
  int exp2;
  long exp10;
  int len = 0;
  byte *digits = dec;

/*
 *	We first deal with the special cases of zeroes, infinities and NaNs
 */

  *exp = 0;

  if (x == 0.0L)
    return (int) strlen(strcpy((char *) digits, "0"));
  else if (x > DBL_MAX)
    return (int) strlen(strcpy((char *) digits, "Infinity"));
  else {
    frexp(x, &exp2);    /* Get the scale of the number */

    exp10 = (long) (exp2 * log10(FLT_RADIX)); /* Convert exponent to base10 exponent  */

    {        /* Fast way of scaling number */
      const double *p = bit_values;
      long n = exp10;

      if (n < 0) {      /* We have a number smaller than 10 */
        for (n = -n; n; p++, n >>= 1)
          if (n & 1)
            x *= *p;
      } else if (n > 0) {
        double f = 1.0;

        for (; n; p++, n >>= 1)
          if (n & 1)
            f *= *p;
        x /= f;
      }
    }

    while (x >= 1.0L) {
      x *= 0.1L;
      ++exp10;
    }

    while (x < 0.1L) {
      x *= 10.0L;
      --exp10;
    }

    while (precision-- >= 0) {  /* Go to one more than required precision */
      double front;

      x = modf((double) (x * 10.0L), &front);  /* extract left-most digit */

      *digits++ = hxDgit((int) front);
      len++;
    }

    *digits = '\0';    /* Terminate digit string */

    *exp = exp10;
    return len - 1;    /* we generated an extra digit */
  }
}

static int countSignificants(string frmt, long from, long limit, string test) {
  int cx = 0;
  long tLen = uniStrLen(test);
  for (long ix = from; ix < limit;) {
    codePoint ch = nextCodePoint(frmt, &ix, limit);
    if (uniIndexOf(test, tLen, 0, ch) >= 0)
      cx++;
  }
  return cx;
}

static retCode
formatDigits(logical isSigned, string digits, long precision, string format, long fLen, byte *out, long outLen,
             long *outPos);

retCode formattedFloat(double dx, byte *out, long *endPos, long outLen, string frmt, long formatLen) {
  logical isSigned = False;

  if (dx < 0) {
    isSigned = True;
    dx = -dx;
  }

  long dotPos = uniIndexOf(frmt, formatLen, 0, '.');
  if (dotPos < 0)
    return Error;
  else {
    long ePos = uniIndexOf(frmt, formatLen, 0, 'e');
    if (ePos < 0)
      ePos = uniIndexOf(frmt, formatLen, 0, 'E');

    int beforePeriod = countSignificants(frmt, 0, dotPos, (string) "09 ");
    int afterPeriod = countSignificants(frmt, dotPos, ePos >= 0 ? ePos : formatLen, (string) "09 ");
    int precision = beforePeriod + afterPeriod;
    byte digits[MAXFILELEN];
    long exp10;

    int len = number2Str(dx, precision, digits, &exp10);

    exp10 -= beforePeriod;

    if (ePos < 0) {
      // no space for an exponent
      if (exp10 > beforePeriod || exp10 + afterPeriod < -len)
        return Error;
      else if (exp10 < 0)
        return formatDigits(isSigned, digits, precision + exp10, frmt, formatLen, out, outLen, endPos);
      else
        return formatDigits(isSigned, digits, precision, frmt, formatLen, out, outLen, endPos);
    } else {
      byte mnBf[128], expBf[128];
      long mnLn, expLn;

      formatDigits(isSigned, digits, len, frmt, ePos, mnBf, NumberOf(mnBf), &mnLn);

      expLn = int2StrByBase(expBf, exp10, 0, 10);

      *endPos = 0;
      uniAppend(out, endPos, outLen, mnBf);
      appendCodePoint(out, endPos, outLen, frmt[ePos]);
      return uniNAppend(out, endPos, outLen, expBf, expLn);
    }
  }
}

retCode formattedLong(integer ix, byte *out, long *endPos, long outLen, string frmt, long formatLen) {
  byte digits[256];
  uint16 base = (uint16) (uniIndexOf(frmt, formatLen, 0, 'X') >= 0 ? 16 : 10);
  logical isSigned = False;
  if (ix < 0) {
    isSigned = True;
    ix = -ix;
  }
  long len = natural2StrByBase(digits, (uinteger) ix, 0, base);

  return formatDigits(isSigned, digits, len, frmt, formatLen, out, outLen, endPos);
}

#define attachChar(O, P, L, Ch) do{ if((*P)>=L) return Error; else O[(*P)++] = Ch; } while(False)

retCode
formatDigits(logical isSigned, string digits, long precision, string format, long formatLen, byte *out, long outLen,
             long *pos) {
  int formSigDigits = countSignificants(format, 0, formatLen, (string) "09X ");
  logical encounteredSign = False;
  int zeroDigits = countSignificants(format, 0, formatLen, (string) "0 ");

  if (precision > formSigDigits)
    return Error;

  *pos = 0;

  for (long ix = formatLen - 1, px = precision - 1; ix >= 0; ix--) {
    int formChar = format[ix];
    switch (formChar) {
      case '-':
        if (isSigned)
          attachChar(out, pos, outLen, '-');
        else
          attachChar(out, pos, outLen, ' ');
        break;
      case '+':
        if (isSigned)
          attachChar(out, pos, outLen, '+');
        else
          attachChar(out, pos, outLen, ' ');
        break;
      case 'P':
        if (isSigned) {
          if (encounteredSign)
            attachChar(out, pos, outLen, '(');
          else {
            attachChar(out, pos, outLen, ')');
            encounteredSign = True;
          }
        } else
          attachChar(out, pos, outLen, ' ');

        break;
      case '.':
        if (px >= 0 || zeroDigits > 0)
          attachChar(out, pos, outLen, formChar);
        break;
      case ',':
      default:
        if (px >= 0 || zeroDigits > 0)
          attachChar(out, pos, outLen, formChar);
        break;
      case ' ':
        if (px >= 0) { // more of the raw result to write out
          attachChar(out, pos, outLen, digits[px]);
          px--;
        } else if (zeroDigits > 0)
          attachChar(out, pos, outLen, ' ');

        zeroDigits--;
        break;
      case '0':
        if (px >= 0) { // more of the raw result to write out
          attachChar(out, pos, outLen, digits[px]);
          px--;
        } else if (zeroDigits > 0)
          attachChar(out, pos, outLen, '0');

        zeroDigits--;
        break;
      case '9':
      case 'X':
        if (px >= 0) { // more of the raw result to write out
          attachChar(out, pos, outLen, digits[px]);
          px--;
        }
        break;
      case 'e':
      case 'E':
      case 'L':
      case 'R':
        return Error;
    }
  }

  return uniReverse(out, *pos);
}

retCode formatDouble(byte *out, long outLen, double x, FloatDisplayMode displayMode, int precision, string prefix,
                     logical sign) {
  byte dec[DBL_DIG * 2];    /* buffer for the decimal mantissae */
  byte *d = dec;
  byte buff[1024];    /* buffer to represent the number string */
  byte *p = buff;
  byte *eP = &buff[NumberOf(buff) - 1]; /* end marker */

  long exp, len, sig;

  if (x < 0) {
    prefix = (string) "-";
    x = -x;
  } else if (sign)
    prefix = (string) "+";      /* Is the number signed? */

  len = sig = number2Str(x, DBL_DIG + 1, dec, &exp);

  while (sig > 0 && dec[sig - 1] == '0')
    sig--;      /* chop off trailing zeroes */

  if (uniIsLit((string) dec, "Infinity") == same) {
    uniCpy(out, outLen, dec);
    return Ok;
  } else {
    if (displayMode == general) {
      if (exp < -3 || (precision == 0 ? exp > DBL_DIG : exp > sig + 1))
        displayMode = scientific;
      else
        displayMode = fractional;
    }
    switch (displayMode) {
      case scientific: {
        *p++ = *d++; /* use scientific format */
        len--;
        sig--;
        *p++ = '.';
        if (precision > 0) {
          while (precision-- > 0)
            if (len-- > 0)
              *p++ = *d++;
            else
              *p++ = *d;    /* trailing zero */
        } else if (precision == 0 && len >= 0) {
          if (sig > 0) {
            while (sig-- > 0)
              *p++ = *d++;
          } else
            *p++ = '0';
        } else             /* ensure that we have the .0 trailing */
          *p++ = '0';

        *p++ = 'E';      /* Show exponent sign */
        if (--exp < 0) {
          *p++ = '-';
          exp = -exp;
        }
        p += natural2StrByBase(p, (uinteger) exp, 0, 10);/* Show exponent value -- adjusted for leading digit*/
        *p = '\0';
        break;
      }
      case fractional:
      default:
        if (exp <= 0) {    /* Use fixed point format */
          int prec = precision;

          *p++ = '0';
          *p++ = '.';

          if (precision == 0)
            while (p < eP && exp++ < 0)
              *p++ = '0';
          else
            while (precision > 0 && p < eP && exp < 0) {
              *p++ = '0';
              precision--;
              exp++;
            }

          if (prec != 0) {
            while (p < eP && precision > 0) {
              if (len-- > 0)
                *p++ = *d++;
              else
                *p++ = '0';
              precision--;
            }
          } else {      /* display all available digits */
            if (sig > 0) {
              while (p < eP && sig-- > 0)
                *p++ = *d++;
            } else
              *p++ = '0';      /* 0.0 */
          }
          *p = '\0';
        } else {
          while (p < eP && exp-- > 0)
            if (len-- > 0) {
              *p++ = *d++;
              sig--;
            } else
              *p++ = *d;

          if (p < eP && precision > 0) {
            *p++ = '.';
            while (p < eP && precision > 0) {  /* copy out the fractional part */
              if (len-- > 0)
                *p++ = *d++;
              else
                *p++ = '0';
              precision--;
            }
          } else if (p < eP && precision == 0) {
            *p++ = '.';
            if (sig > 0) {
              while (p < eP && sig-- > 0)  /* copy out the fractional part */
                *p++ = *d++;
            } else {        /* ensure that we have the .0 trailing */
              *p++ = '0';
            }
          }
          *p = '\0';
        }
    }

    uniCpy(out, outLen, prefix);
    long pos = uniStrLen(prefix);
    return uniAppend(out, &pos, outLen, buff);
  }
}

retCode outDouble(ioPo out, double x, char mode, int width, int precision,
                  codePoint pad, logical left, string prefix, logical sign) {
  byte buffer[256];

  FloatDisplayMode displayMode;

  switch (mode) {
    case 'g':
      displayMode = general;
      break;
    case 'e':
      displayMode = scientific;
      break;
    default:
      displayMode = general;
  }

  retCode ret = formatDouble(buffer, NumberOf(buffer), x, displayMode, precision, prefix, sign);

  if (ret == Ok)
    return outString(out, buffer, (int) uniStrLen(buffer), width, (int) uniStrLen(buffer), pad, left);
  return ret;
}

retCode outFloat(ioPo out, double x) {
  return outDouble(out, x, 'g', 0, 0, ' ', True, (string) "", False);
}

retCode outUStr(ioPo f, string str) {
  return outText(f, str, uniStrLen(str));
}

retCode outString(ioPo f, byte *str, long len, long width, int precision,
                  codePoint pad, logical leftPad) {
  long gaps;
  retCode ret = Ok;

  lock(O_LOCKED(f));

  if (precision > 0 && precision < len)
    len = precision;    /* we only show part of the string */

  if (width > 0) {      /* fixed width */
    if (len > width)
      len = width;    /* never print more than available width */

    if (!leftPad) {    /* right justified */
      gaps = width - len;

      ret = outText(f, str, len);

      while (ret == Ok && gaps-- > 0)
        ret = outChar(f, pad);
    } else {
      gaps = width - len;

      while (ret == Ok && gaps-- > 0)
        ret = outChar(f, pad);

      if (ret == Ok)
        ret = outText(f, str, len);
    }
  } else
    ret = outText(f, str, len);

  unlock(O_LOCKED(f));
  return ret;
}

static retCode quoteChar(ioPo f, codePoint ch, long *gaps) {
  retCode ret;
  switch (ch) {
    case '\a':
      ret = outStr(f, "\\a");
      (*gaps)--;               // An additional character
      break;
    case '\b':
      ret = outStr(f, "\\b");
      (*gaps)--;
      break;
    case '\x7f':
      ret = outStr(f, "\\d");
      (*gaps)--;
      break;
    case '\x1b':
      ret = outStr(f, "\\e");
      (*gaps)--;
      break;
    case '\f':
      ret = outStr(f, "\\f");
      (*gaps)--;
      break;
    case '\n':
      ret = outStr(f, "\\n");
      (*gaps)--;
      break;
    case '\r':
      ret = outStr(f, "\\r");
      (*gaps)--;
      break;
    case '\t':
      ret = outStr(f, "\\t");
      (*gaps)--;
      break;
    case '\v':
      ret = outStr(f, "\\v");
      break;
    case '\\':
      ret = outStr(f, "\\\\");
      (*gaps)--;
      break;
    case '\"':
      ret = outStr(f, "\\\"");
      (*gaps)--;
      break;
    default:
      if (ch < ' ') {
        ret = outChar(f, '\\');
        if (ret == Ok)
          ret = outChar(f, ((ch >> 6) & 3) | '0');
        if (ret == Ok)
          ret = outChar(f, ((ch >> 3) & 7) | '0');
        if (ret == Ok)
          ret = outChar(f, (ch & 7) | '0');
        (*gaps) -= 4;
      } else if (ch > 255) {
        ret = outStr(f, "\\+");
        if (ret == Ok)
          ret = outChar(f, hxDgit((ch >> 12) & 0xf));
        if (ret == Ok)
          ret = outChar(f, hxDgit((ch >> 8) & 0xf));
        if (ret == Ok)
          ret = outChar(f, hxDgit((ch >> 4) & 0xf));
        if (ret == Ok)
          ret = outChar(f, hxDgit(ch & 0xf));
        if (ret == Ok)
          ret = outChar(f, ';');
        (*gaps) -= 6;
      } else
        ret = outChar(f, ch);
  }
  return ret;
}

static retCode dumpText(ioPo f, string str, long len) {
  long gaps = 0;
  retCode ret = Ok;
  int ix;
  for (ix = 0; ret == Ok && ix < len; ix++)
    ret = quoteChar(f, str[ix], &gaps);
  return ret;
}

retCode outUniString(ioPo f, string str, long len, long width, int precision,
                     codePoint pad, logical leftPad, logical alt) {
  long gaps;
  retCode ret = Ok;

  lock(O_LOCKED(f));

  if (precision > 0 && precision < len)
    len = precision;    /* we only show part of the string */

  if (width > 0) {      /* fixed width */
    if (len > width)
      len = width;    /* never print more than available width */

    if (!leftPad) {    /* right justified */
      gaps = width - len;

      if (alt) {
        while (ret == Ok && len-- > 0) {
          byte ch = *str++;
          quoteChar(f, ch, &gaps);
        }
      } else
        ret = outText(f, str, len);

      while (ret == Ok && gaps-- > 0)
        ret = outChar(f, pad);
    } else {
      gaps = width - len;

      while (ret == Ok && gaps-- > 0)
        ret = outChar(f, pad);
      if (ret == Ok)
        ret = outText(f, str, len);
    }
  } else if (alt)
    ret = dumpText(f, str, len);
  else
    ret = outText(f, str, len);  /* variable width */
  unlock(O_LOCKED(f));

  return ret;
}

/**********************************************************************/
/*               Display a formatted message                          */
/**********************************************************************/

static fileMsgProc procs[256];

/* What to invoke... */

static void initMsgProcs(void) {
  static logical inited = False;

  if (!inited) {
    int i;
    for (i = 0; i < 255; i++)
      procs[i] = NULL;
    inited = True;
  }
}

void installMsgProc(char key, fileMsgProc proc) {
  initMsgProcs();
  procs[(unsigned int) key] = proc;
}

/* We have our own version of fprintf too */

/* This one is used in april-log_msg */
retCode __voutMsg(ioPo f, unsigned char *fmt, va_list args) {
  retCode ret = Ok;

  while (ret == Ok && *fmt != '\0') {
    switch (*fmt) {
      case '%': {
        unsigned char c;
        int width = 0;    /* Maximum width of field */
        int precision = 0;    /* Minimum width or precision of field */
        long depth = LONG_MAX;      /* Maximum depth of structure */
        codePoint pad = ' ';
        string prefix = (string) "";
        logical sign = False;
        logical alternate = False;
        logical leftPad = True;
        logical overridePrecision = False;
        logical longValue = False;

        fmt++;

        while (strchr("0 -#+l", *fmt) != NULL) {
          switch (*fmt++) {
            case '0':
              pad = '0';
              continue;
            case ' ':
              prefix = (string) " ";
              continue;
            case '+':
              sign = True;
              continue;
            case 'l':
              longValue = True;
              continue;
            case '#':
              alternate = True;
              continue;
            case '-':
              leftPad = False;
              continue;
            default:;
          }
        }

        while (isNdChar(c = *fmt++))  /* extract the width field */
          width = width * 10 + (c & 0xf);

        while (strchr(".,", (char) c) != NULL) {
          if (c == '.') {    /* We have a precision ... */
            overridePrecision = True;
            while (isNdChar(c = *fmt++))
              precision = precision * 10 + (c & 0xf);
          } else if (c == ',') {
            depth = 0;
            while (isNdChar(c = *fmt++))
              depth = depth * 10 + (c & 0xf);
          } else
            break;
        }

        if (procs[c] != NULL) {
          void *data = (void *) va_arg(args, void*); /* pick up a special value */
          ret = procs[(unsigned int) c](f, data, depth, precision, alternate);
        } else
          switch (c) {
            case '_':
              ret = flushFile(f);
              break;
            case 'c': {    /* Display an integer value as a char */
              codePoint i = (codePoint) (longValue ? va_arg(args, integer) : va_arg(args, int));

              ret = outChar(f, i);
              break;
            }
            case 'd': {    /* Display an integer value */
              integer i = (integer) (longValue ? va_arg(args, integer) : va_arg(args, int));

              ret = outInteger(f, i, 10, width, precision, pad, leftPad, prefix, sign);
              break;
            }
            case 'u': {    /* Display a number as unsigned */
              uinteger i = (uinteger) (longValue ? va_arg(args, uinteger) : va_arg(args, unsigned int));
              byte iBuff[64];

              if (!leftPad)
                pad = ' ';    /* We dont put trailing zeroes */

              long len = natural2StrByBase(iBuff, i, 0, 10);

              ret = outUStr(f, prefix);
              if (ret == Ok)
                ret = outString(f, iBuff, (int) len, width, precision, pad, leftPad);
              break;
            }
            case 'o': {    /* Display an octal value */
              integer i = (integer) (longValue ? va_arg(args, integer) : va_arg(args, long));

              ret = outOctal(f, i, width, precision, pad, leftPad, prefix, sign, alternate);
              break;
            }
            case 'x': {    /* Display a hex value */
              integer i = (integer) (longValue ? va_arg(args, integer) : va_arg(args, long));

              ret = outHex(f, i, width, precision, pad, leftPad, prefix, sign, alternate);
              break;
            }
            case 'b': {    /* Display a binary value */
              integer i = (integer) (longValue ? va_arg(args, integer) : va_arg(args, long));

              ret = outInteger(f, i, 2, width, precision, pad, leftPad, prefix, sign);
              break;
            }
            case 'g':
            case 'G':
            case 'e':
            case 'E':
            case 'F':
            case 'f': {    /* Display floating point number */
              double num = (double) va_arg(args, double);

              if (!overridePrecision) /* default precision for floats */
                precision = 6;
              ret = outDouble(f, num, c, width, precision, pad, leftPad, prefix, sign);
              break;
            }
            case 's': {    /* Display a string */
              string str = (string) va_arg(args, string);

              if (str != NULL)
                ret = outString(f, str, uniStrLen(str), width, precision, ' ', leftPad);
              else
                ret = outStr(f, "(NULL)");
              break;
            }

            case 'S': {    /* Display a data block */
              long len = (long) va_arg(args, long);
              char *str = (char *) va_arg(args, char *);

              if (str != NULL) {
                int i;

                for (i = 0; ret == Ok && i < len; i++)
                  if (isprint((unsigned char) str[i]))
                    ret = outChar(f, (codePoint) str[i]);
                  else
                    ret = outMsg(f, "\\%x\\", str[i] & 0xff);
              } else
                ret = outStr(f, "(NULL)");
              break;
            }

            case 'U': {    /* Display a uniCode string */
              string str = (string) va_arg(args, string);

              if (str != NULL) {
                ret = outUStr(f, prefix);
                if (ret == Ok)
                  ret = outUniString(f, str, uniStrLen(str), width, precision, ' ', leftPad, alternate);
              } else
                ret = outStr(f, "(NULL)");
              break;
            }

            default:
              ret = outChar(f, c);
          }
        break;
      }

      default:
        ret = outChar(f, *fmt++);
    }
  }
  return ret;
}

retCode outMsg(ioPo f, char *fmt, ...) {
  if (f != NULL) {
    retCode ret;

    lock(O_LOCKED(f));

    va_list args;    /* access the generic arguments */
    va_start(args, fmt);    /* start the variable argument sequence */

    ret = __voutMsg(f, (unsigned char *) fmt, args);

    va_end(args);

    unlock(O_LOCKED(f));

    return ret;
  } else
    return Error;
}

retCode logMsg(ioPo out, char *fmt, ...) {
  retCode ret = Ok;

  if (out != NULL) {
    time_t now;
    va_list ap;

    lock(O_LOCKED(out));

    va_start(ap, fmt);

    if (time(&now) != -1) {
      struct tm *t = localtime(&now);
      char stamp[256];
      strftime(stamp, 256, "%a %e/%b/%Y %X", t);

      ret = outMsg(out, "%s - ", stamp);
      if (ret == Ok)
        ret = __voutMsg(out, (unsigned char *) fmt, ap);
      if (ret == Ok)
        ret = outMsg(out, "\n");
    } else {
      ret = __voutMsg(out, (unsigned char *) fmt, ap);
      if (ret == Ok)
        ret = outMsg(out, "\n");
    }

    va_end(ap);

    unlock(O_LOCKED(out));
  }
  flushFile(out);
  return ret;
}

string strMsg(string buffer, long len, char *fmt, ...) {
  bufferPo f = fixedStringBuffer(buffer, len);

  va_list args;      /* access the generic arguments */
  va_start(args, fmt);    /* start the variable argument sequence */

  __voutMsg(O_IO(f), (unsigned char *) fmt, args);  /* Display into the string buffer */

  va_end(args);
  outByte(O_IO(f), 0);                /* Terminate the string */

  closeFile(O_IO(f));
  return buffer;
}

retCode ioErrorMsg(objectPo io, char *fmt, ...) {
  va_list args;    /* access the generic arguments */
  va_start(args, fmt);    /* start the variable argument sequence */

  __voutMsg(logFile, (unsigned char *) fmt, args);

  va_end(args);
  return Error;
}
