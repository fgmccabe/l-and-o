/*
  Time and date functions for the L&O system
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

#include "config.h"		/* pick up standard configuration header */
#include <signal.h>
#include <time.h>
#include <math.h>
#include "lo.h"

/*
 * Implementation of time related escapes 
 */

/* Convert a time to a date:-
   time=>(dow,day,mon,year,hour,min,sec,utc,zone)
*/

#define DATE_DOW 1
#define DATE_DAY 2
#define DATE_MON 3
#define DATE_YEAR 4
#define DATE_HOUR 5
#define DATE_MIN 6
#define DATE_SEC 7
#define DATE_UTC 8
#define DATE_ZONE 9
#define DATE_LEN 9

retCode g__time2date(processPo P, ptrPo a) {
  ptrI T = deRefI(&a[1]);

  if (isvar(T))
    return liberror(P, "_time2date", eINSUFARG);
  else {
    time_t when = (time_t) FloatVal(objV(T));
    double fraction = 0;
    struct tm *now = localtime(&when);
    ptrI val = kvoid;
    retCode ret = Ok;
    rootPo root = gcAddRoot(&P->proc.heap, &val);

    if (isFloat(objV(T))) {
      double ff;
      fraction = modf(floatVal(floatV(T)), &ff);
    }

    val = allocateInteger(&P->proc.heap, now->tm_year + 1900); /* Current year */
    ret = equal(P, &val, &a[DATE_YEAR + 1]);

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_mon + 1); /* Current month */
      ret = equal(P, &val, &a[DATE_MON + 1]);
    }

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_mday); /* Day of the month */
      ret = equal(P, &val, &a[DATE_DAY + 1]);
    }

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_wday);        // Day of the week
      ret = equal(P, &val, &a[DATE_DOW + 1]);
    }

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_hour); /* Hour in the day */
      ret = equal(P, &val, &a[DATE_HOUR + 1]);
    }

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_min); /* Minutes in the hour */
      ret = equal(P, &val, &a[DATE_MIN + 1]);
    }

    if (ret == Ok) {
      val = allocateFloat(&P->proc.heap, now->tm_sec + fraction);
      ret = equal(P, &val, &a[DATE_SEC + 1]);
    }

    if (ret == Ok) {
#ifdef HAVE_TM_ZONE
      val = allocateInteger(&P->proc.heap,now->tm_gmtoff);     // UTC offset
#else
      val = allocateInteger(&P->proc.heap, 0); // dummy
#endif
      ret = equal(P, &val, &a[DATE_UTC + 1]);
    }

    if (ret == Ok) {
#ifdef HAVE_TM_ZONE
      val = allocateCString(&P->proc.heap,now->tm_zone);       // Time zone
#else
      val = allocateInteger(&P->proc.heap, 0); // dummy
#endif
      ret = equal(P, &val, &a[DATE_ZONE + 1]);
    }

    gcRemoveRoot(&P->proc.heap, root);
    return ret;
  }
}

/* Convert a time to a utc date:-
   time=>date(year,month,day,hours,mins,seconds)
*/
retCode g__time2utc(processPo P, ptrPo a) {
  ptrI T = deRefI(&a[1]);

  if (isvar(T))
    return liberror(P, "_time2utc", eINSUFARG);
  else {
    time_t when = (time_t) FloatVal(objV(T));
    double fraction = 0;
    struct tm *now = gmtime(&when);
    ptrI val = kvoid;
    rootPo root = gcAddRoot(&P->proc.heap, &val);
    retCode ret = Ok;

    if (isFloat(objV(T))) {
      double ff;
      fraction = modf(floatVal(floatV(T)), &ff);
    }

    gcAddRoot(&P->proc.heap, &val);

    val = allocateInteger(&P->proc.heap, now->tm_year + 1900); /* Current year */
    ret = equal(P, &val, &a[DATE_YEAR + 1]);

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_mon + 1); /* Current month */
      ret = equal(P, &val, &a[DATE_MON + 1]);
    }

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_mday); /* Day of the month */
      ret = equal(P, &val, &a[DATE_DAY + 1]);
    }

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_wday);        // Day of the week
      ret = equal(P, &val, &a[DATE_DOW + 1]);
    }

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_hour); /* Hour in the day */
      ret = equal(P, &val, &a[DATE_HOUR + 1]);
    }

    if (ret == Ok) {
      val = allocateInteger(&P->proc.heap, now->tm_min); /* Minutes in the hour */
      ret = equal(P, &val, &a[DATE_MIN + 1]);
    }

    if (ret == Ok) {
      val = allocateFloat(&P->proc.heap, now->tm_sec + fraction);
      ret = equal(P, &val, &a[DATE_SEC + 1]);
    }

    if (ret == Ok) {
#ifdef HAVE_TM_ZONE
      val = allocateInteger(&P->proc.heap,now->tm_gmtoff);     // UTC offset
#else
      val = allocateInteger(&P->proc.heap, 0); // dummy
#endif
      ret = equal(P, &val, &a[DATE_UTC + 1]);
    }

    if (ret == Ok) {
#ifdef HAVE_TM_ZONE
      val = allocateCString(&P->proc.heap,now->tm_zone);       // Time zone
#else
      val = allocateInteger(&P->proc.heap, 0); // dummy
#endif
      ret = equal(P, &val, &a[DATE_ZONE + 1]);
    }

    gcRemoveRoot(&P->proc.heap, root);
    return ret;
  }
}


// Construct a time value from a date structure

#define DAY_ARG 1
#define MON_ARG 2
#define YEAR_ARG 3
#define HOUR_ARG 4
#define MIN_ARG 5
#define SEC_ARG 6
#define UTC_ARG 7
#define TIME_ARG 8

retCode g__date2time(processPo P, ptrPo a) {
  struct tm now;
  time_t when;
  ptrI El;
  double fraction = 0.0;

  if (!isvar(El = deRefI(&a[YEAR_ARG])) && isInteger(objV(El)))
    now.tm_year = (int) (integerVal(intV(El)) - 1900); /* Extract the year */
  else
    return liberror(P, "__date2time", eINVAL); /* Invalid inverse */

  if (!isvar(El = deRefI(&a[MON_ARG])) && isInteger(objV(El)))
    now.tm_mon = (int) (integerVal(intV(El)) - 1); /* Extract the month */
  else
    return liberror(P, "__date2time", eINVAL); /* Invalid inverse */

  if (!isvar(El = deRefI(&a[DAY_ARG])) && isInteger(objV(El)))
    now.tm_mday = (int) integerVal(intV(El));          /* Extract the day of the month */
  else
    return liberror(P, "__date2time", eINVAL);              /* Invalid inverse */

  if (!isvar(El = deRefI(&a[HOUR_ARG])) && isInteger(objV(El)))
    now.tm_hour = (int) integerVal(intV(El));           /* Extract the hour */
  else
    return liberror(P, "__date2time", eINVAL);              /* Invalid inverse */

  if (!isvar(El = deRefI(&a[MIN_ARG])) && isInteger(objV(El)))
    now.tm_min = (int) integerVal(intV(El));           /* Extract the minute */
  else
    return liberror(P, "__date2time", eINVAL);              /* Invalid inverse */

  if (!isvar(El = deRefI(&a[SEC_ARG])) && isFloat(objV(El))) {
    double foo;

    fraction = modf(floatVal(floatV(El)), &foo);
    now.tm_sec = (int) foo;
  } else
    return liberror(P, "__date2time", eINVAL);              /* Invalid inverse */

  if (!isvar(El = deRefI(&a[UTC_ARG])) && isInteger(objV(El)))
#ifdef HAVE_TM_ZONE
    now.tm_gmtoff = integerVal(intV(El));           /* Extract the gm offset */
#else
    ;
#endif
  else
    return liberror(P, "__date2time", eINVAL);              /* Invalid inverse */

  now.tm_isdst = -1;                    /* dont know about daylight savings */

  when = mktime(&now);

  {
    ptrI T = allocateFloat(&P->proc.heap, when + fraction);

    return equal(P, &T, &a[TIME_ARG]);
  }
}


// Construct a time value from a date structure

retCode g__utc2time(processPo P, ptrPo a) {
#if HAVE_TIMEGM
  struct tm now;
  time_t when;
  ptrI El;
  double fraction = 0.0;

  if (!isvar(El = deRefI(&a[YEAR_ARG])) && isInteger(objV(El)))
    now.tm_year = (int) (integerVal(intV(El)) - 1900); /* Extract the year */
  else
    return liberror(P, "__utc2time", eINVAL); /* Invalid inverse */

  if (!isvar(El = deRefI(&a[MON_ARG])) && isInteger(objV(El)))
    now.tm_mon = (int) integerVal(intV(El)) - 1; /* Extract the month */
  else
    return liberror(P, "__utc2time", eINVAL); /* Invalid inverse */

  if (!isvar(El = deRefI(&a[DAY_ARG])) && isInteger(objV(El)))
    now.tm_mday = (int) integerVal(intV(El));          /* Extract the day of the month */
  else
    return liberror(P, "__utc2time", eINVAL);              /* Invalid inverse */

  if (!isvar(El = deRefI(&a[HOUR_ARG])) && isInteger(objV(El)))
    now.tm_hour = (int) integerVal(intV(El));           /* Extract the hour */
  else
    return liberror(P, "__utc2time", eINVAL);              /* Invalid inverse */

  if (!isvar(El = deRefI(&a[MIN_ARG])) && isInteger(objV(El)))
    now.tm_min = (int) integerVal(intV(El));           /* Extract the minute */
  else
    return liberror(P, "__utc2time", eINVAL);              /* Invalid inverse */

  if (!isvar(El = deRefI(&a[SEC_ARG])) && isFloat(objV(El))) {
    double foo;

    fraction = modf(floatVal(floatV(El)), &foo);
    now.tm_sec = (int) foo;
  } else
    return liberror(P, "__utc2time", eINVAL);              /* Invalid inverse */

  if (!isvar(El = deRefI(&a[UTC_ARG])) && isInteger(objV(El)))
#ifdef HAVE_TM_ZONE
    now.tm_gmtoff = integerVal(intV(El));           /* Extract the minute */
#else
    ;
#endif
  else
    return liberror(P, "__utc2time", eINVAL);              /* Invalid inverse */

  now.tm_isdst = -1;                    /* dont know about daylight savings */

  when = timegm(&now);

  {
    ptrI T = allocateFloat(&P->proc.heap, when + fraction);

    return equal(P, &T, &a[TIME_ARG]);
  }
#else
  return liberror(P,"__utc2time",eNOTFND);
#endif
}
