/*
  Miscellaneous functions for the L&O engine
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
#include <math.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>		/* system error numbers */
#include <ctype.h>
#include <string.h>
#include "lo.h"
#include "process.h"

/* General off-line unification */
retCode g__unify(processPo P,ptrPo a)
{
  return equal(P,deRef(&a[1]),deRef(&a[2]));
}

retCode g__match(processPo P,ptrPo a)
{
  return match(P,deRef(&a[1]),deRef(&a[2]));
}

retCode g__identical(processPo P,ptrPo a)
{
  if(identical(deRefI(&a[1]),deRefI(&a[2])))
    return Ok;
  else
    return Fail;
}

retCode g_var(processPo P,ptrPo a)
{
  ptrI xx = deRefI(&a[1]);

  if(isvar(xx) || IsFrozenVar(xx))
    return Ok;
  else
    return Fail;
}

retCode g_nonvar(processPo P,ptrPo a)
{
  ptrI xx = deRefI(&a[1]);

  if(isvar(xx))
    return Fail;
  else
    return Ok;
}

retCode g_ground(processPo P,ptrPo a)
{
  ptrI xx = deRefI(&a[1]);

  if(isGroundTerm(&xx))
    return Ok;
  else
    return Fail;
}

