/*
  Number handling functions for L&O
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"
#include <string.h>		/* Access string defs */
#include <stdlib.h>		/* Memory allocation etc. */
#include <assert.h>		/* Run-time predicate verification */

#include "lo.h"
#include "heap.h"
#include "floats.h"

ptrI permInteger(integer i)
{
  return allocateInteger(&globalHeap,i);
}

ptrI permFloat(double f)
{
  return allocateFloat(&globalHeap,f);
}

