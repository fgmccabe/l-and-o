/*
  Standard constant definitions for the Go Engine
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _GO_CONSTANTS_H_
#define _GO_CONSTANTS_H_

/* This is used for many internal fixed char buffers */
#ifndef MAX_SYMB_LEN
#define MAX_SYMB_LEN 1024
#endif

/* This is used for some internal stack buffers */
#ifndef MAX_DEPTH
#define MAX_DEPTH 512
#endif

/* This is used for some internal tables */
#ifndef MAX_TABLE
#define MAX_TABLE 2048
#endif

/* This is used for message buffers */
#ifndef MAX_MSG_LEN
#define MAX_MSG_LEN 1024
#endif

#endif
