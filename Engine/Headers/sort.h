/*
  Sort function escape
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _Q_SORT_H_
#define _Q_SORT_H_

retCode m_sort(processpo p,objPo *args);
retCode m_setof(processpo p,objPo *args);

int cmpcell(register objPo c1,register objPo c2);

#endif
