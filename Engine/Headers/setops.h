/*
  Interface to April set handling functions
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _SETOPS_H_
#define _SETOPS_H_
logical equalcell(register objPo c1,register objPo c2);

/* Escape interface */
retCode m_head(processpo p,objPo *args);
retCode m_front(processpo p,objPo *args);
retCode m_back(processpo p,objPo *args);
retCode m_tail(processpo p,objPo *args);
retCode m_listlen(processpo p,objPo *args);
retCode m_nth(processpo p,objPo *args);
retCode m_union(processpo p,objPo *args);
retCode m_sect(processpo p,objPo *args);
retCode m_diff(processpo p,objPo *args);
retCode m_app(processpo p,objPo *args);
retCode m_iota(processpo p,objPo *args);
retCode m_subset(processpo p,objPo *args);
retCode m_disjoint(processpo p,objPo *args);
retCode m_sort(processpo p,objPo *args);
#endif
