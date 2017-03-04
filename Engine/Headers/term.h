/*
  Header file giving the interface for reading and writing terms 
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
  
#ifndef _TERM_H_
#define _TERM_H_

retCode outCell(ioPo f,ptrPo x,long depth,int prec,logical alt);
retCode decodeTerm(ioPo in,heapPo P,heapPo R,ptrPo tgt,string errorMsg,long msgSize);
retCode skipEncoded(ioPo in,string errorMsg, long msgLen);
#endif
