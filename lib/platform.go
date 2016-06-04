/*
  Platform module, gives Go! programs access to the ACS communications infrastructure
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/


connectACS .. {
  include "sys:go/acs.gh".
  include "sys:go/stdlib.gof".
  include "sys:so/io.gof".
  
  
  -- construct an agent name, used if no agent name is given
  generateName() => implode("Goagent"<>now()^0<>":"<>irand(10000000)^0);
  
  buildHndl(hndl(L))::('name',_) in L => hndl(L).
  buildHndl(hndl(L)) => hndl([('name',any(generateName())),..L]).
  
  sameHndl(hndl(L1),hndl(L2)) :-
    ('name',Nm) in L1!,
    ('name',Nm) in L2!.
    
  -- This is not a function 'cos there may not be a valid address
  pickAddress(hndl(L),K,url) :-
    ('locator',any(Lx)) in L,
    locator(K,url,Q) in Lx,
    \+( HoldUntil(T) in Q, now()>T).

  
    
  
  
}
