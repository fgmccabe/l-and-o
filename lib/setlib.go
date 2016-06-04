/*
  Set library to support Go! programs
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

go.setlib{
  (\/):[list[t],list[t]]=>list[t].
  []\/X=>X.
  [E,..X]\/Y::E in Y => X\/Y.
  [E,..X]\/Y => [E,..X\/Y].
  
  (/\):[list[t],list[t]]=>list[t].
  []/\_=>[].
  [E,..X]/\Y::E in Y => [E,..X/\Y].
  [_,..X]/\Y => X/\Y.
  
  (\):[list[t],list[t]]=>list[t].
  X\ [] => X.
  X\ [E,..Y] => remove(X,E)\Y.
  
  diff:[list[t],list[t]]=>list[t].
  diff(X,[]) => X.
  diff(X,[E,..Y]) => remove(X,E)\Y.
  
  private remove:[list[t],t]=>list[t].
  remove([],_) => [].
  remove([E,..X],E) => remove(X,E).
  remove([E,..X],D) => [E,..remove(X,D)].
  
  subset:[list[t],list[t]]{}.
  subset([],_).
  subset([x,..X],S) :- x in S, subset(X,S).

  -- Convert a list into a set
  setof:[list[t]]=>list[t].
  setof(L) => stOf(L,[]).

  private stOf:[list[t],list[t]]=>list[t].
  stOf([],S)=>S.
  stOf([E,..L],S)::E in S => stOf(L,S).
  stOf([E,..L],S) => stOf(L,[E,..S]).

  filterIn:[list[t],t]=>list[t].
  filterIn([],_)=>[].
  filterIn([E,..L],T)::\+(\+E=T) => [E,..filterIn(L,T)]. -- safe equality check
  filterIn([_,..L],T) => filterIn(L,T).

  filterOut:[list[t],t]=>list[t].
  filterOut([],_)=>[].
  filterOut([E,..L],T)::\+(\+E=T) => filterOut(L,T). -- safe equality check
  filterOut([E,..L],T) => [E,..filterOut(L,T)].

  
}
