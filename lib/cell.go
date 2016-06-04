/*
   Standard program for implementing dynamic programs 
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
go.cell{
  cell[T] <~ { get:[]=>T. set:[T]* }.

  cell:[T]@>cell[T].
  cell(I:T)..{
    V:symbol := __term(I).

    get()::__is(V,E) => E.

    set(N) -> sync{ __remove(V); V:=__term(N)}.

    show()::__is(V,E)=>"$"<>E.show().
  }.
}
