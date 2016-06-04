/*
  A queue package
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

go.queue{
  queue[T] <~ {
	front:[]=>T. 
	push:[T]*. 
	pull:[-T]*. 
	depth:[]=>number. 
	queue:[]=>list[T]}.
                                                             
  queue:[list[T]]@>queue[T].
  queue(Init:list[T])..{
    Q:list[T] := [].

    push(E) -> sync{ Q:= Q<>[E] }.
    pull(E) -> sync{ [E,..R] = Q;
                     Q := R }.

    front() => valof{
		 Q = [H,.._];
		 valis H
	       }.

    queue() => Q.

    depth() => listlen(Q).

    show() => "{"<>showQueue(Q,"")<>"}".

    showQueue:[list[T],string]=>string.
    showQueue([],_) => [].
    showQueue([El,..M],Sep) => Sep<>El.show()<>showQueue(M,", ").
  }
}