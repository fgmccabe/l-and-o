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

go.dynamic{ 

  dynTest[T] <~ { check:[T]{} }.         -- We use this as a more sophisticated match

  dynamic[T] <~ { mem:[T]{}.		 -- Test for membership in the relation
	add:[T]*.			 -- Add element to relation
	del:[T]*.			 -- Delete element from relation
	delc:[dynTest[T]]*.	      -- Delete satisfying element from relation
	delall:[T]*.			 -- Delete all matching elements 
	delallc:[dynTest[T]]*.		 -- Delete all satisfying elements
	ext:[]=>list[T].		 -- pick up extension
	match:[dynTest[T]]=>list[T] }.	 -- pick up matching sub-relation

  dynamic:[list[T]]@>dynamic[T].
  dynamic(Init)..{
    Db:list[symbol] := initDB(Init).

    initDB:[list[t]]=>list[symbol].
    initDB([]) => [].
    initDB([E,..L]) => [__term(E),..initDB(L)].
    initDB(X) => raise error("problem with "<>X.show(),'fail').
       
    mem(P) :- Cl in Db, __is(Cl,P).
       
    add(N) -> sync{ Db := Db<>[__term(N)] }.
       
    del(E) -> sync{ append(F,[Cl,..R],Db), __is(Cl,E) ? Db := F<>R | {} }.

    delc(Tst) -> sync{ append(F,[Cl,..R],Db), __is(Cl,E),Tst.check(E) ? Db := F<>R | {} }.

    delall(E) -> sync{ Db := { Cl .. (Cl::\+__is(Cl,E)) in Db } }.

    delallc(Tst) -> sync{ Db := { Cl .. (Cl::\+(__is(Cl,E),Tst.check(E))) in Db }}.

    ext() => valof{
	       sync{
		 valis { E .. (Cl::__is(Cl,E)) in Db }
	       }
	     }.

    match(K) => valof{
		  sync{
		    valis { E .. (Cl::__is(Cl,E),K.check(E)) in Db }
		  }
		}.
    
    show()=> "{" <> showExt(ext(),"") <> "}".

    showExt:[list[symbol],string]=>string.
    showExt([],_)=>"".
    showExt([E,..Rl],Pre) => Pre<>E.show()<> showExt(Rl,". ").
  }.

}.
