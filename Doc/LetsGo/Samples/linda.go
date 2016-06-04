/*
 * lind dynamic relations class 
 */
 
(linda)..{
  include "sys:go/dynamic.gof".
  
  linda[T](Ext:list[T]) <= dynamic[T](Ext). 
  linda[_](_){
--    replace(Trm1,Trm2) -> 
--	sync{del(Trm1);add(Trm2)}.
--    delw(Term) ->
--	sync{ mem(Term) -> del(Term)}.
--    memw(Term) :-
--	action {sync{ mem(Term) -> {} }}.
    not(Term) :-
        action {sync{ \+ mem(Term) -> {}}}
  }.
}.
