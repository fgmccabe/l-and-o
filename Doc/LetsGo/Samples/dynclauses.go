/*
 * A Module that implements dynamic clause database for the 
 * meta-evaluator 
 */
 (dynamic_facts,dynamic_clauses)..{ 
  include "sys:go/stdlib.gof".
  include "sys:go/dynamic.gof".
  
  include "meta.gh".
    
  dynamic_facts[T](Init:list[T]) <= dynamic[T](Init).
  dynamic_facts[_](_){
    cls(Hd,TRUE) :- Hd in ext().
  }.

  dynamic_clauses[T](Init:list[clause[T]]) <= dynamic[clause[T]](Init).
  dynamic_clauses[T](_:list[clause[T]]){
    cls(Hd,B) :- mem(cl(Hd,B)).
  }.
}