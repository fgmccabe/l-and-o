/* A simple meta-evaluator module 
*/
eval{
  import go.io.
  import go.meta.

  eval[T] <~ {
        holds:(metaQuery[T]){}
      }.

  evaluate:[metaProg[T]]$=eval[T].
  evaluate(axset)..{
    holds(TRUE):--true.
    holds(is(Call)):-- axset.cls(Call,Cont),holds(Cont).
    holds(isnot(Call)):-- \+ (axset.cls(Call,Cont),holds(Cont)).
    holds(not(Q)) :-- \+ holds(Q).
    holds(conj(L)):--evalconj(L).
    holds(disj(L)):--evaldisj(L).    	
    
    evalconj([]).
    evalconj([Call,..Calls]) :- holds(Call),evalconj(Calls).
    
    evaldisj([E,.._]) :- holds(E).
    evaldisj([_,..L]) :- evaldisj(L).
  }.
}
