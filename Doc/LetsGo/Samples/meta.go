/*
 * Components of a meta interpreter
 */
meta{
  metaTp <~ { satisfy:[]{} }.

  metaRel[_] <~ metaTp.
  metaRel[T] <~ { assert:[T]* }.

  conj:[list[metaTp]]$=metaTp.
  conj(L)..{
    satisfy() :-
        cnj(L).
    
    cnj:[list[metaTp]]{}.
    cnj([]).
    cnj([G,..R]) :- G.satisfy(), cnj(R).
  }.

  disj:[list[metaTp]]$=metaTp.
  disj(L)..{
    satisfy() :-
        G in L, G.satisfy().
  }.

  not:[metaTp]$=metaTp.
  not(G)..{
    satisfy():-
        \+G.satisfy().
  }.
}