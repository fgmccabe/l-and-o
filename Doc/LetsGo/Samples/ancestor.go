ancestor{
  import meta.
  import dynamicRules.
  import parent.

  Ancestor:dynRule[(symbol,symbol)] = dynamicRule([((A,B),parent(A,B)),
						   ((A,C),conj([parent(A,B),
								ancestor(B,C)]))]).

  ancestor:[symbol,symbol]$=metaTp.
  ancestor(A,B)..{
    satisfy() :-
        Ancestor.satisfy((A,B)).
  }.
}