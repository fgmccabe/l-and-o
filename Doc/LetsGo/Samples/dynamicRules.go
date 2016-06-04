dynamicRules{
  import go.dynamic.
  import meta.

  dynRule[T] <~ { assert:[T,metaTp]*. satisfy:[T]{} }.
  dynRule[T] <~ dynamic[(T,metaTp)].

  dynamicRule:[list[(T,metaTp)]]@>dynRule[T].
  dynamicRule(I) <= dynamic(I).
  dynamicRule(_)..{
    satisfy(X) :-
        mem((X,B)),
        B.satisfy().
    assert(X,B) ->
        add((X,B)).
  }.
}